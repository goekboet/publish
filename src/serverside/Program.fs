module Publish.App

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.DataProtection
open Microsoft.Extensions.Configuration
open Microsoft.IdentityModel.Tokens
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Publish.SpaRoute
open Publish.Models
open System.Threading.Tasks
open Serilog
open Serilog.Formatting.Elasticsearch
open Microsoft.AspNetCore.Authentication.OpenIdConnect
open Microsoft.IdentityModel.Logging
open Giraffe.GiraffeViewEngine
open System.Security.Cryptography.X509Certificates
open System.IO
open System.Net.Http
open IdentityModel.Client
open Microsoft.Extensions.Options
open System.Security.Claims
open Publish.Api
open Publish.RefreshToken
open Utf8Json


// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : Microsoft.Extensions.Logging.ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse
    >=> ServerErrors.INTERNAL_ERROR ex.Message

// ---------------------------------
// Web app
// ---------------------------------
let cookieScheme = "Cookies"
let oidcScheme = "Ids"

let authScheme 
    (options : AuthenticationOptions) 
    = 
    options.DefaultScheme <- cookieScheme;
    options.DefaultChallengeScheme <- oidcScheme;
    ()


    

let cookieAuth (o : CookieAuthenticationOptions) =
    do
        o.Cookie.HttpOnly     <- true
        o.Cookie.SameSite     <- SameSiteMode.Lax
        o.Cookie.Name         <- "auth"
        o.Cookie.SecurePolicy <- CookieSecurePolicy.SameAsRequest
        o.SlidingExpiration   <- true
        o.ExpireTimeSpan      <- TimeSpan.FromDays 7.0
        o.Events.OnValidatePrincipal <- Func<CookieValidatePrincipalContext, Task>(refreshTokensIfStale)
        o.Events.OnSigningOut <- Func<CookieSigningOutContext, Task>(revokeTokensInCookieProperties)

let finishEarly : HttpFunc = Some >> Task.FromResult

let antiForgeryValidate : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) -> task {
        let af = ctx.GetService<Microsoft.AspNetCore.Antiforgery.IAntiforgery>()
        let! isValid = af.IsRequestValidAsync ctx  
        if isValid then 
            return! next ctx
        else
            return! Task.FromResult None         
    }

let loginHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let returnUrl = 
                ctx.TryGetQueryStringValue "sparoute"
                |> Option.defaultValue "/"

            if ctx.User.Identity.IsAuthenticated
            then 
                do! ctx.Response.Redirect(returnUrl) |> Task.FromResult
            else
                do! ctx.ChallengeAsync(oidcScheme, AuthenticationProperties(RedirectUri = returnUrl))

            return! next ctx
        } 

let logoutHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let returnUrl = 
                ctx.TryGetQueryStringValue "sparoute"
                |> Option.defaultValue "/"
        
        if ctx.User.Identity.IsAuthenticated
        then (signOut cookieScheme >=> signOut oidcScheme) next ctx
        else redirectTo false returnUrl next ctx



let fromClaims (principal : ClaimsPrincipal)
    =
    let name = Seq.tryFind (fun (x : Claim) -> x.Type = "broker.host.name") principal.Claims
    let handle = Seq.tryFind (fun (x : Claim) -> x.Type = "broker.host.handle") principal.Claims

    Option.map2 
        (fun (n : Claim) (h : Claim) -> { name = n.Value; handle = h.Value }) 
        name 
        handle



let parsingErrorHandler err = RequestErrors.BAD_REQUEST err

let withAntiforgery (form : string -> string option -> Publishername option -> XmlNode) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let af = ctx.GetService<Microsoft.AspNetCore.Antiforgery.IAntiforgery>()
        let issue = af.GetAndStoreTokens ctx
        let session = 
            if ctx.User.Identity.IsAuthenticated
            then Some ctx.User.Identity.Name
            else None

        let hostName = fromClaims ctx.User

        htmlView (form issue.RequestToken session hostName) next ctx

let webApp =
    choose [
        GET >=> 
            choose [
                route "/api/times"       >=> mustBeLoggedIn >=> requiresRegisteredPublisher >=> listTimes;
                withAntiforgery layout
            ] 
            
        POST >=>
            choose [
                route "/api/publisher"   >=> mustBeLoggedIn >=> addPublisher; 
                route "/api/times"       >=> mustBeLoggedIn >=> requiresRegisteredPublisher >=> addTime;
                route "/login"           >=> antiForgeryValidate >=> loginHandler;
                route "/logout"          >=> antiForgeryValidate >=> logoutHandler
            ]
        RequestErrors.notFound (text "Not Found") ]



// ---------------------------------
// Main
// ---------------------------------



let configureApp (app : IApplicationBuilder) 
    =
    app.UseStaticFiles()
       .UseAuthentication()
       .UseResponseCaching()
       .UseGiraffeErrorHandler(errorHandler)
       .UseGiraffe webApp

let signFirstHostnameForUser 
    (ctx : TicketReceivedContext)
    =
    task {
        let httpClientFactory = ctx.HttpContext.GetService<IHttpClientFactory>()
        let accessToken = ctx.Properties.GetTokenValue("access_token")
        let http = httpClientFactory.CreateClient("BrokerClient")
        http.SetBearerToken(accessToken) |> ignore

        match! getRegisteredPublishername http with
            | Some hn ->
                let claims = toClaims hn
                let brokerHostname = ClaimsIdentity claims 
                ctx.Principal.AddIdentity brokerHostname
                
                return! Task.CompletedTask
            | _         -> 
                return! Task.CompletedTask
    } :> Task

let configureOidc
    (config : IConfiguration)
    (options : OpenIdConnectOptions)
    =
    config.GetSection("Ids").Bind(options)
    options.ResponseType <- "code"
    options.GetClaimsFromUserInfoEndpoint <- true;
    options.SaveTokens <- true
    options.Scope.Add("offline_access")
    options.Scope.Add("publish")
    options.ClaimActions.MapAllExcept("iss", "nbf", "exp", "aud", "nonce", "iat", "c_hash")
    options.TokenValidationParameters <- 
        TokenValidationParameters (NameClaimType = IdentityModel.JwtClaimTypes.Name, RoleClaimType = IdentityModel.JwtClaimTypes.Role)
        
    
    options.AccessDeniedPath <- PathString("/");
    options.Events.OnTicketReceived <- Func<TicketReceivedContext, Task>(signFirstHostnameForUser)
          
    ()
    
let configureDataProtection
    (conf : IConfiguration)
    (services : IServiceCollection)
    =
    match conf.["Dataprotection:Type"] with
        | "Docker" ->
            services
                .AddDataProtection()
                .PersistKeysToFileSystem(
                    DirectoryInfo(conf.["Dataprotection:KeyPath"])
                )
                .ProtectKeysWithCertificate(
                    new X509Certificate2(
                        conf.["Dataprotection:CertPath"],
                        conf.["Dataprotection:CertPass"]
                    )
                ) |> ignore
        | _ -> services.AddDataProtection() |> ignore

let brokerClient 
    (config : IConfiguration) 
    (client : HttpClient ) = 
    let baseAddress = config.["BrokerClient:BackChannel"]
    client.BaseAddress <- Uri baseAddress
    () 

let idsClient 
    (config : IConfiguration) 
    (client : HttpClient ) = 
    let baseAddress = config.["Ids:Authority"]
    client.BaseAddress <- Uri baseAddress
    () 

let configureServices (services : IServiceCollection) =
    let serviceProvider = services.BuildServiceProvider()
    let config = serviceProvider.GetService<IConfiguration>()

    services
        .AddGiraffe()
        .AddAntiforgery(fun opts -> 
            opts.Cookie.HttpOnly     <- true
            opts.Cookie.SameSite     <- SameSiteMode.Strict
            opts.Cookie.Name         <- "anticsrf"
            )
        .AddAuthentication(authScheme)
        .AddCookie(cookieScheme, Action<CookieAuthenticationOptions>(cookieAuth)) 
        .AddOpenIdConnect(oidcScheme, Action<OpenIdConnectOptions>(configureOidc config))
        |> ignore
    
    services
        .AddHttpClient("BrokerClient", Action<HttpClient>(brokerClient config))
        |> ignore

    services.AddHttpClient("IdsClient") |> ignore

    configureDataProtection config services

let configureLogging
    (host : WebHostBuilderContext)
    (logging : LoggerConfiguration)
    : Unit
    =
    IdentityModelEventSource.ShowPII <- true
    logging.ReadFrom.Configuration(host.Configuration) |> ignore
    match host.Configuration.["Serilog:Configuration"] with
    | "Console" -> 
        logging.WriteTo.Console() |> ignore
        ()
    | "StdOutJson" -> 
        logging.WriteTo.Console(ElasticsearchJsonFormatter()) |> ignore
        ()
    | _ -> failwith "Valid values for Serilog configuration is Console or StdOutJson"

[<EntryPoint>]
let main _ =
    WebHost.CreateDefaultBuilder()
        .UseSerilog(configureLogging)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .Build()
        .Run()
    0

    