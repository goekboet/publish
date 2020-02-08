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
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Publish.HtmlViews
open System.Threading.Tasks
open Serilog
open Serilog.Formatting.Elasticsearch
open Microsoft.AspNetCore.Authentication.OpenIdConnect
open Microsoft.IdentityModel.Logging
open Giraffe.GiraffeViewEngine
open System.Security.Cryptography.X509Certificates
open System.IO
open Microsoft.AspNetCore.Antiforgery


// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : Serilog.ILogger) =
    // logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

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
                ctx.TryGetQueryStringValue "returnurl"
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
                ctx.TryGetQueryStringValue "returnurl"
                |> Option.defaultValue "/"
        
        if ctx.User.Identity.IsAuthenticated
        then signOut oidcScheme next ctx
        else redirectTo false returnUrl next ctx

let parsingErrorHandler err = RequestErrors.BAD_REQUEST err

let withAntiforgery (form : string -> bool -> XmlNode) : HttpHandler=
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let af = ctx.GetService<Microsoft.AspNetCore.Antiforgery.IAntiforgery>()
        let issue = af.GetAndStoreTokens ctx
        let loggedIn = ctx.User.Identity.IsAuthenticated

        htmlView (form issue.RequestToken loggedIn) next ctx

let webApp =
    choose [
        GET >=>
            choose [
                route "/"              >=> (withAntiforgery layout) 
            ]
        POST >=> antiForgeryValidate >=>
            choose [
                route "/login"          >=> loginHandler;
                route "/logout"         >=> logoutHandler
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
       .UseGiraffe webApp

let configureOidc
    (config : IConfiguration)
    (options : OpenIdConnectOptions)
    =
    config.GetSection("Ids").Bind(options)
    options.ResponseType <- "code"
    options.GetClaimsFromUserInfoEndpoint <- true;
    options.SaveTokens <- true
    options.Scope.Add("offline_access")
    
    options.ClaimActions.MapAllExcept("iss", "nbf", "exp", "aud", "nonce", "iat", "c_hash")
    options.TokenValidationParameters <- 
        TokenValidationParameters (NameClaimType = IdentityModel.JwtClaimTypes.Name, RoleClaimType = IdentityModel.JwtClaimTypes.Role)
        
    
    options.AccessDeniedPath <- PathString("/");
    options.Events.OnSignedOutCallbackRedirect <- 
        fun ctx -> ctx.HttpContext.SignOutAsync(
                      cookieScheme)
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

let configureServices (services : IServiceCollection) =
    let serviceProvider = services.BuildServiceProvider()
    let config = serviceProvider.GetService<IConfiguration>()

    services
        .AddGiraffe()
        // .Configure<ForwardedHeadersOptions>(fun (opts : ForwardedHeadersOptions) ->
        //     opts.KnownNetworks.Clear()
        //     opts.KnownProxies.Clear())
        .AddAntiforgery(fun opts -> 
            opts.Cookie.HttpOnly     <- true
            opts.Cookie.SameSite     <- SameSiteMode.Strict
            opts.Cookie.Name         <- "anticsrf"
            )
        .AddAuthentication(authScheme)
        .AddCookie(cookieScheme, Action<CookieAuthenticationOptions>(cookieAuth)) 
        .AddOpenIdConnect(oidcScheme, Action<OpenIdConnectOptions>(configureOidc config))
        |> ignore

    configureDataProtection config services



let configureLogging
    (host : WebHostBuilderContext)
    (logging : LoggerConfiguration)
    : Unit
    =
    IdentityModelEventSource.ShowPII <- true
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