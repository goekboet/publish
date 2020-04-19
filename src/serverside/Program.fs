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
//open Publish.RefreshTokenHandling
open System.Threading.Tasks
open Serilog
open Serilog.Formatting.Elasticsearch
open Microsoft.AspNetCore.Authentication.OpenIdConnect
open Microsoft.IdentityModel.Logging
open Giraffe.GiraffeViewEngine
open System.Security.Cryptography.X509Certificates
open System.IO
open Microsoft.AspNetCore.Antiforgery
open System.Net.Http
open System.Globalization
open IdentityModel.Client
open Microsoft.Extensions.Options
open System.Threading
open Microsoft.IdentityModel.Protocols.OpenIdConnect


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

type AccessTokenFreshness =
    Fresh of TimeSpan
    | Stale of string
    | Unavailiable

let getTokenExpiration (ts : AuthenticationToken list)= 
    List.tryFind (fun (x : AuthenticationToken) -> x.Name = "expires_at") ts
    |> Option.map (fun x -> DateTimeOffset.Parse(x.Value, CultureInfo.InvariantCulture))

let getRefreshtoken (ts : AuthenticationToken list) = 
    List.tryFind (fun (x : AuthenticationToken) -> x.Name = "refresh_token") ts
    |> Option.map (fun x -> x.Value)

let getAccesstoken (ts : AuthenticationToken list) = 
    List.tryFind (fun (x : AuthenticationToken) -> x.Name = "access_token") ts
    |> Option.map (fun x -> x.Value)
    
let getAccesstokenFreshness 
    (ts : AuthenticationToken list)
    (now : DateTimeOffset) =
    getTokenExpiration ts
    |> Option.map (fun x -> x - now.AddMinutes(5.0))
    |> Option.bind (fun x -> 
        if x > TimeSpan.Zero 
        then Some (Fresh x)
        else getRefreshtoken ts |> Option.map Stale)
    |> Option.defaultValue Unavailiable

let useRefreshToken 
    (http : HttpClient)
    (oidcOpts : OpenIdConnectOptions)
    (refreshToken : string)
    (now : DateTimeOffset) =
    task {
        let! conf = oidcOpts.ConfigurationManager.GetConfigurationAsync(CancellationToken.None)
        let! r = 
            http.RequestRefreshTokenAsync ( new RefreshTokenRequest
                ( Address = conf.TokenEndpoint,
                  ClientId = oidcOpts.ClientId,
                  ClientSecret = oidcOpts.ClientSecret,
                  RefreshToken = refreshToken
                ))
        if r.IsError
        then return Error (r.ErrorDescription)
        else
            let expiresat = now.AddSeconds(float r.ExpiresIn).ToString("o", CultureInfo.InvariantCulture)
            return Ok ([ AuthenticationToken (Name = "access_token", Value = r.AccessToken)
                        ; AuthenticationToken (Name = "refresh_token", Value = r.RefreshToken)
                        ; AuthenticationToken (Name = "expires_at", Value = expiresat)])
    }

let refreshTokensIfStale 
    (ctx : CookieValidatePrincipalContext) =
    task {
        let now = ctx.HttpContext.GetService<ISystemClock>().UtcNow
        let httpClientFactory = ctx.HttpContext.GetService<IHttpClientFactory>()
        let oidcOpts = 
            ctx.HttpContext.GetService<IOptionsSnapshot<OpenIdConnectOptions>>().Get(oidcScheme)
        let idsClient = httpClientFactory.CreateClient("IdsClient")
        let logger = ctx.HttpContext.GetService<Microsoft.Extensions.Logging.ILogger<CookieValidatePrincipalContext>>()
        let tokens = ctx.Properties.GetTokens() |> List.ofSeq
        
        match getAccesstokenFreshness tokens now with
            | Fresh t ->
                logger.LogDebug("AccessToken considered fresh for {timespan}.", t) 
                Task.CompletedTask |> ignore
            | Stale refreshToken -> 
                match! useRefreshToken idsClient oidcOpts refreshToken now with
                | Ok freshTokens -> 
                    logger.LogDebug("Accesstoken stale. Using refreshtoken.")
                    ctx.Properties.StoreTokens(freshTokens)
                    ctx.ShouldRenew <- true
                | Error msg -> 
                    logger.LogError("Failed to refresh tokens {errordescription}", msg)
                    ctx.RejectPrincipal()
            | Unavailiable -> 
                logger.LogError("Unable to retrieve tokens from cookie-properties.")
                ctx.RejectPrincipal()
    } :> Task

let requestRefreshtokenRevokation
    (http : HttpClient)
    (oidcOpts : OpenIdConnectOptions)
    (refreshToken : string)
    =
    task {
        let! conf = oidcOpts.ConfigurationManager.GetConfigurationAsync(CancellationToken.None)
        let! r = 
            http.RevokeTokenAsync(new TokenRevocationRequest
                ( Address = conf.AdditionalData.["revocation_endpoint"].ToString(),
                  ClientId = oidcOpts.ClientId,
                  ClientSecret = oidcOpts.ClientSecret,
                  Token = refreshToken,
                  TokenTypeHint = "refresh_token"
                ))
        
        if r.IsError
        then return Error r.Error
        else return Ok ()
    }

let revokeTokensInCookieProperties
    (ctx : CookieSigningOutContext)
    =
    task {
        let! signInStatus = ctx.HttpContext.AuthenticateAsync()
        let httpClientFactory = ctx.HttpContext.GetService<IHttpClientFactory>()
        let logger = ctx.HttpContext.GetService<Microsoft.Extensions.Logging.ILogger<CookieValidatePrincipalContext>>()
        if signInStatus.None
        then return! Task.CompletedTask
        else 
            let tokens = signInStatus.Properties.GetTokens() |> List.ofSeq
            let refreshtoken = getRefreshtoken tokens
            let http = httpClientFactory.CreateClient("IdsClient")
            let oidcOpts = 
                ctx.HttpContext.GetService<IOptionsSnapshot<OpenIdConnectOptions>>().Get(oidcScheme)
            let! r = Option.map (requestRefreshtokenRevokation http oidcOpts) refreshtoken
                    |> Option.defaultValue (Task.FromResult (Ok ()))
            
            match r with
                | Error msg -> 
                    logger.LogError ("Failed to revoke refreshtoken: {error}", msg)
                    return! Task.CompletedTask
                | _ -> 
                    return! Task.CompletedTask
    } :> Task
    

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
        then (signOut cookieScheme >=> signOut oidcScheme) next ctx
        else redirectTo false returnUrl next ctx

let parsingErrorHandler err = RequestErrors.BAD_REQUEST err

let withAntiforgery (form : string -> string option -> Hostname option -> XmlNode) : HttpHandler=
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let af = ctx.GetService<Microsoft.AspNetCore.Antiforgery.IAntiforgery>()
        let issue = af.GetAndStoreTokens ctx
        let session = 
            if ctx.User.Identity.IsAuthenticated
            then Some ctx.User.Identity.Name
            else None

        let hostName = None

        htmlView (form issue.RequestToken session hostName) next ctx

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