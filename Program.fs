module Publish.App

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
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

let finishEarly : HttpFunc = Some >> Task.FromResult

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

let parsingErrorHandler err = RequestErrors.BAD_REQUEST err

let webApp =
    choose [
        GET >=>
            choose [
                route "/"              >=> htmlView layout 
                route "/login"         >=> loginHandler
            ]
        RequestErrors.notFound (text "Not Found") ]

// ---------------------------------
// Main
// ---------------------------------

let cookieAuth (o : CookieAuthenticationOptions) =
    do
        o.Cookie.HttpOnly     <- true
        o.Cookie.Name         <- "publish.session"
        o.Cookie.SecurePolicy <- CookieSecurePolicy.SameAsRequest
        o.SlidingExpiration   <- true
        o.ExpireTimeSpan      <- TimeSpan.FromDays 7.0
    

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
    ()
    


let configureServices (services : IServiceCollection) =
    let serviceProvider = services.BuildServiceProvider()
    let config = serviceProvider.GetService<IConfiguration>()

    services
        .AddGiraffe()
        .AddAuthentication(authScheme)
        .AddCookie(cookieScheme, Action<CookieAuthenticationOptions>(cookieAuth)) 
        .AddOpenIdConnect(oidcScheme, Action<OpenIdConnectOptions>(configureOidc config))
        |> ignore

    services.AddDataProtection() |> ignore



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