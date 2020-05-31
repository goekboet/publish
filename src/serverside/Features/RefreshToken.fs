module Publish.RefreshToken

open System
open Microsoft.AspNetCore.Authentication
open System.Net.Http
open Microsoft.AspNetCore.Authentication.OpenIdConnect
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Globalization
open IdentityModel.Client
open System.Threading
open Microsoft.AspNetCore.Authentication.Cookies
open Giraffe
open Microsoft.Extensions.Options
open System.Threading.Tasks
open Microsoft.Extensions.Logging

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

let refreshTokensIfStale 
    (ctx : CookieValidatePrincipalContext) =
    task {
        let now = ctx.HttpContext.GetService<ISystemClock>().UtcNow
        let httpClientFactory = ctx.HttpContext.GetService<IHttpClientFactory>()
        let oidcOpts = 
            ctx.HttpContext.GetService<IOptionsSnapshot<OpenIdConnectOptions>>().Get("Ids")
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
                ctx.HttpContext.GetService<IOptionsSnapshot<OpenIdConnectOptions>>().Get("Ids")
            let! r = Option.map (requestRefreshtokenRevokation http oidcOpts) refreshtoken
                    |> Option.defaultValue (Task.FromResult (Ok ()))
            
            match r with
                | Error msg -> 
                    logger.LogError ("Failed to revoke refreshtoken: {error}", msg)
                    return! Task.CompletedTask
                | _ -> 
                    return! Task.CompletedTask
    } :> Task