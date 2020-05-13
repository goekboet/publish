module Publish.RefreshToken

open System
open Microsoft.AspNetCore.Authentication
open System.Net.Http
open Microsoft.AspNetCore.Authentication.OpenIdConnect
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Globalization
open IdentityModel.Client
open System.Threading


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