module Publish.Api

open System
open System.Net.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Publish.Models
open Utf8Json
open System.Text
open Giraffe
open Microsoft.AspNetCore.Http
open System.Security.Claims
open IdentityModel.Client
open Microsoft.AspNetCore.Authentication
open System.Text.Json.Serialization

let get 
    (client : HttpClient) 
    (url : string)
    = 
    task {
        let! r = client.GetAsync url
        r.EnsureSuccessStatusCode () |> ignore
        return! r.Content.ReadAsStringAsync ()
    }

let post 
    (client : HttpClient) 
    (url : string)
    (payload : string)
    (fromJson : string -> 'b) = 
    task {
        let p = new StringContent (payload, Encoding.UTF8, "application/json")
        let! r = client.PostAsync (url, p)
        r.EnsureSuccessStatusCode () |> ignore
        let! json = r.Content.ReadAsStringAsync ()

        return fromJson json
    }

let getRegisteredPublishername
    (client : HttpClient)
    =
    task {
        let! json = get client "/publishers" 
        let registrations = JsonSerializer.Deserialize<Publishername[]>(json) 

        return Seq.tryHead registrations
    }

let registerPublisherName
    (client : HttpClient)
    (name : Publishername)
    =
    task {
        match! getRegisteredPublishername client with
        | Some hn -> 
            return hn
        | _       ->
            let payload = JsonSerializer.ToJsonString<Publishername>(name)
            let fromJson = JsonSerializer.Deserialize<Publishername> : string -> Publishername
            
            return! post client "/publishers" payload fromJson
    }

let toClaims (h : Publishername) = 
    [ Claim ("broker.host.name", h.name)
    ; Claim ("broker.host.handle", h.handle) 
    ]

let isRegistered (u : ClaimsPrincipal) =
    u.HasClaim(fun c -> c.Type = "broker.host.handle")

let getRegisteredPublisher (u : ClaimsPrincipal)
    = 
    let c = u.FindFirst "broker.host.handle"
    c.Value

let notLoggedIn =
    RequestErrors.UNAUTHORIZED
        "Ids"
        "Publish"
        "You must be logged in."

let accessDenied = setStatusCode 401 >=> text "Access Denied" : HttpHandler

let mustBeLoggedIn = requiresAuthentication notLoggedIn : HttpHandler

let requiresRegisteredPublisher =
    authorizeUser isRegistered accessDenied : HttpHandler

let getAuthorizedClient
    (ctx : HttpContext)
    =
    task {
        let httpFactory = ctx.GetService<IHttpClientFactory>()
            
        let client = httpFactory.CreateClient("BrokerClient")
        let! tkn = ctx.GetTokenAsync("access_token")
        client.SetBearerToken tkn

        return client
    }

let addPublisher : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! client = getAuthorizedClient ctx

            let! payload = ctx.BindJsonAsync<Publishername>()
            let! publisher = registerPublisherName client payload

            let! auth = ctx.AuthenticateAsync()
            let user = auth.Principal
            let props = auth.Properties
            let claims = toClaims publisher
            let brokerPublisher = ClaimsIdentity claims
            user.AddIdentity brokerPublisher
            
            do! ctx.SignInAsync(user, props) 

            return! Successful.OK publisher next ctx
        }
[<CLIMutable>]
type TimeSubmission = { Start : int64; Name : string; End : int64 }

[<CLIMutable>]
type TimePayload = { Start : int64; Handle : string; Name : string; End : int64 }
  
let addTime : HttpHandler
    =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task { 
            let! client = getAuthorizedClient ctx

            let! payload = ctx.BindJsonAsync<TimeSubmission>()
            let publisher = getRegisteredPublisher ctx.User
            let json = 
                JsonSerializer.ToJsonString<TimePayload>( 
                    { Start = payload.Start
                    ; Handle = publisher
                    ; Name = payload.Name
                    ; End = payload.End
                    })

            let p = new StringContent (json, Encoding.UTF8, "application/json")
            let! r = client.PostAsync ("/times", p)
            ctx.SetStatusCode (int r.StatusCode)
            if r.IsSuccessStatusCode
            then
                return! ctx.WriteJsonAsync payload
            else
                return (Some ctx)
        }

[<CLIMutable>]
type PublisherTimeListing = 
    { [<JsonPropertyName("start")>] Start : int64
    ; [<JsonPropertyName("handle")>] Handle : string
    ; [<JsonPropertyName("name")>] Name : string
    ; [<JsonPropertyName("end")>] End : int64 
    ; [<JsonPropertyName("booked")>] Booked : Boolean
    }

[<CLIMutable>]
type ClientTimeListing = 
    { Start : int64
    ; Name : string
    ; End : int64 
    ; Booked : Boolean
    }

let listTimes : HttpHandler
    =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task { 
            let! client = getAuthorizedClient ctx
            let publisher = getRegisteredPublisher ctx.User
            let f = 
                ctx.TryGetQueryStringValue "from"
                |> Option.map int
                |> Option.defaultValue 0

            let t =
                ctx.TryGetQueryStringValue "to"
                |> Option.map int
                |> Option.defaultValue 0
                
            let url = sprintf "/times?from=%i&to=%i" f t
            let! r = client.GetAsync(url)

            ctx.SetStatusCode (int r.StatusCode)
            if r.IsSuccessStatusCode
            then
                let serializer = ctx.GetJsonSerializer()
                let! json = r.Content.ReadAsByteArrayAsync()
                let payload = serializer.Deserialize json
                let toClient =
                    payload
                    |> Seq.filter (fun x -> x.Handle = publisher)
                    |> Seq.map (fun x -> { Start = x.Start; Name = x.Name; End = x.End; Booked = x.Booked})
                
                return! ctx.WriteJsonAsync toClient
            else
                return (Some ctx)
        }