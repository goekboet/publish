module Publisher.Api

open System
open System.Net.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Publish.Models
open Utf8Json
open System.Text

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
    