module Publish.SpaRoute

open Giraffe.GiraffeViewEngine
open Publish.Models

let layout 
    (anticsrf : string) 
    (username : string option)
    (hostName : Publishername option) =

    let usernameData (n : string option) =
        Option.map (fun u -> attr "data-username" u) username 
                   |> Option.defaultValue (flag "")

    let hostnameData (h : Publishername option) =
        match h with
            | Some h' -> [ attr "data-hostname-handle" h'.handle 
                       ; attr "data-hostname-name" h'.name
                       ]
            | None    -> [ flag "" ]

    html [] [
        head [] [
            title []  [ str "Publish" ];
            link [ _rel "stylesheet" ; _href "/style.css" ];
            link [ _rel "stylesheet" ; _href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.13.0/css/all.min.css" ];
            script [ _src "/app.js" ] []; 
            script 
                (_id "entrypoint"
                :: _src "/index.js"
                :: _defer 
                :: attr "data-anticsrf" anticsrf
                :: usernameData username
                :: hostnameData hostName) 
                []
        ]
        body [] []
    ]


