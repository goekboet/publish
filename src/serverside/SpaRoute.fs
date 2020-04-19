module Publish.SpaRoute

open Giraffe.GiraffeViewEngine
open Publish.Models

let layout 
    (anticsrf : string) 
    (username : string option)
    (hostName : Hostname option) =

    let usernameData (n : string option) =
        Option.map (fun u -> attr "data-username" u) username 
                   |> Option.defaultValue (flag "")

    let hostnameData (h : Hostname option) =
        match h with
            | Some h' -> [ attr "data-hostname-handle" h'.Handle 
                       ; attr "data-hostname-name" h'.Name
                       ]
            | None    -> [ flag "" ]

    html [] [
        head [] [
            title []  [ str "Publish" ];
            link [ _rel "stylesheet" ; _href "style.css" ];
            script [ _src "main.js"; ] []; 
            script 
                (_id "app"
                :: _type "module"
                :: _src "app.js"
                :: _defer 
                :: attr "data-anticsrf" anticsrf
                :: usernameData username
                :: hostnameData hostName) 
                []
        ]
        body [] []
    ]


