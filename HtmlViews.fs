module Publish.HtmlViews

open Giraffe.GiraffeViewEngine

let layout =
    html [] [
        head [] [
            title []  [ str "Publish" ];
            link [ _rel "stylesheet" ; _href "style.css" ] 
        ]
        body [] [
            div [ _class "signup-page" ] [
                div [_class "content" ] [
                    h1 [] [ str "Publish"];
                    p [] [
                        str "Publish lets you post times that people can book you for. ";
                    ]
                    p [] [
                        str "Start by authenticating yourself " ;
                        a [ _href "/login"] [ str "here." ];
                    ]
                ];
            ]
        ]
    ]