module Publish.HtmlViews

open Giraffe.GiraffeViewEngine

let layout (anticsrf : string) =
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
                        form [
                            _action "/login";
                            _method "post";
                        ] [
                            input [
                                _type "hidden";
                                _name "__RequestVerificationToken";
                                _value anticsrf
                            ];
                            button [
                                _type "submit"
                            ] [ str "here" ]
                        ];
                    ]
                ];
            ]
        ]
    ]