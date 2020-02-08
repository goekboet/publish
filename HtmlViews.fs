module Publish.HtmlViews

open Giraffe.GiraffeViewEngine

let antiCsrf (token : string) : XmlNode =
    input [
              _type "hidden";
              _name "__RequestVerificationToken";
              _value token
          ]

let notLoggedIn (token : string) : XmlNode =
    p [] [
             str "Start by authenticating yourself " ;
             form [
                 _action "/login";
                 _method "post";
             ] [
                 antiCsrf token;
                 button [
                     _type "submit"
                 ] [ str "here" ]
             ];
         ]

let loggedIn (token : string) : XmlNode =
    p [] [
        str "Excellent! Soon we'll arrange for something other to do than ";
        form [
            _action "/logout";
            _method "post";
        ] [
            antiCsrf token;
            button [
                _type "submit"
            ] [ str "log out." ]
        ]
    ]

let layout (anticsrf : string) (authenticated : bool) =
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
                        if authenticated
                        then loggedIn anticsrf
                        else notLoggedIn anticsrf

                    ]
                ];
            ]
        ]
    ]


