let appelement = document.getElementById("app");
let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: appelement.getAttribute("data-username")
    , hostName: appelement.getAttribute("data-hostname-name")
    , hostHandle: appelement.getAttribute("data-hostname-handle")
    }

Elm.Main.init({ flags: flags });