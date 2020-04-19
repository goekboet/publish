let appelement = document.getElementById("app");
let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: appelement.getAttribute("data-username")
    }

Elm.Main.init({ flags: flags });