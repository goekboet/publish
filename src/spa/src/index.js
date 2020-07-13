import { mapToTimes, tsLookup } from './dates.js'

let appelement = document.getElementById("entrypoint");

let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: appelement.getAttribute("data-username")
    , hostName: appelement.getAttribute("data-hostname-name")
    , hostHandle: appelement.getAttribute("data-hostname-handle")
    , tsLookup: tsLookup(null)
    }

let app = Elm.Main.init({ flags: flags });

app.ports.moveWeekpointer.subscribe(ts => {
    app.ports.newWeekpointer.send(tsLookup(ts))
})
