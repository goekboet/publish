import { initWptr, moveWptr, identifyTime, mapToTimes } from './dates.js'

let appelement = document.getElementById("entrypoint");

let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: appelement.getAttribute("data-username")
    , hostName: appelement.getAttribute("data-hostname-name")
    , hostHandle: appelement.getAttribute("data-hostname-handle")
    , weekpointer: initWptr()
    }

let app = Elm.Main.init({ flags: flags });

app.ports.moveWeekpointer.subscribe(v => {
    let wptr = v[0] === 0
      ? initWptr()
      : moveWptr(v[0], v[1])

    app.ports.gotWeekpointer.send(wptr);
})

app.ports.idTimeSubmission.subscribe(t => {
    var id = identifyTime(t[0], t[1]);
    app.ports.timeSubmissionId.send(id);
})

app.ports.formatTimeListing.subscribe(ts => {
    var ft = mapToTimes(ts);
    app.ports.timelistingFormatted.send(ft);
})