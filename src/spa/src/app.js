let appelement = document.getElementById("app");
var wptr = new URLSearchParams(window.location.search)
    .get('wptr');

let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: appelement.getAttribute("data-username")
    , hostName: appelement.getAttribute("data-hostname-name")
    , hostHandle: appelement.getAttribute("data-hostname-handle")
    , weekpointer: getWeekpointer(wptr, 0)
    }

let app = Elm.Main.init({ flags: flags });

app.ports.nextWeekpointer.subscribe(v => {
    app.ports.gotWeekpointer.send(getWeekpointer(v, 1));
})

app.ports.currWeekpointer.subscribe(v => {
    app.ports.gotWeekpointer.send(getWeekpointer(v, 0));
})

app.ports.prevWeekpointer.subscribe(v => {
    app.ports.gotWeekpointer.send(getWeekpointer(v, -1));
})