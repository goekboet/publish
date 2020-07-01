let appelement = document.getElementById("app");
var query = new URLSearchParams(window.location.search)
    .get('wptr');

if ( query === null ) {
    query = initWptrQuery()
} 

let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: appelement.getAttribute("data-username")
    , hostName: appelement.getAttribute("data-hostname-name")
    , hostHandle: appelement.getAttribute("data-hostname-handle")
    , weekpointer: getWeekpointer(query, 0)
    }

let app = Elm.Main.init({ flags: flags });

app.ports.nextWeekpointer.subscribe(v => {
    let wptr = getWeekpointer(v, 1)

    app.ports.gotWeekpointer.send(wptr);
})

app.ports.currWeekpointer.subscribe(v => {
    let wptr = getWeekpointer(v, 0)

    app.ports.gotWeekpointer.send(wptr);
})

app.ports.prevWeekpointer.subscribe(v => {
    let wptr = getWeekpointer(v, -1)

    app.ports.gotWeekpointer.send(wptr);
})

app.ports.idTimeSubmission.subscribe(t => {
    // console.log(t);
    const q = new URLSearchParams(window.location.search).get('wptr');
    //console.log(q);
    var id = identifyTime(q, t);
    let searchParams = new URLSearchParams(window.location.search)
    searchParams.set("wptr", id[0])
    var newRelativePathQuery = window.location.pathname + '?' + searchParams.toString();
    history.pushState(null, '', newRelativePathQuery);
    // console.log(id);
    app.ports.timeSubmissionId.send(id[1]);
})

app.ports.formatTimeListing.subscribe(ts => {
    //console.log(ts);
    var ft = mapToTimes(ts);
    //console.log(ft);
    app.ports.timelistingFormatted.send(ft);
})