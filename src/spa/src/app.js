let appelement = document.getElementById("app");
let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: appelement.getAttribute("data-username")
    , hostName: appelement.getAttribute("data-hostname-name")
    , hostHandle: appelement.getAttribute("data-hostname-handle")
    }

let app = Elm.Main.init({ flags: flags });

// app.ports.getWeekpointer.subscribe(v => {
//     //console.log(v);
//     let vnext = getWeekpointer(v);
//     //console.log(vnext);
//     app.ports.gotWeekpointer.send(vnext);
// })

// app.ports.getTimesClock.subscribe(v => {
//     //console.log("times")
//     //console.log(v);
//     app.ports.gotTimesClock.send(printOclock(v));
// })

// app.ports.getBookingsClock.subscribe(v => {
//     //console.log("bookings")
//     //console.log(v);
//     app.ports.gotBookingsClock.send(printOclock(v));
// })

// var app = Elm.Main.init({flags: appstate});
//     app.ports.getWeekpointer.subscribe(v => {
//         app.ports.gotWeekpointer.send(getWeekpointer(v));
//     })
//     app.ports.getTimesClock.subscribe(v => {
//         //console.log("times")
//         //console.log(v);
//         app.ports.gotTimesClock.send(printOclock(v));
//     })
//     app.ports.getBookingsClock.subscribe(v => {
//         //console.log("bookings")
//         //console.log(v);
//         app.ports.gotBookingsClock.send(printOclock(v));
//     })