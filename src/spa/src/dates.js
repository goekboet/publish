var getUnixTime = require('date-fns/getUnixTime')
var setISOWeek = require('date-fns/setISOWeek')
var startOfISOWeek = require('date-fns/startOfISOWeek')
var endOfISOWeek = require('date-fns/endOfISOWeek')
var getISOWeek = require('date-fns/getISOWeek')
var format = require('date-fns/format')
var setDay = require('date-fns/setDay')
var setHours = require('date-fns/setHours')
var setMinutes = require('date-fns/setMinutes')
var startOfMinute = require('date-fns/startOfMinute')
var addMinutes = require('date-fns/addMinutes')
var getUnixTime = require('date-fns/getUnixTime')
var setHours = require('date-fns/setHours')
var setMinutes = require('date-fns/setMinutes')


var days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' ]
var toIndex = function (d) { 
    var i = days.indexOf(d);
    if (i === -1) {
        return 0;
    }

    return i;

}

function parseTime(t) {
    var hhmm = t.split(':');
    var h = parseInt(hhmm[0], 10);
    var m = parseInt(hhmm[1], 10);

    return { h: h === null ? 0 : h, m: m === null ? 0 : m }
}

window.identifyTime = function (startOfWeek, t) {
    var d = new Date(startOfWeek * 1000)
    var d2 = setDay(d, toIndex(t.day), { weekStartsOn: 1 });
    var hhmm = parseTime(t.start);
    var hh = setHours(d2, hhmm.h);
    var mm = setMinutes(hh, hhmm.m)
    var start = startOfMinute(mm);
    var end = addMinutes(start, t.durMinutes);
   
    t.name = 
        format(start, 'HH:mm')
        + " - "
        + format(end, 'HH:mm');
    t.id = getUnixTime(start);
    t.day = format(start, 'iii');

    return t;
}

function toWeekpointer(d) {
    return { 
        name: format(d, "MMM dd, 'w.'II yyyy"),
        day: format(d, 'iii'),
        window: [ getUnixTime(startOfISOWeek(d)),
                  getUnixTime(endOfISOWeek(d))
                ]
    }
}

window.initWptr = function initWptr() {
    var d = new Date()
    
    return toWeekpointer(d)
}


window.moveWptr = function moveWptr(offset, wptr) {
    let d = new Date(wptr.window[0] * 1000)
    let w = getISOWeek(d)
    let d2 = setISOWeek(d, w + offset)
    var d3 = setDay(d2, toIndex(wptr.day), { weekStartsOn: 1 });
    
    return toWeekpointer(d3)
}

function toTime(t) {
    var d = new Date(parseInt(t.start) * 1000);
    var end = new Date(parseInt(t.end) * 1000);
    
    return {
        id: t.start,
        start: format(d, 'HH:mm'),
        day: format(d, 'iii'),
        end: parseInt(t.end),
        name: format(d, 'HH:mm') + ' - ' + format(end, 'HH:mm'),
        status: t.booked ? "Booked" : "Published"
    }
}

window.mapToTimes = function (ts) {
    return ts.map(toTime);
}
