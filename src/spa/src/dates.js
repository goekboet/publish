var getUnixTime = require('date-fns/getUnixTime')
var setISOWeek = require('date-fns/setISOWeek')
var getISOWeeksInYear = require('date-fns/getISOWeeksInYear')
var startOfISOWeek = require('date-fns/startOfISOWeek')
var endOfISOWeek = require('date-fns/endOfISOWeek')
var getISOWeekYear = require('date-fns/getISOWeekYear')
var getISOWeek = require('date-fns/getISOWeek')
var format = require('date-fns/format')
var fromUnixTime = require('date-fns/fromUnixTime')
var getDay = require('date-fns/getDay')
var setDay = require('date-fns/setDay')
var setHours = require('date-fns/setHours')
var setMinutes = require('date-fns/setMinutes')
var startOfMinute = require('date-fns/startOfMinute')
var addMinutes = require('date-fns/addMinutes')
var getUnixTime = require('date-fns/getUnixTime')
var add = require('date-fns/add')

var days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' ]
var toIndex = function (d) { 
    var i = days.indexOf(d);
    if (i === -1) {
        return 0;
    }

    return i;

}

function parseweek(s) {
    var tokens = s === null ? [] : s.split('-');
    // console.log(tokens);
    if (tokens.length == 3) {
        var y = parseInt(tokens[0], 10);
        var w = parseInt(tokens[1], 10);
        var d = tokens[2];
        
        if (isNaN(y) || isNaN(w))
        {
            // console.log("failed to parse year: " + y + " week " + w)
            return null;
        }

        if (w < 1 && getISOWeeksInYear(new Date(y, 0)) > w)
        {
            // console.log("Reject week: " + w);
            return null;
        }

        if (days.indexOf(d) == -1)
        {
            return null;
        }

        return { year: y, week: w, day: d };
    }
    else {
        return null;
    }
}

function zeroPad2w(s) {
    return ("0" + s).slice(-2);
}

function toWeek(d) {
    var y = getISOWeekYear(d);
    var w = getISOWeek(d);

    return y.toString().concat('-', zeroPad2w(w));
}

function parseTime(t) {
    var hhmm = t.split(':');
    var h = parseInt(hhmm[0], 10);
    var m = parseInt(hhmm[1], 10);

    return { h: h === null ? 0 : h, m: m === null ? 0 : m }
}

window.identifyTime = function (query, t) {
    var d = new Date();
    var wpt = parseweek(query);
    if (wpt !== null) {
        var year = new Date(wpt.year,0);
        var week = setISOWeek(year, wpt.week);
        d = setDay(week, toIndex(wpt.day), { weekStartsOn: 1 });
    }
    else {
        d = new Date();
    }

    var hhmm = parseTime(t.start);
    var hh = setHours(d, hhmm.h);
    var mm = setMinutes(hh, hhmm.m)
    var start = startOfMinute(mm);
    var end = addMinutes(start, t.durMinutes);
 
    t.name = 
        format(start, "iii, MMM dd, 'w.'II yyyy") 
        + " " 
        + format(start, 'HH:mm')
        + " - "
        + format(end, 'HH:mm');
    t.id = getUnixTime(start);
    t.day = format(d, 'iii');

    return t;
}

window.getWeekpointer = function getWeekpointer(query, offset) {
    var d = new Date();
    var wpt = parseweek(query);
    if (wpt !== null) {
        var year = new Date(wpt.year,0);
        var week = setISOWeek(year, wpt.week + offset);
        d = setDay(week, toIndex(wpt.day), { weekStartsOn: 1 });
    }
    else {
        d = new Date();
    }
    
    return [ 
        format(d, 'yyyy-II-iii'), 
        {
            name: format(d, "iii, MMM dd, 'w.'II yyyy"),
            day: format(d, 'iii'),
            window: [ getUnixTime(startOfISOWeek(d)),
                      getUnixTime(endOfISOWeek(d))]
        }]
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
