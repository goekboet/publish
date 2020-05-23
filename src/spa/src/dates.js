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

window.getWeekpointer = function getWeekpointer(query, offset) {

    var d = new Date();
    var wpt = parseweek(query);
    if (wpt !== null) {
        var year = new Date(wpt.year,0);
        var week = setISOWeek(year, wpt.week + offset);
        
        d = setDay(week, toIndex(wpt.day));
    }
    else {
        d = new Date();
    }
    
    return [ 
        format(d, 'yyyy-II-iii'), 
        {
            name: format(d, "iii, MMM dd, 'w.'II yyyy"),
            day: format(d, 'iii'),
            window: [ getUnixTime(startOfISOWeek(d)) * 1000,
                      getUnixTime(endOfISOWeek(d) * 1000)]
        }]
}

function oclock(t) {
    return format(fromUnixTime(t), 'PPpp');

}

window.printOclock = function printOclock(appts)
{
     return appts.map(function f(a) {
        return {
            hostId: a.hostId,
            name: a.name,
            start: a.start,
            oclock: oclock(a.start / 1000),
            dur: a.dur
        };
     })
}
