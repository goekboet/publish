import getUnixTime from 'date-fns/getUnixTime'
import setISOWeek from 'date-fns/setISOWeek'
import startOfISOWeek from 'date-fns/startOfISOWeek'
import endOfISOWeek from 'date-fns/endOfISOWeek'
import getISOWeek from 'date-fns/getISOWeek'
import format from 'date-fns/format'
import setDay from 'date-fns/setDay'
import setHours from 'date-fns/setHours'
import setMinutes from 'date-fns/setMinutes'
import startOfMinute from 'date-fns/startOfMinute'
import addMinutes from 'date-fns/addMinutes'


const days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' ]
let toIndex = (d) => { 
    var i = days.indexOf(d);
    if (i === -1) {
        return 0;
    }

    return i;

}

let parseTime = (t) =>{
    let hhmm = t.split(':');
    let h = parseInt(hhmm[0], 10);
    let m = parseInt(hhmm[1], 10);

    return { h: h === null ? 0 : h, m: m === null ? 0 : m }
}

let identifyTime = (startOfWeek, t) => {
    let d = new Date(startOfWeek * 1000)
    let d2 = setDay(d, toIndex(t.day), { weekStartsOn: 1 });
    let hhmm = parseTime(t.start);
    let hh = setHours(d2, hhmm.h);
    let mm = setMinutes(hh, hhmm.m)
    let start = startOfMinute(mm);
    let end = addMinutes(start, t.durMinutes);
   
    t.name = 
        format(start, 'HH:mm')
        + " - "
        + format(end, 'HH:mm');
    t.id = getUnixTime(start);
    t.day = format(start, 'iii');

    return t;
}

let toWeekpointer = (d) => {
    return { 
        name: format(d, "MMM dd, 'w.'II yyyy"),
        day: format(d, 'iii'),
        window: [ getUnixTime(startOfISOWeek(d)),
                  getUnixTime(endOfISOWeek(d))
                ]
    }
}

let initWptr = () => toWeekpointer(new Date())


let moveWptr = (offset, wptr) => {
    let d = new Date(wptr.window[0] * 1000)
    let w = getISOWeek(d)
    let d2 = setISOWeek(d, w + offset)
    let d3 = setDay(d2, toIndex(wptr.day), { weekStartsOn: 1 });
    
    return toWeekpointer(d3)
}

let toTime = (t) => {
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

let mapToTimes = (ts) => ts.map(toTime);

export { initWptr, moveWptr, identifyTime, mapToTimes }