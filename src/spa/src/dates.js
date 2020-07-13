import getISOWeek from 'date-fns/getISOWeek' 
import startOfISOWeek from 'date-fns/startOfISOWeek' 
import setISOWeek from 'date-fns/setISOWeek'
import format from 'date-fns/format'
import getUnixTime from 'date-fns/getUnixTime'
import isThisISOWeek from 'date-fns/isThisISOWeek' 
import startOfDay from 'date-fns/startOfDay' 
import endOfDay from 'date-fns/endOfDay' 
import eachHourOfInterval from 'date-fns/eachHourOfInterval'
import isThisHour from 'date-fns/isThisHour' 
import endOfISOWeek from 'date-fns/endOfISOWeek'
import eachDayOfInterval from 'date-fns/eachDayOfInterval'
import isToday from 'date-fns/isToday'

let week = d => {
    let w = getISOWeek(d)
    let curr = startOfISOWeek(d)
    let prev = startOfISOWeek(setISOWeek(d, w - 1))
    let next = startOfISOWeek(setISOWeek(d, w + 1))

    let toOutput = x => { 
        return { 
            name: format(x, "yyyy 'w.'II"), 
            ts: getUnixTime(x), 
            isNow: isThisISOWeek(x) } 
        }

    return {
        current: toOutput(curr),
        previous: toOutput(prev),
        next: toOutput(next)
    }
}

let hours = d => {
    let s = startOfDay(d)
    let e = endOfDay(d)

    return eachHourOfInterval({ start: s, end: e})
        .map(x => {
            return {
                name: format(x, 'HH'),
                ts: getUnixTime(x),
                isNow: isThisHour(x)
            }
        })
}

let days = d => {
    let s = startOfISOWeek(d)
    let e = endOfISOWeek(d)

    return eachDayOfInterval({start: s, end: e})
        .map(x => {
            return { 
                key: format(x, 'iii'), 
                name: format(x, 'MMM dd'),
                start: getUnixTime(x),
                end: getUnixTime(endOfDay(x)),
                isNow: isToday(x),
                hours: hours(x)
            } 
            
        })
}

let tsLookup = ts => {
    let d = ts === null
        ? new Date()
        : new Date(ts * 1000)

    return {
        week: week(d),
        days: days(d)
    }
}

export { tsLookup }