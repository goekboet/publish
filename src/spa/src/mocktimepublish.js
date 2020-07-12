var eachH = require('date-fns/eachHourOfInterval')
var f = require('date-fns/format')
var ts = require('date-fns/getUnixTime')
var addDays = require('date-fns/addDays')

var reference = new Date(2020, 6, 11, 8, 0, 0, 0)

var toPT = function (d) {
    return {
        start: ts(d),
        name: f(d, 'HH:mm'),
        end: ts(d) + (60 * 45)
    }
}

var pTs = eachH({ start: reference, end: addDays(reference, 1) })
    .map(toPT)

