var moment = require('moment');

function make(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Moment = elm.Native.Moment || {};

    if (elm.Native.Moment.values) return elm.Native.Moment.values;

    function boxed(value) {
        var box = {ctor: 'Moment', isoString: value.toISOString()};
        Object.defineProperty(box, 'value', {value: value});
        return box;
    }

    function now() {
        return elm.Task.values.succeed(boxed(moment.utc()));
    }

    function parse(string) {
        return elm.Result.values.Ok(boxed(moment.utc(string)));
    }

    function format(value) {
        return value.value.toISOString();
    }

    function compare(a) {
        return function(b) {
            if (a.value.isBefore(b.value)) {
                return elm.Basics.values.LT;
            } else if (a.value.isAfter(b.value)) {
                return elm.Basics.values.GT;
            } else {
                return elm.Basics.values.EQ;
            }
        };
    }

    function durationBetween(a) {
        return function(b) {
            return b.value.diff(a.value);
        }
    }

    function from(a) {
        return function(b) {
            return a.value.from(b.value);
        }
    }

    return elm.Native.Moment.values = {
        now: now,
        parse: parse,
        format: format,
        compare: compare,
        durationBetween: durationBetween,
        from: from
    };
};

Elm.Native.Moment = {};
Elm.Native.Moment.make = make;
