var moment = require('moment');

function make(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Moment = elm.Native.Moment || {};

    if (elm.Native.Moment.values) return elm.Native.Moment.values;

    function parse(string) {
        var value = moment.utc(string);
        var box = {ctor: 'Moment', isoString: value.toISOString()};
        Object.defineProperty(box, 'value', {value: value});
        return elm.Result.values.Ok(box);
    }

    function format(value) {
        return value.value.toISOString();
    }

    return elm.Native.Moment.values = {
        parse: parse,
        format: format
    };
};

Elm.Native.Moment = {};
Elm.Native.Moment.make = make;
