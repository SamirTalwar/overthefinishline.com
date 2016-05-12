var moment = require('moment');

var Basics = {
    EQ: _elm_lang$core$Basics$EQ,
    LT: _elm_lang$core$Basics$LT,
    GT: _elm_lang$core$Basics$GT,
};
var Result = {
    Ok: _elm_lang$core$Result$Ok,
};
var Task = {
    succeed: _elm_lang$core$Task$succeed,
};

function boxed(value) {
    var box = {ctor: 'Moment', isoString: value.toISOString()};
    Object.defineProperty(box, 'value', {value: value});
    return box;
}

function now() {
    return Task.succeed(boxed(moment.utc()));
}

function parse(string) {
    return Result.Ok(boxed(moment.utc(string)));
}

function format(value) {
    return value.value.toISOString();
}

function compare(a) {
    return function(b) {
        if (a.value.isBefore(b.value)) {
            return Basics.LT;
        } else if (a.value.isAfter(b.value)) {
            return Basics.GT;
        } else {
            return Basics.EQ;
        }
    };
}

function durationOf(value) {
    return function(unit) {
        return moment.duration(value, unit).asMilliseconds();
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

global['_SamirTalwar$overthefinishline_com$Native_Moment'] = {
    now: now,
    parse: parse,
    format: format,
    compare: compare,
    durationOf: durationOf,
    durationBetween: durationBetween,
    from: from,
};
