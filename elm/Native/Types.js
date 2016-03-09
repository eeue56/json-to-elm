var make = function make(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Types = elm.Native.Types || {};

    if (elm.Native.Types.values) return elm.Native.Types.values;

    var List = Elm.Native.List.make(elm);
    var Maybe = Elm.Maybe.make(elm);
    var Result = Elm.Result.make(elm);
    var _U = Elm.Native.Utils.make(elm);
    var $Json$Decode = Elm.Json.Decode.make(elm);
    var $Json$Encode = Elm.Json.Encode.make(elm);
    var $Json$Decode$Extra = Elm.Json.Decode.Extra.make(elm);


    var KNOWN_DECODERS = [
        'maybe',
        'list' ,
        'int' ,
        'float' ,
        'bool',
        'string' ];

    var isInt = function(n) {
       return n % 1 === 0;
    };

    var makeGuessAtType = function(item) {
        if (item === null) {
            return 'Maybe _Unknown';
        }

        var type = typeof(item);

        if (type === 'boolean'){
            return 'Bool';
        }

        if (type === 'string'){
            return 'String';
        }

        if (type === 'number'){
            if (isInt(item)){
                return 'Int';
            }

            return 'Float';
        }

        if (Array.isArray(item)){
            if (item.length === 0){
                return 'List a';
            }

            return 'List ' + makeGuessAtType(item[0]);
        }

        if (type === 'object'){
            return 'Something';
        }

        return 'Unknown';
    };

    var toValue = function(text){
        try {
            return JSON.parse(text);
        } catch (e) {
            return {};
        }
    };

    var keys = function(obj){
        return List.fromArray(Object.keys(obj));
    };

    var get = function(name, obj){
        if (typeof obj[name] === "undefined"){
            return Maybe.Nothing;
        }

        return Maybe.Just(obj[name]);
    };

    var unsafeGet = function(name, obj){
        return obj[name];
    };

    var unsafeEval = function(aliasName, constructorString, decoderString, encoderString, testData){
        try {
            console.log(constructorString);
            console.log(encoderString);
            console.log(testData);
            var constructor = eval(constructorString);
            var encoder = eval(encoderString);
            var decoder = eval(decoderString);
            var something = eval('decode' + aliasName + '(' + JSON.stringify(testData) + ')');
            console.log(something);
            var somethingElse = eval('encode' + aliasName + '(' + JSON.stringify(something) + ')');
            console.log(somethingElse);
        } catch (e){
            return Result.Err(e.message);
        }

        return Result.Ok(something);
    };

    return elm.Native.Types.values = {
        'makeGuessAtType' : makeGuessAtType,
        'toValue': toValue,
        'keys': keys,
        'get': F2(get),
        'unsafeGet': F2(unsafeGet),
        'unsafeEval': F5(unsafeEval)
    };
};

Elm.Native.Types = {};
Elm.Native.Types.make = make;
