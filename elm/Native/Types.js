var _eeue56$json_to_elm$Native_Types = (function(){

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

    var fromArray = function fromArray(arr)
    {
        var out = { "ctor": "[]" };
        for (var i = arr.length; i--; )
        {
            out = { "ctor": "::", _0: arr[i], _1: out};
        }
        return out;
    }


    var keys = function(obj){
        return fromArray(Object.keys(obj));
    };

    var get = function(name, obj){
        if (typeof obj[name] === "undefined"){
            return {'ctor': 'Nothing'};
        }

        return {
            'ctor': "Just",
            _0: obj[name]
        };
    };

    var unsafeGet = function(name, obj){
        return obj[name];
    };

    var unsafeEval = function(aliasName, constructorString, decoderString, encoderString, testData){
        return { "ctor": "Err", _0: "Not currently supported" };
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

    return {
        'makeGuessAtType' : makeGuessAtType,
        'toValue': toValue,
        'keys': keys,
        'get': F2(get),
        'unsafeGet': F2(unsafeGet),
        'unsafeEval': F5(unsafeEval)
    };
})();
