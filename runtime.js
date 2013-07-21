var fs = require("fs");

var output = function(s) {
    try {
        process.stdout.write(String.fromCharCode(s)); // byte -> ASCII
    } catch (e) {
        // simply don't print non-ASCII characters
    }
}

var memory         = [];
var program        = ".+[.+]";
var memoryPointer  = 0;
var programPointer = 0;

// a helper because JS sucks
var remainder = function (a, n) {
    while(a < 0) a += n;
    return a % n;
};

var getMemory = function() {
    if(typeof(memory[memoryPointer]) !== "number") {
        memory[memoryPointer] = 0;
    }
    return memory[memoryPointer];
}

var jumpToMatchingRightBrace = function (bracketCount) {
    var c = program[programPointer];
    if(c === "[") {
        ++programPointer;
        jumpToMatchingRightBrace(bracketCount + 1);
    }
    else if(c === "]") {
        if(bracketCount === 0) {
            ++programPointer; // found matching brace; we are done
        }
        else if(bracketCount > 0) {
            ++programPointer;
            jumpToMatchingRightBrace(bracketCount - 1);
        }
        else if (bracketCount < 0) {
            // violates invariant enforced by Haskell; impossible
            throw "malformed program: mismatched braces";
        }
    }
    else { // c is not a bracket
        ++programPointer;
        jumpToMatchingRightBrace(bracketCount);
    } 
}

var jumpToMatchingLeftBrace = function (bracketCount) {
    var c = program[programPointer];
    if(c === "[") {
        if(bracketCount === 0) {
            ++programPointer; // go just past the matching left bracket
        }
        else if(bracketCount > 0) {
            --programPointer;
            jumpToMatchingLeftBrace(bracketCount - 1);
        }
        else if(bracketCount < 0) {
            // violates invariant enforced by Haskell; impossible
            throw "malformed program: mismatched braces";
        }
    }
    else if(c === "]") {
        --programPointer;
        jumpToMatchingLeftBrace(bracketCount + 1);
    }
    else { // c is not a bracket
        --programPointer;
        jumpToMatchingLeftBrace(bracketCount);
    } 
}



// the main interpreter/runtime/whatever
bfi = {
    '>' : function () {
        ++memoryPointer;
        ++programPointer;
    },
    '<' : function () {
        --memoryPointer;
        if(memoryPointer < 0) {
            throw "Program error: tried to go left past cell 0 of memory.";
        }
        ++programPointer;
    },
    '+' : function () {
        var b                 = getMemory();
        memory[memoryPointer] = remainder((b + 1), 256);
        ++programPointer;
    },
    '-' : function () {
        var b                 = getMemory();
        memory[memoryPointer] = remainder((b - 1), 256);
        ++programPointer;
    },
    '.' : function () {
        output(getMemory());
        ++programPointer;
    },
//    ',' : function () {
//        
//    },
    '[' : function () {
        ++programPointer;
        if(getMemory() === 0) {
            jumpToMatchingRightBrace(0);
        }
    },
    ']' : function () {
        if(memory[memoryPointer] !== 0) {
            --programPointer;
            jumpToMatchingLeftBrace();
        }
        else {
            ++programPointer;
        }
    },
}


var main = function () {
    while(programPointer < program.length) {
        var command = program[programPointer];
        if (command in bfi) {
            bfi[command]();
        }
        else {
            ++programPointer;
        }
    }
};


var filename = process.argv[process.argv.length - 1]

fs.readFile(filename, "utf8", function(err, data) {
    if (err) {
        console.error("Error finding file named " + filename);
    } else {
        program = data;
        main();
    }
});
