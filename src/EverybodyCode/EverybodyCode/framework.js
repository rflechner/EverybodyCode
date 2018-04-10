
function __init_interval__(start, stop) {
    var a = Array();
    if (start <= stop) {
        for (var i = start; i <= stop; i++) a.push(i);
    }
    else {
        for (var i = start; i > stop; i--) a.push(i);
    }
    return a;
}

function echo(text) {
    console.log(text);
}
