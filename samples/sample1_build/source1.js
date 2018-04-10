var count = 0;
var colors = ["red","green","blue","purple"];
console.log(colors);
function Coucou(
){if(count<4) {

count = count+1;
}else{

count = 0;
}
console.log("coucou number "+count);
jQuery("#message").html("coucou number "+count+" ça marche !");
$("body").css("background-color",colors[count]);
}var payButton = document.getElementById("payButton");
console.log(payButton);
payButton.onclick = Coucou;

//# sourceURL=source1.code
//# sourceMappingURL=source1.code.map