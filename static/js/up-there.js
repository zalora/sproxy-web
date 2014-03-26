$(".up-there").click(function() {
    highlight("#animat-gr");
    highlight("#animat-do");
});

function highlight(elt) {
    var col = $(".navbar").css("background-color");
    $(elt).animate(
        { backgroundColor: "#600" },
        1000
    ).animate( 
        { backgroundColor: col },
        1000
     );
}