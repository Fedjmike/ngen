$(function () {
    $("input[type=radio]").click(function (event) {
        $(".adjectives").hide();
        $("." + event.target.value).show();
    });
    
    $("input[type=radio][value=\"by-inflection\"]").click();
    
    $("input[type=checkbox]").click(function (event) {
        $(".adjectives-list").hide();
        $(event.target.checked ? ".with-articles" : ".without-articles").show();
    });
    
    $("input[type=checkbox]").click().click();
});