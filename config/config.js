$(document).ready(function() {
    slideId = getCurrentSlide();
    displaySlide(slideId);

    // Detect hash change
    $(window).bind('hashchange', function(e) {
        slideId = getCurrentSlide();
        displaySlide(slideId);
    });
});

function getCurrentSlide() {
    var hash = window.location.hash;
    return hash === ''? 'main' : hash.substring(1);
}

function displaySlide(slideId) {
    console.log(slideId);
    $('.slide:visible:not(#' + slideId + ')').hide();
    $('#' + slideId).show();
}

$('a.prev').click(function() {
    var prev_slide = $(this).attr('href');
    console.log(prev_slide);
});