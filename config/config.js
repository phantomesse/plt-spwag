$(document).ready(function() {
    // Display current slide on load
    slideId = getCurrentSlide();
    displaySlide(slideId);

    // Detect hash change
    $(window).bind('hashchange', function(e) {
        slideId = getCurrentSlide();
        displaySlide(slideId);
    });

    // Handle keypresses
    $(window).keydown(function(e) {
        switch(e.keyCode) {
            case 39: next(); break; // right arrow
            case 37: prev(); break; // left arrow
            case 32: next(); break; // spacebar
        }

        function prev() {
            var currentSlide = $('.slide:visible');
            if (currentSlide.children('a.prev').length == 1) {
                window.location.hash = currentSlide.children('a.prev').attr('href');
            }
        }

        function next() {
            var currentSlide = $('.slide:visible');
            if (currentSlide.children('a.next').length == 1) {
                window.location.hash = currentSlide.children('a.next').attr('href');
            }
        }

        return false;
    });
});

function getCurrentSlide() {
    var hash = window.location.hash;
    return hash === ''? 'main' : hash.substring(1);
}

function displaySlide(slideId) {
    $('.slide:visible:not(#' + slideId + ')').hide();
    $('#' + slideId).show();
}