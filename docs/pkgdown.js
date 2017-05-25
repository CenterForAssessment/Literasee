$(function() {
  $("#sidebar").stick_in_parent({
    offset_top: $("#sidebar").offset().top
  });
  $('body').scrollspy({
    target: '#sidebar'
  });
});

$(window).scroll(function(){
    if($(window).scrollTop() > 50) {
        $('.navbar').addClass("shrink");
    } else {
        $('.navbar').removeClass("shrink");
    }
});
