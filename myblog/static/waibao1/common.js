$(window).scroll(function(){
    topsize = $('.fixnav').offset().top;
    if (topsize > 0){
        $('.fixnav').addClass("bgcolor");
    }else{
        $('.fixnav').removeClass("bgcolor");
    }
})

$('#carouselExampleControls_1').carousel({
    interval: 20000
})

$('#carouselExampleControls_2').carousel({
    interval: 20000
})

$("a.category").hover(
    function(){
        $(this).prev().children("img").css('bottom','-4px')
    },
    function(){
        $(this).prev().children("img").removeAttr('style')
    }
)

$(".choicewrapper .choice .choicecontent").hover(
    function(){
        $(this).siblings('.coverall').css('bottom','0')
    }
)

$(".choicewrapper .choice .choiceimage").hover(
    function(){
        $(this).siblings('.coverall').css('bottom','0')
    }
)

$(".choicewrapper .choice .coverall").hover(
    function(){
        1;
    },
    function(){
        $(this).removeAttr('style')
    }
)