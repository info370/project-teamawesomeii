var slides = document.querySelector('.slides');

var leftArrow = document.querySelector('.fa-chevron-left');
var rightArrow = document.querySelector('#controls');

var currentSlide = 0;
/*
leftArrow.addEventListener('click', function (e) {
    if (currentSlide > 0) {
        currentSlide--;
    }

    updatePosition();
});
*/

rightArrow.addEventListener('click', function (e) {
    if (currentSlide < 4) {
        currentSlide++;
    }

    updatePosition();
});

function updatePosition() {
    slides.style.left = -1 * currentSlide * 100 + '%';

    var dots = document.querySelectorAll('.dot');
    dots.forEach(dot => {
        dot.classList.remove('active');
    });

    //dots.item(currentSlide).classList.add('active');
/*
    if (currentSlide > 0) {
        leftArrow.classList.add('active');
    } else {
        leftArrow.classList.remove('active');
    }
*/
    if (currentSlide < 4) {
        rightArrow.classList.add('active');
    } else {
        rightArrow.classList.remove('active');

    }

}
