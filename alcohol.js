var slides = document.querySelector('.slides');

var leftArrow = document.querySelector('.fa-chevron-left');
var rightArrow = document.querySelector('.fa-chevron-right');

var currentSlide = 0;

leftArrow.addEventListener('click', function (e) {
    if (currentSlide > 0) {
        currentSlide--;
    }

    updatePosition();
});

rightArrow.addEventListener('click', function (e) {
    if (currentSlide < 3) {
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

    dots.item(currentSlide).classList.add('active');

    if (currentSlide > 0) {
        leftArrow.classList.add('active');
    } else {
        leftArrow.classList.remove('active');
    }

    if (currentSlide < 3) {
        rightArrow.classList.add('active');
    } else {
        rightArrow.classList.remove('active');

    }

}
