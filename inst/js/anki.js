$('.anki-marker .anki-text').click(function(){
  if ($(this).hasClass('open')) {
    $(this).css('opacity', '0');
  } else {
    $(this).css('opacity', '1');
  }
    $(this).toggleClass('open');
  });
});
