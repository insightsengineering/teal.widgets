$(document).ready(function() {
  $('.panel-heading, .card-heading').click(function() {
    $(this).toggleClass('collapsed');
    var $icon = $(this).find('.dropdown-icon');
    if ($(this).hasClass('collapsed')) {
      $icon.removeClass('fa-angle-right').addClass('fa-angle-down');
      $(this).attr('aria-expanded', 'false');
    } else {
      $icon.removeClass('fa-angle-down').addClass('fa-angle-right');
      $(this).attr('aria-expanded', 'true');
    }
  });
});
