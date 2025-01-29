$(document).ready(function () {
  $(document).on("click", ".panel-heading, .card-heading", function () {
    var $icon = $(this).find(".dropdown-icon");

    if ($(this).hasClass("collapsed")) {
      $icon.removeClass("fa-angle-down").addClass("fa-angle-right");
      $(this).attr("aria-expanded", "true");
    } else {
      $icon.removeClass("fa-angle-right").addClass("fa-angle-down");
      $(this).attr("aria-expanded", "false");
    }
  });
});
