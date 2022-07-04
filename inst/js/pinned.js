
  $(document).ready(function(event) {
    $(document).on("click", "div.standard_layout_output_panel", function(el) {
        $(el.target).closest("div.standard_layout_output_panel").toggleClass("affix");
    })
  })
