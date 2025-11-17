# snapshot test for optionalSliderInput

    Code
      as.character(optionalSliderInput("my slider", "my label", 0, 10, 2))
    Output
      [1] "<div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"my slider-label\" for=\"my slider\">my label</label>\n  <input class=\"js-range-slider\" id=\"my slider\" data-skin=\"shiny\" data-min=\"0\" data-max=\"10\" data-from=\"2\" data-step=\"1\" data-grid=\"true\" data-grid-num=\"10\" data-grid-snap=\"false\" data-prettify-separator=\",\" data-prettify-enabled=\"true\" data-keyboard=\"true\" data-data-type=\"number\"/>\n</div>"

