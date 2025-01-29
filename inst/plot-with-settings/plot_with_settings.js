// Function that registers resize observer for particular parent elements.
// Arguments are namespaced ids of DOM elements.
var establishPlotResizing = function (plot_out, flex_width, plot_modal_width) {
  // Create resize observer to trigger shiny actions when plot container is resized.
  var plotObserver = new ResizeObserver(function () {
    Shiny.onInputChange(
      flex_width,
      document.getElementById(plot_out).clientWidth
    );
    Shiny.onInputChange(plot_modal_width, 0.87 * window.innerWidth);
    //based on modal CSS property, also accounting for margins
  });

  // Create mutation observer to delay activation of resize observer.
  // Activating observer before the target element exists is impossible.
  var plotNoticer = new MutationObserver(function () {
    const plotContainer = document.getElementById(plot_out);
    if (plotContainer === null) return;
    plotObserver.observe(plotContainer, { box: "border-box" });
  });

  plotNoticer.observe(document, { subtree: true, childList: true });
};
