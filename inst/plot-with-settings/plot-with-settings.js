// Function that registers resize observer for particular parent elements.
// Arguments are namespaced ids of DOM elements.
var establishPlotResizing = function (plot_out, flex_width) {
  // Create resize observer to trigger shiny actions when plot container is resized.
  var plotObserver = new ResizeObserver(function () {
    const paddingTolerance = 10;
    Shiny.onInputChange(
      flex_width,
      document.getElementById(plot_out).clientWidth - paddingTolerance // Reducing the plot width to avoid overflow
    );
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
