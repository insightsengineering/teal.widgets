# teal.widgets 0.1.0.9002

### Enhancements

* The `option` `teal.plot_dpi` controls the `dpi` for rendering and downloading plots in `plot_with_settings`. If `option` not used then a default of `72dpi` is used
(which is unchanged from the previous behavior).

### New features
* Add ability to pin and un-pin the main panel in the `standard_layout`. 

### Bug fixes

* Fixed an edge case bug where zooming in or out from the browser would cause a plot of a `grob` object to not re-render instead displaying a blank white space.

# teal.widgets 0.1.0

* Initial release of `teal.widgets` a package providing shiny widgets for `teal` applications.

## Changes (from behavior when functionality was part of `teal`)

### Bugfix

* Fixed location of pagination toggle for downloading tables using `table_with_settings`.
