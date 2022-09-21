# teal.widgets 0.1.1.9018

### Breaking changes
* `panel_group` and `panel_item` functions are no longer offer to be optionally a shiny input.

### Enhancements
* Updated `standard_layout` function to contain class not id for each output block.
* Added the `dim` slot to the list returned by the `plot_with_settings` module.
* Added `style` argument to `verbatim_popup_srv` to control whether the content is styled using `styler::style_text`.
* `condition` objects can now be displayed in `verbatim_popup`.

### Bug fixes
* Fixed bug in `verbatim_popup_srv` where `disabled` argument didn't influence the popup button. 
* Fixed `optionalSliderInput` validation to remove a warning message in certain cases.

### New features
* Added a new module - `verbatim_popup`.

# teal.widgets 0.1.1

### New features
* Added a new widget `draggable_buckets`.

### Enhancements
* The `option` `teal.plot_dpi` controls the `dpi` for rendering and downloading plots in `plot_with_settings`. If `option` is not used then a default of `72dpi` is used (unchanged from the previous behavior).

### Bug fixes
* Fixed a bug where zooming in or out from the browser would cause a plot of a `grob` object to not re-render instead displaying a blank white space.

### Miscellaneous
* Updated package authors.

# teal.widgets 0.1.0

* Initial release of `teal.widgets` a package providing shiny widgets for `teal` applications.

## Changes (from behavior when functionality was part of `teal`)

### Bug fixes
* Fixed location of pagination toggle for downloading tables using `table_with_settings`.
