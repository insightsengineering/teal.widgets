#' Bootstrap accordion unified wrapper
#'
#' Provide support for BS4 and BS5
#'
#' @param id Accordion unique id.
#' @param items Slot for \link{bs_accordion_item}.
#'
#' @export
bs_accordion <- function(id, items) {

  accordion_tag <- tags$div(
    class = "accordion",
    id = id,
    items
  )

  tagAddRenderHook(accordion_tag, function(x) {
    # get theme and version
    theme <- bslib::bs_current_theme()
    version <- if (bslib::is_bs_theme(theme)) {
      bslib::theme_version(theme)
    }

    if (version == "3") {
      stop(
        sprintf(
          "accordion is not available for Bootstrap %s",
          version
        )
      )
    }

    # process accordion items to add
    # missing attributes
    new_items <- lapply(seq_along(items), function(i) {

      # temp ids based on the parent id
      heading_id <- paste(id, "heading", i, sep = "_")
      controls_id <- paste0(id, "_collapse_", i)
      target_id <- paste0("#", controls_id)

      # resolve bs_according_item
      items[[i]] <- as.tags(items[[i]])

      # BS4 and BS5 have minor differences
      switch(
        version,
        "4" = tagQuery(items[[i]])$
          find(".card-header")$
          addAttrs("id" = heading_id)$
          find(".btn")$
          addAttrs(
            "data-target" = target_id,
            "aria-controls" = controls_id
          )$
          resetSelected()$
          find(".collapse")$
          addAttrs(
            "id" = controls_id,
            "aria-labelledby" = heading_id,
            "data-parent" = paste0("#", id)
          )$
          allTags(),
        "5" = tagQuery(items[[i]])$
          find(".accordion-header")$
          addAttrs("id" = heading_id)$
          children()$
          addAttrs(
            "data-bs-target" = target_id,
            "aria-controls" = controls_id
          )$
          resetSelected()$
          find(".accordion-collapse")$
          addAttrs(
            "id" = controls_id,
            "aria-labelledby" = heading_id,
            "data-bs-parent" = paste0("#", id)
          )$
          allTags()
      )
    })

    # alter main tag structure
    tagQuery(x)$
      # replace accordion items processed above
      empty()$
      append(new_items)$
      allTags()
  })
}



#' Bootstrap unified accordion item wrapper
#'
#' Provide support for BS4 and BS5
#'
#' @param title Item title.
#' @param content Item content.
#' @param active Whether to open the itm at start. Default to FALSE.
#'
#' @export
bs_accordion_item <- function(title, content, active = FALSE) {

  item_body <- tags$div(
    # id will be added from bs_accordion
    # aria-labelledby also added from bs_accordion
    # class differs between BS4 and BS5
    # data parent differs between BS4 and BS5
    class = paste("collapse", if (active) "show"),
    tags$div(
      # class differs between BS4 and BS5
      content
    )
  )

  # accordion item wrapper
  accordion_item_tag <- tags$div(
    # class differs between BS4 and BS5
    item_body
  )

  tagAddRenderHook(accordion_item_tag, function(x) {
    # get theme and version
    theme <- bslib::bs_current_theme()
    version <- if (bslib::is_bs_theme(theme)) {
      bslib::theme_version(theme)
    }

    # create accordion item header
    item_header <- if (version == "4") {
      tags$div(
        class = "card-header",
        # id will be added from bs_accordion
        tags$h2(
          class = "mb-0",
          tags$button(
            class = "btn btn-link btn-block text-left",
            type = "button",
            `data-toggle` = "collapse",
            # data-target will be added from bs_accordion
            `aria-expanded` = tolower(active),
            # aria-controls will be added from bs_accordion
            title
          )
        )
      )
    } else if (version == "5") {
      tags$h2(
        class = "accordion-header",
        tags$button(
          class = "accordion-button",
          type = "button",
          `data-bs-toggle` = "collapse",
          `aria-expanded` = tolower(active),
          title
        )
      )
    }

    # alter tag structure
    switch(
      version,
      # don't need to handle BS3
      "4" =  tagQuery(x)$
        addClass("card")$
        # prepend header tag
        prepend(item_header)$
        find(".collapse")$
        children()$
        # add class to item body
        addClass("card-body")$
        allTags(),
      "5" = tagQuery(x)$
        addClass("accordion-item")$
        prepend(item_header)$
        find(".collapse")$
        addClass("accordion-collapse")$
        children()$
        addClass("accordion-body")$
        allTags()
    )
  })
}