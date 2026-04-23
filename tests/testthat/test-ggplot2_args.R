testthat::test_that("ggplot2_args, no error with empty inputs", {
  testthat::expect_no_error(ggplot2_args())
  testthat::expect_no_error(ggplot2_args(labs = list(), theme = list()))
  testthat::expect_no_error(ggplot2_args(labs = list()))
  testthat::expect_no_error(ggplot2_args(theme = list()))
  testthat::expect_no_error(ggplot2_args(list()))
  testthat::expect_no_error(ggplot2_args(list(), list()))
})

testthat::test_that("ggplot2_args function returns object of ggplot2_args and list classes", {
  testthat::expect_true(inherits(ggplot2_args(), c("list", "ggplot2_args")))
})

testthat::test_that("ggplot2_args accepts named lists for arguments labs and theme", {
  testthat::expect_no_error(
    ggplot2_args(
      labs = list(title = "SOME TITLE"),
      theme = list(axis.title = ggplot2::element_blank())
    )
  )
})

testthat::test_that("ggplot2_args throws error when argument name is not valid.", {
  testthat::expect_error(ggplot2_args(labs = list(WRONG = "SOME TITLE")))
  testthat::expect_error(ggplot2_args(theme = list(WRONG = "STH")))
})

testthat::test_that("resolve_ggplot2_args accepts empty inputs", {
  testthat::expect_no_error(resolve_ggplot2_args())
})

testthat::test_that("resolve_ggplot2_args support NULL inputs", {
  testthat::expect_identical(
    resolve_ggplot2_args(),
    resolve_ggplot2_args(
      user_plot = NULL,
      user_default = NULL,
      module_plot = NULL,
      app_default = NULL
    )
  )
})

testthat::test_that("resolve_ggplot2_args throws error with empty list", {
  testthat::expect_error(
    resolve_ggplot2_args(list()),
    "'is.null(user_plot) || inherits(user_plot, \"ggplot2_args\")' failed: Must be TRUE."
  )
})

testthat::test_that("resolve_ggplot2_args function returns object of ggplot2_args and list classes", {
  testthat::expect_true(inherits(resolve_ggplot2_args(ggplot2_args()), c("list", "ggplot2_args")))
})

testthat::test_that("parse_ggplot2_args accepts empty inputs", {
  testthat::expect_no_error(parse_ggplot2_args(resolve_ggplot2_args()))
  testthat::expect_no_error(parse_ggplot2_args())
})

testthat::test_that("parse_ggplot2_args throws error with empty list", {
  testthat::expect_error(
    parse_ggplot2_args(list()),
    "'is.null(ggplot2_args) || inherits(ggplot2_args, \"ggplot2_args\")' failed: Must be TRUE."
  )
})

testthat::test_that("parse_ggplot2_args accepts an empty ggplot2_args as minimum requirement.", {
  testthat::expect_identical(parse_ggplot2_args(ggplot2_args = ggplot2_args()), list())
})

labs_base <- list(
  title = "DEFAULT_TITLE",
  caption = "PROJECT Y",
  subtitle = "DEFAULT_SUBTITLE"
)

labs_plot1 <- list(
  title = "PLOT1_TITLE",
  caption = "PROJECT X"
)

ggplot2_args_multi <- list(
  default = ggplot2_args(labs = labs_base),
  plot1 = ggplot2_args(labs = labs_plot1)
)

dev_ggplot2_args <- ggplot2_args(
  labs = list(
    x = quote(paste0("Fitted values\nlm(", reg_form, ")")),
    y = "Residuals",
    title = "Residuals vs Fitted",
    caption = "PROJECT Z"
  )
)

testthat::test_that("resolve_ggplot2_args uses input from only end user provided ggplot2_args source", {
  testthat::expect_identical(
    resolve_ggplot2_args(user_plot = ggplot2_args_multi$plot1),
    ggplot2_args(labs = list(title = "PLOT1_TITLE", caption = "PROJECT X"))
  )
  # If there is only one user provided source, then we could use user_default and user_plot arguments interchangeably
  testthat::expect_identical(
    resolve_ggplot2_args(user_default = ggplot2_args_multi$plot1),
    ggplot2_args(labs = list(title = "PLOT1_TITLE", caption = "PROJECT X"))
  )
  testthat::expect_identical(
    resolve_ggplot2_args(user_default = ggplot2_args_multi$default),
    ggplot2_args(labs = list(title = "DEFAULT_TITLE", caption = "PROJECT Y", subtitle = "DEFAULT_SUBTITLE"))
  )
})

testthat::test_that(
  "parse_ggplot2_args, when resolve_ggplot2_args uses input from only end user provided ggplot2_args source",
  code = {
    testthat::expect_identical(
      parse_ggplot2_args(ggplot2_args = resolve_ggplot2_args(user_plot = ggplot2_args_multi$plot1))$labs,
      quote(ggplot2::labs(title = "PLOT1_TITLE", caption = "PROJECT X"))
    )
    testthat::expect_identical(
      parse_ggplot2_args(ggplot2_args = resolve_ggplot2_args(user_default = ggplot2_args_multi$default))$labs,
      quote(ggplot2::labs(
        title = "DEFAULT_TITLE", caption = "PROJECT Y",
        subtitle = "DEFAULT_SUBTITLE"
      ))
    )
  }
)

testthat::test_that("resolve_ggplot2_args prioritizes user_plot input over user_default input.", {
  testthat::expect_identical(
    resolve_ggplot2_args(
      user_plot = ggplot2_args_multi$plot1,
      user_default = ggplot2_args_multi$default,
      module_plot = NULL
    ),
    ggplot2_args(labs = list(
      title = "PLOT1_TITLE", caption = "PROJECT X",
      subtitle = "DEFAULT_SUBTITLE"
    ))
  )
})

testthat::test_that(
  "parse_ggplot2_args, when resolve_ggplot2_args prioritizes user_plot input over user_default input.",
  code = {
    testthat::expect_identical(
      parse_ggplot2_args(
        resolve_ggplot2_args(
          user_plot = ggplot2_args_multi$plot1,
          user_default = ggplot2_args_multi$default
        )
      ),
      list(labs = quote(ggplot2::labs(
        title = "PLOT1_TITLE", caption = "PROJECT X",
        subtitle = "DEFAULT_SUBTITLE"
      )))
    )
  }
)

testthat::test_that("resolve_ggplot2_args refers to global teal.ggplot2_args options with empty input", {
  testthat::expect_identical(
    withr::with_options(list(teal.ggplot2_args = ggplot2_args(labs = list(title = "ENV_TITLE"))), {
      resolve_ggplot2_args()
    }), ggplot2_args(labs = list(title = "ENV_TITLE"))
  )
})

testthat::test_that(
  "parse_ggplot2_args, when resolve_ggplot2_args refers to global teal.ggplot2_args options with empty input",
  code = {
    testthat::expect_identical(
      withr::with_options(list(teal.ggplot2_args = ggplot2_args(labs = list(title = "ENV_TITLE"))), {
        parse_ggplot2_args(
          ggplot2_args = resolve_ggplot2_args()
        )
      }), list(labs = quote(ggplot2::labs(title = "ENV_TITLE")))
    )
  }
)

testthat::test_that("parse_ggplot2_args converts evaluated element objects to clean calls", {
  parse_element <- parse_ggplot2_args(
    resolve_ggplot2_args(
      module_plot = ggplot2_args(
        theme = list(axis.text = ggplot2::element_blank())
      )
    )
  )

  testthat::expect_identical(
    parse_element,
    list(theme = quote(ggplot2::theme(axis.text = ggplot2::element_blank())))
  )
})

testthat::test_that("parse_ggplot2_args handles evaluated element_text with deparse round-trip", {
  parse_element <- parse_ggplot2_args(
    resolve_ggplot2_args(
      module_plot = ggplot2_args(
        theme = list(text = ggplot2::element_text(size = 20))
      )
    )
  )

  deparsed <- deparse(parse_element$theme)
  result <- eval(parse(text = deparsed))
  testthat::expect_true(inherits(result, "theme"))
  testthat::expect_identical(result$text$size, 20)
})

testthat::test_that("to_call passes through already-quoted calls unchanged", {
  q <- quote(ggplot2::element_text(size = 20))
  testthat::expect_identical(teal.widgets:::to_call(q), q)
})

testthat::test_that("to_call passes through atomic values unchanged", {
  testthat::expect_identical(teal.widgets:::to_call("red"), "red")
  testthat::expect_identical(teal.widgets:::to_call(42), 42)
  testthat::expect_identical(teal.widgets:::to_call(TRUE), TRUE)
  testthat::expect_null(teal.widgets:::to_call(NULL))
})

testthat::test_that("to_call reconstructs element_text", {
  result <- teal.widgets:::to_call(ggplot2::element_text(size = 20))
  testthat::expect_true(is.call(result))
  # Verify round-trip
  evaled <- eval(result)
  testthat::expect_identical(evaled$size, 20)
})

testthat::test_that("to_call reconstructs element_rect", {
  result <- teal.widgets:::to_call(ggplot2::element_rect(fill = "blue", colour = "black"))
  testthat::expect_true(is.call(result))
  evaled <- eval(result)
  testthat::expect_identical(evaled$fill, "blue")
  testthat::expect_identical(evaled$colour, "black")
})

testthat::test_that("to_call reconstructs element_text with custom margin via round-trip", {
  original <- ggplot2::element_text(size = 14, margin = ggplot2::margin(5, 10, 5, 10))
  result <- teal.widgets:::to_call(original)
  testthat::expect_true(is.call(result))
  deparsed <- deparse(result)
  evaled <- eval(parse(text = deparsed))
  testthat::expect_identical(evaled$size, 14)
  testthat::expect_equal(as.numeric(evaled$margin), c(5, 10, 5, 10))
})

testthat::test_that("to_call reconstructs rel()", {
  result <- teal.widgets:::to_call(ggplot2::rel(1.5))
  testthat::expect_true(is.call(result))
  testthat::expect_identical(eval(result), ggplot2::rel(1.5))
})

testthat::test_that("to_call warns for unrecognized non-atomic objects", {
  # An arbitrary list that isn't a ggplot2 element
  testthat::expect_warning(
    teal.widgets:::to_call(structure(list(), class = "some_custom_thing")),
    "could not be converted to a call"
  )
})

testthat::test_that(
  "resolve_ggplot2_args priorotizes input in the order: user_plot, user_default, teal.ggplot2_args and module_plot",
  code = {
    result <- withr::with_options(list(teal.ggplot2_args = ggplot2_args(labs = list(title = "ENV_TITLE"))), {
      resolve_ggplot2_args(
        user_plot = ggplot2_args(labs = list(y = "USER_YLAB_DIRECT")),
        user_default = ggplot2_args(labs = list(x = "USER_XLAB_DEFAULT", y = "USER_YLAB_DEFAULT")),
        module_plot = ggplot2_args(
          labs = list(subtitle = "DEVELOPER_SUBTITLE", x = "USER_XLAB_DEV"),
          theme = list(axis.text = ggplot2::element_blank())
        )
      )
    })

    testthat::expect_identical(
      result$labs,
      list(
        y = "USER_YLAB_DIRECT",
        x = "USER_XLAB_DEFAULT",
        title = "ENV_TITLE",
        subtitle = "DEVELOPER_SUBTITLE"
      )
    )
    testthat::expect_true(inherits(result$theme$axis.text, "element_blank"))
  }
)

testthat::test_that(
  paste0(
    "parse_ggplot2_args, when resolve_ggplot2_args priorotizes input in the order: ",
    "user_plot, user_default, teal.ggplot2_args and module_plot"
  ),
  code = {
    parsed_all <- withr::with_options(list(teal.ggplot2_args = ggplot2_args(labs = list(title = "ENV_TITLE"))), {
      parse_ggplot2_args(
        resolve_ggplot2_args(
          user_plot = ggplot2_args(labs = list(y = "USER_YLAB_DIRECT")),
          user_default = ggplot2_args(labs = list(x = "USER_XLAB_DEFAULT", y = "USER_YLAB_DEFAULT")),
          module_plot = ggplot2_args(
            labs = list(subtitle = "DEVELOPER_SUBTITLE", x = "USER_XLAB_DEV"),
            theme = list(axis.text = ggplot2::element_blank())
          )
        ),
        ggtheme = "gray"
      )
    })

    testthat::expect_identical(
      parsed_all$labs,
      quote(ggplot2::labs(
        y = "USER_YLAB_DIRECT", x = "USER_XLAB_DEFAULT",
        title = "ENV_TITLE", subtitle = "DEVELOPER_SUBTITLE"
      ))
    )

    testthat::expect_identical(
      parsed_all$ggtheme,
      quote(ggplot2::theme_gray())
    )

    testthat::expect_identical(
      parsed_all$theme,
      quote(ggplot2::theme(axis.text = ggplot2::element_blank()))
    )
  }
)
