testthat::test_that("basic_table_args returns empty basic_table_args with empty inputs", {
  testthat::expect_identical(
    basic_table_args(NULL),
    structure(list(NULL), class = "basic_table_args")
  )
  testthat::expect_identical(
    basic_table_args(),
    structure(list(), class = "basic_table_args")
  )
})

testthat::test_that("basic_table_args function returns list of the basic_table_args class", {
  testthat::expect_true(inherits(basic_table_args(), c("list", "basic_table_args")))
})

testthat::test_that("basic_table_args rtables arguments validation, correct input", {
  testthat::expect_error(basic_table_args(title = "SOME TITLE"), NA)
})

testthat::test_that("basic_table_args rtables arguments validation, incorrect input", {
  testthat::expect_error(basic_table_args(WRONG = "SOME TITLE"))
})

testthat::test_that("resolve_basic_table_args returns empty basic_table_args with empty input", {
  testthat::expect_error(resolve_basic_table_args(), NA)
})

testthat::test_that("resolve_basic_table_args error with empty list, as need a basic_table_args class", {
  testthat::expect_error(resolve_basic_table_args(list()))
})

testthat::test_that("resolve_basic_table_args need a basic_table_args class or NULL, for each of its arguments", {
  testthat::expect_identical(
    resolve_basic_table_args(structure(list(), class = "basic_table_args")),
    basic_table_args()
  )
  testthat::expect_identical(
    resolve_basic_table_args(NULL),
    basic_table_args()
  )
})

testthat::test_that(
  "resolve_basic_table_args returns empty basic_table_args with any list input in basic_table_args",
  code = {
    testthat::expect_identical(
      resolve_basic_table_args(basic_table_args(list(a = 2))),
      basic_table_args()
    )
    testthat::expect_identical(
      resolve_basic_table_args(basic_table_args(list())),
      basic_table_args(),
    )
  }
)

testthat::test_that(
  "resolve_basic_table_args error with any list or NULL input in its arguments, when it is not only one",
  code = {
    testthat::expect_error(resolve_basic_table_args(basic_table_args(title = "aa"), basic_table_args(list())))
    testthat::expect_error(
      resolve_basic_table_args(
        basic_table_args(title = "aa"),
        basic_table_args(list(title = "b"))
      )
    )
    testthat::expect_error(resolve_basic_table_args(basic_table_args(title = "aa"), basic_table_args(NULL)))
  }
)

testthat::test_that("resolve_basic_table_args support NULL inputs", {
  testthat::expect_identical(
    resolve_basic_table_args(),
    resolve_basic_table_args(
      user_table = NULL,
      user_default = NULL,
      module_table = NULL,
      app_default = NULL
    )
  )
})

testthat::test_that("resolve_basic_table_args returns object of basic_table_args and list classes", {
  testthat::expect_true(inherits(resolve_basic_table_args(), c("list", "basic_table_args")))
  testthat::expect_true(inherits(resolve_basic_table_args(basic_table_args()), c("list", "basic_table_args")))
})

testthat::test_that("parse_basic_table_args throws error with empty list", {
  testthat::expect_error(
    parse_basic_table_args(list()),
    "'is.null(basic_table_args) || inherits(basic_table_args, \"basic_table_args\")' failed: Must be TRUE."
  )
})

testthat::test_that("parse_basic_table_args returns language object of empty rtables::basic_table() call", {
  testthat::expect_identical(parse_basic_table_args(), quote(rtables::basic_table()))
  testthat::expect_identical(parse_basic_table_args(resolve_basic_table_args()), quote(rtables::basic_table()))
  testthat::expect_identical(
    parse_basic_table_args(
      resolve_basic_table_args(
        basic_table_args()
      )
    ), quote(rtables::basic_table())
  )
})

table_args_multi <- list(
  default = basic_table_args(
    title = "USER_DEFAULT_TITLE",
    subtitles = "USER_DEFAULT_SUBTITLE"
  ),
  table1 = basic_table_args(title = "USER_TABLE_TITLE")
)

testthat::test_that("resolve_basic_table_args uses input from only end user provided basic_table_args source", {
  testthat::expect_identical(
    resolve_basic_table_args(user_table = table_args_multi$table1),
    basic_table_args(title = "USER_TABLE_TITLE")
  )
  # If there is only one user provided source, then we could use user_default and user_plot arguments interchangeably
  testthat::expect_identical(
    resolve_basic_table_args(user_default = table_args_multi$table1),
    basic_table_args(title = "USER_TABLE_TITLE")
  )
  testthat::expect_identical(
    resolve_basic_table_args(user_default = table_args_multi$default),
    basic_table_args(title = "USER_DEFAULT_TITLE", subtitles = "USER_DEFAULT_SUBTITLE")
  )
})

testthat::test_that(
  paste0(
    "parse_basic_table_args, when resolve_basic_table_args uses input",
    "from only end user provided basic_table_args source"
  ),
  code = {
    testthat::expect_identical(
      parse_basic_table_args(basic_table_args = resolve_basic_table_args(user_table = table_args_multi$table1)),
      quote(rtables::basic_table(title = "USER_TABLE_TITLE"))
    )
    testthat::expect_identical(
      parse_basic_table_args(basic_table_args = resolve_basic_table_args(user_default = table_args_multi$default)),
      quote(rtables::basic_table(title = "USER_DEFAULT_TITLE", subtitles = "USER_DEFAULT_SUBTITLE"))
    )
  }
)

testthat::test_that("resolve_basic_table_args refers to global teal.basic_table_args options with empty input", {
  testthat::expect_identical(
    withr::with_options(list(teal.basic_table_args = basic_table_args(title = "ENV_TITLE")), {
      resolve_basic_table_args()
    }), basic_table_args(title = "ENV_TITLE")
  )
})

testthat::test_that(
  paste0(
    "parse_basic_table_args, when resolve_basic_table_args refers to global teal.basic_table_args",
    "options with empty input"
  ),
  code = {
    testthat::expect_identical(
      withr::with_options(list(teal.basic_table_args = basic_table_args(title = "ENV_TITLE")), {
        parse_basic_table_args(
          resolve_basic_table_args(
            user_table = NULL,
            user_default = NULL,
            module_table = NULL
          )
        )
      }), quote(rtables::basic_table(title = "ENV_TITLE"))
    )
  }
)

testthat::test_that(
  paste0(
    "resolve_basic_table_args prioritizes input in the order: ",
    "user_table, user_default, teal.basic_table_args and module_plot"
  ),
  code = {
    testthat::expect_identical(
      withr::with_options(
        list(
          teal.basic_table_args = basic_table_args(
            title = "ENV_TITLE",
            main_footer = "ENV_FOOTER"
          )
        ),
        code = {
          resolve_basic_table_args(
            user_table = basic_table_args(title = "USER_TITLE_TABLE"),
            user_default = basic_table_args(title = "USER_TITLE_DEFAULT", prov_footer = "USER_FOOTER_DIRECT"),
            module_table = basic_table_args(subtitles = "DEVELOPER_SUBTITLE")
          )
        }
      ), basic_table_args(
        title = "USER_TITLE_TABLE", prov_footer = "USER_FOOTER_DIRECT",
        main_footer = "ENV_FOOTER", subtitles = "DEVELOPER_SUBTITLE"
      )
    )
  }
)


testthat::test_that(
  paste0(
    "parse_basic_table_args, when resolve_basic_table_args prioritizes input in the order: ",
    "user_table, user_default, teal.basic_table_args and module_plot"
  ),
  code = {
    testthat::expect_identical(
      withr::with_options(
        list(
          teal.basic_table_args = basic_table_args(
            title = "ENV_TITLE",
            main_footer = "ENV_FOOTER"
          )
        ),
        code = {
          parse_basic_table_args(
            resolve_basic_table_args(
              user_table = basic_table_args(title = "USER_TITLE_TABLE"),
              user_default = basic_table_args(title = "USER_TITLE_DEFAULT", prov_footer = "USER_FOOTER_DIRECT"),
              module_table = basic_table_args(subtitles = "DEVELOPER_SUBTITLE")
            )
          )
        }
      ), quote(
        rtables::basic_table(
          title = "USER_TITLE_TABLE", prov_footer = "USER_FOOTER_DIRECT",
          main_footer = "ENV_FOOTER", subtitles = "DEVELOPER_SUBTITLE"
        )
      )
    )
  }
)
