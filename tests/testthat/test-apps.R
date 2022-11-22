
# tests the example apps in ./apps
# each app lives in its own directory, specific to the purpose of the test(s)
# the test specification lives under the app directory

# example:
# apps/pws_ggplot2 tests plot with settings click functionality with a ggplot2
# apps/tws tests table with settings: manipulating and downloading


skip_if_too_deep(5)
skip_on_cran()
skip_on_ci()
