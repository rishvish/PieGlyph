# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(PieGlyph)

# Taken from ggside see github issue #141 on vdiffr
if ((nzchar(Sys.getenv("CI")) ||
     !nzchar(Sys.getenv("NOT_CRAN"))) &&
    identical(Sys.getenv("VDIFFR_RUN_TESTS"), 'false')) {
  #if we are running tests remotely AND
  # we are opting out of using vdiffr
  # assigning a dummy function

  expect_doppelganger <- function(...) {
    testthat::skip("`VDIFFR_RUN_TESTS` set to false on this remote check")
  }
} else {
  expect_doppelganger <- vdiffr::expect_doppelganger
}

test_check("PieGlyph")
