library(mapuloh)
library(testthat)
library(plyr)

context("Coord lookup")

test_that("Overall test",{
          expect_that(coord_lookup(address = 2),
                      throws_error("Use character values"))
          expect_that(is.data.frame(coord_lookup(address = "björnskogsgränd 18")),
                      is_true())
          expect_that(coord_lookup(address = "90 Orchard St, New York, NY 10002, USA")[,"lat"],
                      equals(40.718181))
          expect_that(coord_lookup(address = "?"),
                      throws_error("No match found"))
})

context("Address lookup")

test_that("Overall test", {
          expect_that(address_lookup(latlong = 2),
                     throws_error("Use character values"))
          expect_that(address_lookup(latlong = "björnskogsgränd 18"),
             throws_error("No match found"))
})





