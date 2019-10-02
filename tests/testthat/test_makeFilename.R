
test_that("make_filename works correct",
          {
            expect_equal(make_filename(2015),system.file("extdata",
                                                         "accident_2015.csv.bz2",
                                                         package = "TestPackage",
                                                         mustWork = TRUE))
            expect_equal(make_filename("2014"),system.file("extdata",
                                                           "accident_2014.csv.bz2",
                                                           package = "TestPackage",
                                                           mustWork = TRUE))
          })

test_that("summarize function is 12x2 dimensional",
          {
            expect_equal(dim(fars_summarize_years(2015)), c(12,2))
          })
