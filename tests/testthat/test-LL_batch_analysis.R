test_that("LL_batch_analysis returns correct types",
          {


            setwd(here::here('tests', 'testthat', 'test-data'))


            results <- LL_batch_analysis(dir ='att-lims/atterberg-limits_2021-06-03/', tin_tares = asi468::tin_tares)

            # computes both the values and makes the plots?
            expect_equal(length(results), 2)

            # correct value for non-NA data?
            expect_equal(round(results$LL_results$water_content[1], 4),
                         0.1544)

            # NA handled correctly?
            expect_identical(is.na(results$LL_results$water_content[2]),
                             TRUE)

            # NA values give a NULL value for plot?
            expect_identical(is.null(
              results$flow_curve_plots$water_content[2]
              ),
                             TRUE)

          })
