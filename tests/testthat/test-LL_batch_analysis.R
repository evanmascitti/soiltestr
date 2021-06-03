test_that("LL_batch_analysis returns correct types",
          {

            results <- LL_batch_analysis(dir = 'test-data/att-lims/atterberg-limits_2021-06-03/')

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
