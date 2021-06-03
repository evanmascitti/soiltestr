

# working directory when tests are run is tests/testthat

# just for interactive use; help with autocomplee
# setwd(here::here('tests/testthat/'))

LL_data <- readr::read_csv('test-data/att-lims/atterberg-limits_2021-06-03/LL-raw-data_2021-06-03.csv', col_types = 'cDcciiiddcc', na = '-') |>
  dplyr::left_join(asi468::tin_tares$`2020-05-24`, by = c('tin_tare_set', 'tin_number')) |>
  add_w()

test_that(desc = 'compute_LL handles missing data',
          code = {
            expect_true(is.na(compute_LL(df = LL_data[5:8, ])))
          })


test_that('compute_LL calculates LL correctly',
           code = {
             expect_equal(object = unname(round(compute_LL(df = LL_data[1:4, ]), digits = 4)),
                          expected = 0.1544)
           })


