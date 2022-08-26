test_that("flow curves are right", {


  flow_indices <- compute_flow_index(
    dir = here::here("tests", "testthat", "test-data", "att-lims",
                     "atterberg-limits_2022-08-21"),
    tin_tares = asi468::tin_tares
  ) %>%
    dplyr::pull("flow_index")



  expect_equal(flow_indices, c(-0.0394154557420809, -0.0422662817661552, -0.0281530185567864),
               tolerance = 0.0001)
})
