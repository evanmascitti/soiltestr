test_that("add_w() is greater than zero", {
  w <- data.frame(tin_w_wet_sample= 10.2,
              tin_w_OD_sample= 9.7,
              tin_tare= 7.2) %>%
    add_w() %>%
    .$water_content
  w_pos <- w > 0
  expect_equal(w_pos, TRUE)
})


