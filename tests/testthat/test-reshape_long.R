context("Checking reshape_long")

test_that("reshape_long fails on unequal series",{
  x <- r_data_frame(
    n = 500,
    id,
    age,
    sex, sex,
    r_series(date_stamp, 5, name = "Date"),
    r_series(likert, 5, name = "Question")
  )
  expect_error(reshape_long(x))
})

test_that("reshape_long generates correct nrows",{
  x <- r_data_frame(
    n = 500,
    id,
    age,
    sex,
    r_series(date_stamp, 5, name = "Date"),
    r_series(likert, 5, name = "Question")
  )
  expect_equal(nrow(reshape_long(x)), 500*5)
})

test_that("reshape_long can take multiple ids",{
  x <- r_data_frame(
    n = 500,
    id,
    age,
    sex,
    r_series(date_stamp, 5, name = "Date"),
    r_series(likert, 5, name = "Question")
  )
  expect_equal(nrow(reshape_long(x, c('ID','Age','Sex'))), 500*5)
})
