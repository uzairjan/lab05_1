library(httr)
obj <- coronaApi()


test_that("Type checking",{
  expect_type(obj$getSingleCountryList("sweden"), "list")

  expect_type(obj$getOverAllList(), "list")

  expect_type(obj$getOverAllList(), "list")

  expect_type(obj$getOverAllList(), "list")
})

test_that("expect error to be returned",{
  input <-  "ads"
  expect_error(obj$getSingleCountryList(input))
  input_error <-c(1,"2015-06-06-21-00",1)

  expect_error(obj$getSingleCountryList(input))

  expect_error(obj$getOverAllList("test"))

  expect_error(obj$getOverAllList(input))

  input_error <-c(1,"pakistan")

  expect_error(obj$getSingleCountryList(input))
})










