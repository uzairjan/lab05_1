library(httr)
obj <- coronaApi()

test_that("Type checking",{
  expect_type(obj$getSingleCountryList("sweden"), "list")

  expect_type(obj$getOverAllList(), "list")

  expect_type(obj$getDailyReportedData('daily'), "list")
  expect_type(obj$getContrylist(), "list")

})

test_that("should always return greater than zero", {
  expect_true(length(obj$getOverAllList())!=0)
  expect_true(length(obj$getDailyReportedData('daily'))!=0)
  expect_true(length(obj$getSingleCountryList("sweden"))!=0)
  expect_true(length(obj$getContrylist()) !=0)

})

test_that("check keywords", {
  cols  <- obj$getOverAllList()

  attrib_cols<-c("confirmed" , "recovered"  ,  "deaths" )
  expect_equal(attributes(cols)[[1]],attrib_cols)

  cols_swed  <- obj$getSingleCountryList("sweden")
  attrib_swed<-c("confirmed" , "recovered"  ,  "deaths" )
  expect_equal(attributes(cols_swed)[[1]],attrib_swed)

  cols_daily  <- obj$getDailyReportedData("daily")
  attrib_daily<-c("totalConfirmed" , "reportDate" )
  expect_equal(attributes(cols_daily)[[1]],attrib_daily)


  cols_countries  <- obj$getContrylist()
  attrib_country<-c("country" )
  expect_equal(attributes(cols_countries)[[1]],attrib_country)

})

test_that("expect error to be returned",{
  input <-  "ads"
  expect_error(obj$getSingleCountryList(input))

  expect_error(obj$getDailyReportedData(input))

  expect_error(obj$getOverAllList(input))

  expect_error(obj$getContrylist(input))

})










