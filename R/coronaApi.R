library(httr)
#' this package is used to retrieve data for covid 19
#'
#' @field url character.
#' @field response_list list.
#' @field countries list.
#'
#' @export coronaApi
#'
#' @import methods
#'
#' @exportClass coronaApi

coronaApi <- setRefClass(
Class = 'coronaApi',
  fields = list(
    url ="character",
    response_list = "list",
    country = "character"
  ),
  methods =list(
    initialize = function(){
      if(!is.character(country)){
        stop("Please provide a country")
      }
      url <<- 'https://covid19.mathdro.id/api/'
    },
    getOverAllList = function(){
      required_fields = c("confirmed", "recovered", "deaths", "dailySummary", "countries")
      AccidentURL <- url
      tryCatch(
        {
          apiResult <- GET(AccidentURL)

          OVerAllData <- content(apiResult, as = "parsed", type = "application/json")
          OverAlldff <- data.frame(
            confirmed = c(OVerAllData$confirmed$value),
            recovered = c(OVerAllData$recovered$value),
            deaths    = c(OVerAllData$deaths$value)
          )
          return(OverAlldff)
        },
        #error
        error = function(error_message) {
          message(error_message)
          return(NA)
        }
      )


    },
    getSingleCountryList = function(country){
      apiUrl <- paste0(paste0(url,'countries/'), country)

      countryData <- GET(apiUrl)
        tryCatch(
          {
            countryData <- content(countryData, as = "parsed", type = "application/json")
            if(length(countryData) > 1){
              singleDF <- data.frame(confirmed =c(countryData$confirmed$value), recovered = c(countryData$recovered$value), deaths = c(countryData$deaths$value) )
              return(singleDF)
            }else{
              stop("could not find record for the country")
            }
          },
          #error
          error = function(error_message) {
            message(error_message)
            return(NA)
          }
        )

    },
    getDailyReportedData  =  function(partial){

      dailyUrl <- paste0(url,partial)
      dailyData <- GET(dailyUrl)

      dailyData <- content(dailyData, as = "parsed", type = "application/json")

      tryCatch(
        {
          # print(dailyData)
          if(!hasName(dailyData, 'error')){
            datalist <- data.frame(totalConfirmed =c(), reportDate = c() )
            for (data in dailyData) {

               newdataframe <- data.frame(totalConfirmed = data$totalConfirmed, reportDate = data$reportDate )
               datalist <- rbind(newdataframe,datalist)
            }
             return(datalist)
          }else{
            stop("please enter correct type")
          }
        },
        #error
        error = function(error_message){
            message(error_message)
           return(NA)
        }
      )
    },

    getContrylist= function(){

      countriesList <- paste0(url,'countries')
      contrlist <- GET(countriesList)
      contrlist <- content(contrlist, as = "parsed", type = "application/json")

      tryCatch(
        {
          if(!hasName(contrlist, 'error')){
            countriesdf <- data.frame(country =c() )
             i <- 1
            for (data in contrlist$countries) {

              newdataframe <- data.frame(country = data$name )
              countriesdf <- rbind(newdataframe,countriesdf)
              i <- i+1
            }
            return(countriesdf)
          }else{
            stop("please enter correct type")
          }
        },
        #error
        error = function(error_message){
          message(error_message)
          return(NA)
        }
      )
    }
))




