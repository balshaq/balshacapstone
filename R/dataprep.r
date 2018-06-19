library(readr)
library(dplyr)
library(sqldf)
library(dplyr)
library(lubridate)
library(tidyr)
library(tools)


#' Mastering Software Development in R Specialization Capstone Project

#' Data loading function
#' The function loads the raw NOAA data from a given file to a dataframe
#' Data loading function
#' The function loads the raw NOAA data from a given file to a dataframe

#' @importFrom readr read_delim
#'
#' @param filename  A name of the file
#'
#' @return  dataset
#'
#' @examples
#' \dontrun{
#'   sample_file <- eq_clean_data("signif.txt")
#' }
#'
#' @export


eq_read_data <- function (filename) {
if(!file.exists(filename))
    stop("file '", filename, "' does not exist")

 read_delim(filename, delim = "\t",col_names = TRUE,na = c("", "NA")) %>% select(YEAR,MONTH,DAY,LATITUDE,LONGITUDE,LOCATION_NAME) %>% filter(!is.na(DAY))
}


#' Data cleaning function
#' The function  takes raw NOAA data frame and returns a clean data frame.
#'
#' @param raw_data The raw NOAA dataset (data frame)
#'
#' @importFrom dplyr mutate
#'
#' @return Clean dataset
#'
#' @examples
#' \dontrun{
#'   clean_data <- eq_clean_data(raw_data)
#' }
#'
#' @export


eq_clean_data <- function(x) {
  date <- NULL
  LATITUDE <- NULL
  LONGITUDE <- NULL
  data <- sqldf("select trim(YEAR,'-')||MONTH||DAY date ,LATITUDE,LONGITUDE,LOCATION_NAME  from x limit  10")  %>%
    dplyr::mutate(LATITUDE= as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE= as.numeric(LONGITUDE)) %>%
    dplyr::mutate(date = lubridate::ymd(date))
  return(data)
}




#' Location cleaning function
#' The function takes NOAA data and cleans the LOCATION_NAME column
#' by stripping out the country name (including the colon) and
#' converting names to title case
#'
#' @importFrom dplyr mutate
#'
#' @param raw_data The raw NOAA dataset
#'
#' @return Dataset with cleaned LOCATION_NAME column
#'
#' @examples
#' \dontrun{
#'   clean_data <- location_clean(raw_data)
#' }
#'
#' @export


location_clean <- function(x) {
  LOCATION_NAME <- NULL
  data <- x %>%
    dplyr::mutate_each(funs(gsub(".*:", "", LOCATION_NAME)),LOCATION_NAME)%>%
    dplyr::mutate(LOCATION_NAME = base::tolower(LOCATION_NAME)) %>%
    dplyr::mutate(LOCATION_NAME = tools::toTitleCase(LOCATION_NAME))

  return(data)
}
