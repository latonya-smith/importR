#Each function should be in its own file

#' @title Import a dataset
#'
#' @description
#' This \code{import} function can import data from
#' delimited text files, EXCEL spreadsheets, and
#' statistical packages such as SAS, SPSS, and Stata.
#'
#' @details
#' The import function is a wrapper for the
#' \href{https://haven.tidyverse.org/}{haven},
#' \href{https://readxl.tidyverse.org/}{readxl},
#' and \href{https://github.com/r-lib/vroom}{vroom} packages.
#'
#' @seealso
#' read_sas,
#' read_dta,
#' read_spss,
#' read_excel,
#' fromJSON,
#' vroom
#'
#' @param file datafile to import
#' @param ... parameters passed to the import function.
#'
#' @import haven
#' @import readxl
#' @import vroom
#' @import tools
#' @import rjson
#'
#' @export
#' @return a dataframe




import <- function(file, ...){

 if (missing(file)){
  file <- file.choose()
 }

 if (!file.exists(file)){
  stop("File doesn't exist! Please choose another file")
 }

 extension <- tools::file_ext(file)

 if(extension == "xls" | extension == "xlsx"){
  dataset <- readxl::read_excel(file, ...)
 }

 else if(extension == "sas7bdat") {
  dataset <- haven::read_sas(file, ...)
 }

 else if(extension =="sav") {
  dataset <- haven::read_sav(file, ...)
 }

 else if(extension == "dta") {
  dataset <- haven::read_dta(file, ...)
 }

 else if(extension == "json"){
  dataset <- rjson::fromJSON(file, ...)
 }

 else {
  dataset<- vroom::vroom(file, ...)
 }

 return(dataset)
}
