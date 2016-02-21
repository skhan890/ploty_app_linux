## trying to add the options
##
library(shiny)
library(sas7bdat)
library(XLConnect)
library(rsatscan)
library(rgdal)
library(sp)
library(plyr)
library(htmltools)
library(shinyjs)
library(shinythemes)
library(shinyBS)
library(XML)
# library(ggvis)
# library(ggplot2)
# library(devtools)
# library(rCharts)
library(stringi)
library(stringr)
library(qdapRegex)
library(dplyr)
library(lubridate)
library(plotly)
library(htmlTable)

#system.file("SaTScan/SaTScan.exe", package="rsatscan") 

##DATE FUNCTION##
outputDir<-"www/"
#defects<-read.table("www/Icons/list_of_Defects.txt", header=T, sep=",")

mydateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
                        format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", minviewmode="months",
                        width = NULL) {
  
  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    tags$div(id = inputId,
             class = "shiny-date-input form-group shiny-input-container",
             style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
             
             controlLabel(inputId, label),
             tags$input(type = "text",
                        # datepicker class necessary for dropdown to display correctly
                        class = "form-control datepicker",
                        `data-date-language` = language,
                        `data-date-weekstart` = weekstart,
                        `data-date-format` = format,
                        `data-date-start-view` = startview,
                        `data-date-min-view-mode` = minviewmode,
                        `data-min-date` = min,
                        `data-max-date` = max,
                        `data-initial-date` = value
             )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

datePickerDependency <- htmlDependency(
  "bootstrap-datepicker", "1.0.2", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/datepicker.css")
