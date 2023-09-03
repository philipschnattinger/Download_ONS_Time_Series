ons_download <- function(url, frequency) { 
  # Function written by Philip Schnattinger
  # Url enter ONS time series in JSON form
  # Enter frequency: Acceptable inputs: "months", "quarters", "years"
  # Only works for existing frequencies
  # Example:
  # url = "https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms"
  # series = ons_download(url, "months")
  
  require("rjson"); require("data.table"); require("tidyverse");
  require("lubridate")
  url<- paste0(url,"/data")
  jsonfile <- rjson::fromJSON(file = url)
  combination <- paste0("jsonfile$",frequency)
  df <-rbindlist(eval(parse(text=combination)))%>%
    select(value, date, sourceDataset)%>% tibble()%>%
    mutate(value = as.double(value))
  if (frequency == "months") {
    df<- df%>% mutate(date = ym(date))
  }
  if (frequency == "quarters") {
    df<- df%>% mutate(date = yq(date))
  }
  if (frequency == "years") {
    df<- df%>% mutate(date = years(date))%>%
      mutate(date = as.character(date))%>%
      mutate(date = as.double(substr(date,1,4)))
  }
  return(df)
}

