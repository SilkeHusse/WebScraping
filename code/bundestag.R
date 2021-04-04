# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R - Project
# Silke Husse
# Winter Semester 2020/21
#
# scraping of bundestag website (bundestag.de)
# ---------------------------------------------------------------------------------------

### preparations ###

rm(list=ls(all=TRUE))
setwd("/Web Data Collection with R/") # set working directory
source("R code/packages.R") # install and load packages

# set user-agent
str_c("email", # insert email here 
      "collecting data for study purposes",
      R.version$platform,
      R.version$version.string,
      sep = ", ") %>%
  httr::user_agent() %>%
  httr::set_config()

### data collection ###

url <- "https://www.bundestag.de/abgeordnete/biografien"
#browseURL(url)
url_parsed <- xml2::url_parse(url) # parse url
# check permission
bundestag_robotstxt <- url_parsed$server %>%
  robotstxt::robotstxt()
bundestag_robotstxt$permissions
bundestag_robotstxt$check(url_parsed$path)
bundestag_robotstxt$crawl_delay

# connect to server
remote_driver <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox")
remote_driver$open()
#remote_driver$getStatus()
remote_driver$navigate("https://www.bundestag.de/abgeordnete/biografien")

final_data <- list()
# note : currently 709 members à 12 per view -> 60 button clicks
# additional 1 button clicks as 34 members dropped out but are still presented on website
for (i in 1:62) {
  
  # web element containing 12 members
  person_element <- remote_driver$findElement(using = "xpath", "//div[@class = 'slick-track']")
  final_data[[i]] <- person_element$getElementText()[[1]]

  # press button to get next web element
  button_element <- remote_driver$findElement(using = "xpath", "//button[@class = 'slick-next slick-arrow']")
  button_element$sendKeysToElement(list("\uE007")) # encoding for key 'enter'

  Sys.sleep(3) # voluntary crawl delay
}

# save data
save(final_data, file="WebScraping/code/df_bundestag_raw.Rda") 

remote_driver$close()
