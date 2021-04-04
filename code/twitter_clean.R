# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R - Project
# Silke Husse
# Winter Semester 2020/21
#
# data cleaning (twitter)
# ---------------------------------------------------------------------------------------

### preparations ###

rm(list=ls(all=TRUE))
setwd("/Web Data Collection with R/") # set working directory
source("R code/packages.R") # install and load packages

load("WebScraping/code/df_twitter_raw.Rda") # load data

### data cleaning ###

twitter_data <- tweets[which(!is.na(tweets))] # remove missing elements

# extract source url
get_urls <- function(x) {
  idx <- which(!is.na(x["url"]))
  return(x["url"][[1]][idx])
}
urls <- lapply(twitter_data, get_urls)

# clean source url
extract_url <- function(x) {
  url <- x %>%
    stringr::str_extract("([\\/\\/]){2}[^\\/]*") %>% # string between // and /
    stringr::str_sub(3,) %>% # remove former //
    gsub("^.*www\\.","", x=.) # remove redundant subdomain
  return(url)
}
urls <- lapply(urls, extract_url)

# save data frame and list
save(twitter_data, file="WebScraping/code/df_twitter_clean.Rda") 
save(urls, file="WebScraping/code/df_twitter_clean_url.Rda") 
