# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R - Project
# Silke Husse
# Winter Semester 2020/21
#
# data cleaning (mediabiasfactcheck.com)
# ---------------------------------------------------------------------------------------

### preparations ###

rm(list=ls(all=TRUE))
setwd("/Web Data Collection with R/") # set working directory
source("R code/packages.R") # install and load packages

load("WebScraping/code/df_factcheck_raw.Rda") # load data

### data cleaning ###

# remove row if missing or corrupted value
remove_idx <- list()
idx <- 1
for(i in 1:nrow(df)) {
  # check for NULL, character(0), or other irregularities
  if (length(unlist(df$url[i])) == 0 | length(unlist(df$url[i])) > 1) {
    remove_idx[idx] <- i
    idx <- idx + 1
  }
}
df <- df[-unlist(remove_idx),]

df <- subset(df, select = -c(sub_url,available)) # remove redundant cols
df$url <- sapply(df$url, function(x) unlist(x)) # unlist col 'url'

# extract url from name (if applicable)
df$url[which(df$url == FALSE)] <- df$name[which(df$url == FALSE)] %>%
  stringr::str_extract("\\(([^)]*)\\)[^(]*$") %>% # string between ( and )
  stringr::str_sub(2,-2) # remove outer ( and )

# cleaning
df$name <- gsub("\\(.*$","",df$name) # remove url given within brackets
df$url <- gsub("(^.*:\\/\\/)|(^.*www\\.)|(\\/?)","",df$url) # remove scheme and subdomain from url + '/' at the end

df$bias_num <- ifelse(df$bias == "left", -1,
                ifelse(df$bias == "left-center", -0.5,
                ifelse(df$bias == "center", 0,
                ifelse(df$bias == "right-center", 0.5, 
                ifelse(df$bias == "right", 1,
                NA)))))
  
# save data frame
save(df, file="WebScraping/code/df_factcheck_clean.Rda") 