# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R - Project
# Silke Husse
# Winter Semester 2020/21
#
# scraping of media bias website (mediabiasfactcheck.com)
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

urls <- list("https://mediabiasfactcheck.com",
              "https://mediabiasfactcheck.com/left/",
              "https://mediabiasfactcheck.com/leftcenter/",
              "https://mediabiasfactcheck.com/center/",
              "https://mediabiasfactcheck.com/right-center/",
              "https://mediabiasfactcheck.com/right/")
#browseURL(urls[[1]])

# parse url
urls_parsed = list()
for (i in 1:length(urls)) {
  urls_parsed[[i]] <- xml2::url_parse(urls[[i]])
}

# check permission
factcheck_robotstxt <- urls_parsed[[1]]$server %>%
  robotstxt::robotstxt()
factcheck_robotstxt$permissions
factcheck_robotstxt$check(urls_parsed[[1]]$path)
factcheck_robotstxt$crawl_delay

# create folder structure
path <- file.path("WebScraping","data","factcheck")
if (!dir.exists(path)) {
    path %>%
      dir.create(recursive = TRUE)
  }

# download HTML files (main websites)
filename = list()
filename_new = list()
pb <- progress::progress_bar$new(total = (length(urls)-1),
                                 show_after = 0,
                                 clear = FALSE)
for (i in 1:(length(urls)-1)) {
  filename[[i]] <- urls[[i+1]] %>%
    basename() %>%
    stringr::str_c(".html")
  
  # download if file does not exist
  if (!file.exists(file.path(path, filename[[i]]))) {
    xml2::download_html(url = urls[[i+1]],
                        file = file.path(path, filename[[i]]))
  }
  # check for updated version if file already exists
  else {
    # download current version
    filename_new[[i]] <- urls[[i+1]] %>%
      basename() %>%
      stringr::str_c("_new.html")
    xml2::download_html(url = urls[[i+1]],
                        file = file.path(path, filename_new[[i]]))
    old = file.path(path, filename[[i]])
    new = file.path(path, filename_new[[i]])
    # compare hash sums
    if (md5sum(old) == md5sum(new)) {
      file.remove(new)
    }
    else{
      file.remove(old)
      file.rename(new,old)
      cat(sprintf("\n %s was updated\n", old))
    }
  }
  Sys.sleep(1) # voluntary crawl delay
  pb$tick() # progress bar tick
}

# parse HTML files (main websites)
websites_parsed <- list()
for (i in 1:length(filename)) {
  websites_parsed[[i]] <- path %>%
    file.path(filename[[i]]) %>%
    xml2::read_html()
}

# extract element (name + if available url) and ref link from main website
names <- list()
sub_urls <- list()
for (i in 1:length(websites_parsed)) {
  names[[i]] <- websites_parsed[[i]] %>%
    rvest::html_nodes(xpath = "/html/body/div/div/div/article/div/table/tbody/tr/td/span/a") %>%
    rvest::html_text()
  sub_urls[[i]] <- websites_parsed[[i]] %>%
    rvest::html_nodes(xpath = "/html/body/div/div/div/article/div/table/tbody/tr/td/span/a") %>%
    rvest::html_attr("href")
}
# 'right-center.html' has different html structure
names[[4]] <- websites_parsed[[4]] %>%
  rvest::html_nodes(xpath = "/html/body/div/div/div/article/div/div/table/tbody/tr/td/span/a") %>%
  rvest::html_text()
sub_urls[[4]] <- websites_parsed[[4]] %>%
  rvest::html_nodes(xpath = "/html/body/div/div/div/article/div/div/table/tbody/tr/td/span/a") %>%
  rvest::html_attr("href")

# store data (name, sub_url, bias)
num_elements = sum(unlist(lapply(names, length)))
bias <- list("left","left-center","center","right-center","right")
df <- matrix(ncol = 3, nrow = num_elements) %>%
  data.frame() %>%
  set_names(c("name", "sub_url", "bias"))
row_idx = 1
for (j in 1:length(names)) {
  for (i in 1:length(names[[j]])) {
    df[row_idx, 1] <- names[[j]][[i]]
    df[row_idx, 2] <- sub_urls[[j]][[i]]
    df[row_idx, 3] <- bias[[j]]
    row_idx = row_idx + 1
  }
}
# add col 'available' (boolean) to indicate url availability in name
df$available <- df$name %>%
  stringr::str_detect("\\((.*\\..*)\\)")
# save respective urls for further data collection
refs_urls <- df$sub_url[which(df$available == FALSE)]

# create folder structure
refs_path <- file.path("WebScraping","data","factcheck","sub_sites")
if (!dir.exists(refs_path)) {
    refs_path %>%
      dir.create(recursive = TRUE)
}

# download HTML files (sub websites)
sub_filename = list()
pb <- progress::progress_bar$new(total = length(refs_urls),
                                 show_after = 0,
                                 clear = FALSE)
for (i in 1:length(refs_urls)) {
  sub_filename[[i]] <- refs_urls[i] %>%
    basename() %>%
    stringr::str_c(".html")
  
  # download if file does not exist
  if (!file.exists(file.path(refs_path, sub_filename[[i]]))) {
    # error handling especially for HTTP error 404
    tryCatch({xml2::download_html(url = paste(urls[[1]], refs_urls[i], sep=""),
                                  file = file.path(refs_path, sub_filename[[i]]))}
             , error = function(e) {cat(sprintf("\n error for index i = %d\n", i))})
  }
  pb$tick() # progress bar tick
}

# parse HTML files (sub websites)
sub_websites_parsed <- list()
for (i in 1:length(sub_filename)) {
  try(
    sub_websites_parsed[[i]] <- refs_path %>%
      file.path(sub_filename[[i]]) %>%
      xml2::read_html(), silent = TRUE)
  # note : NULL objects for former errors
}

# extract website url 
sub_urls_parsed <- list()
for (i in 1:length(sub_websites_parsed)) {
  try(
    sub_urls_parsed[[i]] <- sub_websites_parsed[[i]] %>%
      rvest::html_nodes(xpath = "//p[contains(text(), 'Source:')]/a") %>%
      rvest::html_text(), silent = TRUE)
}
# some files have different html structure
for (i in 1:length(sub_websites_parsed)) {
  if (rlang::is_empty(sub_urls_parsed[[i]])) {
    try(
      sub_urls_parsed[[i]] <- sub_websites_parsed[[i]] %>%
        rvest::html_nodes(xpath = "//span[contains(text(), 'Source:')]/a") %>%
        rvest::html_text(), silent = TRUE)
  }
}
for (i in 1:length(sub_websites_parsed)) {
  if (rlang::is_empty(sub_urls_parsed[[i]])) {
    try(
      sub_urls_parsed[[i]] <- sub_websites_parsed[[i]] %>%
        rvest::html_nodes(xpath = "//span[contains(text(), 'Source:')]/following-sibling::a") %>%
        rvest::html_text(), silent = TRUE)
  }
}
for (i in 1:length(sub_websites_parsed)) {
  if (rlang::is_empty(sub_urls_parsed[[i]])) {
    try(
      sub_urls_parsed[[i]] <- sub_websites_parsed[[i]] %>%
        rvest::html_nodes(xpath = "//p[contains(text(), 'Notes:')]/a") %>%
        rvest::html_text(), silent = TRUE)
  }
}

# store data (url)
df$url <- FALSE
df$url[which(df$available == FALSE)] <- sub_urls_parsed

# save data frame
save(df, file="WebScraping/code/df_factcheck_raw.Rda")
