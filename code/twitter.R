# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R - Project
# Silke Husse
# Winter Semester 2020/21
#
# scraping of tweets (via twitter API)
# ---------------------------------------------------------------------------------------

### preparations ###

rm(list=ls(all=TRUE))
setwd("/Web Data Collection with R/") # set working directory
source("R code/packages.R") # install and load packages

load("WebScraping/code/df_bundestag_clean.Rda") # load data
members <- df[which(!is.na(df$twitter)),] # select politicians with available twitter name

# set user-agent
str_c("email", # insert email here
      "collecting data for study purposes",
      R.version$platform,
      R.version$version.string,
      sep = ", ") %>%
  httr::user_agent() %>%
  httr::set_config()

url <- "https://twitter.com/home"
#browseURL(url)
url_parsed <- xml2::url_parse(url) # parse url
# check permission
twitter_robotstxt <- url_parsed$server %>%
  robotstxt::robotstxt()
twitter_robotstxt$permissions
twitter_robotstxt$check(url_parsed$path)
twitter_robotstxt$crawl_delay

# access twitter API via OAuth 1.0
# assign your personal credentials here !
consumer_key <- "xxx"
consumer_secret <- "xxx"
access_token <- "xxx"
access_secret <- "xxx"
application <- httr::oauth_app(appname = "ScRapeSeminar",
                               key = consumer_key, 
                               secret = consumer_secret)
signature <- httr::sign_oauth1.0(application, 
                                 token = access_token,
                                 token_secret = access_secret)

### data collection ###

tweets <- vector(mode="list", length=nrow(members))
names(tweets) <- members$name

for (j in 1:nrow(members)) { 
  
  posts <- list() # list for requests (each containing 200 tweets)

  # query full tweets (200 most recent posts) using API Standard v1.1 and GET
  get_posts <- try(httr::RETRY(verb = "GET",
                         url = "https://api.twitter.com/1.1/statuses/user_timeline.json?",
                         query = list(screen_name = members$twitter[j],
                                      trim_user = "true",
                                      tweet_mode = "extended", # full tweet
                                      count = 200), # max number of tweets per call
                         config = signature,
                         times = 100,
                         quite = FALSE,
                         terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 422, 429)))
  posts_content <- httr::content(get_posts)
  
  # if no tweets or other error
  if (length(posts_content) == 0 || length(posts_content$errors) != 0 || length(posts_content$error) != 0) {
    tweets[[j]] <- NA
    
    Sys.sleep(1) # crawl delay
    current_rate_limit <- as.integer(get_posts$headers$`x-rate-limit-remaining`)
    current_rate_limit_reset <- get_posts$headers$`x-rate-limit-reset` %>%
      as.integer() %>%
      as.POSIXct(origin = "1970-01-01", tz="CET")
    # check if rate limit is exhausted
    if (current_rate_limit <= 1) {
      # suspend exexution until rate limit is reset
      Sys.sleep(current_rate_limit_reset-Sys.time())
    }
  }
  else  { 
    # parse tweets (id, content)
    parsed_posts <- posts_content %>%
      purrr::map_dfr(magrittr::extract, c("id_str", "full_text"))
    
    parsed_posts$url <- NA
    # check if any url is cited in tweet, if applicable save in col 'url'
    for (i in 1:nrow(parsed_posts)) {
      if (length(posts_content[[i]]$entities$urls) != 0) {
        parsed_posts$url[i] <- posts_content[[i]]$entities$urls[[1]]$expanded_url
      }
    }

    # add col 'retweet' (boolean) to indicate retweets
    parsed_posts$retweet <- str_detect(parsed_posts$full_text, "^RT ")

    last_id_str <- tail(parsed_posts$id_str, 1)
    posts[[1]] <- parsed_posts
  
    Sys.sleep(1) # crawl delay
    current_rate_limit <- as.integer(get_posts$headers$`x-rate-limit-remaining`)
    current_rate_limit_reset <- get_posts$headers$`x-rate-limit-reset` %>%
      as.integer() %>%
      as.POSIXct(origin = "1970-01-01", tz="CET")
    # check if rate limit is exhausted
    if (current_rate_limit <= 1) {
      # suspend exexution until rate limit is reset
      Sys.sleep(current_rate_limit_reset-Sys.time())
    }

    # repeat procedure to get more tweets (in total 1,000 most recent posts)
    for (k in 2:5) {
      # query full tweets (200 most recent posts since last_id_str) using API Standard v1.1 and GET
      get_posts <- try(httr::RETRY(verb = "GET",
                         url = "https://api.twitter.com/1.1/statuses/user_timeline.json?",
                         query = list(screen_name = members$twitter[j],
                                      trim_user = "true",
                                      tweet_mode = "extended", # full tweet
                                      count = 200, # max number of tweets per call
                                      max_id = last_id_str),
                         # default is 20
                         config = signature,
                         times = 100,
                         quite = FALSE,
                         terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 422, 429)))
      posts_content <- httr::content(get_posts)
  
      # parse tweets (id, content)
      parsed_posts <- posts_content %>%
        purrr::map_dfr(magrittr::extract, c("id_str", "full_text"))
      
      parsed_posts$url <- NA
      # check if any url is cited in tweet, if applicable save in col 'url'
      for (i in 1:nrow(parsed_posts)) {
        if (length(posts_content[[i]]$entities$urls) != 0) {
          parsed_posts$url[i] <- posts_content[[i]]$entities$urls[[1]]$expanded_url
        }
      }

      # add col 'retweet' (boolean) to indicate retweets
      parsed_posts$retweet <- str_detect(parsed_posts$full_text, "^RT ")
      
      last_id_str <- tail(parsed_posts$id_str, 1)
      posts[[k]] <- parsed_posts

      Sys.sleep(1) # crawl delay
      current_rate_limit <- as.integer(get_posts$headers$`x-rate-limit-remaining`)
      current_rate_limit_reset <- get_posts$headers$`x-rate-limit-reset` %>%
        as.integer() %>%
        as.POSIXct(origin = "1970-01-01", tz="CET")
      # check if rate limit is exhausted
      if (current_rate_limit <= 1) {
        # suspend exexution until rate limit is reset
        Sys.sleep(current_rate_limit_reset-Sys.time())
      }
    }
  
  all_posts <- do.call("rbind", posts) # combine all requests in data frame
  tweets[[j]] <- all_posts[!duplicated(all_posts$id_str), ] # remove duplicates
  }
}

# save list
save(tweets, file="WebScraping/code/df_twitter_raw.Rda")
