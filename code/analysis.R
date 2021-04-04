# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R - Project
# Silke Husse
# Winter Semester 2020/21
#
# analysis
# ---------------------------------------------------------------------------------------

### preparations ###

rm(list=ls(all=TRUE))
setwd("/Web Data Collection with R/") # set working directory
source("R code/packages.R") # install and load packages

# load data
# 1) media_bias : media bias placement of news mediums
# 2) politicians : information (political party, sex, ethnicity, twitter display name, urls)
# 3) twitter : tweets (id_str, full_text, url, retweet)
load("WebScraping/code/df_factcheck_clean.Rda")
media_bias <- df
load("WebScraping/code/df_bundestag_clean.Rda")
politicians <- df 
load("WebScraping/code/df_twitter_clean.Rda")
twitter <- twitter_data
load("WebScraping/code/df_twitter_clean_url.Rda")
politicians$url <- NA
politicians$url[which(politicians$name %in% names(twitter))] <- urls
rm(df, twitter_data) # remove redundant variables

# save plots in pdf
pdf(file="WebScraping/data/plots.pdf")

### analysis ###

# notes : 
# - exclude 'fraktionslos' in analysis
# - denote bundestag with politicians presented on website (may not be actual number of members)

n_bundestag <- nrow(politicians)
n_twitter <- length(twitter)

# CDU/CSU, Die Linke, FDP, Bündnis90/Die Grünen, SPD, AfD, fraktionslos
pp <- unique(politicians$political_party)

# names of polititcians by political party (bundestag) 
pp_politicians <- list()
for (i in 1:length(pp)) {
  pp_politicians[[i]] <- politicians$name[which(politicians$political_party == pp[i])]
}
names(pp_politicians) <- pp

# information of politicians by political party (twitter)
pp_twitter <- list()
for (i in 1:length(pp)) {
  pp_twitter[[i]] <- twitter[which(names(twitter) %in% pp_politicians[[i]])]
}
names(pp_twitter) <- pp

# configurations for plotting
colors_name <- c("black", "deeppink", "yellow", "green", "red", "deepskyblue")
colors_sorted <- c("deepskyblue", "green", "grey30", "deeppink", "yellow", "grey", "red")
colors_bundestag <- c("deepskyblue", "yellow", "grey30", "green", "red", "deeppink")
colors_rgb <- col2rgb(colors_name)
colors <- list()
for (i in 1:(length(pp)-1)) {
  colors[[i]] <- rgb(colors_rgb[1,i],
                     colors_rgb[2,i],
                     colors_rgb[3,i],
                     maxColorValue = 255, 
                     alpha = 100)
}
names(colors) <- pp[-length(pp)]

### twitter account usage ###

# share of twitter accounts in whole bundestag
( n_twitter / n_bundestag ) * 100

# per political party
n_pp_accounts <- list()
for (i in 1:length(pp)) {
  n_pp_accounts[[i]] <- length(pp_twitter[[i]])
}
names(n_pp_accounts) <- pp
( unlist(n_pp_accounts) / lengths(pp_politicians) ) * 100

# distribution across political party
share_bundestag <- table(politicians$political_party) / n_bundestag
share_twitter <- table(politicians$political_party[which(politicians$name %in% names(twitter))]) / n_twitter
( share_twitter - share_bundestag ) *100
# pie-charts denoting share for bundestag and twitter, respectively
par(mfrow = c(1,2))
pie(share_bundestag, main="Bundestag", col=colors_sorted,labels=names(share_bundestag))
pie(share_twitter, main="Twitter", col=colors_sorted,labels=names(share_twitter))

### twitter account activity ###

# average number of tweets per politician
n_tweets <- lapply(twitter, function(x) length(x$full_text))
mean(unlist(n_tweets))

# per political party
n_pp_tweets <- list() 
for (i in 1:(length(pp)-1)) {
    n_pp_tweets[[i]] <- mean(unlist(n_tweets[which(names(n_tweets) %in% pp_politicians[[i]])]))
}
names(n_pp_tweets) <- pp[-length(pp)]
unlist(n_pp_tweets)

# bar-chart denoting average number of tweets per political party
barplot(sort(unlist(n_pp_tweets), decreasing = TRUE), col = c("green", "deeppink", "yellow", "red", "deepskyblue", "black"), main = "Average Tweets")

### url citation usage ###

# average number of url citation used in (996) tweets per politician
mean(lengths(urls))
( sum(lengths(urls)) / sum(unlist(n_tweets)) ) * 100

# per political party
n_pp_citations <- list()
for (i in 1:(length(pp)-1)) {
  n_pp_citations[[i]] <- mean(lengths(urls[which(names(urls) %in% pp_politicians[[i]])]))
}
names(n_pp_citations) <- pp[-length(pp)]
citation_usage <- ( unlist(n_pp_citations) / unlist(n_pp_tweets) ) * 100
citation_usage

# bar-chart denoting url citation usage per political party
barplot(sort(citation_usage, decreasing = TRUE), col = c("deepskyblue", "deeppink", "yellow", "green", "black", "red"), main = "Url Citation")

### media bias score ###

get_bias <- function(x) {
  websites <- x[which(x %in% media_bias$url)]
  bias <- lapply(websites, function(x) media_bias$bias_num[which(media_bias$url == x)])
  names(bias) <- websites
  return(bias)
}
# bias placements of all urls cited for each politician
bias_politicians <- lapply(urls, get_bias)

# store data (name, political party, number of bias placed urls, bias by cont. mean, var)
df <- matrix(ncol = 6, nrow = n_twitter) %>%
  data.frame() %>%
  set_names(c("name", "political_party", "sex", "n_urls", "bias_mean", "bias_var"))
df$name <- names(bias_politicians) # col : 'name'
row_idx = 1
for (i in 1:n_twitter) {
  df[row_idx, 2] <- politicians$political_party[which(politicians$name == df$name[i])] # col : 'political_party'
  df[row_idx, 3] <- politicians$sex[which(politicians$name == df$name[i])]
  if (length(bias_politicians[[i]]) != 0) {
    bias_table <- table(unlist(bias_politicians[[i]]))
    bias_total <- length(bias_politicians[[i]])
    bias_mean <- sum(bias_table*as.numeric(names(bias_table))) / bias_total
    df[row_idx, 4] <- bias_total # col : 'n_urls'
    df[row_idx, 5] <- bias_mean # col : 'bias_mean'
    df[row_idx, 6] <- sum(bias_table*((as.numeric(names(bias_table))-bias_mean)^2)) / bias_total # col : 'bias_var'
  }
  else {
    df[row_idx,3:5] <- NA
  }
  row_idx = row_idx + 1
}
# clean data frame to get reliable data with respect to bias placement
df <- df[which(!is.na(df$n_urls)),]    # 1) remove missing rows
df <- df[which(df$n_urls > 1),]        # 2) remove rows with only 1 url (not representative)

### plotting ###

min <- -1
max <- 1
ax <- pretty(min:max, n = 30)

# histogram : whole data
hist(df$bias_mean, main="Media Bias", xlim = c(min,max), xlab = "Bias Score")

pp_df <- list()
pp_hist <- list()
for (i in 1:(length(pp)-1)) {
  pp_df[[i]] <- df[which(df$political_party == pp[i]),]
  pp_hist[[i]] <- hist(pp_df[[i]]$bias_mean, breaks = ax, plot = FALSE)
}
names(pp_df) <- pp[-length(pp)]
names(pp_hist) <- pp[-length(pp)]

# histogram : political party
par(mfrow = c(3,2))
plot(pp_hist$`Die Linke`, col = colors$`Die Linke`, main = pp[[2]], xlab = "Bias Score")
plot(pp_hist$`CDU/CSU`, col = colors$`CDU/CSU`, main = pp[[1]], xlab = "Bias Score")
plot(pp_hist$`Bündnis 90/Die Grünen`, col = colors$`Bündnis 90/Die Grünen`, main = pp[[4]], xlab = "Bias Score")
plot(pp_hist$FDP, col = colors$FDP, main = pp[[3]], xlab = "Bias Score")
plot(pp_hist$SPD, col = colors$SPD, main = pp[[5]], xlab = "Bias Score")
plot(pp_hist$AfD, col = colors$AfD, main = pp[[6]], xlab = "Bias Score")
# histogram : comparison bt Die Linke and AfD
plot(pp_hist$`Die Linke`, col = colors$`Die Linke`, main = "Die Linke vs AfD", xlab = "Bias Score")
plot(pp_hist$AfD, col = colors$AfD, add = T)

pp_bias_placements <- lapply(pp_df, function(x) x$bias_mean)
pp_bias_placements_sorted <- pp_bias_placements[c("AfD","FDP","CDU/CSU","Bündnis 90/Die Grünen","SPD","Die Linke")]
par(mar = c(5.1, 11.1, 4.1, 2.1))
boxplot(pp_bias_placements_sorted, col = colors_bundestag, horizontal = T, ylim = c(-0.75,0.75), las = 1)

# outliers per political party
outlier_linke_max <- pp_df$`Die Linke`$name[match(max(pp_df$`Die Linke`$bias_mean),pp_df$`Die Linke`$bias_mean)]
unlist(bias_politicians$`Katrin Werner`)
outlier_linke_min <- pp_df$`Die Linke`$name[match(min(pp_df$`Die Linke`$bias_mean),pp_df$`Die Linke`$bias_mean)]
unlist(bias_politicians$`Jörg Cezanne`)
outlier_spd_max <- pp_df$SPD$name[match(max(pp_df$SPD$bias_mean),pp_df$SPD$bias_mean)]
unlist(bias_politicians$`Edgar Franke`)
outlier_gruen_min <- pp_df$`Bündnis 90/Die Grünen`$name[match(min(pp_df$`Bündnis 90/Die Grünen`$bias_mean),pp_df$`Bündnis 90/Die Grünen`$bias_mean)]
unlist(bias_politicians$`Julia Verlinden`)
outlier_afd_min <- pp_df$AfD$name[match(min(pp_df$AfD$bias_mean), pp_df$AfD$bias_mean)]
unlist(bias_politicians$`Marcus Bühl`)

# close pdf 
dev.off()
