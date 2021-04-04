# Web Data Collection Project

Seminar Project Winter Term 2020/21 - Web Data Collection with R

This repository incoporates all requiered files to successfully execute a webscraping project with R. It comprises 3 steps, each step implementing a different strategy of web data collection. Overall, the goal is to connect the concept of media bias to url citations in tweets of members of the German Bundestag. For more information, I refer to `report.pdf`.

## Data

1) Media Bias Placements of News Mediums

*https://mediabiasfactcheck.com* provides positionings of over 3,600 media sources
- `mediabias_factcheck.R` implements web data collection of multiple static subwebsites using XPath queries
- `mediabias_factcheck_clean.R` performs data cleaning on `df_factcheck_raw.Rda`
- resulting data : `df_factcheck_clean.Rda`

2) Information about Politicians

*https://www.bundestag.de/abgeordnete/biografien* provides names and political affiliation of members of the German Bundestag
- `bundestag.R` implements web data collection of the dynamic website using RSelenium
- `bundestag_clean.R` performs data cleaning on `df_bundestag_raw.Rda`
- resulting data : `df_bundestag_clean.Rda`

3) Tweets

- `twitter.R` implements web data collection using the GET statuses/user_timeline endpoint of the Twitter API
- `twitter_clean.R` performs data cleaning on `df_twitter_raw.Rda`
- resulting data : `df_twitter_clean.Rda` and `df_twitter_clean_url.Rda`

`analysis.R` implements the analysis described in `report.pdf`

## Author Information

**Silke Husse** <br />
Email: silke.husse@uni-konstanz.de 
