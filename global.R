#
#
# octavian
# Git
# /OneDrive/metis2/metic_select_retrieve/octavian
# - resegment_observe branch

# SERVER
#
#
# IMPORTANT NOTE - If plyr is loaded after dplyr (in tidyverse()), then group_by statement will fail
# with group factor not carried across
#
#

library(shiny)
library(tidyverse)
library(RMariaDB)
library(rlang)

print( "Global 0 - About to connect - Server/Remote 1")
# Establish connection to Digital Ocean (remote) database
print("Global 1 - Pre creation of conR")

remoteuserpassword <- "m3t1sz"
conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
print("Connected remote 1")
dbListTables(conR)

print("Global 2 - Post creation of conR")
## Normalise for factor which has positive and negative values in single variable
sa_method_norm1 <- function(score_name){
  score_pos <- sum(score_name[score_name >0], na.rm = TRUE)
  score_neg <- sum(score_name[score_name <0], na.rm = TRUE)
  if(score_pos == 0){score_pos <- 1}
  if(score_neg == 0){score_pos <- 1}
  score_adjust <- score_neg/score_pos
}

## Normalise for factor which has positive and negative values in separate variables
sa_method_norm2 <- function(score_name1, score_name2){
  score_pos <- sum(score_name1[score_name1 >0], na.rm = TRUE)
  score_neg <- -1* sum(score_name2[score_name2 <0], na.rm = TRUE)
  if(score_pos == 0){score_pos <- 1}
  if(score_neg == 0){score_pos <- 1}
  score_adjust <- score_neg/score_pos
}

# Retrieve RSS feed static data
dbQuery <- dbSendQuery(conR, "SELECT * FROM rssSources")
rssSources <- dbFetch(dbQuery)
print("RSS Feeds static data retrieved")
rssSources.names <- unique(dplyr::select(rssSources,Feed))
rssSources.names <- as_tibble(sort(rssSources.names[,1]))
colnames(rssSources.names) <- "cname"
rssSources.names$cname <- as_utf8_character(rssSources.names$cname)
rss.SourceTypes <- unique(dplyr::select(rssSources,SourceType))
rss.SourceTypes <- sort(rss.SourceTypes[,1])
rss.Countries <- unique(dplyr::select(rssSources,Country))
rss.Countries <- sort(rss.Countries[,1])
rss.Regions <- unique(dplyr::select(rssSources,Region))
rss.Regions <- sort(rss.Regions[,1])
rss.Orientation <- unique(dplyr::select(rssSources,Orientation))
rss.Orientation <- sort(rss.Orientation[,1])
rss.Lookups <- unique(dplyr::select(rssSources,URL, Orientation))



