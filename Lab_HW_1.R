require(pacman)
p_load(tidyverse,
       mice)

set.seed(123)

setwd("C:/Users/clcar/Dropbox/College/Senior Year/Machine_Learning_for_Social_Science/AHS 2017 National PUF v3.0 CSV/")
household <- read_csv("household.csv",
                      # N.B. For this assignment, I have decided to only classify responses where
                      # the respondent chose not to answer (or claimed not to know) as 
                      # missing. These are denoted by a '-9' in the responses. If the question did 
                      # not pertain to the individual i.e. was 'out of scope' for the interview,
                      # I am considering that data to be valid. These responses are denoted by a
                      # value of '-6'.
                      na = c("",
                             "NA",
                             "None",
                             "'-9'",
                             "-9")
                      ) %>%
  # This line removes variables which are included as replication weights. These variables aren't
  # real observations, so for our purposes, they should be discarded (at least for now)
  select(-contains("REPW"))

house_sub <- household[c("TENURE",
                         "OWNLOT",
                         "YRBUILT",
                         "LOTSIZE",
                         "ADEQUACY",
                         "NUMPEOPLE",
                         "NUMYNGKIDS",
                         "NUMOLDKIDS",
                         "HHAGE",
                         "HINCP",
                         "PERPOVLVL")]

md.pattern(house_sub)



house_sub$OWNLOT <- as.factor(house_sub$OWNLOT)

house_sub_comp <- mice(house_sub,
                       m = 1) %>%
  complete()

md.pattern(house_sub_comp)




house_sub %>%
  group_by(LOTSIZE) %>%
  summarise(Frequency = n())
