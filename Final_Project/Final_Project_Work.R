require(pacman)
p_load(tidyverse,
       caret,
       psych,
       mice,
       glmnet,
       gbm,
       pdp,
       partykit,
       party,
       mclust,
       mixtools,
       e1071)

load("C:/Users/clcar/Dropbox/College/Senior Year/Machine_Learning_for_Social_Science/Final_Project/NSDUH-2018/NSDUH_2018.RData")

nsduh <- PUF2018_100819

data_filtered <- nsduh %>%
  select(TXEVRRCVD2, # Received Treatment OUTCOME
         irsex, # Gender
         irmarit, # Marital Status
         IREDUHIGHST2, # Level of Education
         AGE2, # Age
         service, # Military Service
         HEALTH2, # Self-Assessment of Health
         sexatract, # Sexual Orientation
         NEWRACE2, # Ethnicity
         irwrkstat, # Employment Status
         IRHHSIZ2, # Number of People in Household
         IRKI17_2, # Number of children under 18
         irmcdchp, # Has Medicaid/CHIP
         irmedicr, # Has Medicare
         irprvhlt, # Has Private Health Insurance,
         IRINSUR4, # Has any kind of Health Insurance,
         POVERTY3, # Living in Poverty
         ircigrc, # Cigarette Recency
         iralcrc, # Alcohol Recency
         irmjrc, # Marijuana Recency
         ircocrc, # Cocaine Recency
         ircrkrc, # Crack Recency
         irherrc, # Heroin Recency
         irhallucrec, # Hallucinogen Recency
         irecstmorec, # Ecstacy Recency
         irketminrec, # Ketamine Recency
         irdamtfxrec, # DMT/AMT/FOXY Recency
         irsalviarec, # Salvia Recency
         irinhalrec, # Inhalant Recency
         irmethamrec, # Methamphetamine Recency
         irpnrnmrec, # Pain Reliever Misuse Recency
         irtrqnmrec, # Tranquilizer Misuse Recency
         irstmnmrec, # Stimulate Misuse Recency
         irsednmrec, # Sedative Misuse Recency
         iralcfy, # Alcohol Frequency pst. yr.
         irmjfy, # Marijuana Frequency pst. yr
         ircocfy, # Cocaine Frequency pst. yr.
         ircrkfy, # Crack Frequency pst. yr.
         irherfy, # Heroin Frequency pst. yr.
         irhallucyfq, # Hallucinogen Frequency pst. yr.
         irinhalyfq, # Inhalant Frequency pst. yr.
         irmethamyfq # Meth Frequency pst. yr.
         ) %>%
  mutate(TXEVRRCVD2 = factor(TXEVRRCVD2))

data_filtered$TXEVRRCVD2 <- as_factor(data_filtered$TXEVRRCVD2) %>%
  recode(`1` = "Treated", `0` = "NotTreated")

elastic_trainControl <- trainControl(method = "cv",
                                     number = 10,
                                     classProbs = T,
                                     summaryFunction = prSummary,
                                     sampling = "up")

elastic_trainControl2 <- trainControl(method = "cv",
                                     number = 10,
                                     classProbs = T,
                                     summaryFunction = prSummary)

elastic_noresamp <- train(TXEVRRCVD2 ~ .,
                 data = data_filtered,
                 method = "glmnet",
                 trControl = elastic_trainControl2,
                 tuneGrid = expand.grid(
                   .alpha = 0.5,
                   .lambda = seq(0, 1, by = 0.01)
                 ),
                 na.action = na.omit,
                 metric = "ROC")
 


elastic <- train(TXEVRRCVD2 ~ .,
                 data = data_filtered,
                 method = "glmnet",
                 trControl = elastic_trainControl,
                 tuneGrid = expand.grid(
                   .alpha = 0.5,
                   .lambda = seq(0, 1, by = 0.01)
                 ),
                 na.action = na.omit,
                 metric = "ROC")


knitr::kable(elastic$results)


plot(elastic$finalModel)
plot(elastic$finalModel, xvar = "dev")
plot(elastic$finalModel, xvar = "lambda")
summary(elastic$results)
plot(elastic)


gbmGrid <-  expand.grid(interaction.depth = c(1,2), 
                        n.trees = c(100,500, 1000), 
                        shrinkage = c(.1,.01, .001),
                        n.minobsinnode = 100)

boost <- train(TXEVRRCVD2 ~ .,
               data = data_filtered,
               method = "gbm",
               trControl = elastic_trainControl,
               tuneGrid = gbmGrid,
               na.action = na.omit,
               metric = "ROC")
summary(boost$finalModel)
summary(boost$results)
boost$finalModel %>%
  partial(pred.var = "IREDUHIGHST2",
          n.trees = boost$finalModel$n.trees,
          train = boost$trainingData,
          plot=T,
          rug=T)
gbm.perf(boost$finalModel)


grid.arrange(dothe3d("irmjrc", "IREDUHIGHST2", 40, -70),

dothe3d("irmjrc", "AGE2", 60, -70),

dothe3d("irmjrc", "NEWRACE2", 40, -70),

dothe3d("irmjrc", "POVERTY3", 40, -70),

dothe3d("irmjrc", "irherrc", 60, -70),

dothe3d("irherrc", "IREDUHIGHST2", 40, -70),

dothe3d("irherrc", "NEWRACE2", 60, -70),

dothe3d("irherrc", "POVERTY3", 40, -70),

dothe3d("irsex", "POVERTY3", 50, -80),

ncol = 3, nrow = 3)
