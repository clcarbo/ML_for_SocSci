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