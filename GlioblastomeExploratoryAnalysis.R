library('ggplot2') # visualization
library('ggthemes') # visualization
library('ggridges') # visualization
library('ggforce') # visualization
library('ggExtra') # visualization
library('GGally') # visualisation
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('VIM') # missing values
#suppressPackageStartupMessages(library(heatmaply)) # visualisation
library('dplyr') # data manipulation
library('tidyr') # data manipulation
library('readr') # data input
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('modelr') # factor manipulation
library('randomForest') # classification
library('xgboost') # classification
library('ROCR') # model validation

  
summary(g_clin_tib)
glimpse(g_clin_tib)

#Missing values
library(VIM)

aggr(g_clin_tib, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

g_clin_tib <- g_clin_tib %>%
  select(-CANCER_TYPE, -CANCER_TYPE_DETAILED, -ONCOTREE_CODE) %>%   ##Non informative all are Gliobastome Multiforme and GBM
  filter(!is.na(OS_STATUS))
aggr(g_clin_tib, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

##sample
set.seed(1573)
g_clin_tib <- g_clin_tib[sample(nrow(g_clin_tib), 65),]

##Obtain covariates and observations and censored times
Xobs <- g_clin_tib %>%
  filter(OS_STATUS == "DECEASED") %>%
  select(-OS_STATUS, -OS_MONTHS, -DFS_MONTHS, -DFS_STATUS, -sample)  ##only keep explanatory vars

yobs <- g_clin_tib %>%
  filter(OS_STATUS == "DECEASED") %>%
  select(OS_MONTHS)

Xcen <- g_clin_tib %>%
  filter(OS_STATUS != "DECEASED") %>%
  select(-OS_STATUS, -OS_MONTHS, -DFS_MONTHS, -DFS_STATUS, -sample)  ##only keep explanatory vars

ycen <- g_clin_tib %>%
  filter(OS_STATUS != "DECEASED") %>%
  select(OS_MONTHS)

##Further subclassify in classical covariates and new biomarkers

Xobs_bg <- Xobs %>%
  select(AGE, SEX, THERAPY_CLASS)
Xobs_biom <- Xobs %>%
  select(-AGE, -SEX, -THERAPY_CLASS)

Xcen_bg <- Xcen %>%
  select(AGE, SEX, THERAPY_CLASS)
Xcen_biom <- Xcen %>%
  select(-AGE, -SEX, -THERAPY_CLASS)

#transform to model matrix

Xobs_bg <- model.matrix(~., Xobs_bg)
Xobs_biom <- model.matrix(~., Xobs_biom)
Xcen_bg <- model.matrix(~., Xcen_bg)
Xcen_biom <- model.matrix(~., Xcen_biom)
#

Nobs <- 49
Ncen <- 16
M_bg <- 
M_
