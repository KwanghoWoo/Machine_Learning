#library
library(tidyverse)
library(tidymodels)
#import
heart<-read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", col_names = F)
# Renaming var 
colnames(heart)<- c("age", "sex", "rest_cp", "rest_bp",
                    "chol", "fast_bloodsugar","rest_ecg","ex_maxHR","ex_cp",
                    "ex_STdepression_dur", "ex_STpeak","coloured_vessels", "thalassemia","heart_disease")
#elaborating cat var
##simple ifelse conversion 
heart<-heart %>% mutate(sex= ifelse(sex=="1", "male", "female"),fast_bloodsugar= ifelse(fast_bloodsugar=="1", ">120", "<120"), ex_cp=ifelse(ex_cp=="1", "yes", "no"),
                        heart_disease=ifelse(heart_disease=="0", "no", "yes")) 
## complex ifelse conversion using `case_when`
heart<-heart %>% mutate(
  rest_cp=case_when(rest_cp== "1" ~ "typical",rest_cp=="2" ~ "atypical", rest_cp== "3" ~ "non-CP pain",rest_cp== "4" ~ "asymptomatic"), rest_ecg=case_when(rest_ecg=="0" ~ "normal",rest_ecg=="1" ~ "ST-T abnorm",rest_ecg=="2" ~ "LV hyperthrophy"), ex_STpeak=case_when(ex_STpeak=="1" ~ "up/norm", ex_STpeak== "2" ~ "flat",ex_STpeak== "3" ~ "down"), thalassemia=case_when(thalassemia=="3.0" ~ "norm", 
                                                                                                                                                                                                                                                                                                                                                                                thalassemia== "6.0" ~ "fixed", thalassemia== "7.0" ~ "reversable")) 
# convert missing value "?" into NA
heart<-heart%>% mutate_if(is.character, funs(replace(., .=="?", NA)))
# convert char into factors
heart<-heart %>% mutate_if(is.character, as.factor)
#train/test set 
set.seed(4595)
data_split <- initial_split(heart, prop=0.75, strata = "heart_disease")
heart_train <- training(data_split)
heart_test <- testing(data_split)
# create recipe object
heart_recipe<-recipe(heart_disease ~., data= heart_train) %>%
  step_knnimpute(all_predictors())
# process the traing set/ prepare recipe(non-cv)
heart_prep <-heart_recipe %>% prep(training = heart_train, retain = TRUE)
# model building
dt_model<-decision_tree( min_n = 20, tree_depth = 30, mode = "classification") %>% set_engine("rpart") %>% fit(heart_disease ~ ., data = juice(heart_prep))


rpart.plot::rpart.plot(dt_model$fit,
                       #explicitly label the criteria for each node instead of default type 2 which labels yes/no 
                       type=4, 
                       # display the number and percentage of obs in the node
                       extra = 101, 
                       branch.lty=3,
                       #display the node numbers
                       nn=TRUE,
                       # If roundint=T, will display warning "Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables)"
                       roundint=F)


dt_model$fit$variable.importance

round(100 * dt_model$fit$variable.importance / sum(dt_model$fit$variable.importance),0)

summary(dt_model$fit) 