# Load Packages and Data============================================================================
#load packages
library(pacman) 
pacman::p_load(here, tidyverse, rsample, tidymodels)

#read in data
in_fp <- here("data", "tidy_data", "train_tidy.rds")
df_house_train <- readRDS(in_fp)



# Create Folds for Cross-Validation=================================================================
df_house_train %>%
  vfold_cv(v=10) -> df_vfold



# Modelling=========================================================================================
## Pull predictors
all_preds <- names(df_house_icrcn) %>%
  .[!. %in% "sale_price"]
  

## Decision tree
### Define model
dt_mod <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression") %>%
  translate()


### Construct workflow
dt_wf <- workflow() %>%
  add_model(dt_mod) %>%
  add_formula(as.formula(paste("sale_price ~", paste(all_preds, collapse=" + "))))


### Fit multiple models via resampling
set.seed(11)
dt_fit_rs <- dt_wf %>%
  fit_resamples(df_vfold) 
#errors: need to combine fcts andr ords into larger categories


