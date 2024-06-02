#03_cross-validation_modelling_tuning

#This script 1) creates folds for cross-validation, 2) models training using three different model
  #types (i.e., decision tree, random forest, KNN), 3) tunes hyperparameters for all three model
  #types, 4) selects model, 5) finalizes model,  6) runs diagnostics on final
  #model, and 7) saves final model


# Load Packages and Data============================================================================
#load packages
library(pacman) 
pacman::p_load(here, tidyverse, rsample, tidymodels, vip)

#read in data
in_fp <- here("data", "tidy_data", "train_tidy.rds")
df_house_train <- readRDS(in_fp)



# Create Folds for Cross-Validation=================================================================
df_house_train %>%
  vfold_cv(v=10) -> df_vfold



# Modelling=========================================================================================
## Pull predictors--------------------
all_preds <- names(df_house_train) %>%
  .[!. %in% c("id", "sale_price")]


## Decision tree--------------------
### Define model
mod_dt <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression") %>%
  translate()


### Construct workflow
wf_dt <- workflow() %>%
  add_model(mod_dt) %>%
  add_formula(as.formula(paste("sale_price ~", paste(all_preds, collapse=" + "))))


### Fit multiple models via resampling
set.seed(25)
fit_rs_dt <- wf_dt %>%
  fit_resamples(df_vfold) 


### Assess performance
collect_metrics(fit_rs_dt)


## Random Forest--------------------
### Define model
mod_rf <- rand_forest() %>%
  set_engine("ranger", importance="impurity") %>%
  set_mode("regression") %>%
  translate()


### Construct workflow
wf_rf <- workflow() %>%
  add_model(mod_rf) %>%
  add_formula(as.formula(paste("sale_price ~", paste(all_preds, collapse=" + "))))


### Fit multiple models via resampling
set.seed(25)
fit_rs_rf <- wf_rf %>%
  fit_resamples(df_vfold) 


### Assess performance
collect_metrics(fit_rs_rf)


## K-nearest Neighbors--------------------
### Define model
mod_knn <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("regression") %>%
  translate()


## Construct workflow
wf_knn <- workflow() %>%
  add_model(mod_knn) %>%
  add_formula(as.formula(paste("sale_price ~", paste(all_preds, collapse=" + "))))


### Fit multiple models via resampling
set.seed(25)
fit_rs_knn <- wf_knn %>%
  fit_resamples(df_vfold)


### Assess performance
collect_metrics(fit_rs_knn)



# Tune Hyperparameters==============================================================================
## Decision tree--------------------
### Define model
mod_tune_dt <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression") %>%
  translate()


### Create tuning grid
grid_dt <- grid_regular(tree_depth(),
                        min_n(),
                        levels=5)
grid_dt


### Construct workflow
wf_tune_dt <- workflow() %>%
  add_model(mod_tune_dt) %>%
  add_formula(as.formula(paste("sale_price ~", paste(all_preds, collapse=" + "))))


### Fit multiple models via resampling
set.seed(25)
fit_rs_tune_dt <- wf_tune_dt %>%
  tune_grid(
    resamples=df_vfold,
    grid=grid_dt
  )


### Assess performance
collect_metrics(fit_rs_tune_dt) 

fit_rs_tune_dt %>%
  show_best(metric="rsq")

fit_rs_tune_dt %>%
  show_best(metric="rmse")



## Random forest--------------------
### Define model
mod_tune_rf <- rand_forest(
  trees = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance="impurity") %>%
  set_mode("regression") %>%
  translate()


### Create tuning grid
grid_rf <- grid_regular(trees(),
                        min_n(),
                        levels=5)
grid_rf


### Construct workflow
wf_tune_rf <- workflow() %>%
  add_model(mod_tune_rf) %>%
  add_formula(as.formula(paste("sale_price ~", paste(all_preds, collapse=" + "))))


### Fit multiple models via resampling
set.seed(25)
fit_rs_tune_rf <- wf_tune_rf %>%
  tune_grid(
    resamples=df_vfold,
    grid=grid_rf
  )


### Assess performance
collect_metrics(fit_rs_tune_rf)

fit_rs_tune_rf %>%
  show_best(metric="rsq")

fit_rs_tune_rf %>%
  show_best(metric="rmse")



## K-nearest Neighbors--------------------
### Define model
mod_tune_knn <- nearest_neighbor(
  neighbors = tune(),
  weight_func = tune()
) %>%
  set_engine("kknn") %>%
  set_mode("regression") %>%
  translate()


### Create tuning grid
grid_knn <- grid_regular(neighbors(),
                         weight_func(),
                         levels=5)
grid_knn


### Construct workflow
wf_tune_knn <- workflow() %>%
  add_model(mod_tune_knn) %>%
  add_formula(as.formula(paste("sale_price ~", paste(all_preds, collapse=" + "))))


### Fit multiple models via resampling
set.seed(25)
fit_rs_tune_knn <- wf_tune_knn %>%
  tune_grid(
    resamples=df_vfold,
    grid=grid_knn
  )


### Assess performance
collect_metrics(fit_rs_tune_knn)

fit_rs_tune_knn %>%
  show_best(metric="rsq")

fit_rs_tune_knn %>%
  show_best(metric="rmse")



# Plot Results of Tuned Models======================================================================
#random forest models as they generally performed better
fit_rs_tune_rf %>%
  collect_metrics() %>%
  mutate(across(.cols=c("trees", "min_n"), ~as.factor(.x))) %>%
  ggplot(aes(x=trees, y=mean, color = min_n)) +
  geom_line(aes(group=min_n), linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme_bw()
#recall that best parameters: trees = 2000, min_n = 2

fit_rs_tune_rf %>%
  show_best(metric="rsq")

fit_rs_tune_rf %>%
  show_best(metric="rmse")



# Finalize Model====================================================================================
## Select best model
rf_best <- fit_rs_tune_rf %>%
  select_best(metric = "rmse")


## Finalize model: update workflow object with values form select_best()
wf_rf_final <- wf_tune_rf %>%
  finalize_workflow(rf_best)

wf_rf_final


## Fit model to training data and use test data to evaluate model 
final_fit <- fit(wf_rf_final, df_house_train) %>%
  extract_fit_parsnip() 


## Model diagnostics
final_fit

vi(final_fit) 
vip(final_fit, num_features=20)



# Write Model to File===============================================================================
model_fp <- here("models", "final_model.rds")

# saveRDS(final_fit, model_fp)










