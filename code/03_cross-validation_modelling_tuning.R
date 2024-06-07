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
df_metrics_dt <- collect_metrics(fit_rs_dt)
df_metrics_dt


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
df_metrics_rf <- collect_metrics(fit_rs_rf)
df_metrics_rf


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
df_metrics_knn <- collect_metrics(fit_rs_knn)
df_metrics_knn


## Combine metrics tables for report--------------------
tab_model_cv_metrics <- list(
  "decision_tree"=df_metrics_dt, 
  "random_forest"=df_metrics_rf, 
  "k-nearest neighbor"=df_metrics_knn
  ) %>%
  bind_rows(.id="model") %>%
  select(!c(.estimator, .config)) %>%
  pivot_wider(names_from=".metric", values_from=c("mean", "n", "std_err"), 
              names_glue="{.metric}_{.value}", names_vary="slowest") %>%
  select(!rsq_n) %>%
  relocate(n="rmse_n", .after="model") %>%
  arrange(rmse_mean)

tab_model_cv_metrics



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
tab_cv_metrics_tune_rf <- collect_metrics(fit_rs_tune_rf) %>%
  select(-.estimator) %>%
  pivot_wider(names_from=.metric, values_from=c(mean, n, std_err),
              names_vary="slowest", names_glue="{.metric}_{.value}") %>% 
  select(trees, min_n, n="rmse_n", rmse_mean, rmse_std_err, rsq_mean, rsq_std_err, .config) %>%
  arrange(rmse_mean)
  
tab_cv_metrics_tune_rf

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
fig_metric_tune_rf <- fit_rs_tune_rf %>%
  collect_metrics() %>%
  mutate(across(.cols=c("trees", "min_n"), ~as.factor(.x))) %>%
  ggplot(aes(x=trees, y=mean, color = min_n)) +
  geom_line(aes(group=min_n), linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme_bw()
#recall that best parameters: trees = 2000, min_n = 2

fig_metric_tune_rf

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

tab_vi_rf_final_model <- vi(final_fit) 
tab_vi_rf_final_model

fig_vip_rf_final_model <- vip(final_fit, num_features=20) 
fig_vip_rf_final_model


## Show relationship between overall_qual and sale_price
fig_overall_qual_sale_price_box <- df_house_train %>%
  ggplot() +
  geom_boxplot(aes(x=overall_qual, y=sale_price)) +
  theme_bw()

fig_overall_qual_sale_price_box 


# Write Modelling Objects to Files===================================================================
## Create file paths
model_cv_metrics_fp <- here("modelling", "mod_cv_metrics.rds")
grid_rf_fp <- here("modelling", "grid_rf.rds")
cv_metrics_tune_rf_fp <- here("modelling", "metrics_tune_rf.rds")
metric_tune_rf_plot_fp <- here("modelling", "metric_tune_rf_plot.rds")
final_model_fp <- here("modelling", "final_model.rds")
vi_final_model_fp <- here("modelling", "vi_final_model.rds")
vip_final_model_fp <- here("modelling", "vip_final_model.rds")
overall_qual_sale_price_plot_fp <- here("modelling", "overall_qual_sale_price_plot.rds")


## Save files
saveRDS(tab_model_cv_metrics, model_cv_metrics_fp)
saveRDS(grid_rf, grid_rf_fp)
saveRDS(tab_cv_metrics_tune_rf, cv_metrics_tune_rf_fp)
saveRDS(fig_metric_tune_rf, metric_tune_rf_plot_fp)
saveRDS(final_fit, final_model_fp)
saveRDS(tab_vi_rf_final_model, vi_final_model_fp)
saveRDS(fig_vip_rf_final_model, vip_final_model_fp)
saveRDS(fig_overall_qual_sale_price_box, overall_qual_sale_price_plot_fp)







