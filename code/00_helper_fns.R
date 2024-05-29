# Helper functions for Ames, IA, Housing Prices Project

# Data Checking & Wrangling=========================================================================
## Function to summarize data by type
skim_by_type <- function(data, fn) {
  skim(data, where(fn)) %>%
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3)))
}

## Function to clean col names, handle NA values, and to create ordered factors
initial_clean_house <- function(data) {
  data %>%
    #clean up col names for each of use
    clean_names() %>% 
    #complicated NAs
    #garage_yr_blt gets -1000 if not applicable and NA if missing
    mutate(garage_yr_blt=ifelse(is.na(garage_type), -1000, garage_yr_blt),
           #garage_na_cols get "NG" level of factor if type is NA
           across(.cols=all_of(garage_na_cols), 
                  ~ifelse(is.na(garage_type), "NG", .x)),
           across(.cols=all_of(garage_na_cols), ~as_factor(.x)),
           #three bsmt cols need to be NA for their values to be converted to "NB"
           across(.cols=all_of(bsmt_na_cols), 
                  ~ifelse(is.na(bsmt_qual) & is.na(bsmt_cond) & is.na(bsmt_exposure), "NB", .x)),
           across(.cols=all_of(bsmt_na_cols), ~as_factor(.x)),
           #if bsmt_qual is "NB", then bsmt area cols are set to 0
           across(.cols=all_of(bsmt_area_bath_cols), 
                  ~if_else(bsmt_qual=="NB", 0, .x, .x))) %>%
    #simple conversion of NAs in factors to explicit levels
    mutate(alley=fct_na_value_to_level(alley, "NAl"),
           fireplace_qu=fct_na_value_to_level(fireplace_qu, "NFp"),
           pool_qc=fct_na_value_to_level(pool_qc, "NP"),
           fence=fct_na_value_to_level(fence, "NFe"),
           misc_feature=fct_na_value_to_level(misc_feature, "NMF")) %>%
    #convert unordered to ordered factors
    mutate(lot_shape=factor(lot_shape, levels=rev(lot_shape_levs), ordered=TRUE),
           utilities=factor(utilities, levels=rev(util_levs), ordered=TRUE),
           land_slope=factor(land_slope, levels=rev(land_slope_levs), ordered=TRUE),
           overall_qual=factor(overall_qual, levels=1:10, ordered=TRUE),
           overall_cond=factor(overall_qual, levels=1:10, ordered=TRUE),
           exter_qual=factor(exter_qual, levels=rev(cond_levs), ordered=TRUE),
           exter_cond=factor(exter_cond, levels=rev(cond_levs), ordered=TRUE),
           bsmt_qual=factor(bsmt_qual, levels=rev(bsmt_cond_levs), ordered=TRUE),
           bsmt_cond=factor(bsmt_cond, levels=rev(bsmt_cond_levs), ordered=TRUE),
           bsmt_exposure=factor(bsmt_exposure, levels=rev(bsmt_exp_levs), ordered=TRUE),
           bsmt_fin_type1=factor(bsmt_fin_type1, levels=rev(bsmt_fin_levs), ordered=TRUE),
           bsmt_fin_type2=factor(bsmt_fin_type2, levels=rev(bsmt_fin_levs), ordered=TRUE),
           heating_qc=factor(heating_qc, levels=rev(cond_levs), ordered=TRUE),
           electrical=factor(electrical, levels=rev(elec_levs), ordered=TRUE),
           kitchen_qual=factor(kitchen_qual, levels=rev(cond_levs), ordered=TRUE),
           functional=factor(functional, levels=rev(func_levs), ordered=TRUE),
           fireplace_qu=factor(fireplace_qu, levels=rev(fire_cond_levs), ordered=TRUE),
           garage_finish=factor(garage_finish, levels=rev(garage_fin_levs), ordered=TRUE),
           garage_qual=factor(garage_qual, levels=rev(garage_cond_levs), ordered=TRUE),
           garage_cond=factor(garage_cond, levels=rev(garage_cond_levs), ordered=TRUE),
           paved_drive=factor(paved_drive, levels=rev(paved_drive_levs), ordered=TRUE),
           pool_qc=factor(pool_qc, levels=rev(pool_cond_levs), ordered=TRUE),
           fence=factor(fence, levels=rev(fence_levs), ordered=TRUE)) %>%
    #convert nearly all remaining chr vars to fcts
    mutate(across(.cols=!c(where(is.numeric), where(is.factor), id), ~as.factor(.x))) -> data1
  
  return(data1)
}

# Feature Engineering===============================================================================
## Function to replace diagonal elements of a matrix with NAs
replace_diag_na <- function(mat) {
  diag(mat) <- NA
  
  return(mat)
}



## Function to show sale_price-factor bar plots and frequency plots and tables of factor
explore_rare_cats <- function(data=df_house_icr, predictor) {
  nm_pred <- rlang::as_name(enquo(predictor))
  
  p1 <- data %>%
    select({{predictor}}, sale_price) %>%
    mutate({{predictor}} := fct_reorder({{predictor}}, sale_price, .fun=mean, .desc=TRUE)) %>%
    ggplot(aes(x={{predictor}}, y=sale_price)) +
    stat_summary(geom="col", fun=mean, fill="blue") +
    stat_summary(geom="errorbar", fun.data=mean_se) +
    ggtitle(paste("sale_price versus", 
                  nm_pred,
                  "in sale price order")) +
    theme_bw()
  
  p2 <- data %>%
    select({{predictor}}, sale_price) %>%
    mutate({{predictor}} := fct_infreq({{predictor}})) %>%
    ggplot(aes(x={{predictor}}, y=sale_price)) +
    stat_summary(geom="col", fun=mean, fill="darkred") +
    stat_summary(geom="errorbar", fun.data=mean_se) + 
    ggtitle(paste("sale_price versus", 
                  nm_pred,
                  "in order of descending frequency of",
                  nm_pred)) +
    theme_bw()
  
  p3 <- data %>%
    mutate({{predictor}} := fct_infreq({{predictor}})) %>%
    ggplot(aes(x={{predictor}})) +
    geom_bar(fill="green4") + 
    ggtitle(paste("Frequency of", 
                  nm_pred)) +
    theme_bw()
  
  plot <- plot_grid(p1, p2, p3, nrow=3)
  
  tab <- data %>% 
    select(where(is.factor)) %>% 
    map(tabyl) %>%
    pluck(nm_pred) %>%
    arrange(desc(percent))
  
  result <- list("plots"=plot, 
                 "freq_table"=tab)
  
  return(result)
}



## Function to create batches of qqplots
make_qqplots <- function(preds) {
  df_house_icrc %>%
    select(all_of(preds)) %>%
    pivot_longer(cols=everything(), names_to="variable", values_to="value") %>%
    ggplot(aes(sample=value, color=variable)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~variable, scales="free") +
    theme_bw() +
    theme(legend.position="none")
}




