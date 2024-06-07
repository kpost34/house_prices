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


## Function to create bubble plots of pairs of unordered factors from list of x^2 results
plot_counts <- function(plot_num, legend=FALSE) {
  comp <- low_p_val_x2_feat[plot_num]
  
  low_p_val_vars <- comp %>%
    str_split_1(pattern="__")
  
  var1 <- low_p_val_vars[1]
  var2 <- low_p_val_vars[2]
  
  list_house_fct_chis[[comp]][["matrix"]] %>%
    as.data.frame() %>%
    rownames_to_column(var=var1) %>%
    pivot_longer(cols=-(!!ensym(var1)), names_to=var2, values_to="n") %>%
    uncount(n) %>%
    ggplot() +
    geom_count(aes(x=!!ensym(var1), 
                   y=!!ensym(var2))) +
    scale_size_area() + 
    theme(axis.text.x=element_text(angle=90)) +
    {if(legend) theme(legend.position="top") 
      else theme(legend.position="none")}
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



## Function to bin rare factor levels
bin_rare_levels <- function(data) {
  data %>% 
    mutate(
      #factors (unordered)
      ms_zoning=fct_collapse(ms_zoning, Other=c("RH", "RM", "C (all)")),
      lot_config=fct_collapse(lot_config, FR2_3=c("FR2", "FR3")),
      neighborhood=fct_collapse(neighborhood, Other1=c("StoneBr", "Veenker")),
      neighborhood=fct_collapse(neighborhood, Other2=c("ClearCr", "Blmngtn")),
      neighborhood=fct_collapse(neighborhood, Other3=c("SWISU", "MeadowV", "BrDale", "NPkVill", "Blueste")),
      condition1=fct_collapse(condition1, RRnear=c("RRAn", "RRAe", "RRNn", "RRNe")),
      condition1=fct_collapse(condition1, NormPos=c("Norm", "PosN", "PosA")),
      house_style=fct_collapse(house_style, Story_2_2.5=c("2Story", "2.5Fin")),
      house_style=fct_collapse(house_style, Other=c("SFoyer", "1.5Unf", "2.5Unf")),
      roof_style=fct_collapse(roof_style, Other=c("Flat", "Gambrel", "Mansard", "Shed")),
      exterior1st=fct_collapse(exterior1st, Other=c("WdShing", "Stucco", "AsbShng", "BrkComm", "Stone",
                                                    "AsphShn", "CBlock", "ImStucc")),
      mas_vnr_type=fct_collapse(mas_vnr_type, BrkFace_Cmn=c("BrkFace", "BrkCmn")),
      foundation=fct_collapse(foundation, Other=c("Slab", "Stone", "Wood")),
      heating=fct_collapse(heating, Other=c("GasW", "Grav", "Wall", "OthW", "Floor")),
      garage_type=fct_collapse(garage_type, Other=c("Basment", "CarPort", "2Types")),
      misc_feature=fct_collapse(misc_feature, MF=c("Shed", "Gar2", "Othr", "TenC")),
      sale_type=fct_collapse(sale_type, Other=c("COD", "ConLD", "ConLI", "ConLw", "CWD", "Oth", "Con")),
      sale_condition=fct_collapse(sale_condition, Other=c("Family", "Alloca", "AdjLand")),
      
      #ordered factors
      lot_shape=fct_collapse(lot_shape, IR2_3=c("IR2", "IR3")), 
      land_slope=fct_collapse(land_slope, Mod_Sev=c("Mod", "Sev")),
      overall_qual=fct_collapse(overall_qual, `4_and_less`=c("1", "2", "3", "4")),
      overall_qual=fct_collapse(overall_qual, `9_10`=c("9", "10")),
      exter_qual=fct_collapse(exter_qual, TA_and_less=c("Po", "Fa", "TA")),
      exter_cond=fct_collapse(exter_cond, Ex_Gd=c("Ex", "Gd")),
      exter_cond=fct_collapse(exter_cond, Fa_Po=c("Fa", "Po")),
      bsmt_cond=fct_collapse(bsmt_cond, Ex_Gd=c("Ex", "Gd")),
      bsmt_cond=fct_collapse(bsmt_cond, Fa_Po=c("Fa", "Po")),
      bsmt_fin_type2=fct_collapse(bsmt_fin_type2, ALQ_GLQ=c("ALQ", "GLQ")),
      heating_qc=fct_collapse(heating_qc, Fa_Po=c("Fa", "Po")),
      electrical=fct_collapse(electrical, FuseF_P_Mix=c("FuseF", "FuseP", "Mix")),
      functional=fct_collapse(functional, Poor=c("Mod", "Maj1", "Maj2", "Sev", "Sal")),
      fireplace_qu=fct_collapse(fireplace_qu, Ex_Gd=c("Ex", "Gd")),
      fireplace_qu=fct_collapse(fireplace_qu, Fa_Po=c("Fa", "Po")),
      garage_qual=fct_collapse(garage_qual, Ex_Gd_TA=c("Ex", "Gd", "TA")),
      garage_qual=fct_collapse(garage_qual, Fa_Po=c("Fa", "Po")),
      garage_cond=fct_collapse(garage_cond, Ex_Gd_TA=c("Ex", "Gd", "TA")),
      garage_cond=fct_collapse(garage_cond, Fa_Po=c("Fa", "Po")),
      fence=fct_collapse(fence, Ww=c("GdWo", "MnWw"))) -> data_out
    
    return(data_out)
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



## Function to normalize data
### Normalization equation
normalize <- function(var) {
  norm_var <- (var - min(var))/(max(var) - min(var))
  
  return(norm_var)
}


### Wrapper function
norm_num_preds <- function(data, vec_preds) {
  data %>%
    mutate(
      across(.cols=all_of(vec_preds),
             ~normalize(.x))
    ) -> data1
  
  return(data1)
}










