#01_data-clean-check-impute.R

#This script 1) performs initial data cleaning, 2) accomplishes initial data checking, 3) assesses
  #missingness and imputes values, and 4) writes new dataframe to file


# Load Packages, Objects, Functions, and Data=======================================================
#packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, naniar, finalfit, mice)

#objects
source(here("code", "00_objects.R"))

#functions
source(here("code", "00_helper_fns.R"))

#data
df_house0 <- read_csv(here("data", "raw_data", "train.csv"),
                      col_types=col_types_train)



# Initial Data Cleaning and Wrangling===============================================================
#UPDATE: just use this after loading objs and fns
df_house <- initial_clean_house(df_house0)


# #clean col names, replace NA factor levels when they are not missing data, & create ordered fcts
# 
# #col obj for explicit NA levels
# bsmt_na_cols <- c("bsmt_cond", "bsmt_exposure", "bsmt_qual", "bsmt_fin_type1", "bsmt_fin_type2")
# bsmt_area_bath_cols <- c("bsmt_fin_sf1", "bsmt_fin_sf2", "bsmt_unf_sf", "total_bsmt_sf", 
#                          "bsmt_full_bath", "bsmt_half_bath")
# garage_na_cols <- c("garage_type","garage_finish", "garage_qual", "garage_cond")
# 
# #level vectors - check that levels here cover what's in train + test and nothing additional
# lot_shape_levs <- c("Reg", "IR1", "IR2", "IR3")
# util_levs <- c("AllPub", "NoSewr", "NoSeWa", "ELO")
# land_slope_levs <- c("Gtl", "Mod", "Sev")
# cond_levs <- c("Ex", "Gd", "TA", "Fa", "Po")
# bsmt_cond_levs <- c(cond_levs, "NB")
# bsmt_exp_levs <- c("Gd", "Av", "Mn", "No", "NB")
# bsmt_fin_levs <- c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "NB")
# elec_levs <- c("SBrkr", "FuseA", "FuseF", "FuseP", "Mix")
# func_levs <- c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal")
# fire_cond_levs <- c(cond_levs, "NFp")
# garage_fin_levs <- c("Fin", "RFn", "Unf", "NG")
# garage_cond_levs <- c(cond_levs, "NG")
# paved_drive_levs <- c("Y", "P", "N")
# pool_cond_levs <- c("Ex", "Gd", "TA", "Fa", "NP")
# fence_levs <- c("GdPrv", "MnPrv", "GdWo", "MnWw", "NFe")
# #NOTE: if garage_type == "Detchd" then NA values for other garage-related variables will be considered
#   #missing (applies to test data); but if garage_type is NA, then it should be converted to "NG" and
#   #so should all other NA values for garage-related variables
# 
# #initial wrangling
# df_house <- df_house0 %>%
#   #clean up col names for each of use
#   clean_names() %>% 
#   #complicated NAs
#   #garage_yr_blt gets -1000 if not applicable and NA if missing
#   mutate(garage_yr_blt=ifelse(is.na(garage_type), -1000, garage_yr_blt),
#          #garage_na_cols get "NG" level of factor if type is NA
#          across(.cols=all_of(garage_na_cols), 
#                 ~ifelse(is.na(garage_type), "NG", .x)),
#          across(.cols=all_of(garage_na_cols), ~as_factor(.x)),
#          #three bsmt cols need to be NA for their values to be converted to "NB"
#          across(.cols=all_of(bsmt_na_cols), 
#                 ~ifelse(is.na(bsmt_qual) & is.na(bsmt_cond) & is.na(bsmt_exposure), "NB", .x)),
#          across(.cols=all_of(bsmt_na_cols), ~as_factor(.x)),
#          #if bsmt_qual is "NB", then bsmt area cols are set to 0
#          across(.cols=all_of(bsmt_area_bath_cols), 
#                 ~if_else(bsmt_qual=="NB", 0, .x, .x))) %>%
#   #simple conversion of NAs in factors to explicit levels
#   mutate(alley=fct_na_value_to_level(alley, "NAl"),
#          fireplace_qu=fct_na_value_to_level(fireplace_qu, "NFp"),
#          pool_qc=fct_na_value_to_level(pool_qc, "NP"),
#          fence=fct_na_value_to_level(fence, "NFe"),
#          misc_feature=fct_na_value_to_level(misc_feature, "NMF")) %>%
#   #convert unordered to ordered factors
#   mutate(lot_shape=factor(lot_shape, levels=rev(lot_shape_levs), ordered=TRUE),
#          utilities=factor(utilities, levels=rev(util_levs), ordered=TRUE),
#          land_slope=factor(land_slope, levels=rev(land_slope_levs), ordered=TRUE),
#          overall_qual=factor(overall_qual, levels=1:10, ordered=TRUE),
#          overall_cond=factor(overall_qual, levels=1:10, ordered=TRUE),
#          exter_qual=factor(exter_qual, levels=rev(cond_levs), ordered=TRUE),
#          exter_cond=factor(exter_cond, levels=rev(cond_levs), ordered=TRUE),
#          bsmt_qual=factor(bsmt_qual, levels=rev(bsmt_cond_levs), ordered=TRUE),
#          bsmt_cond=factor(bsmt_cond, levels=rev(bsmt_cond_levs), ordered=TRUE),
#          bsmt_exposure=factor(bsmt_exposure, levels=rev(bsmt_exp_levs), ordered=TRUE),
#          bsmt_fin_type1=factor(bsmt_fin_type1, levels=rev(bsmt_fin_levs), ordered=TRUE),
#          bsmt_fin_type2=factor(bsmt_fin_type2, levels=rev(bsmt_fin_levs), ordered=TRUE),
#          heating_qc=factor(heating_qc, levels=rev(cond_levs), ordered=TRUE),
#          electrical=factor(electrical, levels=rev(elec_levs), ordered=TRUE),
#          kitchen_qual=factor(kitchen_qual, levels=rev(cond_levs), ordered=TRUE),
#          functional=factor(functional, levels=rev(func_levs), ordered=TRUE),
#          fireplace_qu=factor(fireplace_qu, levels=rev(fire_cond_levs), ordered=TRUE),
#          garage_finish=factor(garage_finish, levels=rev(garage_fin_levs), ordered=TRUE),
#          garage_qual=factor(garage_qual, levels=rev(garage_cond_levs), ordered=TRUE),
#          garage_cond=factor(garage_cond, levels=rev(garage_cond_levs), ordered=TRUE),
#          paved_drive=factor(paved_drive, levels=rev(paved_drive_levs), ordered=TRUE),
#          pool_qc=factor(pool_qc, levels=rev(pool_cond_levs), ordered=TRUE),
#          fence=factor(fence, levels=rev(fence_levs), ordered=TRUE)) %>%
#   #convert nearly all remaining chr vars to fcts
#   mutate(across(.cols=!c(where(is.numeric), where(is.factor), id), ~as.factor(.x))) 
# 
# #NOTE: garage_yr_blt is incorrect for max year for test data --> replace with NA


#ASIDE--------------------------------------------------------------------
#check that factor levels in training data are in test data and vice versa
#ran code with each DF loaded
# train_lev <- df_house %>%
#   select(where(is.factor)) %>%
#   purrr::map(levels)
# 
# test_lev <- df_house %>%
#   select(where(is.factor)) %>%
#   purrr::map(levels)
# 
# setdiff(train_lev, test_lev) #FALSE
# setdiff(train_lev, test_lev) %>%
#   names() -> diff_nm #names of factors
# 
# #final set of factors in training data
# fp_train <- here("data", "tidy_data", "train_tidy.rds")
# df_house_icrcn <- readRDS(fp_train) 
# df_house_icrcn %>%
#   select(where(is.factor)) %>%
#   names() -> nm_fct_train
# 
# #which factors have different levels b/t train and test data AND are in final training set
# nm_isect <- intersect(nm_fct_train, diff_nm) 
# nm_isect #house_style, exterior1st, heating, garage_type, misc_feature
# 
# #which of these five factors have levels unique to test data
# nm_isect %>%
#   purrr::map(function(x) {
#     setdiff(test_lev[[x]], train_lev[[x]])
#   }) %>%
#   set_names(nm_isect) #none for all
#----------------------------------------------------------------------------

## Show variable classes
tab_var_class <- df_house %>%
  purrr::map(function(x) {
    class(x)[[1]]
  }) %>%
  enframe(name="variable", value="class") %>%
  unnest(class) %>%
  arrange(class, variable)

tab_var_class 


# Preliminary Data Checking=========================================================================
#basics
dim(df_house) #1460 x 81 (id + 79 explanatory + 1 dv)
glimpse(df_house) #the col classes match the data; however, some cols need to be re-classified or perhaps mutated 
head(df_house, n=10); tail(df_house, n=10) 
summary(df_house) 


#skim by type: chr, factor, numeric
tab_train_chr <- skim_by_type(data=df_house, fn=is.character)
tab_train_fct <- skim_by_type(data=df_house, fn=is.factor)
tab_train_num <- skim_by_type(data=df_house, fn=is.numeric)

tab_train_chr
tab_train_fct 
tab_train_num 

# skim(df_house, where(is.character)) %>%
#   as_tibble() %>% 
#   select(-skim_type) %>% 
#   mutate(across(where(is.numeric), ~signif(.x,3)))
# 
# skim(df_house, where(is.factor)) %>% 
#   as_tibble() %>% 
#   select(-skim_type) %>% 
#   mutate(across(where(is.numeric), ~signif(.x,3))) %>%
#   print(n=46)
# 
# skim(df_house, where(is.numeric)) %>% 
#   as_tibble() %>% 
#   select(-skim_type) %>% 
#   mutate(across(where(is.numeric), ~signif(.x,3))) %>%
#   print(n=34)



# Missingness=======================================================================================
#NOTE: for test data, the following variables have true missing data: [enter here]

## Assess missing data--------------------
### Visualize missing data
#all data
vis_dat(df_house) 

fig_train_overall_miss <- vis_miss(df_house, sort_miss=TRUE, cluster=TRUE) 
fig_train_overall_miss
#0.2 % missing

#cols with at least one NA
fig_train_sub_miss <- df_house %>%
  select(where(~any(is.na(.)))) %>%
  vis_miss(sort_miss=TRUE,
           cluster=TRUE) 

fig_train_sub_miss
#for cols where at least one value is NA, total of 3.2% of values are missing

#missing value dot plot
df_house %>%
  gg_miss_var() 
#lot_frontage by far has most NAs followed by the two mas_ cols, then three others

#missing values by row
fig_train_miss_row <- df_house %>%
  gg_miss_case() 

fig_train_miss_row 
#by far the cases with missing values have only one value missing

#missing value patterns
fig_train_miss_patt <- df_house %>%
  gg_miss_upset(nsets=6)

fig_train_miss_patt 
#257/259 lot_frontage NA values occur without other missing values
#2/259 co-occur with mas_vnr_type and mas_vnr_area
#6/8 mas_vnr_type & mas_vnr_area occur without other vars
#bsmt_exposure, bsmt_fin_type2, and electrical NAs occur by themselves

df_house %>%
  missing_plot() 


#### Examine missingness
tab_train_miss <- map_int(df_house, function(x) sum(is.na(x))) %>%
  enframe(name="predictor", value="n_na") %>%
  filter(n_na > 0) %>%
  mutate(pct_na={(n_na/1460) * 100} %>%
           signif(3)) %>%
  arrange(desc(n_na)) 

tab_train_miss 
#6 cols with at least one NA
#3 cols with one NA
#1 col > .6% missing (lot_frontage)


### Determine pattern of missingness
#only three variables with missing data that have > 1 NA: lot_frontage, mas_vnr_type, and 
  #mas_vnr_area
#plot sale_price (DV) against each of three three IVs with multiple missing values

#### lot_frontage
#compare distribution of y for cases when lot_frontage is present vs missing
fig_sp_dist_lf_miss <- df_house %>%
  mutate(na_lot_frontage=is.na(lot_frontage)) %>%
  ggplot() +
  geom_histogram(aes(x=sale_price, fill=na_lot_frontage), color="black") +
  scale_fill_viridis_d() +
  facet_wrap(~na_lot_frontage, nrow=2) +
  theme_bw() +
  theme(legend.position="none")

fig_sp_dist_lf_miss
#similar distributions...but 'FALSE' plot is more skewed and different peak than 'TRUE' plot

#compare to a more closely related x variable: lot_shape
fig_ls_dist_lt_miss <- df_house %>%
  mutate(na_lot_frontage=is.na(lot_frontage)) %>%
  ggplot() +
  geom_bar(aes(x=lot_shape, fill=na_lot_frontage), color="black") +
  scale_fill_viridis_d() +
  facet_wrap(~na_lot_frontage, nrow=2) +
  theme_bw() +
  theme(legend.position="none")

fig_ls_dist_lt_miss
#clearly distribution differs based on presence/absence of lot_frontage data
#evidence that it's not MCAR

#### mas_vnr_type (and ._area as they have the same missingness)
#8/1460 is very small but this suggests that it's not MCAR
fig_foundation_dist_mvt <- df_house %>%
  mutate(na_mas_vnr_type=is.na(mas_vnr_type)) %>%
  ggplot() +
  geom_bar(aes(x=foundation, fill=na_mas_vnr_type), color="black") +
  scale_fill_viridis_d() +
  facet_wrap(~na_mas_vnr_type, nrow=2) +
  theme_bw() +
  theme(legend.position="none")

fig_foundation_dist_mvt
#all poured concrete
  
#clearly, these variables are not mcar and thus we'll use a multivariate imputation technique


## Data imputation using mice method "cart"--------------------
### Imputation using CART (pmm failed b/c of singularity--unbalanced factors)
df_house %>%
  mice(method="cart", m=2, maxit=2) %>%
  complete() %>%
  as_tibble() -> df_house_i


### Compare pre- and post-imputation
#data: presence vs absence
fig_train_pre_post_impute <- vis_compare(df_house, df_house_i) 
fig_train_pre_post_impute


#lot_frontage
#visually
fig_train_lf_pre_post_impute <- df_house_i %>%
  select(id, lot_frontage_imp="lot_frontage", sale_price) %>%
  left_join(df_house %>%
              select(id, lot_frontage), 
            by="id") %>%
  pivot_longer(cols=starts_with("lot_frontage"), 
               names_to="lf_type", 
               values_to="lot_frontage_value") %>%
  mutate(lf_val_log=log(lot_frontage_value)) %>%
  ggplot(aes(x=lf_val_log, y=sale_price, color=lf_type)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", color="blue") +
  scale_color_viridis_d(end=0.5) +
  facet_wrap(~lf_type, nrow=2) +
  labs(x="Lot frontage (log) (ft)",
       y="Sale price ($)") +
  theme_bw() +
  theme(legend.position="none")

fig_train_lf_pre_post_impute
  
#statistically
lf_nnas <- !is.na(df_house$lot_frontage)

cor(log(df_house$lot_frontage[lf_nnas]), df_house$sale_price[lf_nnas])^2 #r2 = .122

cor(log(df_house_i$lot_frontage), df_house_i$sale_price)^2 #r2 = .105

#perhaps revisit if prediction is poor



# Write Imputed Data to File========================================================================
#output fp
out_train_fp <- here("data", "tidy_data", "train_imputed.rds")

# saveRDS(df_house_i, out_train_fp)


