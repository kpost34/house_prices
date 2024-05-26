# Load Packages=====================================================================================
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, naniar, finalfit, mice)
               
               
               # rstatix, GGally, 
               # cowplot, rstatix, rsample, tidymodels)



# Data Import=======================================================================================
#read in fcts as chr and convert later
col_type_vals <- "cccnncccccccccccccciicccccncccccccncnnnccccnnnniiiiiiciciccicincccnnnnnncccniiccn"


# df_house0 <- read_csv(here("data", "raw_data", "test.csv"),
df_house0 <- read_csv(here("data", "raw_data", "train.csv"),
                           col_types=col_type_vals)



# Initial Data Cleaning and Wrangling===============================================================
#clean col names, replace NA factor levels when they are not missing data, & create ordered fcts

#col obj for explicit NA levels
bsmt_na_cols <- c("bsmt_cond", "bsmt_exposure", "bsmt_qual", "bsmt_fin_type1", "bsmt_fin_type2")
bsmt_area_bath_cols <- c("bsmt_fin_sf1", "bsmt_fin_sf2", "bsmt_unf_sf", "total_bsmt_sf", 
                         "bsmt_full_bath", "bsmt_half_bath")
garage_na_cols <- c("garage_type","garage_finish", "garage_qual", "garage_cond")

#level vectors - check that levels here cover what's in train + test and nothing additional
lot_shape_levs <- c("Reg", "IR1", "IR2", "IR3")
util_levs <- c("AllPub", "NoSewr", "NoSeWa", "ELO")
land_slope_levs <- c("Gtl", "Mod", "Sev")
cond_levs <- c("Ex", "Gd", "TA", "Fa", "Po")
bsmt_cond_levs <- c(cond_levs, "NB")
bsmt_exp_levs <- c("Gd", "Av", "Mn", "No", "NB")
bsmt_fin_levs <- c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "NB")
elec_levs <- c("SBrkr", "FuseA", "FuseF", "FuseP", "Mix")
func_levs <- c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal")
fire_cond_levs <- c(cond_levs, "NFp")
garage_fin_levs <- c("Fin", "RFn", "Unf", "NG")
garage_cond_levs <- c(cond_levs, "NG")
paved_drive_levs <- c("Y", "P", "N")
pool_cond_levs <- c("Ex", "Gd", "TA", "Fa", "NP")
fence_levs <- c("GdPrv", "MnPrv", "GdWo", "MnWw", "NFe")
#NOTE: if garage_type == "Detchd" then NA values for other garage-related variables will be considered
  #missing (applies to test data); but if garage_type is NA, then it should be converted to "NG" and
  #so should all other NA values for garage-related variables

#initial wrangling
df_house <- df_house0 %>%
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
  mutate(across(.cols=!c(where(is.numeric), where(is.factor), id), ~as.factor(.x))) 

#NOTE: garage_yr_blt is incorrect for max year for test data --> replace with NA

#check that factor levels in training data are in test data and vice versa
#ran code with each DF loaded
train_lev <- df_house %>%
  select(where(is.factor)) %>%
  purrr::map(levels)

# test_lev <- df_house %>%
#   select(where(is.factor)) %>%
#   purrr::map(levels)

# setequal(train_lev, test_lev) #TRUE



# Preliminary Data Checking=========================================================================
#basics
dim(df_house) #1460 x 81 (id + 79 explanatory + 1 dv)
glimpse(df_house) #the col classes match the data; however, some cols need to be re-classified or perhaps mutated 
head(df_house, n=10); tail(df_house, n=10) 
summary(df_house) 


#skim by type: chr, factor, numeric
skim(df_house, where(is.character)) %>%
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3)))

skim(df_house, where(is.factor)) %>% 
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3))) %>%
  print(n=46)

skim(df_house, where(is.numeric)) %>% 
  as_tibble() %>% 
  select(-skim_type) %>% 
  mutate(across(where(is.numeric), ~signif(.x,3))) %>%
  print(n=34)



# Missingness=======================================================================================
#NOTE: for test data, the following variables have true missing data: [enter here]

## Assess missing data--------------------
### Visualize missing data
#all data
vis_dat(df_house) 
vis_miss(df_house, 
         sort_miss=TRUE, 
         cluster=TRUE) 
#0.2 % missing

#cols with at least one NA
df_house %>%
  select(where(~any(is.na(.)))) %>%
  vis_miss(sort_miss=TRUE,
           cluster=TRUE)  
#for cols where at least one value is NA, total of 3.2% of values are missing

df_house %>%
  gg_miss_var() 
#lot_frontage by far has most NAs followed by the two mas_ cols, then three others

df_house %>%
  gg_miss_case() 
#by far the cases with missing values have only one value missing

df_house %>%
  gg_miss_upset(nsets=6)
#257/259 lot_frontage NA values occur without other missing values
#2/259 co-occur with mas_vnr_type and mas_vnr_area
#6/8 mas_vnr_type & mas_vnr_area occur without other vars
#bsmt_exposure, bsmt_fin_type2, and electrical NAs occur by themselves

df_house %>%
  missing_plot() 

#### Examine missingness
map_int(df_house, function(x) sum(is.na(x))) %>%
  enframe(name="predictor", value="n_na") %>%
  filter(n_na > 0) %>%
  mutate(pct_na=(n_na/1460) * 100) %>%
  arrange(desc(n_na)) 
#6 cols with at least one NA
#1 col > .6% missing (lot_frontage)


### Determine pattern of missingness
#only three variables with missing data that have > 1 NA: lot_frontage, mas_vnr_type, and 
  #mas_vnr_area
#plot sale_price (DV) against each of three three IVs with multiple missing values

#### lot_frontage
#compare distribution of y for cases when lot_frontage is present vs missing
df_house %>%
  mutate(na_lot_frontage=is.na(lot_frontage)) %>%
  ggplot() +
  geom_histogram(aes(x=sale_price)) +
  facet_wrap(~na_lot_frontage)
#similar distributions

#compare to a more closely related x variable: lot_shape
df_house %>%
  mutate(na_lot_frontage=is.na(lot_frontage)) %>%
  ggplot() +
  geom_bar(aes(x=lot_shape)) +
  facet_wrap(~na_lot_frontage)
#clearly distribution differs based on presence/absence of lot_frontage data
#evidence that it's not MCAR

#### mas_vnr_type (and ._area as they have the same missingness)
#8/1460 is very small but this suggests that it's not MCAR
df_house %>%
  mutate(na_mas_vnr_type=is.na(mas_vnr_type)) %>%
  ggplot() +
  geom_bar(aes(x=foundation)) +
  facet_wrap(~na_mas_vnr_type)
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
vis_compare(df_house, df_house_i) #for report--trim predictors to first x chr

#lot_frontage
#visually
df_house_i %>%
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
  facet_wrap(~lf_type) +
  labs(x="Lot frontage (log) (ft)",
       y="Sale price ($)") +
  theme_bw() +
  theme(legend.position="none")
  
#statistically
lf_nnas <- !is.na(df_house$lot_frontage)

cor(log(df_house$lot_frontage[lf_nnas]), df_house$sale_price[lf_nnas])^2 #r2 = .122

cor(log(df_house_i$lot_frontage), df_house_i$sale_price)^2 #r2 = .105

#perhaps revisit if prediction is poor



# Write Imputed Data to File========================================================================
#output fp
out_fp <- here("data", "tidy_data", "train_imputed.rds")

# saveRDS(df_house_i, out_fp)



