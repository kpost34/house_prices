# Load Packages, Objects, Functions, & Data=========================================================
#load packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, naniar, finalfit, mice)

#objects
source(here("code", "00_objects.R"))

#functions
source(here("code", "00_helper_fns.R"))

#data
# col_type_vals <- "cccnncccccccccccccciicccccncccccccncnnnccccnnnniiiiiiciciccicincccnnnnnncccniiccn"

df_test0 <- read_csv(here("data", "raw_data", "test.csv"),
                     col_types=col_types_test)



# Initial Data Checking and Cleaning================================================================
## Data checking
dim(df_test0)
summary(df_test0)
skim(df_test0)


## Data cleaning
df_test <- initial_clean_house(df_test0)


## Data checking
#overall
summary(df_test)

#by type
skim_by_type(data=df_test, fn=is.character)
skim_by_type(data=df_test, fn=is.factor)
skim_by_type(data=df_test, fn=is.numeric) 



# Missingness=======================================================================================
## Assess missing data--------------------
### Visualize missing data
vis_dat(df_test)
vis_miss(df_test, 
         sort_miss=TRUE, 
         cluster=TRUE) 

#cols with at least one NA
df_test %>%
  select(where(~any(is.na(.)))) %>%
  vis_miss(sort_miss=TRUE,
           cluster=TRUE)  

df_test %>%
  gg_miss_upset()


#### Examine missingness
map_int(df_test, function(x) sum(is.na(x))) %>%
  enframe(name="predictor", value="n_na") %>%
  filter(n_na > 0) %>%
  mutate(pct_na=(n_na/1460) * 100) %>%
  arrange(desc(n_na)) 
#lot_frontage: ~15.5%
#mas_vnr_type & mas_vnr_area: ~1%
#other 16: < .3%


## Data imputation using mice method "cart"--------------------
### Imputation using CART
df_test %>%
  mice(method="rf", m=2, maxit=2) %>%
  complete() %>%
  as_tibble() -> df_test_i

skim(df_test_i)
#note: utilities was not imputed; however, utilities gets dropped due to constancy


### Compare pre- and post-imputation
vis_compare(df_test, df_test_i)

vis_compare(df_test %>%
              select(starts_with("garage")), 
            df_test_i %>%
              select(starts_with("garage")))



# Feature Selection: Drop Low 'Variance' (frequency) & Highly Correlated Features===================
df_test_i %>%
  #drop low-variance features
  select(-c(street, utilities, condition2, pool_qc, roof_matl)) %>%
  #drop highly correlated features
  select(-c(overall_cond, ms_sub_class, exterior2nd)) -> df_test_icr 



# Feature Engineering: Rare-label Encoding & Feature Scaling=========================================


df_test_icr %>%
  bin_rare_levels() -> df_test_icrc

tabyl(df_test_icrc, house_style) 




















