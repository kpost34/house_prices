#04_prep-test-data.R

#This script follows the prep work of the training data by 1) performing initial data checking and
  #cleaning, 2) assessing missingness and implementing same imputation method as training data, 
  #3) employing feature selection and engineering that mirrors training data handling,  and
  #4) writes tidied test dataframe to file


# Load Packages, Objects, Functions, & Data=========================================================
#load packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, naniar, finalfit, mice)

#objects
source(here("code", "00_objects.R"))

#functions
source(here("code", "00_helper_fns.R"))

#data
df_test0 <- read_csv(here("data", "raw_data", "test.csv"),
                     col_types=col_types_test)



# Initial Data Checking and Cleaning================================================================
## Data checking
#overall
dim(df_test0)
head(df_test0, n=10)
summary(df_test0)
skim(df_test0)

#by type
skim_by_type(data=df_test0, fn=is.character)
skim_by_type(data=df_test0, fn=is.factor)
skim_by_type(data=df_test0, fn=is.numeric) 


## Data cleaning
df_test <- initial_clean_house(df_test0)



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
tab_test_missing <- map_int(df_test, function(x) sum(is.na(x))) %>%
  enframe(name="predictor", value="n_na") %>%
  filter(n_na > 0) %>%
  mutate(pct_na=(n_na/1460) * 100) %>%
  arrange(desc(n_na)) %>%
  mutate(pct_na=signif(pct_na, 3))

tab_test_missing
#lot_frontage: ~15.5%
#mas_vnr_type & mas_vnr_area: ~1%
#other 16: < .3%


## Data imputation using mice method "cart"--------------------
### Imputation using CART
df_test %>%
  mice(method="cart", m=2, maxit=2) %>%
  complete() %>%
  as_tibble() -> df_test_i

tab_summ_num_imp <- skim_by_type(df_test_i, fn=is.numeric)
tab_summ_num_imp
#note: utilities was not imputed; however, utilities gets dropped due to constancy


### Compare pre- and post-imputation
#### Visually
vis_compare(df_test, df_test_i)

vis_compare(df_test %>%
              select(starts_with("garage")), 
            df_test_i %>%
              select(starts_with("garage")))
#major diff occurs with garage_cars (and likely co-occurring missingness with multiple garage 
  #predictors)


#### Determine how these predictors differ
df_test %>%
  select(id, garage_cars) %>%
  left_join(
    df_test_i %>%
      select(id, garage_cars),
    by="id"
  ) %>%
  mutate(diff=garage_cars.x-garage_cars.y) %>%
  distinct(diff) #0 or NA; no diff in values (except missingness)

class(df_test$garage_cars)
class(df_test_i$garage_cars)
#difference in class


#### Check classes of all predictors
class_test <- map_chr(df_test, function(x) {
  class(x)[[1]]
  }) %>%
  enframe(name="var", value="class")


class_test_i <- map_chr(df_test_i, function(x) {
  class(x)[[1]]
  }) %>%
  enframe(name="var", value="class")

class_test %>%
  left_join(class_test_i, by="var") %>%
  mutate(diff=class.x!=class.y) %>% 
  filter(diff)
#garage_cars is only one with an issue
#NOTE: not an issue with training data


# Feature Selection: Change garage_cars class, drop Low 'Variance' & Highly Correlated Features=====
df_test_i %>%
  #change class of garage_cars
  mutate(garage_cars=as.integer(garage_cars)) %>%
  #drop low-variance features
  select(-c(street, utilities, condition2, pool_qc, roof_matl)) %>%
  #drop highly correlated features
  select(-c(overall_cond, ms_sub_class, exterior2nd)) -> df_test_icr 



# Feature Engineering: Rare-label Encoding & Feature Scaling=========================================
df_test_icr %>%
  bin_rare_levels() %>%
  norm_num_preds(vec_preds=num_preds) -> df_test_icrcn
#warnings (from binning) generated b/c of missing initial levels but these are all parts of bins 
  #in tidy data



# Write Clean Test Data to File=====================================================================
#output_fp
out_test_fp <- here("data", "tidy_data", "test_tidy.rds")

# saveRDS(df_test_icrcn, out_test_fp)





