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


