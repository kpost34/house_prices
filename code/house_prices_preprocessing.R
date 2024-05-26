# Load Packages & Source Functions==================================================================
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, naniar, finalfit, mice, rstatix, GGally, 
               cowplot, rstatix, rsample, tidymodels)

source(here("code", "00_helper_fns.R"))
               
               # recipes, car, )

#TO DO: 
#1) reverse order of all factors--perhaps just add rev() to each set of levels
#2) continue combining other categories


#From spaceship titanic app
#1. Read in data - DONE
#2. Clean col names (janitor) - DONE
#3. Separate composite cols into multiple cols - NA
#4. Convert chr vars to fcts - DONE
#5. Check dims, summaries, and samples of data - DONE
#6. Assess degrees of missingness per variable - DONE
#7. Look at data summaries by var type (e.g., chr, fct, log, etc) - DONE
#8. EDA: univariate, bivariate, multivariate ('tri' variate) [tables and plots]

#9. Discretized a numerical (chr) var into bins

#10. Chr string imputation: - NA
  #a) Assess missingngess in tabular and graphical forms
  #b) Assess missingness against another variable (two different ones)
  #c) Consider options: ignore, drop cols, remove rows, populate using other var (two options)) 

#11. Data imputation: 
  #a) assess missingness visually (vis_dat() and vis_miss() & other fns)
  #b) handle missing data-listwise deletion, mean/median/most frequent cat imputation, 
    #multiple imputation
  #c) post-imputation check

#12. Post-imputation check: vis_compare() among other fns


# Data Import=======================================================================================
# col_type_vals <- "cffnnffffffffffffffiifffffnfffffffnfnnnffffnnnniiiiiifififfifinfffnnnnnnfffniiffn"
#read in fcts as chr and convert later
col_type_vals <- "cccnncccccccccccccciicccccncccccccncnnnccccnnnniiiiiiciciccicincccnnnnnncccniiccn"


# df_house0 <- read_csv(here("data", "raw_data", "test.csv"),
df_house0 <- read_csv(here("data", "raw_data", "train.csv"),
                           col_types=col_type_vals)



# Data Cleaning, Wrangling, and Preprocessing=======================================================
## 1 Clean col names, replace NA factor levels when they are not missing data, & create ordered fcts
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

test_lev <- df_house %>%
  select(where(is.factor)) %>%
  purrr::map(levels)

setequal(train_lev, test_lev) #TRUE


## 2 Preliminary data checking--------------------
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


## 3 Missingness decision making--------------------
#NOTE: for test data, the following variables have true missing data: [enter here]

### Assess missing data
#### Visualize missing data
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


#### Determine pattern of missingness
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


## 4 Data imputation using mice method "cart"--------------------
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


## 5 Feature Selection: Drop low 'variance' (frequency) features--------------------
### Find candidates (>= 98% of data are in one group)
feat_low_var <- df_house_i %>%
  select(where(is.factor)) %>% 
  purrr::map(function(col) {
    col %>%
      tabyl() %>%
      mutate(percent=round(percent, 3),
             high_pct=percent > 0.98) %>%
      reframe(low_var=sum(high_pct)==1)
  }) %>%
  unlist() %>%
  enframe(name="feature", value="low_variance") %>%
  mutate(feature=str_remove(feature, "\\.low_var$")) %>%
  filter(low_variance) %>%
  pull(feature)

feat_low_var #street, utilities, condition2, roof_matl, pool_qc


### Assess candidates
#street
df_house_i %>%
  count(street)
(1454/1460) * 100 #99.59% pave
#so few gravel cases such that k-fold cv will not work

df_house_i %>%
  ggplot() +
  geom_density(aes(x=sale_price)) +
  facet_wrap(~street) 
#such small n but similar distribution--gravel shifted to lower prices

#utilities
df_house_i %>%
  count(utilities)
#1459/1460 are "AllPub", so "yes" this indicates constancy and thus should be dropped

#condition2
df_house_i %>% 
  count(condition2) %>% 
  arrange(desc(n))
(1445/1460) * 100 #98.97% Norm & 1-2 occurrences in 6 other categories, so drop

#pool_qc
df_house_i %>%
  count(pool_qc) 
(1453/1460) * 100 #99.5% NP, so drop

#roof_matl
df_house_i %>%
  count(roof_matl)
(1434/1460) * 100 #98.22 % CompShg
#given such small counts in non-CompShg category, these would need to show a strong relationship
  #with sale_price when combined

df_house_i %>%
  mutate(roof_matl=as.character(roof_matl),
         roof_matl=ifelse(roof_matl=="CompShg", roof_matl, "Other")) %>%
  ggplot(aes(x=roof_matl, y=sale_price)) +
  # geom_boxplot()
  stat_summary(geom="bar", fun=mean) +
  stat_summary(geom="errorbar", fun.data=mean_se)
#let's keep this for now and consider rare-label encoding


### Remove said features
df_house_i %>%
  select(-c(street, utilities, condition2, pool_qc)) -> df_house_ic



## 6 Feature Selection: Remove highly correlated features--------------------

### Numeric-numeric
df_house_ic %>%
  select(!sale_price) %>%
  select(where(is.numeric)) %>%
  cor() %>%
  replace_diag_na() %>% 
  as.data.frame() %>%
  mutate(across(everything(), ~round(.x, 3))) %>%
  filter(if_any(everything(), ~abs(.) >= 0.9)) #retain rows where 1+ value has > 90% correlation
#empty DF; no features dropped for high correlation


### Ordered factor-ordered factor
df_house_ic %>%
  select(where(is.ordered)) %>%
  mutate(across(everything(), ~as.integer(.x))) %>%
  cor(method="spearman") %>%
  replace_diag_na() %>%
  as.data.frame() %>%
  mutate(across(everything(), ~round(.x, 3))) %>%
  filter(if_any(everything(), ~abs(.) >= 0.9)) %>%
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("var") %>%
  filter(if_any(.cols=!var, ~. > 0.9))
  #overall_qual and overall_cond are 'perfectly' correlated, so drop one of them: overall_cond
#NOTE: use a heat map for document/report
  

### Factor-factor
#### Select only factor columns
df_house_ic %>%
  select(where(is.factor)) %>%
  select(!where(is.ordered)) -> df_house_ic_fct

#### Run chi-square tests on all pairwise combinations
#isolate all pairwise combinations
all_fct_combos <- combn(names(df_house_ic_fct),2) %>%
  t() %>%
  as_tibble() 

#generate vector of labels
labs_fct <- all_fct_combos %>%
  mutate(lab=paste(V1, V2, sep="__")) %>%
  pull(lab) 

#run all pairwise chi-square tests
all_fct_combos %>%
  pull(V1) %>%
  purrr::map2(.x=., .y=all_fct_combos$V2, .f=function(x, y) {
    
    df_house_ic_fct %>%
      select(all_of(c(x, y))) %>%
      group_by(across(everything())) %>%
      count() %>%
      ungroup() %>%
      pivot_wider(names_from=y, values_from="n") %>%
      mutate(across(everything(), ~replace_na(.x, 0))) %>%
      column_to_rownames(var=x) %>%
      as.matrix() -> mat
    
    mat %>%
      chisq.test() -> chi_sq_result
    
    list(matrix=mat,
         chisq_result=chi_sq_result)
  
  }) -> list_house_fct_chis

#name each list element
names(list_house_fct_chis) <- labs_fct

#pull p-values from chi-square test results
list_house_fct_chis %>%
  purrr::map(`[[`, 2) %>%
  purrr::map("p.value") %>%
  enframe(name="feature_pair", value="p.value") %>%
  unnest(p.value) %>%
  filter(p.value <= 0.05) %>%
  arrange(p.value) %>% 
  # filter(p.value==0) %>%
  pull(feature_pair) -> low_p_val_x2_feat


list_house_fct_chis[[1]]["matrix"] %>%
  as.data.frame() %>%
  rename_with(.cols=everything(), ~str_remove(.x, "^matrix\\.")) %>%
  rownames_to_column(var="ms_sub_class") %>%
  pivot_longer(cols=!ms_sub_class, names_to="ms_zoning", values_to="n") %>%
  uncount(n) %>%
  ggplot() +
  geom_count(aes(x=ms_sub_class, y=ms_zoning)) +
  scale_size_area()


plot_counts <- function(plot_num) {
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
    scale_size_area()
}

#most 'suspicious' (after looking at 10 pairwise comps with lowest p-values)
plot_counts(plot_num=2)
plot_counts(plot_num=6)

low_p_val_x2_feat[1:6]

list_house_fct_chis[[low_p_val_x2_feat[2]]]["matrix"]
list_house_fct_chis[[low_p_val_x2_feat[6]]]["matrix"]
list_house_fct_chis[[low_p_val_x2_feat[3]]]["matrix"]

#drop ms_sub_class and exterior2nd


### Remove features
df_house_ic %>%
  select(-c(overall_cond, ms_sub_class, exterior2nd)) -> df_house_icr
#i = imputed data, c=dropped vars due to constancy, r=dropped vars due to redundancy/multicollinearity



## 7 Feature Engineering: Rare-label encoding--------------------
### Factors
#### Identify which have rare categories
feat_rare_cat <- df_house_icr %>%
  select(where(is.factor)) %>%
  select(!where(is.ordered)) %>%
  purrr::map(function(col) {
    col %>%
      tabyl() %>%
      mutate(percent=round(percent, 3),
             low_pct=percent > 0 & percent < 0.02) %>%
      reframe(rare=sum(low_pct) > 0)
  }) %>%
  unlist() %>%
  enframe(name="feature", value="rare_category") %>%
  mutate(feature=str_remove(feature, "\\.rare$")) %>%
  filter(rare_category) %>%
  pull(feature)

feat_rare_cat


#### Look at examples
list_rare_cat_viz <- feat_rare_cat %>%
  set_names(feat_rare_cat) %>%
  purrr::map(function(x) {
    explore_rare_cats(predictor=!!sym(x)) 
  })

#ms_zoning: 
list_rare_cat_viz["ms_zoning"] 
explore_rare_cats(predictor=ms_zoning) 
#combine RH and C (all) into 'other' 
#RH has slightly higher sale price avg than RM, but fewer samples and larger variance
#makes more sense to make an other category
#Other: RH, C (all)

#lot_config
list_rare_cat_viz["lot_config"]
#FR3 is the only rare category but it makes sense to combine with FR2 given similarity of 
  #category and sale price (despite very low n)
#FR2_3: FR2, FR3

#neighborhood  
list_rare_cat_viz["neighborhood"]
#group the rare ones into three other categories by closeness in sale price (but not with more
  #common neighborhoods as these have greater sample size and I'm unusre how these are related)
#Other1: StoneBr, Veenker
#Other2: ClearCr, Blmngtn, 
#Other3: SWISU, MeadowV, BrDale, NPkVill, Blueste

#condition1 
list_rare_cat_viz["condition1"]
#many groups with small percentages so need to group multiple to hit 2% threshhold
#RRnear: RRAn, RRAe, RRNn, RRNe
#PosClose: PosN, PosA

#house_style   
list_rare_cat_viz["house_style"] 
#group 2.5Fin with 2Story b/c similar stories and finished status and mean sale_price
#group the other two rare cats together b/c both have unfinished second story and a half story
#Story_2_2.5: 2Story, 2.5Fin
#Unf_half: 1.5Unf, 2.5Unf

#roof_style  
list_rare_cat_viz["roof_style"] 
#group the four rare categories as 'other' b/c don't see any other relationships
#Other: Flat, Gambrel, Mansard, Shed

#roof_matl
list_rare_cat_viz["roof_matl"] 
#per notes from earlier, I decided to keep roof_matl b/c of differences in relationship with sale
  #price if look at CompShg vs Other, so will group this way
#Other: Tar&Grv, WdShngl, WdShake, ClyTile, Membran, Metal, Roll

#exterior1st    
list_rare_cat_viz["exterior1st"] 
#group < 2% as other: 8 categories and 5 of them have only n = 1 or 2; the three on their own don't
  #group well together or with existing categories.
#Other: WdShing, Stucco, AsbShng, BrkComm, Stone, AsphShn, CBlock, ImStucc

#mas_vnr_type   
list_rare_cat_viz["mas_vnr_type"] 
#although BrkCmn is closer to None in sale_price, it has few numbers and is a 'present' category
  #and shares the same material as BrkFace, so these should be grouped together
#BrkFace_Cmn: BrkFace, BrkCmn

#foundation    
list_rare_cat_viz["foundation"] 
#stone and wood seem much different than the others but are too small for their own group, so
  #group with Slab, the other rare category
#Other: Slab, Stone, Wood

#heating    
list_rare_cat_viz["heating"] 
#group non-GasA as 'Other' b/c GasA is so common and the others and thus much smaller with high
  #variability
#Other: GasW, Grav, Wall, OthW, Floor

#garage_type    
list_rare_cat_viz["garage_type"] 
#again, three categories with very low frequencies. A basement garage is most similar to a built-in
  #garage, but their sale price values differ wildly. The other two are different than the rest, 
  #so again let's group the rare categories as 'Other'
#Other: Basment, CarPort, 2Types

#misc_feature  
list_rare_cat_viz["misc_feature"] 
#given the rarity of Gar2, Othr, and TenC and that NMF (the NA cat) makes up 96% of values to
  #simply combine the rest into MF
#MF: Shed, Gar2, Othr, TenC

#sale_type   
list_rare_cat_viz["sale_type"] 
#the problem here is that the contract categories could go together but combined the percentage is
  #too low. The Oth category is extremely rare and has no natural combination, so let's combine all
  #rare ones into a larger Other category
#Other: ConLD, ConLI, ConLw, CWD, Oth, Con

#sale_condition
list_rare_cat_viz["sale_condition"] 
#no natural combinations so group using other
#Other: Family, Alloca, AdjLand


### Ordered factors
feat_rare_ord <- df_house_icr %>%
  select(where(is.ordered)) %>%
  purrr::map(function(col) {
    col %>%
      tabyl() %>%
      mutate(percent=round(percent, 3),
             low_pct=percent > 0 & percent < 0.02) %>%
      reframe(rare=sum(low_pct) > 0)
  }) %>%
  unlist() %>%
  enframe(name="feature", value="rare_category") %>%
  mutate(feature=str_remove(feature, "\\.rare$")) %>%
  filter(rare_category) %>%
  pull(feature)

feat_rare_ord

#### Look at examples
#### Identify which have rare categories
list_rare_ord_viz <- feat_rare_ord %>%
  set_names(feat_rare_ord) %>%
  purrr::map(function(x) {
    explore_rare_cats(predictor=!!sym(x)) 
  })

#lot_shape
list_rare_ord_viz["lot_shape"] 
explore_rare_cats(predictor=lot_shape) 
#makes most sense to combine IR2 and IR3 b/c ordinal nature and they are close in avg sale price
#IR2_3: IR2, IR3

#land_slope
list_rare_ord_viz["land_slope"] 
#naturally makes most sense to combine moderate and severe slope
#Mod_Sev: Mod, Sev

#overall_qual
list_rare_ord_viz["overall_qual"] 
#combine 1-4 since overall quality does align well with sale_price
#4_and_less: 1, 2, 3, 4

#exter_qual
list_rare_ord_viz["exter_qual"] 
#exterior quality aligns well with sale price so combine lowest three categories
#TA_and_less: Po, Fa, TA

#exter_cond
list_rare_ord_viz["exter_cond"] 
#exterior condition aligns well with sale price so combine good and excellent into one category
  #and poor and fair into another
#Ex_Gd: Ex, Gd
#Fa_Po: Fa, Po

#bsmt_cond
list_rare_ord_viz["bsmt_cond"] 
#this predictor also aligns well with sale price with the caveat that no basement is associated
  #with a greater sale price, on average, than a basement of poor condition
#since no basement is different than the rest and poor is so rare, let's combine poor with fair
#let's also combine good and excellent (Ex is absent in training set)
#Fa_Po: Fa, Po
#Ex_Gd: Ex, Gd

#bsmt_fin_type2
list_rare_ord_viz["bsmt_fin_type2"]  
#ordinal categories don't align perfectly with sale price
#still makes most sense to group close ranks together; thus, GLQ and ALQ should be grouped
#ALQ_GLQ: ALQ, GLQ

#heating_qc
list_rare_ord_viz["heating_qc"]  
#ranks align well with sale price, so makes sense to group poor with fair
#Fa_Po: Fa, Po

#electrical
list_rare_ord_viz["electrical"]  
#the rarest three categories are also the three least ranked and the three with the lowest sale
  #prices, so these should be grouped
#FuseF_P_Mix: FuseF, FuseP, Mix

#functional
list_rare_ord_viz["functional"] 
#don't align 'perfectly' with sale price but all cats but Typ are infrequent
#group Mod and below into one category: "Poor"
#Poor: Mod, Maj1, Maj2, Sev, Sal

#fireplace_qu
list_rare_ord_viz["fireplace_qu"] 
#aligns well with sale price except NFp is above Po; Ex and Po are rare so should be grouped
  #with Gd and Fa, respectively
#Ex_Gd: Ex, Gd
#Fa_Po: Fa, Po

#garage_qual
list_rare_ord_viz["garage_qual"] 
#aligns well with sale price except that NG is above Po; Gd, Ex, and Po are are so group the
  #former two with TA and the latter with Fa
#Ex_Gd_TA: Ex, Gd, TA
#Fa_Po: Fa, Po

#garage_cond
list_rare_ord_viz["garage_cond"] 
#does not align well with sale price, but Gd, Po, and Ex are rare; again, cluster by rank
#Ex_Gd_TA: Ex, Gd, TA
#Fa_Po: Fa, Po


### Collapsing
df_house_icr %>% 
  mutate(
    #factors (unordered)
    ms_zoning=fct_collapse(ms_zoning, Other=c("RH", "C (all)")),
    lot_config=fct_collapse(lot_config, FR2_3=c("FR2", "FR3")),
    neighborhood=fct_collapse(neighborhood, Other1=c("StoneBr", "Veenker")),
    neighborhood=fct_collapse(neighborhood, Other2=c("ClearCr", "Blmngtn")),
    neighborhood=fct_collapse(neighborhood, Other3=c("SWISU", "MeadowV", "BrDale", "NPkVill", "Blueste")),
    condition1=fct_collapse(condition1, RRnear=c("RRAn", "RRAe", "RRNn", "RRNe")),
    condition1=fct_collapse(condition1, PosClose=c("PosN", "PosA")),
    house_style=fct_collapse(house_style, Story_2_2.5=c("2Story", "2.5Fin")),
    house_style=fct_collapse(house_style, Unf_half=c("1.5Unf", "2.5Unf")),
    roof_style=fct_collapse(roof_style, Other=c("Flat", "Gambrel", "Mansard", "Shed")),
    roof_matl=fct_collapse(roof_matl, Other=c("Tar&Grv", "WdShngl", "WdShake", "ClyTile", "Membran",
                                              "Metal", "Roll")),
    exterior1st=fct_collapse(exterior1st, Other=c("WdShing", "Stucco", "AsbShng", "BrkComm", "Stone",
                                                  "AsphShn", "CBlock", "ImStucc")),
    mas_vnr_type=fct_collapse(mas_vnr_type, BrkFace_Cmn=c("BrkFace", "BrkCmn")),
    foundation=fct_collapse(foundation, Other=c("Slab", "Stone", "Wood")),
    heating=fct_collapse(heating, Other=c("GasW", "Grav", "Wall", "OthW", "Floor")),
    garage_type=fct_collapse(garage_type, Other=c("Basment", "CarPort", "2Types")),
    misc_feature=fct_collapse(misc_feature, MF=c("Shed", "Gar2", "Othr", "TenC")),
    sale_type=fct_collapse(sale_type, Other=c("ConLD", "ConLI", "ConLw", "CWD", "Oth", "Con")),
    sale_condition=fct_collapse(sale_condition, Other=c("Family", "Alloca", "AdjLand")),
    #ordered factors
    lot_shape=fct_collapse(lot_shape, IR2_3=c("IR2", "IR3")), 
    land_slope=fct_collapse(land_slope, Mod_Sev=c("Mod", "Sev")),
    overall_qual=fct_collapse(overall_qual, `4_and_less`=c("1", "2", "3", "4")),
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
    garage_qual=fct_collapse(garage_qual, Ex_Gd=c("Ex", "Gd")),
    garage_qual=fct_collapse(garage_qual, Fa_Po=c("Fa", "Po")),
    garage_cond=fct_collapse(garage_cond, Ex_Gd=c("Ex", "Gd")),
    garage_cond=fct_collapse(garage_cond, Fa_Po=c("Fa", "Po"))) -> df_house_icrc
#last c = collapsing
    

  
## 8 Feature Engineering: Discretization-------------------- [Note: revisit later if model performs poorly]
### Rationale to  discretization
#reasons to discretize
#1) non-linear relationships between predictor and target variable
#2) decision trees handle discrete variables well 
#3) increases interpretability
#4) can reduce noise and overfitting, especially with outliers
#5) can be helpful if data are skewed and/or has extreme outliers

#reasons not to discretize
#1) linear regression does not handle discrete variables well
#2) loss of information
#3) added computational processing

#why discretize?
#1) non-linear relationship
#2) skewed data
#3) outliers


### Determine predictors poorly correlated with target variable
#table of correlations b/t preds and target variable
tab_cor_pred_dep <- df_house_icrc %>%
  select(where(is.numeric)) %>%
  cor(method="pearson") %>%
  as.data.frame() %>%
  select(sale_price) %>%
  mutate(sale_price=round(sale_price, 3)) %>%
  # arrange(desc(sale_price)) %>%
  rownames_to_column(var="predictor") %>%
  as_tibble() %>%
  filter(predictor!="sale_price") %>% 
  rename(pearson_cor="sale_price") %>%
  mutate(pearson_cor_abs=abs(pearson_cor)) %>% 
  arrange(desc(pearson_cor_abs))

#subset for predictors where absolute value of correlation is less than 0.3
preds <- tab_cor_pred_dep %>%
  filter(pearson_cor_abs < .3) %>%
  pull(predictor) %>%
  sort()

preds1 <- preds[1:9]
preds2 <- preds[10:17]


### Visualize data
#histograms
df_house_icrc %>%
  select(all_of(preds1)) %>% 
  pivot_longer(cols=everything(), names_to="variable", values_to="value") %>%
  ggplot() +
  geom_histogram(aes(x=value, fill=variable), color="black") +
  facet_wrap(~variable, scales="free") +
  scale_fill_viridis_d(end=.7) +
  theme_bw() +
  theme(legend.position="none")

df_house_icrc %>%
  select(all_of(preds2)) %>% 
  pivot_longer(cols=everything(), names_to="variable", values_to="value") %>%
  ggplot() +
  geom_histogram(aes(x=value, fill=variable), color="black") +
  facet_wrap(~variable, scales="free") +
  scale_fill_viridis_d(end=.7) +
  theme_bw() +
  theme(legend.position="none")
  
#scatterplots
df_house_icrc %>%
  select(all_of(preds1), sale_price) %>%
  pivot_longer(cols=!sale_price, names_to="variable", values_to="value") %>%
  ggplot(aes(x=value, y=sale_price, color=variable)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", color="blue") +
  facet_wrap(~variable, scales="free") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position="none")


df_house_icrc %>%
  select(all_of(preds2), sale_price) %>%
  pivot_longer(cols=!sale_price, names_to="variable", values_to="value") %>%
  ggplot(aes(x=value, y=sale_price, color=variable)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", color="blue") +
  facet_wrap(~variable, scales="free") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position="none")



## 9 Feature Engineering: Feature Scaling--------------------
### Rationale
#normalization
  #different scales/units
  #non-normal distribution

#standardization
  #normal distribution

#min-max scaling
  #data with bounded range
  #when data are normally distributed without outliers

#max abs scaling
  #sparse matrices
  #presence of many zero values

#robust scaling
  #outliers present

#log transform
  #skewed data
  #varying variance across range of values
  #multiplicative relationship between variables
  #percent change variables
  #compress ranges (e.g., image processing)
  #non-normal data (helps normalize it)


### Check for normal distribution
num_preds <- tab_cor_pred_dep %>%
  pull(predictor) %>%
  unique() %>%
  sort() 

length(num_preds)

num_preds1 <- num_preds[1:12]
num_preds2 <- num_preds[13:24]
num_preds3 <- num_preds[25:33]

df_house_icrc %>%
  select(all_of(num_preds1)) %>%
  pivot_longer(cols=everything(), names_to="variable", values_to="value") %>%
  ggplot(aes(sample=value, color=variable)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~variable, scales="free") +
  theme_bw() +
  theme(legend.position="none")

df_house_icrc %>%
  select(all_of(num_preds2)) %>%
  pivot_longer(cols=everything(), names_to="variable", values_to="value") %>%
  ggplot(aes(sample=value, color=variable)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~variable, scales="free") +
  theme_bw() +
  theme(legend.position="none")

df_house_icrc %>%
  select(all_of(num_preds3)) %>%
  pivot_longer(cols=everything(), names_to="variable", values_to="value") %>%
  ggplot(aes(sample=value, color=variable)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~variable, scales="free") +
  theme_bw() +
  theme(legend.position="none")

#NOTE: will need to functionalize these then map over it

#major concerns about normal distribution

df_house_icrc %>%
  select(all_of(num_preds)) %>%
  pivot_longer(cols=everything(), names_to="predictor", values_to="value") %>%
  group_by(predictor) %>%
  shapiro_test(value) %>%
  arrange(desc(p)) 
#all highly significant


### Normalization
#non-normal data, different scales of measurement
normalize <- function(var) {
  norm_var <- (var - min(var))/(max(var) - min(var))
  
  return(norm_var)
}


df_house_icrc %>%
  mutate(
    across(.cols=all_of(num_preds),
           ~normalize(.x))
  ) -> df_house_icrcn
#n = normalization



## 10 Create Folds for Cross-validation--------------------
df_house_icrcn %>%
  vfold_cv(v=2) -> df_vfold




# 11 Modelling--------------------
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

















### Numerical predictors


#create correlation matrix
df_house_ic %>%
  select(where(is.numeric)) %>%
  ggcorr()
#adjust options

#look at strong correlations more closely -- individual/faceted ggplots
gg1 <- df_house_ic %>%
  select(where(is.numeric)) %>%
  ggpairs() 
  # ggpairs(columns=c("lot_frontage", "lot_area", "sale_price")) 

#also might be worthwhile to look at poor and moderate correlaitons and try with/without scaling
  #to see improvement

#scale features so test out and re-compute correlations
primary_var <- "sale_price"
pvar_pos <- match(primary_var, gg1$xAxisLabels)

n1 <- gg1$ncol/2
plots1 <- lapply(1:n1, function(j) getPlot(gg1, i = pvar_pos, j = j))
ggmatrix(
    plots1,
    nrow = 1,
    ncol = n1,
    xAxisLabels = gg1$xAxisLabels[1:n1],
    yAxisLabels = primary_var
)

plots2 <- lapply((gg1$ncol/2) + 1:gg1$ncol, function(j) getPlot(gg1, i = pvar_pos, j = j))
ggmatrix(
    plots2,
    nrow = 1,
    ncol = gg1$ncol,
    xAxisLabels = gg1$xAxisLabels,
    yAxisLabels = primary_var
)










### Dependent variable info
range(df_house$sale_price) #34,900-755,000
summary(df_house$sale_price)
hist(df_house$sale_price)
hist(log(df_house$sale_price)) 


### Relationships between predictors and dependent variable
#lot_frontage (259)
#visualize relationship
ggplot(df_house,aes(lot_frontage, sale_price)) + 
  geom_point() +
  geom_smooth(method=lm)
#appears to be positive linear but greater error at higher values

#remove high lot_frontage values
df_house %>% 
  filter(lot_frontage<=200) %>%
  ggplot(aes(lot_frontage,sale_price)) +
  geom_point() +
  geom_smooth(method=lm)
#reduces error

#determine correlation
lot_frontage_vals <- which(!is.na(df_house$lot_frontage)) #index of non-na lot_frontage values
cor(df_house$lot_frontage[lot_frontage_vals],df_house$sale_price[lot_frontage_vals]) #rho = 0.351
0.351^2 #r^2 = 12.3%; worth keeping (especially with 259 NAs and intuitive reasons)

#plot on log-log scale
ggplot(df_house,aes(log(lot_frontage), log(sale_price))) + 
  geom_point() +
  geom_smooth(method=lm)
#tighter relationship on log-log scale


#fireplace_qu (690)
#convert NA value to a factor level
df_house %>%
  mutate(fireplace_qu=fct_na_value_to_level(fireplace_qu, level="NFp")) %>%
  group_by(fireplace_qu) %>%
  summarize(n_fire_qu=n(), 
            mean_sale_price=mean(sale_price)) %>%
  ungroup() %>%
  mutate(fireplace_qu=fct_reorder(fireplace_qu, mean_sale_price, .desc=TRUE)) %>%
  ggplot() +
  geom_col(aes(x=fireplace_qu, y=mean_sale_price, fill=fireplace_qu), color="black") +
  geom_text(aes(x=fireplace_qu, y=mean_sale_price, label=n_fire_qu), 
            nudge_y=5000) +
  labs(x="Fireplace quality",
       y="Mean sale price ($)") +
  scale_y_continuous(expand=expansion(mult=c(0, .1))) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position="none") 
#mean sale price by fireplace quality category shows discrimination among categories


#fence (1179)
#convert NA value to a factor level
levels(df_house$fence) #NA is a level, but R does not recognize that
df_house$fence<-fct_expand(df_house$fence,"NF") #add an NF (no fence) level to the factor
df_house$fence[which(is.na(df_house$fence))]<-"NF"

fence_num<-tabyl(df_house$fence)$n #numbers of each fac level
tapply(df_house$sale_price,df_house$fence,mean) #188 K NF, 179 K GdPrv, 149 K MnPrv, 140 K GdWo, 134 K MnWw
tapply(df_house$sale_price,df_house$fence,sd)/fence_num #SEs
#shows some differentiation in sale_price by fence quality


#alley (1369)
tabyl(df_house$alley) #93.7% NA
levels(df_house$alley) #NA is a level, but R does not recognize that
df_house$alley<-fct_expand(df_house$alley,"NAl") #add an NAl (no alley) level to the factor
df_house$alley[which(is.na(df_house$alley))]<-"NAl"
tapply(df_house$sale_price,df_house$alley,mean) #122 K Grvl, 168 K Pave, 183 K NAl
#high frequency of one level--problem?


#misc_feature (1406)
tabyl(df_house$misc_feature) #96% NA
levels(df_house$misc_feature) #NA is a level, but R does not recognize that
df_house$misc_feature<-fct_expand(df_house$misc_feature,"NMF") #add an NMF (no misc_feature) level to the factor
df_house$misc_feature[which(is.na(df_house$misc_feature))]<-"NMF"
tapply(df_house$sale_price,df_house$misc_feature,mean) #No Elev, Gar2 170 K, Othr 94 K, Shed 151 K, TenC 250 K, 182 K NMF
#problem is that there is constancy in this variable and select factor levels lack representation;
#thus, it makes it difficult for cross-validation and predictive power for test data would lack precision
#e.g., if test data contain multiple homes with tennis courts or elevators


#pool_qc (1453)
tabyl(df_house$pool_qc) #99.5% NA (NP)
levels(df_house$pool_qc) #NA is a level, but R codes it as missing data
df_house$pool_qc<-fct_expand(df_house$pool_qc,"NP") #add an NP (no pool) level to the factor
df_house$pool_qc[which(is.na(df_house$pool_qc))]<-"NP" #now to the data
tapply(df_house$sale_price,df_house$pool_qc,mean) 
#Ex 490K, Gd 202 K, Fa 215 K, NP 180 K; no Ta category; problem--
#lack of variability? Fa > Gd; regression problem and not a classification problem

#investigate all vars for miscoded NAs
house_NAs<-map_int(df_house,function(x) sum(is.na(x))) #check each col using purrr
sort(house_NAs,decreasing=TRUE)
#electrical, mas_vnr_type, mas_vnr_area, bsmt_qual, bsmt_cond, bsmt_fin_type1, bsmt_exposure,
#bsmt_fin_type2, garage_type, garage_yr_blt, garage_finish, garage_qual, garage_cond

tabyl(df_house$electrical) #real NA (1)
tabyl(df_house$mas_vnr_type) #real NAs (8)
sum(is.na(df_house$mas_vnr_area)) #real NAs (8)

tabyl(df_house$bsmt_qual) #not a real NA
levels(df_house$bsmt_qual)
df_house$bsmt_qual<-fct_expand(df_house$bsmt_qual,"NB") #add an NB (no basement) level to the factor
df_house$bsmt_qual[which(is.na(df_house$bsmt_qual))]<-"NB" #now to the data

tabyl(df_house$bsmt_cond) #not a real NA
levels(df_house$bsmt_cond)
df_house$bsmt_cond<-fct_expand(df_house$bsmt_cond,"NB") #add an NB (no basement) level to the factor
df_house$bsmt_cond[which(is.na(df_house$bsmt_cond))]<-"NB" #now to the data

tabyl(df_house$bsmt_fin_type1) #not a real NA
levels(df_house$bsmt_fin_type1)
df_house$bsmt_fin_type1<-fct_expand(df_house$bsmt_fin_type1,"NB") #add an NB (no basement) level to the factor
df_house$bsmt_fin_type1[which(is.na(df_house$bsmt_fin_type1))]<-"NB" #now to the data

tabyl(df_house$bsmt_exposure) #not a real NA
levels(df_house$bsmt_exposure)
df_house$bsmt_exposure<-fct_expand(df_house$bsmt_exposure,"NB") #add an NB (no basement) level to the factor
df_house$bsmt_exposure[which(is.na(df_house$bsmt_exposure))]<-"NB" #now to the data

tabyl(df_house$bsmt_fin_type2) #not a real NA
levels(df_house$bsmt_fin_type2)
df_house$bsmt_fin_type2<-fct_expand(df_house$bsmt_fin_type2,"NB") #add an NB (no basement) level to the factor
df_house$bsmt_fin_type2[which(is.na(df_house$bsmt_fin_type2))]<-"NB" #now to the data

tabyl(df_house$garage_type) #not a real NA
levels(df_house$garage_type)
df_house$garage_type<-fct_expand(df_house$garage_type,"NG")
df_house$garage_type[which(is.na(df_house$garage_type))]<-"NG"

tabyl(df_house$garage_yr_blt)
sum(is.na(df_house$garage_yr_blt)) #real NAs (81)

tabyl(df_house$garage_finish) #not a real NA
levels(df_house$garage_finish)
df_house$garage_finish<-fct_expand(df_house$garage_finish,"NG")
df_house$garage_finish[which(is.na(df_house$garage_finish))]<-"NG"

tabyl(df_house$garage_qual) #not a real NA
levels(df_house$garage_qual)
df_house$garage_qual<-fct_expand(df_house$garage_qual,"NG")
df_house$garage_qual[which(is.na(df_house$garage_qual))]<-"NG"

tabyl(df_house$garage_cond) #not a real NA
levels(df_house$garage_cond)
df_house$garage_cond<-fct_expand(df_house$garage_cond,"NG")
df_house$garage_cond[which(is.na(df_house$garage_cond))]<-"NG"

#assess missingness after correcting NAs
vis_miss(df_house) #0.3%
house_NAs<-map_int(df_house,function(x) sum(is.na(x))) #check each col using purrr
sort(house_NAs) #only 5 cols (down from 19)






## 6 Feature engineering




# Data Export=======================================================================================
save(c_house,file=here("data","tidy_data","tidy_house.rda"))
#saves tbl and preserves data types






#---------------------------------------------------------------
#UNUSED code

#6. Add levels to factors that are not present in training data
c_house %>% 
  select(where(is.factor)) %>% 
  map(levels) %>%
  map_int(length) #returns number of levels of each factor

sort(levels(c_house$ms_zoning)) #missing 3
c_house$ms_zoning<-fct_expand(c_house$ms_zoning,c("A","I","RP")) 
sort(levels(c_house$exterior1st))#missing 2
c_house$exterior1st<-fct_expand(c_house$exterior1st,c("Other","PreCast")) 
sort(levels(c_house$exterior2nd)) #missing 1
c_house$exterior2nd<-fct_expand(c_house$exterior2nd,"PreCast")
sort(levels(c_house$mas_vnr_type)) #missing 1
c_house$mas_vnr_type<-fct_expand(c_house$mas_vnr_type,"CBlock")
sort(levels(c_house$exter_qual)) #missing 1
c_house$exter_qual<-fct_expand(c_house$exter_qual,"Po")
sort(levels(c_house$bsmt_qual)) #missing 1
c_house$bsmt_qual<-fct_expand(c_house$bsmt_qual,"Po")
sort(levels(c_house$bsmt_cond)) #missing 1
c_house$bsmt_cond<-fct_expand(c_house$bsmt_cond,"Ex")
sort(levels(c_house$kitchen_qual)) #missing 1
c_house$kitchen_qual<-fct_expand(c_house$kitchen_qual,"Po")
sort(levels(c_house$functional)) #missing 1
c_house$functional<-fct_expand(c_house$functional,"Sal")
sort(levels(c_house$misc_feature)) #missing 1
c_house$misc_feature<-fct_expand(c_house$misc_feature,"Elev")
sort(levels(c_house$sale_type)) #missing 1
c_house$sale_type<-fct_expand(c_house$sale_type,"VWD")

f_house<-c_house


#8. Assumptions of linear regression
c_house %>% select(where(is.integer)) %>% names() #18
c_house %>% select(where(is.double)) %>% names() #20
c_house %>% select(where(is.factor)) %>% names() #38

#store predictors into tibbles
house_quant_preds<-c_house %>%
  select(c(where(is.integer),where(is.double),-c(id,sale_price)))

#create function to store list of models
lm_mod_maker<-function(predictors, dep.var){
  mod_list<-vector(mode="list",length=length(predictors))
  for(i in seq_along(predictors)){
    names(mod_list)[[i]]<-str_c(names(predictors[i]),"_mod")
    mod_list[[i]]<-lm(dep.var~predictors[[i]])
  }
  mod_list
}

#create mods
house_mods<-lm_mod_maker(house_quant_preds,c_house$sale_price)
house_mods

house_log_mods<-lm_mod_maker(house_quant_preds,log(c_house$sale_price))
house_log_mods


#1. linearity

#2. independence (lack of auto-correlation)
#check with DW test (see below; from car package)
house_DW_results<-map(house_mods,durbinWatsonTest) 
house_DW_results #no p<.05; thus no issue with auto-correlation

#4. normality
#check with Shapiro-Wilk test
house_mod_resids<-map(house_mods,resid)
house_mod_resid_norm<-map(house_mod_resids,shapiro.test)
house_mod_resid_norm

house_log_mod_resids<-map(house_log_mods,resid)
house_log_mod_resid_norm<-map(house_log_mod_resids,shapiro.test)
house_log_mod_resid_norm

#check with qq plot
map(house_mod_resids,qqPlot)
map(house_log_mod_resids,shapiro.test)




####################################################################################################
# archive - code to develop chi-square testing
df_house_id_fct %>%
  select(alley, land_contour) %>%
  group_by(across(everything())) %>%
  # group_by(alley, land_contour) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from="land_contour", values_from="n") %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  column_to_rownames(var="alley") %>%
  as.matrix() %>% 
  chisq.test() -> chi_result #%>%
  list() -> chisq_result

names(chisq_result) <- paste("alley", "land_contour", sep="_")
