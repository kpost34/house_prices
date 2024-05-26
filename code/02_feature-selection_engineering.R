# Load Packages, Source Functions & Read in Imputed Data============================================
#load packages
library(pacman) 
pacman::p_load(here, tidyverse, cowplot, rstatix)
               # janitor, skimr, visdat, naniar, finalfit, mice, rstatix, , 
               # cowplot, rstatix)
               
               # GGally, rsample, tidymodels)

#source functions
source(here("code", "00_helper_fns.R"))

#read in data
in_fp <- here("data", "tidy_data", "train_imputed.rds")
df_house_i <- readRDS(in_fp)



# Feature Selection: Drop Low 'Variance' (frequency) Features=======================================
## Find candidates (>= 98% of data are in one group)
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


## Assess candidates
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



# Feature Selection: Remove Highly Correlated Features==============================================

## Numeric-numeric--------------------
df_house_ic %>%
  select(!sale_price) %>%
  select(where(is.numeric)) %>%
  cor() %>%
  replace_diag_na() %>% 
  as.data.frame() %>%
  mutate(across(everything(), ~round(.x, 3))) %>%
  filter(if_any(everything(), ~abs(.) >= 0.9)) #retain rows where 1+ value has > 90% correlation
#empty DF; no features dropped for high correlation


## Ordered factor-ordered factor--------------------
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
  

## Factor-factor--------------------
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


## Remove features--------------------
df_house_ic %>%
  select(-c(overall_cond, ms_sub_class, exterior2nd)) -> df_house_icr
#i = imputed data, c=dropped vars due to constancy, r=dropped vars due to redundancy/multicollinearity



# Feature Engineering: Rare-label Encoding==========================================================
## Factors
### Identify which have rare categories
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


### Look at examples
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


## Ordered factors
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

### Look at examples
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
    

  
# Feature Engineering: Discretization===============================================================
#[Note: revisit later if model performs poorly]
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


## Determine predictors poorly correlated with target variable
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


## Visualize data
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



# Feature Engineering: Feature Scaling==============================================================
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


## Check for normal distribution
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


## Normalization
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



# Write Clean Training Data to File=================================================================
#clean training data = imputed data with dropped variables (constancy, redundancy), collapsed 
  #factors, and normalized numerical predictors to file

#output fp
out_fp <- here("data", "tidy_data", "train_tidy.rds")

saveRDS(df_house_icrcn, out_fp)







