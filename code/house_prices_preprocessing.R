# Load Packages & Source Functions==================================================================
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, naniar, finalfit, mice, rstatix, GGally, 
               cowplot, rstatix, rsample, tidymodels)

source(here("code", "00_helper_fns.R"))
               





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
