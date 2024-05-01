#load packages
library(here)
library(readr)
library(dplyr)
library(janitor)
library(visdat)
library(purrr)
library(ggplot2)
library(forcats)
library(recipes)
library(car)
library(stringr)



#I. Data import
#**************
house<-read_csv(here("data","raw_data","train.csv"),
                  col_types="iifnnffffffffffffiiiifffffnfffffffnfnnnffffnnnniiiiiifififfifinfffnnnnnnfffniiffn")


#II. Data cleaning, wrangling, and preprocessing
#***********************************************
#1. Preliminary data checking
dim(house) #1460 x 81 (id + 79 explanatory + 1 dv)
glimpse(house) #the col classes match the data; however, some cols need to be re-classified or perhaps mutated 
head(house,n=10); tail(house,n=10) #noticed some NAs (particularly in alley column)


#2. Data cleaning
house<-clean_names(house) #makes names lowercase; adds _ b/t words
#rearranging cols, re-leveling factors would be here; but there are so many cols, so will save for later


#3. Missingness decision making
#Assess missing data
vis_miss(house) #5.9% missing; a few cols with heavy missingness and many appear complete
house_NAs<-map_int(house,function(x) sum(is.na(x))) #check each col using purrr
sort(house_NAs,decreasing=TRUE)
sum(house_NAs>0) #19 cols have at least 1 NA and of those, 6 have at least 100 NAs (62 do not)
  #lot_frontage (259), fireplace_qu (690), fence (1179), alley (1369), misc_feature (1406), and pool_qc (1453)


#dependent variable info
range(house$sale_price) #34,900-755,000
summary(house$sale_price)
hist(house$sale_price)

#high NA variable info
#lot_frontage (259)
#visualize relationship
ggplot(house,aes(lot_frontage,sale_price)) + 
  geom_point() +
  geom_smooth(method=lm)
#appears to be positive linear but greater error at higher values

#remove high lot_frontage values
house %>% 
  filter(lot_frontage<=200) %>%
  ggplot(aes(lot_frontage,sale_price)) +
  geom_point() +
  geom_smooth(method=lm)
#reduces error

#determine correlation
lot_frontage_vals<-which(!is.na(house$lot_frontage)) #index of non-na lot_frontage values
cor(house$lot_frontage[lot_frontage_vals],house$sale_price[lot_frontage_vals]) #rho = 0.351
0.351^2 #r^2 = 12.3%; worth keeping (especially with 259 NAs and intuitive reasons)


#fireplace_qu (690)
tabyl(house$fireplace_qu) #47.3% NA
levels(house$fireplace_qu) #NA is a level, but R does not recognize that
house$fireplace_qu<-fct_expand(house$fireplace_qu,"NFp") #add an NFp (no fireplace) level to the factor
house$fireplace_qu[which(is.na(house$fireplace_qu))]<-"NFp"

fireplace_num<-tabyl(house$fireplace_qu)$n #numbers of each fac level
tapply(house$sale_price,house$fireplace_qu,mean) 
#337 K Ex, 226 K Gd, 206 K Ta, 167 K Fa, 130 K Po, 141 K NFp
#not a problem given distribution of factor levels


#fence (1179)
tabyl(house$fence) #80.8% NA
levels(house$fence) #NA is a level, but R does not recognize that
house$fence<-fct_expand(house$fence,"NF") #add an NF (no fence) level to the factor
house$fence[which(is.na(house$fence))]<-"NF"

fence_num<-tabyl(house$fence)$n #numbers of each fac level
tapply(house$sale_price,house$fence,mean) #188 K NF, 179 K GdPrv, 149 K MnPrv, 140 K GdWo, 134 K MnWw
tapply(house$sale_price,house$fence,sd)/fence_num #SEs
#shows some differentiation in sale_price by fence quality


#alley (1369)
tabyl(house$alley) #93.7% NA
levels(house$alley) #NA is a level, but R does not recognize that
house$alley<-fct_expand(house$alley,"NAl") #add an NAl (no alley) level to the factor
house$alley[which(is.na(house$alley))]<-"NAl"
tapply(house$sale_price,house$alley,mean) #122 K Grvl, 168 K Pave, 183 K NAl
#high frequency of one level--problem?


#misc_feature (1406)
tabyl(house$misc_feature) #96% NA
levels(house$misc_feature) #NA is a level, but R does not recognize that
house$misc_feature<-fct_expand(house$misc_feature,"NMF") #add an NMF (no misc_feature) level to the factor
house$misc_feature[which(is.na(house$misc_feature))]<-"NMF"
tapply(house$sale_price,house$misc_feature,mean) #No Elev, Gar2 170 K, Othr 94 K, Shed 151 K, TenC 250 K, 182 K NMF
#problem is that there is constancy in this variable and select factor levels lack representation;
#thus, it makes it difficult for cross-validation and predictive power for test data would lack precision
#e.g., if test data contain multiple homes with tennis courts or elevators


#pool_qc (1453)
tabyl(house$pool_qc) #99.5% NA (NP)
levels(house$pool_qc) #NA is a level, but R codes it as missing data
house$pool_qc<-fct_expand(house$pool_qc,"NP") #add an NP (no pool) level to the factor
house$pool_qc[which(is.na(house$pool_qc))]<-"NP" #now to the data
tapply(house$sale_price,house$pool_qc,mean) 
#Ex 490K, Gd 202 K, Fa 215 K, NP 180 K; no Ta category; problem--
#lack of variability? Fa > Gd; regression problem and not a classification problem

#investigate all vars for miscoded NAs
house_NAs<-map_int(house,function(x) sum(is.na(x))) #check each col using purrr
sort(house_NAs,decreasing=TRUE)
#electrical, mas_vnr_type, mas_vnr_area, bsmt_qual, bsmt_cond, bsmt_fin_type1, bsmt_exposure,
#bsmt_fin_type2, garage_type, garage_yr_blt, garage_finish, garage_qual, garage_cond

tabyl(house$electrical) #real NA (1)
tabyl(house$mas_vnr_type) #real NAs (8)
sum(is.na(house$mas_vnr_area)) #real NAs (8)

tabyl(house$bsmt_qual) #not a real NA
levels(house$bsmt_qual)
house$bsmt_qual<-fct_expand(house$bsmt_qual,"NB") #add an NB (no basement) level to the factor
house$bsmt_qual[which(is.na(house$bsmt_qual))]<-"NB" #now to the data

tabyl(house$bsmt_cond) #not a real NA
levels(house$bsmt_cond)
house$bsmt_cond<-fct_expand(house$bsmt_cond,"NB") #add an NB (no basement) level to the factor
house$bsmt_cond[which(is.na(house$bsmt_cond))]<-"NB" #now to the data

tabyl(house$bsmt_fin_type1) #not a real NA
levels(house$bsmt_fin_type1)
house$bsmt_fin_type1<-fct_expand(house$bsmt_fin_type1,"NB") #add an NB (no basement) level to the factor
house$bsmt_fin_type1[which(is.na(house$bsmt_fin_type1))]<-"NB" #now to the data

tabyl(house$bsmt_exposure) #not a real NA
levels(house$bsmt_exposure)
house$bsmt_exposure<-fct_expand(house$bsmt_exposure,"NB") #add an NB (no basement) level to the factor
house$bsmt_exposure[which(is.na(house$bsmt_exposure))]<-"NB" #now to the data

tabyl(house$bsmt_fin_type2) #not a real NA
levels(house$bsmt_fin_type2)
house$bsmt_fin_type2<-fct_expand(house$bsmt_fin_type2,"NB") #add an NB (no basement) level to the factor
house$bsmt_fin_type2[which(is.na(house$bsmt_fin_type2))]<-"NB" #now to the data

tabyl(house$garage_type) #not a real NA
levels(house$garage_type)
house$garage_type<-fct_expand(house$garage_type,"NG")
house$garage_type[which(is.na(house$garage_type))]<-"NG"

tabyl(house$garage_yr_blt)
sum(is.na(house$garage_yr_blt)) #real NAs (81)

tabyl(house$garage_finish) #not a real NA
levels(house$garage_finish)
house$garage_finish<-fct_expand(house$garage_finish,"NG")
house$garage_finish[which(is.na(house$garage_finish))]<-"NG"

tabyl(house$garage_qual) #not a real NA
levels(house$garage_qual)
house$garage_qual<-fct_expand(house$garage_qual,"NG")
house$garage_qual[which(is.na(house$garage_qual))]<-"NG"

tabyl(house$garage_cond) #not a real NA
levels(house$garage_cond)
house$garage_cond<-fct_expand(house$garage_cond,"NG")
house$garage_cond[which(is.na(house$garage_cond))]<-"NG"

#assess missingness after correcting NAs
vis_miss(house) #0.3%
house_NAs<-map_int(house,function(x) sum(is.na(x))) #check each col using purrr
sort(house_NAs) #only 5 cols (down from 19)


#Impute missing data
house_imputed_recipe<- #create imputation recipe
  recipe(house) %>%
  update_role(sale_price,new_role="outcome") %>%
  update_role(id,new_role="id") %>%
  update_role(ms_sub_class:sale_condition,new_role="predictor") %>%
  step_knnimpute(electrical,mas_vnr_type,mas_vnr_area,garage_yr_blt,lot_frontage)
i_house<-prep(house_imputed_recipe) %>% bake(new_data=house) #impute data
sum(is.na(house)); sum(is.na(i_house)) #compare NAs--now 0 NAs


#4. Assess constancy
#already covered: fireplace_qu, fence, alley, misc_feature, and pool_qc

#factor variables
i_house %>% select(where(is.factor)) %>% summary() #assess constancy of factor variables
tabyl(i_house$street) #99.6% street
tabyl(i_house$utilities) #99.9 % AllPub, 1 NoSeWa, and 0 NoSewr and 0 ELO
tabyl(i_house$condition2) #99% Norm
tabyl(i_house$roof_matl) #98.2% CompShg (+ four other levels have only 1 obs)
#these will be dropped (as well as pool_qc) due to near constancy

#integer variables
i_house %>% select(where(is.integer)) %>% map_dbl(sd,na.rm=TRUE) %>% sort() #shows sds of integer vars
#select 5 vars with lowest variability to assess constancy

tabyl(i_house$kitchen_abv_gr) #95.3% 1
tabyl(i_house$bsmt_half_bath) #94.4% 0
#although variance is low for these variables, they are not near-constant

#continuous variables
i_house %>% select(where(is.double)) %>% map_dbl(sd,na.rm=TRUE) %>% sort() #shows sds of double vars
plot(i_house$x3ssn_porch) #one example, which indicates variability


#5. Drop unimportant columns (i.e., excessive NAs, constancy)
#drop: street, utilities, condition2, roof_matl, pool_qc
c_house<-i_house %>% 
  select(-c(street,utilities,condition2,roof_matl,pool_qc)) #remove cols with excessive constancy
dim(i_house); dim(c_house) #check that removed
  

#(Ordinal encoding--keep in mind if you want to rerun model)

#7. Feature engineering
c_house %>% select(where(is.factor)) %>% map(tabyl)
#returns distribution of factor levels per factor

#lot_config
c_house$lot_config<-fct_collapse(c_house$lot_config,FR=c("FR2","FR3"))
tabyl(c_house$lot_config)

#neighborhood
sort(tapply(c_house$sale_price,c_house$neighborhood,mean))
tabyl(c_house$neighborhood)
c_house$neighborhood<-fct_collapse(c_house$neighborhood,other=c("Blueste","NPkVill"))
#combine the least two frequent neighborhoods

#condition1
sort(tapply(c_house$sale_price,c_house$condition1,mean))
tabyl(c_house$condition1)
c_house$condition1<-fct_collapse(c_house$condition1,RRnear=c("RRNe","RRNn"))
#combine the least two frequenct condition1 levels (which are both 200' from RR)

#roof_style
sort(tapply(c_house$sale_price,c_house$roof_style,mean))
tabyl(c_house$roof_style)
c_house$roof_style<-fct_collapse(c_house$roof_style,other=c("Shed","Mansard"))


#exterior1st
sort(tapply(c_house$sale_price,c_house$exterior1st,mean))
tabyl(c_house$exterior1st)
c_house$exterior1st<-fct_collapse(c_house$exterior1st,other=c("BrkComm","AsphShn","Stone","ImStucc","CBlock"))
#here this creates the level 'other' but know that other is a possible level, so it will just be combined with it

#exterior2nd
sort(tapply(c_house$sale_price,c_house$exterior2nd,mean))
tabyl(c_house$exterior2nd)
c_house$exterior2nd<-fct_collapse(c_house$exterior2nd,other=c("Other","Brk Cmn","AsphShn","Stone","ImStucc","CBlock"))

#exter_cond
sort(tapply(c_house$sale_price,c_house$exter_cond,mean))
tabyl(c_house$exter_cond)
c_house$exter_cond<-fct_collapse(c_house$exter_cond,Gd_Ex=c("Gd","Ex"),Po_Fa=c("Po","Fa"))
#combined good and excellent as well as poor and fair

#foundation
sort(tapply(c_house$sale_price,c_house$foundation,mean))
tabyl(c_house$foundation)
c_house$foundation<-fct_collapse(c_house$foundation,other=c("Wood","Stone"))

#bsmt_cond
sort(tapply(c_house$sale_price,c_house$bsmt_cond,mean))
tabyl(c_house$bsmt_cond)
c_house$bsmt_cond<-fct_collapse(c_house$bsmt_cond,Po_Fa=c("Po","Fa"))

#heating
sort(tapply(c_house$sale_price,c_house$heating,mean))
tabyl(c_house$heating)
c_house$heating<-fct_collapse(c_house$heating,Grav_Wall_Floor=c("Grav","Wall","Floor"),Water=c("GasW","OthW"))

#heating_qc
sort(tapply(c_house$sale_price,c_house$heating_qc,mean))
tabyl(c_house$heating_qc)
c_house$heating_qc<-fct_collapse(c_house$heating_qc,Po_Fa=c("Po","Fa"))


#electrical
sort(tapply(c_house$sale_price,c_house$electrical,mean))
tabyl(c_house$electrical)
c_house$electrical<-fct_collapse(c_house$electrical,other=c("FuseF","FuseP","Mix"))


#functional
sort(tapply(c_house$sale_price,c_house$functional,mean))
tabyl(c_house$functional)
c_house$functional<-fct_collapse(c_house$functional,Sev_Maj2=c("Sev","Maj2"))


#garage_qual
sort(tapply(c_house$sale_price,c_house$garage_qual,mean))
tabyl(c_house$garage_qual)
c_house$garage_qual<-fct_collapse(c_house$garage_qual,Po_Fa=c("Po","Fa"),Gd_Ex=c("Gd","Ex"))


#garage_cond
sort(tapply(c_house$sale_price,c_house$garage_cond,mean))
tabyl(c_house$garage_cond)
c_house$garage_cond<-fct_collapse(c_house$garage_cond,Po_Fa=c("Po","Fa"),Gd_Ex=c("Gd","Ex"))


#misc_feature
sort(tapply(c_house$sale_price,c_house$misc_feature,mean))
tabyl(c_house$misc_feature)
c_house$misc_feature<-fct_collapse(c_house$misc_feature,other=c("Gar2","Othr","TenC"))

#sale_type
sort(tapply(c_house$sale_price,c_house$sale_type,mean))
tabyl(c_house$sale_type)
c_house$sale_type<-fct_collapse(c_house$sale_type,other=c("COD","ConLD","ConLI","CWD","ConLw","Con","Oth"))


#III. Data export
#****************
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
