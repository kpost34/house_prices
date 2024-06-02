# Objects for Ames, IA, Housing Prices Project

# Object For Reading in Data========================================================================
col_types_train <- "cccnncccccccccccccciicccccncccccccncnnnccccnnnniiiiiiciciccicincccnnnnnncccniiccn"
col_types_test <- str_remove(col_types_train, "n$")



# Objects For Initial Data Cleaning and Wrangling===================================================

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



# Objects for Feature Engineering===================================================================
## Read in data for objects
fp_clean_train <- here("data", "tidy_data", "train_tidy.rds")
df_clean_train <- readRDS(fp_clean_train)

## Create objects
num_preds <- df_clean_train %>%
  select(where(is.numeric)) %>%
  select(!sale_price) %>%
  names() %>%
  sort() 

length(num_preds)

num_preds1 <- num_preds[1:12]
num_preds2 <- num_preds[13:24]
num_preds3 <- num_preds[25:33]

rm(fp_clean_train, df_clean_train)


