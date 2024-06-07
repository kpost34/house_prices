#05_predict-tv_and_evaluate-model.R

#This script 1) predicts sale_price on test data and 2) writes predicted sale price with house id
  #to file


# Load Packages, Data, and Model====================================================================
#load packages
library(pacman) 
pacman::p_load(here, parsnip)

#data
test_fp <- here("data", "tidy_data", "test_tidy.rds")
df_test_final <- readRDS(test_fp)

#model
model_fp <- here("modelling", "final_model.rds")
final_fit <- readRDS(model_fp)



# Predict sale_price================================================================================
df_pred_sale_price <- parsnip::predict.model_fit(object=final_fit, 
                           new_data=df_test_final,
                           type="numeric") %>%
  bind_cols(df_test_final["id"], .) %>%
  rename(Id="id", SalePrice=".pred")



# Write predicted sale_price to file================================================================
predict_fp <- here("data", "tidy_data", "predicted_sale_prices.csv")

write_csv(df_pred_sale_price, predict_fp)





