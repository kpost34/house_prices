# Helper functions for Ames, IA, Housing Prices Project

# Feature Engineering===============================================================================
# Function to replace diagonal elements of a matrix with NAs
replace_diag_na <- function(mat) {
  diag(mat) <- NA
  
  return(mat)
}



# Function to show sale_price-factor bar plots and frequency plots and tables of factor
explore_rare_cats <- function(data=df_house_icr, predictor) {
  nm_pred <- rlang::as_name(enquo(predictor))
  
  p1 <- data %>%
    select({{predictor}}, sale_price) %>%
    mutate({{predictor}} := fct_reorder({{predictor}}, sale_price, .fun=mean, .desc=TRUE)) %>%
    ggplot(aes(x={{predictor}}, y=sale_price)) +
    stat_summary(geom="col", fun=mean, fill="blue") +
    stat_summary(geom="errorbar", fun.data=mean_se) +
    ggtitle(paste("sale_price versus", 
                  nm_pred,
                  "in sale price order")) +
    theme_bw()
  
  p2 <- data %>%
    select({{predictor}}, sale_price) %>%
    mutate({{predictor}} := fct_infreq({{predictor}})) %>%
    ggplot(aes(x={{predictor}}, y=sale_price)) +
    stat_summary(geom="col", fun=mean, fill="darkred") +
    stat_summary(geom="errorbar", fun.data=mean_se) + 
    ggtitle(paste("sale_price versus", 
                  nm_pred,
                  "in order of descending frequency of",
                  nm_pred)) +
    theme_bw()
  
  p3 <- data %>%
    mutate({{predictor}} := fct_infreq({{predictor}})) %>%
    ggplot(aes(x={{predictor}})) +
    geom_bar(fill="green4") + 
    ggtitle(paste("Frequency of", 
                  nm_pred)) +
    theme_bw()
  
  plot <- plot_grid(p1, p2, p3, nrow=3)
  
  tab <- data %>% 
    select(where(is.factor)) %>% 
    map(tabyl) %>%
    pluck(nm_pred) %>%
    arrange(desc(percent))
  
  result <- list("plots"=plot, 
                 "freq_table"=tab)
  
  return(result)
}



# Function to create batches of qqplots
make_qqplots <- function(preds) {
  df_house_icrc %>%
    select(all_of(preds)) %>%
    pivot_longer(cols=everything(), names_to="variable", values_to="value") %>%
    ggplot(aes(sample=value, color=variable)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~variable, scales="free") +
    theme_bw() +
    theme(legend.position="none")
}



