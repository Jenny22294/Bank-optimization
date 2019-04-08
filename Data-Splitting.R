
# Split our data: 

df_train <- df %>% 
  group_by(BAD) %>% 
  sample_frac(0.5) %>% 
  ungroup() # Use 50% data set for training model. 



# Use 50% data set for validation. 

df_test <- dplyr::setdiff(df, df_train)