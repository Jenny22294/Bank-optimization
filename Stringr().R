

library(stringr)
do.call("bind_rows", my_card) %>% 
  slice(-1) %>% 
  select(-breaks, -is_special_values, -count, -count_distr, -good, -bad, -badprob) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  mutate(bin = bin %>% 
           str_replace_all("\\[", "From ") %>% # Replace [-Inf,6000) -> from... to...
           str_replace_all("\\,", " to ") %>% 
           str_replace_all("\\)", "")) -> iv_for_predictors_point