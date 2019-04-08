

############# Data Wrangling ########

# Data Wrangling is a very important part 
# (probably no less than 50% of the time of an analysis project is allocated to this section). 
# In this section we will use the tidyverse ecosystem to implement this task.

# Practical data is about ebola. Description of the data (and downloadable) here.
# https://data.world/brianray/ebola-cases




# Load package tidyverse: 
library(tidyverse)

# Ð???c d??? li???u: 
ebola <- read.csv("C:/Users/mlcl.local/Downloads/ebola_data_db_format.csv", header = TRUE, sep = ',')


## filter()

### This command is used to filter data by line.
### For example, we want to extract lines that are "Cumulative number of confirmed Ebola deaths" in the column Indicator:

df1 <- ebola %>% 
  filter(Indicator == "Cumulative number of confirmed Ebola deaths")


# group_by()
# Suppose we want to calculate the total number of people who have been confirmed dead by Ebola by country.
# This time, the group_by () command will be extremely useful. 
# But note that the command group_by () only becomes useful if it comes with other commands, 
# like in this case, sum () - sum:
  
  df1 %>% 
  group_by(Country) %>% 
  summarise(total = sum(value)) ->> ebola_deaths
  
  
# Or want to see the first 10 of total number of deaths over time:
  df1 %>% 
    group_by(Date) %>% 
    summarise(total = sum(value)) %>% 
    head(10)

  
## arrange()
# This command is used to rearrange the rows of data by the value of,
# for example, the total variable column:
  
  
  # Sort by descending:
  ebola_deaths %>% 
    arrange(-total)  
  
  
  ebola_deaths %>% 
    arrange(total)
  
  
  ### Dealing with missing data ##################################
  

  # Load some packages for data manipulation: 
  library(tidyverse)
  library(magrittr)
  
  # Clear workspace: 
  rm(list = ls())
  
  # Import data: 
  hmeq <- read.csv("http://www.creditriskanalytics.net/uploads/1/9/5/1/19511601/hmeq.csv")
  
  # Function for detecting NA observations: 
  na_rate <- function(x) {x %>% is.na() %>% sum() / length(x)}
  sapply(hmeq, na_rate) %>% round(2)
  
  # Function replaces NA by mean: 
  replace_by_mean <- function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  }
  
  # A function imputes NA observations for categorical variables: 
  
  replace_na_categorical <- function(x) {
    x %>% 
      table() %>% 
      as.data.frame() %>% 
      arrange(-Freq) ->> my_df
    
    n_obs <- sum(my_df$Freq)
    pop <- my_df$. %>% as.character()
    set.seed(29)
    x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)
    return(x)
  }
  
  # Use the two functions: 
  df <- hmeq %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(REASON = case_when(REASON == "" ~ NA_character_, TRUE ~ REASON), 
           JOB = case_when(JOB == "" ~ NA_character_, TRUE ~ JOB)) %>%
    mutate_if(is_character, as.factor) %>% 
    mutate_if(is.numeric, replace_by_mean) %>% 
    mutate_if(is.factor, replace_na_categorical)
  
  
  
  
 