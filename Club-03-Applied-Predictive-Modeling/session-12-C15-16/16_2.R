### Problem 16.2

library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)

################################################################################
# (a) Read in the data into R, conduct exploratory analysis, and determine the best method for 
#     encoding the predictors
################################################################################

yes_no_logical <- function(column, true = 'yes', false = 'no', missing = NA) {
  dplyr::if_else(column == 'yes', TRUE, dplyr::if_else(column == 'no', FALSE, NA))
}

read_train <- function(filename) {
  readr::read_csv(filename,
      col_types = cols(
        .default = col_character(),
        job = col_factor(),
        marital = col_factor(),
        education = col_factor(),
        contact = col_factor(),
        month = col_factor(),
        day_of_week = col_factor(),
        previous_outcome = col_factor(),
        age = col_integer(),
        duration = col_integer(),
        campaign = col_integer(),
        days_since_previous = col_integer(),
        previous = col_integer(),
        emp.var.rate = col_double(),
        cons.price.idx = col_double(),
        cons.conf.idx = col_double(),
        euribor3m = col_double(),
        nr.employed = col_double()
      )) %>% 
    dplyr::mutate(housing = yes_no_logical(housing)) %>% 
    dplyr::mutate(loan = yes_no_logical(loan)) %>% 
    dplyr::mutate(default = yes_no_logical(default)) %>% 
    dplyr::mutate(response = yes_no_logical(response)) %>% 
    dplyr::mutate(days_since_previous = dplyr::if_else(days_since_previous == 999, NA_integer_, as.integer(days_since_previous))) %>% 
    dplyr::mutate(previous = dplyr::if_else(previous == 0, NA_integer_, as.integer(previous)))
}

train <- read_train("bank_marketing_training")
# Turns out that these are the same files - arrgh
# test <- read_train("bank_marketing_test") 

print_label <- function(label, length = 80) {
  print(paste(label, paste0(rep('=', length - length(label)), collapse = "")))
}

print_fivenum <- function(label, data) {
  print_label(label)
  metric <- c("minimum", "lower-hinge", "median", "upper-hinge", "maximum", "NA")
  print(paste(metric, c(fivenum(data), sum(is.na(data))), collapse = ' '))
}

print_table <- function(label, data) {
  print_label(label)
  print(table(data))
}

with(train, {
  print_fivenum('age:' , age)
  print_table('job', job)
  print_table('marital', marital)
  print_table('education', education)
  print_table('default', default)           # NZV 
  print_table('housing', housing)            
  print_table('loan', loan)
  print_table('contact', contact)
  print_table('month', month)
  print_table('day_of_week', day_of_week)
  print_fivenum('duration:', duration)
  print_table("campaign", campaign)        # many classes have few observations
  print_fivenum("days_since_previous", days_since_previous)
  print_table("previous", previous)
  print_table("previous_outcome", previous_outcome)
  print_fivenum("emp.var.rate", emp.var.rate)
  print_fivenum("cons.price.idx", cons.price.idx)
  print_fivenum("cons.conf.idx", cons.conf.idx)      
  print_fivenum("euribor3m", euribor3m)
  print_fivenum("nr.employed", nr.employed)
  print_table("response", response)  
  
})


