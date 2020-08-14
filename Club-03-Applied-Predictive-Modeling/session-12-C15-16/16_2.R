### Problem 16.2

library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
library(gridExtra)
library(magrittr)

################################################################################
# (a) Read in the data into R, conduct exploratory analysis, and determine the best method for 
#     encoding the predictors
################################################################################

yes_no_logical <- function(column, true = 'yes', false = 'no', missing = NA) {
  dplyr::if_else(column == 'yes', TRUE, dplyr::if_else(column == 'no', FALSE, NA))
}

read_marketing <- function(filename) {
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
      )) #%>% 
    #dplyr::mutate(housing = yes_no_logical(housing)) %>% 
    #dplyr::mutate(loan = yes_no_logical(loan)) %>% 
    #dplyr::mutate(default = yes_no_logical(default)) %>% 
    #dplyr::mutate(response = yes_no_logical(response)) %>% 
    #dplyr::mutate(days_since_previous = dplyr::if_else(days_since_previous == 999, NA_integer_, as.integer(days_since_previous))) %>% 
    #dplyr::mutate(previous = dplyr::if_else(previous == 0, NA_integer_, as.integer(previous)))
}

marketing <- read_marketing("bank_marketing_training")
# Turns out that these are the same files - arrgh
# test <- read_marketing("bank_marketing_test") 

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

with(marketing, {
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

# campaign has a lot of classes. Create a new feature that combines all the campaigns above
# 10 into a single class.

# Look at the fivenum variables using a histogram. A number of them appear skewed

grid.arrange(
  ggplot(marketing, aes(x = age)) + geom_histogram(),
  ggplot(marketing, aes(x = log(age))) + geom_histogram(),
  ggplot(marketing, aes(x = duration)) + geom_histogram(),
  ggplot(marketing, aes(x = log(duration))) + geom_histogram(),
  ggplot(marketing, aes(x = days_since_previous)) + geom_histogram(),
  ggplot(marketing, aes(x = log(days_since_previous))) + geom_histogram(),
  ggplot(marketing, aes(x = emp.var.rate)) + geom_histogram(),
  ggplot(marketing, aes(x = log(emp.var.rate))) + geom_histogram(),
  ggplot(marketing, aes(x = cons.price.idx)) + geom_histogram(),
  ggplot(marketing, aes(x = log(cons.price.idx))) + geom_histogram(),
  ggplot(marketing, aes(x = euribor3m)) + geom_histogram(),
  ggplot(marketing, aes(x = log(euribor3m))) + geom_histogram(),
  ggplot(marketing, aes(x = nr.employed)) + geom_histogram(),
  ggplot(marketing, aes(x = log(nr.employed))) + geom_histogram(),
  ncol = 4)

# apply a log transform on age and duration  

marketing %<>%
  dplyr::mutate(age_log = log(age),
                duration_log = log(duration),
                campaign_group = ifelse(campaign <= 10, campaign, 11))


################################################################################
# (b) Determine the appropriate split for the data and build several 
#     classification models
################################################################################

index <- caret::createDataPartition(marketing$response, p = 0.7, list = FALSE)
training <- marketing[index, ]
testing <- marketing[-index, ]

# Large class imbalance
table(training$response)

training_complete_cases <- training[complete.cases(training), ]
# Large class imbalance
table(training_complete_cases$response)


# Erroring
training_smote <- DMwR::SMOTE(response ~ ., dplyr::select(training, -duration_log), 
                              perc.over = 500, k = 5, perc.under = 100)

ctrl <- caret::trainControl(method = "cv",
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)

# this fails because of NA but there are no NAs
  

# rf_model <- caret::train(response ~ age + job + marital + education + default + housing +
#                            loan + contact + month + day_of_week + duration + campaign +
#                            days_since_previous + previous + previous_outcome +
#                            emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m +
#                            nr.employed + age_log +  campaign_group, #duration_log, 
rf_model <- caret::train(response ~ ., 
                         data = dplyr::select(training, -duration_log),
                         method = 'rf',
                         trControl = ctrl,
                         ntree = 1500,
                         tuneLength = 5,
                         metric = 'Sens')

fda_model <- caret::train(response ~ ., 
                          data = dplyr::select(training, -duration_log),
                          method = 'fda',
                          tuneGrid = data.frame(.degree = 1, .nprune = 1:25),
                          metric = 'Sens',
                          trControl = ctrl)


# Error in { : task 1 failed - "'from' must be a finite number"
# In addition: There were 40 warnings (use warnings() to see them)