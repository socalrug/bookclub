library(tidymodels)  
library(mlbench)

# Helper packages
library(readr)       # for importing data
library(vip)         # for variable importance plots

set.seed(200)
tmp <- mlbench.friedman1(200, sd = 1)
bind_cols(as_tibble(tmp$x), tibble(y = tmp$y)) -> simulated

################################################################################
# (a) Fit a random forest model to all the predictors, then estimate the 
#     variable importance scores
################################################################################

rand_forest(trees = 1000) %>% 
  set_engine("randomForest") %>% 
  set_mode("regression") -> rf_mod

rf_mod %>% 
  fit(y ~ ., data = simulated) -> rf_fit

rf_fit %>% 
  vip()

# No, V6-10 were generally not important

################################################################################
# (b) Now add an additional predictor that is highly correlated with one of the
#     informative predictors. Then fit another random forest model to the data.
#     Did the importance score change for V1? What happens when you add another
#     predictor that is also highly correlated to V1?
################################################################################

simulated2 <-  simulated
simulated2$duplicate1 <- simulated$V1 + rnorm(200) * 0.1
simulated2$duplicate2 <- simulated$V1 + rnorm(200) * 0.1

rf_mod %>% 
  fit(y ~ ., data = select(simulated2, -duplicate2)) %>% 
  vip()
# There is a drop in the importance of V1 and duplicate1 is fairly important

rf_mod %>% 
  fit(y ~ ., data = simulated2) %>% 
  vip()
# There is an ever larger drop in imporntance of V1 and duplicate1 and
# duplicate2 are fairly important

################################################################################
# (c) Instead of working on the actual question (c) I wanted to do a hyperparameter
#     tuning exercise for the RandomF Forest
################################################################################

splits <- initial_split(simulated)
simulated_train <- training(splits)
simulated_test <- testing(splits)

val_set <- validation_split(simulated_train, prop = 0.80)

# https://parsnip.tidymodels.org/reference/rand_forest.html
rand_forest(trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") -> rf_mod2

# Preprocess the data. No real need to do this, just playing
recipe(y ~ ., data = simulated_train) %>% 
  step_zv(all_predictors()) %>% 
  step_rm(V10) %>% 
  step_normalize(all_predictors()) -> rf_recipe

# Note: You can use a recipe that has a formula in it or you can
# used add_formula() in the workflow()

workflow() %>% 
  add_model(rf_mod2) %>% 
  add_recipe(rf_recipe) -> rf_workflow

# Tuning parameters - a tibble
rf_tune_grid <- grid_regular(trees(), min_n(), levels = 5)

# We are going to do vfold CV
rf_folds <- vfold_cv(simulated_train, v = 5)

# https://tune.tidymodels.org/reference/tune_grid.html
# https://tune.tidymodels.org/reference/control_grid.html
# https://yardstick.tidymodels.org/reference/metric_set.html
rf_workflow %>% 
  tune_grid(val_set,
            grid = rf_tune_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse),
            resamples = rf_folds) -> rf_res

# Show details the best performing models
rf_res %>% 
  show_best(metric = 'rmse')

# Plot model performance
autoplot(rf_res)

# Select the parameters for the best models
rf_res %>% 
  select_best(metric = 'rmse') -> rf_best

rf_res %>% 
  collect_predictions()

###########################################
# How to do predictions on the test data?
###########################################
#rf_workflow %>% 
#  pull_workflow_fit() -> a
#predict()
#  https://workflows.tidymodels.org/reference/predict-workflow.html
#predict(rf_workflow, simulated_test)
# This does not work because we called tune_grid not fit()

#  Create a workflow that has the best tuning parometers
rf_workflow %>% 
  finalize_workflow(rf_best) -> rf_workflow_final

# Create the final model using all of the data
rf_workflow_final %>% 
  fit(data = simulated) -> rf_mod_final
