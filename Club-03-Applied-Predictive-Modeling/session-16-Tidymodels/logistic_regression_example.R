library(tidymodels)  

# Helper packages
library(readr)       # for importing data
library(vip)         # for variable importance plots

read_csv('https://tidymodels.org/start/case-study/hotels.csv') %>%
  mutate_if(is.character, as.factor) -> hotels

splits <- initial_split(hotels, strata = children)
hotel_other <- training(splits)
hotel_test <- testing(splits)

val_set <- validation_split(hotel_other,
                            strata = children,
                            prop = 0.80)

logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") -> lr_mod

holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) -> lr_recipe

workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe) -> lr_workflow

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc)) -> lr_res

lr_res %>% collect_metrics()

lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) -> top_models

lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12) -> lr_best

lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression") -> lr_auc

autoplot(lr_auc)
