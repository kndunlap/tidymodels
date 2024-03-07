
# Chapter 4 - The Ames Housing Data ---------------------------------------------------------------

library(modeldata)
data(ames)
data(ames, package = "modeldata")

dim(ames)

# 4.1 - Exploring Features of Homes in Ames -------------------------------

library(tidymodels)
tidymodels_prefer()

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white") +
  scale_x_log10()

ames <- ames |>
  mutate(Sale_Price = log10(Sale_Price))


# Chapter 5 - Spending our Data -------------------------------------------


# 5.1 - Common Methods for Splitting Data ---------------------------------

set.seed(501)
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)


# 5.2 - What about a validation set? --------------------------------------

set.seed(52)
ames_val_split <- initial_validation_split(ames, prop = c(0.6, 0.2))
ames_val_split

ames_train <- training(ames_val_split)
ames_test <- testing(ames_val_split)
ames_val <- validation(ames_val_split)


library(tidymodels)
data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)


# Chapter 6 - Fitting Models with parsnip ---------------------------------


# 6.1 - Create a Model ----------------------------------------------------

lm_model <-
  linear_reg() |>
  set_engine("lm")

lm_form_fit <-
  lm_model |>
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model |>
  fit_xy(
    x = ames_train |> select(Longitude, Latitude),
    y = ames_train |> pull(Sale_Price)
  )

lm_form_fit
lm_xy_fit


# 6.2 - Use the Model Results ---------------------------------------------

lm_form_fit |> extract_fit_engine()


# 6.3 - Make Predictions --------------------------------------------------

ames_test_small <- ames_test |>
  slice(1:5) 
predict(lm_form_fit, new_data = ames_test_small)
  
ames_test_small |>
  select(Sale_Price) |>
  bind_cols(predict(lm_form_fit, ames_test_small)) |>
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))

tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))


library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")


# Chapter 7 - A Model Workflow --------------------------------------------


# 7.2 - Workflow Basics ---------------------------------------------------

library(tidymodels)
tidymodels_prefer()

lm_model <-
  linear_reg() |>
  set_engine("lm")

lm_wflow <- 
  workflow() |>
  add_model(lm_model)

lm_wflow <-
  lm_wflow |>
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test |> slice(1:3))

lm_fit |> update_formula(Sale_Price ~ Longitude)


# 7.3 - Adding Raw Variables to the workflow ------------------------------

lm_wflow <-
  lm_wflow |>
  remove_formula() |>
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_wflow


# 7.6 - Evaluating the Test Set ----------------------------

final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)
collect_metrics(final_lm_res)
collect_predictions(final_lm_res) |>
  slice(1:5)

library(tidymodels)
data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_fit <- fit(lm_wflow, ames_train)

# Chapter 8 - Feature Engineering with Recipes ----------------------------


# 8.1 - A Simple Recipe() for the Ames housing data -----------------------

lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, data = ames)


# 8.2 - Using Recipes -----------------------------------------------------
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
lm_wflow <- lm_wflow |>
  remove_variables() |>
  add_recipe(simple_ames)

lm_wflow
lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test |> slice(1:3))

