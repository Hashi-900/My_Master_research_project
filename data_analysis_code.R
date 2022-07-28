dir()
library(tidyverse)
library(readxl)

tidy_df <- read_xlsx('thesis_data.xlsx')

fdi_df <- read_csv('fdi_flow_sub-sahara.csv')

str(fdi_df)

fdi_df %>% 
  view()

str(tidy_df)

average_tidy <- tidy_df %>%
  mutate(oda = log10(oda)) %>%
  replace_na(list(oda=0)) %>% 
  group_by(year) %>% 
  summarise(average_oda=median(oda),
            average_industry = median(`industry value added(%GDP)`),
            average_fdi = median(`FDI infllows`)) 

  ggplot(average_tidy,aes(year, average_oda)) +geom_line() + 
    geom_line(aes(y=average_fdi), color='red') + 
    geom_line(aes(y=average_industry), color='blue')
 
head(average_tidy)

average_tidy %>% 
  pivot_longer(cols = c(average_oda, average_industry, average_fdi),
               names_to = 'indicator', values_to = 'value') %>% 
  ggplot(aes(year, value, col=indicator)) + geom_line() +
  scale_y_continuous(labels=scales::percent_format(scale = 1)) + 
  theme_minimal()

?replace_na

## modeling the lag data

full_tidy_data <- tidy_df %>% 
  mutate(oda_diff = oda-lag(oda),
         fdi_diff = `FDI infllows` - lag(`FDI infllows`),
         indust_diff = `industry value added(%GDP)` - lag(`industry value added(%GDP)`)) 


summary(lm(indust_diff ~ oda_diff + fdi_diff + access_to_electricty, data = full_tidy_data))




fdi_tidy <- fdi_df %>%
  filter(YEAR!= 'ECONOMY') %>% 
  pivot_longer(!YEAR, names_to = 'year', values_to = 'fdi')

fdi_df


#### we need to modify variables selection and methodology.

# so we have to come up new methodology that captures the variation of modeled variables more.

# this need an extensive review of past literature and concluded results.

tidy_df <- tidy_df %>% 
  rename("FDI" = `FDI infllows`, "Manufacturing" = `industry value added(%GDP)`,
         "ODA" = oda)

  tidy_df %>% 
    ggplot(aes(FDI, Manufacturing)) +
  geom_point(col='midnightblue', alpha=0.4) + scale_x_log10() + scale_y_log10() +
  geom_smooth(method = lm, se=FALSE)
  
  
  
  
  tidy_df %>% 
    ggplot(aes(ODA, Manufacturing)) +
    geom_point(col='midnightblue', alpha=0.4) + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = lm, se=FALSE)
  
  
  
  tidy_df %>% 
    ggplot(aes(ODA, FDI)) +
    geom_point(col='midnightblue', alpha=0.4) + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = lm, se=FALSE)

  
  ## Modelling the data using regression
  
  thesis_df <- tidy_df %>% 
    rename("GCFM" = `cross-C-formulation`, 
           "ELECTR" = access_to_electricty,
           "AGRIC" = `Agriculture, forestry, and fishing, value added (% of GDP)`,
           "GDP" = `GDP(constant2010`) %>% 
    mutate(year = make_date(year),
          GDP = log(GDP), 
           openess_trade = (import + export)/GDP) %>% 
    select(Manufacturing, ODA, openess_trade, GCFM, ELECTR, GDP, FDI)

library(tidymodels)  
library(usemodels)
  
## splitting data into training and testing data
  set.seed(234)
  thesis_split <- initial_split(thesis_df, prop = 0.80, strata = Manufacturing)
  
  thesis_train <- training(thesis_split)
  
  thesis_test <- testing(thesis_split)
  
  dim(thesis_train)
  dim(thesis_test)

  thesis_mod <- linear_reg() %>% 
    set_engine('lm')
  
  thesis_mod
  
  thesis_spec <- recipe(
    Manufacturing ~ ., data = thesis_train
  ) %>% 
    step_dummy(country) %>% 
    step_zv(all_predictors())
  
  thesis_spec

  thesis_workflow <- workflow() %>% 
    add_model(thesis_mod) %>% 
    add_recipe(thesis_spec)

  thesis_workflow

  thesis_fit <- thesis_workflow %>% 
    fit(data=thesis_train)

  tidy(thesis_fit)  %>% 
    print(n=42)
  
  glance(thesis_fit$fit)


  ## Random Forest algorithms
  
  thesis_vfold <- vfold_cv(thesis_train, v=10)
  
  use_ranger(
    Manufacturing ~ ., data = thesis_train
  )
  
  
  ranger_recipe <- 
    recipe(formula = Manufacturing ~ ., data = thesis_train) %>% 
    step_string2factor(one_of("country")) 
  
  ranger_spec <- 
    rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
    set_mode("regression") %>% 
    set_engine("ranger") 
  
  ranger_workflow <- 
    workflow() %>% 
    add_recipe(ranger_recipe) %>% 
    add_model(ranger_spec) 
  
  set.seed(16276)
  ranger_tune <-
    tune_grid(ranger_workflow, resamples = thesis_vfold, 
              grid = 5) 
  
  
  
  
  show_best(ranger_tune, metric = 'rmse')
  
  show_best(ranger_tune, metric = 'rsq')

  autoplot(ranger_tune)  

  finalize_model <- ranger_workflow %>% 
    finalize_workflow(select_best(ranger_tune))

  finalize_model 
  
  tidy(finalize_model$fit)

  thesis_last_fit <- last_fit(finalize_model, thesis_split)  

  thesis_last_fit  

  collect_metrics(thesis_last_fit)  

  collect_predictions(thesis_last_fit)  
  