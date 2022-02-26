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
  summarise(average_oda=mean(oda),
            average_industry = mean(`industry value added(%GDP)`),
            average_fdi = mean(`FDI infllows`)) 

  ggplot(average_tidy,aes(year, average_oda)) +geom_line() + 
    geom_line(aes(y=average_fdi), color='red') + 
    geom_line(aes(y=average_industry), color='blue')
 
head(average_tidy)

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



