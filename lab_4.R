install.packages("dslabs")
library(tidyverse)
library(dslabs) 
library(dplyr)

# After you have loaded the dslabs package, you can access the data stored in `gapminder`. Let's look at the top 5 lines 
gapminder %>% as_tibble() %>% 
  head(5)

gapminder %>% as_tibble() %>% 
  View()

gapminder %>% as_tibble() %>% 
  dim()
## Which five countries had the largest population size in 2015
top_5_pop_country <- gapminder %>% as_tibble() %>% 
  filter(year == 2015)%>%
  arrange(-population)%>%
  select(country)%>%
  head(5)%>% 
  pull()## why need pull()?
top_5_pop_country

## how has the population sizes in those countries changes since 1960?
gapminder %>% 
  filter(country %in% top_5_pop_country)%>%
  ggplot() +
  geom_line(mapping = aes(x = year, y = population, color = country))

## Question 2. Rank the following countries in infant mortality rate in 2015.
gapminder %>%
  filter(year==2015, country %in% c("Turkey", "Poland", "South Korea", "Russia", "Vietnam", "South Africa")) %>%
  arrange(infant_mortality) %>%
  select(country, infant_mortality) %>% 
  knitr::kable()





