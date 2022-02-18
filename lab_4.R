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

## Question 3. What is the general relationship between per-capita GDP and fertility rate?
gapminder %>%
  filter(year==2000) %>%
  ggplot(aes(y=fertility, x=gdp/population)) +
  geom_point() +
  geom_smooth(se=F, method = "lm")

## Question 4. If you break down the relationship between per-capita GDP and fertility rate by continent, which continent (or regions) stands out as an outlier? (Bonus question: why might this be?)
gapminder %>%
  filter(year==2000) %>%
  ggplot(aes(y=fertility, x=gdp/population, color=continent)) +
  geom_point() +
  geom_smooth(se=F, method = "lm") +
  facet_wrap(~continent, scales = "free_y")

eu_2000 <- gapminder %>%
  filter(year==2000, continent == "Europe") 
eu_2000 %>%
  filter(fertility > 1.5, gdp/population > 20000) %>%
  ggplot(aes(y=fertility, x=gdp/population, color=region)) +
  ggrepel::geom_label_repel(aes(label=country)) +
  geom_point(data=eu_2000)
