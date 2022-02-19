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
  filter(year==2015)%>%
  filter(country %in% c("Turkey", "Poland", "South Korea", "Russia", "Vietnam", "South Africa")) %>%
  arrange(infant_mortality) %>%
#   select(country, infant_mortality) %>% 
  knitr::kable()

## Question 3. What is the general relationship between per-capita GDP and fertility rate?
## B. Negetive relationship
view(gapminder)
gapminder %>%
  filter(year==2010|year==2000) %>%
  ggplot(aes(y=fertility, x=gdp/population)) +
  geom_point() +
  geom_smooth(se=TRUE, method = "lm")

## Question 4. If you break down the relationship between per-capita GDP and fertility rate by continent, which continent (or regions) stands out as an outlier? (Bonus question: why might this be?)
## C. Europe
gapminder %>%
  filter(year==2000|year==2010) %>%
  ggplot(aes(y=fertility, x=gdp/population, color=continent)) +
  geom_point() +
  geom_smooth(se=F, method = "lm") +
  facet_wrap(~continent, scales = "free_y")
##Europe is different,The fertility rate increases as gdp increases


eu_2000 <- gapminder %>%
  filter(year==2000, continent == "Europe") 
eu_2000 %>%
  filter(fertility > 1.5, gdp/population > 20000) %>%
  ggplot(aes(y=fertility, x=gdp/population, color=region)) +
  ggrepel::geom_label_repel(aes(label=country)) +
  geom_point(data=eu_2000)

## Question 5. There are roughly seven billion people in the world today. Which map shows where people live? 
## (Each figure represents 1 billion people.)

## answer A
gapminder %>%
  filter(year==2015) %>%
  group_by(continent) %>%
  summarize(population_in_billion=sum(population)/10^9) %>% ##convert population to billion so divide by 1000000000
  ggplot(aes(x=continent, y=population_in_billion)) +
  geom_col()


## Question 6. What is the overall life expectancy for the world population (i.e. global average)?
## C. 70 years
gapminder %>%
  filter(year==2015) %>%
  summarize(life_expectancy=sum(life_expectancy*population)/sum(population))

## Question 7. What is the gap in life expectancy between Europe and Africa?
## B. 15 years
gapminder %>%
  filter(year==2015) %>%
  group_by(continent) %>%
  summarize(life_expectancy=sum(life_expectancy*population)/sum(population))%>%
  ggplot(aes(x=continent, y=life_expectancy)) +
  geom_col()

gapminder %>%
  filter(year==2015) %>%
  ggplot(aes(x=continent, y=life_expectancy)) +
  geom_jitter(aes(color=continent),height = 0) +
  geom_boxplot(alpha=0, outlier.alpha = 0)

##Exercise 2: Use data transformation and visualization to explore the following open-ended question in breakout rooms
years <- c(1960, 1970, 1980, 1990, 2000, 2010)
continents <- c("Europe", "Asia")
gapminder %>% 
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(log(gdp/population), life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year) 

gapminder %>% 
  filter(continent %in% continents) %>%
  ggplot(aes(x=year, y=life_expectancy, group=country)) +
  geom_line()+
  facet_wrap(~continent) 

gapminder %>% 
  filter(year %in% c(1960, 2010)) %>%
  ggplot(aes(x=life_expectancy, fill=continent)) +
  geom_density(alpha=0.5)+
  facet_wrap(~year, nrow=2) 

gapminder %>% 
  filter(year %in% c(1960, 2010)) %>%
  ggplot(aes(x=log(gdp/population), fill=continent)) +
  geom_density(alpha=0.5)+
  facet_wrap(~year, nrow=2) 


gapminder %>% 
  filter(year %in% c(1960, 2010)) %>%
  ggplot(aes(continent, log(gdp/population), fill = as.character(year))) +
  geom_boxplot()
