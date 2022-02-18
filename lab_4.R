install.packages("dslabs")
library(tidyverse)
library(dslabs) 

# After you have loaded the dslabs package, you can access the data stored in `gapminder`. Let's look at the top 5 lines 
gapminder %>% as_tibble() %>% 
  head(5)
