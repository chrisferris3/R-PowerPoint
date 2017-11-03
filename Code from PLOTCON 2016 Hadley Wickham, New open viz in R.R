##########################################
# libraries #
##########################################

library(gapminder)    # data package
library(tidyverse)
library(broom)

# created the nested data frame from the source

g <- gapminder 

by_country <- gapminder %>%
  mutate(year1950 = year - 1950 ) %>%  # dataset starts at 1950
  group_by(continent, country) %>%     
  nest()                  # all columns is placed in a dataframe 
                          # which is placed into a column "data"


# create the model function ############
country_model <- function(df){
  lm(lifeExp ~ year1950, data = df)
}


# create the model data frame
models <- by_country %>% 
  mutate(model = map(data, country_model)) %>%
  
# model results are summarised in tidy dataframes using broom ###
  mutate(glance = map(model, broom::glance),
         rsq    = glance %>% map_dbl("r.squared"),
         tidy   = map(model, broom::tidy),
         augment= map(model, broom::augment))

models


#### plots ####
ggplot(models, aes(x = rsq, y= reorder(country, rsq))) +
  geom_point(aes(color = continent)) +
  labs(y = "country") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y= element_blank())

#### you can get the data out of each of the result dataframes using the unnest() function.

models %>% unnest(glance)


######
models %>% 
  unnest(tidy) %>% 
  select(continent, country, term, estimate, rsq) %>% 
  spread(term, estimate) %>% 
  ggplot(aes(`(Intercept)`, year1950)) +  # Life Expectancy in 1950 vs. slope ("yearlt increase")
  geom_point(aes(color= continent, size = rsq))+
  geom_smooth(se= FALSE) + 
  xlab("Life expectancy (1950)") + 
  ylab("Yearly Improvement") +
  scale_size_area()

###### Residual plot of residuals from all 100+ models

models %>% 
  unnest(augment) %>% 
  ggplot(aes(year1950, .resid)) +
  geom_line(aes(group= country), alpha = 1/3) +
  geom_hline(yintercept = 0, color= "white", size= 2) +
  geom_smooth(se= FALSE) +
  facet_wrap(~ continent)
