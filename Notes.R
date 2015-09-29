###### 29 September 2015 ########

# 1. Calclate the mean gdpPercap for each country.

gapminder %>%
  group_by(country) %>%
  summarize(mean_gdpPer = mean(gdpPercap))



# 2. Calculate the mean gdp for each country

# by country, get gdp, find mean gdp

gapminder %>%
  group_by(country) %>%
  mutate(gdp = pop *gdpPercap) %>%
  summarize(mean_gdpP = mean(gdp))

# 3. Create a graph of the total population of each continent over time

# by continent, by year, total population

total_pop_df <- 
gapminder %>%
  group_by(continent, year) %>%
  summarize(sum_pop = sum(pop)) 

ggplot(data = total_pop_df, aes(x = year, y = sum_pop,
                                color = continent)) + 
  geom_point()


# 4. For each continent, what country had the smallest populatioin in 1952, 1972, and 2002? 

# by continent, subset 1952, 1972, and 2002 each, min pop

gapminder2 <- tbl_df(gapminder) #makes table and data frame
# won't print out a bunch of data 

gapminder2 %>%
  filter(year %in% c(1952, 1972, 2002)) %>%
  # filter(year == 1952 | year == 1972 | year == 2002)
  group_by(continent, year) %>%
  slice(which.min(pop)) %>%
  ungroup() %>%
  select(country, year, pop)
  # select(country, year, pop)
  #summarize(min_pop = min(pop)) # <- doesn't tell you country
  
# 1. How many countries are there on each continent?

gapminder2 %>%
  group_by(continent) %>%


# 2. What countries have the best and worst life expectancies in each continent?

### Challenge ####
# 3. Which country experienced the sharpest 5 year drop 
## in life expectancy (sharpest drop between data points)?





mean(group_by((filter(gapminder, year == 2002)),
              continent)$pop)
mean(filter(gapminder, year == 2002)$pop) # same thing

gapminder %>%
  filter(year == 2002) %>%
  #group_by(continent) %>%
  summarize(mean_pop = mean(pop))