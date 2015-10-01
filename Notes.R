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


#### October 1, 2015 ####

# Readings on Subsetting

x <- c(2.1, 4.2, 3.3, 5.4)

x[c(3,1)]
x[order(x)]
x[c(1,1)]
x[c(2.1, 2.9)] # truncated to integers (2, 2)

x[-c(3, 1)] # everything but 3rd and 1st numbers

### Logical Vectors ###

x[c(T, T, F, F)] # only the first two?

x[x > 3]

### A little confused by the shorter logical vectors

y <- setNames(x, letters[1:4])
y

y[c('d', 'c', 'a')]
y[c('a', 'a', 'a')]

# when subsetting with [, names are always matched exactly 
z <- c(abc = 1, def = 2)
z[c('a', 'd')]
z

a <- matrix(1:9, nrow = 3)
a

colnames(a) <- c('A', 'B', 'C')
a[1:2, ] # first two rows

a[c(T, F, T), c('B', 'A')]
# first and third rows, only columns B, then A

a[0, -2] # don't really get what negative 2 does?


# Data frames possess characteristics of both lists and matrices

#If you subset with single vectors = like list
#subset with two vectors = like matrices

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df

df[df$x == 2, ] # wherever row = 2

df[c(1,3), ] # first and third rows

# selecting rows
## like a list
df[c('x', 'z')]

## like a matrix
df[, c('x', 'z')]

# if selecting single column, matrix subsetting simplifies
## list subsetting does not

str(df['x']) # like a list
str(df[, 'x']) # like a matrix

# fix these

# a. double equals
# b. can't have negatives
# c. needs comma after 5
# d. 
mtcars[mtcars$cyl == 4 | 6, ] # did this work?

# [[ can only return a single value, un like [
# $ shorthand for [[ combined with character subsetting
# [[ for lists

a <- list(a = 1, b = 2)
a
a[[1]]
a[['a']]

# simplifying = returns simplest possible data structure
# preserving = keeps structure of the output the same as input



#1. What are three subsetting operators

#[, [[, $. 

#2. What is a vector



# 3. What types of vectors are there
## (you already saw at least 3)?
characters, factors? 

#4. What are 5 diff. data types?
Atomic vectors, lists, matrices, arrays, data frames

# 5. If this was enough,
## you can stop at subsetting & assign. heading


