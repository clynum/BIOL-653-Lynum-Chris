###### 29 September 2015 ########
library(ggplot2)
library(gapminder)
library(dplyr)
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


# 2. What countries have the best and worst life expectancies in each continent?

#by continent
#which country
#max(lifeExp)
#min(lifeExp)

gapminder2 %>%
  group_by(continent) %>%
  slice(which.min(lifeExp))

gapminder2 %>%
  group_by(continent) %>%
  slice(which.max(lifeExp))

### Challenge ####
# 3. Which country experienced the sharpest 5 year drop 
## in life expectancy (sharpest drop between data points)?


gapminder2 %>%
  group_by(country) %>%
  arrange(year) %>%
  # grouping works on each individual chunk
  # arrange sorts - think about sorting in excel
  mutate(diff = lifeExp - lead(lifeExp)) %>%
  # lead = previous now
  # lag = next row
  # mutate creates a whole new column
  group_by(continent) %>%
  # modified this problem slightly to 
  # find sharpest drop for each continent
  # (country and year)
  slice(which.min(diff))



######### NOTES AGAIN ###
mean(group_by((filter(gapminder, year == 2002)),
              continent)$pop)
mean(filter(gapminder, year == 2002)$pop) # same thing

gapminder %>%
  filter(year == 2002) %>%
  #group_by(continent) %>%
  summarize(mean_pop = mean(pop))


#### Reading for Oct 1, 2015 ####

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
head(mtcars[mtcars$cyl == 4 | 6, ]) 
head(mtcars[mtcars$cyl == 4 | mtcars$cyl ==6, ])

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
#  characters, factors, numbers, integers, logical? 

#4. What are 5 diff. data types?
#  Atomic vectors, lists, matrices, arrays, data frames

# 5. If this was enough,
## you can stop at subsetting & assign. heading


#### Class October 1, 2015 ####

# what do I mean when I say indexing?

# get the 5th and 10th element of the vector:

meow <- c(1, 2, 3, 4, 10, 11, 12)
meow[c(5, length(meow))] # there is no 10 elements

# what does str() do?

str(meow)
# gives you the structure of the object


######### OBJECTS IN R ##

# Atomic Vectors ##

dbl_vec <- c(1,2, 1.3, 1.4, 2.5)
str(dbl_vec)
typeof(dbl_vec) # tells you the type of
class(dbl_vec)

# creating an integer vector (numeric, but discrete)
int_vec <- c(1, 2, 3, 4) # this is still a double
int_vec <- c(1L, 2L, 3L, 4L) # L just means it's an integer
typeof(int_vec) # as an integer
int_vect <- c(1:4)
typeof(int_vect) # also an integer


# LOGICAL vectors #
log_vec <- c(T, F, T)
typeof(log_vec)
log_vec
sum(log_vec) # because trues = 1, falses = 0

# character vectors

char_vec <- c('a', 'b', 'c')
char_vec
typeof(char_vec)







# Coercion #

z <- read.csv(text = "value\n12\n1\n.\n9")
z
str(z) # factor with four levels
typeof(z) # it's a list
typeof(z[1])
typeof(z[[1]])

# factors name these numbers
as.double(z[[1]]) # gives us 3, 2, 1, 4 instead of 12, 1, ., 9
# numeric is a broader representation of numbers

as.character(z[[1]]) # first have to get the actual characters
# then change them to numbers
as.double(as.character(z[[1]])) # gives us the 12, 1, ., 9
# period turns into NA

## if you highlight a section and then add a
## parentheses it puts both of them on there

zztop <- read.csv(text = "value\n12\n1\n.\n9",
                  stringsAsFactors = F)
typeof(zztop[[1]])
# it's a character now because we told it to 
## NOT string as factor
as.double(zztop[[1]])
# now we didn't have to define as character first before
## turning into actually numbers

### MATRICES, ARRAYS, AND DATA FRAMES ###

# 2 dimensional structures

# Matrix

a <- matrix(1:6, ncol = 3, nrow = 2)
a
str(a)
a[, 3] # third column in a

length(a)
nrow(a)
dim(a)

x <- c('a', 'b', 'c', 'd', 'e', 'f')
a <- matrix(x, ncol = 3, nrow = 2)
a

x <- c(x, 1, 2, 3)
x

library(gapminder)
str(gapminder)


#### October 6, 2015 ####

## SUBSETTING AND FOR LOOPS


# select rows 10 to 20 of the country and life expectency columns

gapminder[10:20, c('country', 'lifeExp')]

# select rows 1 to 10, 20, and 100 in gapminder

gapminder[c(1:10, 20, 100),]


## THREE DIFF SUBSETS ##
# 1. object[] - multiple values

# 2. object$ - named values

# 3. object[[]] - lists (mostly)

## PEPPER PACKETS! (Subsetting nested objects)

pp1 <- c('g1', 'g2', 'g3', 'g4')
pp2 <- pp1
pp3 <- pp1

shaker <- list(pp1, pp2, pp3)
# get pepper packet 1
shaker[[1]]
# what's diff between shaker[1] 
  ## brings back a list instead of character
# get second grain out of pepper packet 1
shaker[[1]][[2]]
shaker[[1]][2]

#### October 13, 2015 ####

# FOR LOOPS? ##

foo <- c(7, 10, 11, 52, 55, 83, 101, 9)
foo[1]
foo[4]

for (i in 1:5){
  x <- foo[i] + 2
  print(x)
}