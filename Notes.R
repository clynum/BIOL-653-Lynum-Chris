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
  

### Challenge ####

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


#### October 15, 2015 ####

# MORE FOR LOOPS ###

animals <- c('wombat', 'kangaroo', 'whale shark',
             'fox', 'snail')
# A vector, set of objects, list of things

for (animal in animals) {
  len <- nchar(animal)
  print(len)
}

for (i in 1:length(animals)) {
  len <- nchar(animals[i])
  print(len)
}

# write a for loop that calculates the square of 1-10

for (i in 1:10) {
  y <- i^2
  print(y)
}

sq <- vector(length = 10)
for (i in 1:10) {
  sq[i] <- i^2
}


for (i in 1:10){
  sums <- sums[i + 1] + sum[i]
}


# Calculate the cumulative sum of the values 1:10
x <- 0
for (i in 1:10){
  #browser()
  x <- x + i
  print(x)
}


#### October 20, 2015 ####

mat <- matrix(1:100, nrow = 10, ncol = 10)

for (i in length(mat)){
  print(i)
}

mat7 <- mat*7
mat7

set.seed(1)
x <- round(runif(min = 10, max = 100, n = 15))
x

# write a for loop to add 'n =' to the list


for (i in 1:10) {
  print(2 ^ i)
}

# 1) Print each value of this matrix.

mat <- matrix(1:100, nrow = 10, ncol = 10)

for (i in mat){
  print(i)
}

# 2) Multiply each value in this matrix by 7 and store it in a 10 x 10 matrix

mat <- matrix(1:100, nrow = 10, ncol = 10)

for (i in length(mat)){
  
}

# 3a) Print these values as part of a string that looks something like 'n = 16'.


set.seed(1)
x <- round(runif(min = 10, max = 100, n = 15))

for (i in x){
  counts <- paste('n = ', i)
  print(counts)
}



# b) Now modify this loop to store these strings in a new vector called counts.

set.seed(1)
x <- round(runif(min = 10, max = 100, n = 15))

counts <- rep(NA, 15)
for (i in x){
  counts <- paste('n = ', i)
  print(counts)
}


counts

# 4) Make a vector for which each entry is 2 raised to the power of it’s index (ex: the 3rd item in the vector is equal to 2^3).


for (i in 1:10) {
  2 ^ i
}


# 5) Make a matrix where each entry, using indexes i for row and j for column, is equivalent to i*j. Your final output should look like:



#Including conditional statements
#A conditional statement is an if this, do that. In programming, you’ll hear people talk about if statements, or if/else statements: if this then that else do something different.

#If statements test whether a condition (e.g., x > 5, value == max_value, country == 'Canada') is TRUE or FALSE.
#Before you begin, if you have not used if/else statements before checkout at least this first link to get you going. This first link is nice and bare bones, getting to the how to right away.

#http://www.programiz.com/r-programming/if-else-statement
#If you would like more to read, I often remind myself of how to write if/else statements from these:
  
#  ‘How to Use If Statements in R for Dummies’

# ‘How to Use If…Else Statements in R for Dummies’



# 6) Make a vector where each entry is TRUE or FALSE, based on whether it’s index is even or odd.

x <- 1:10


# 7) Run this code to set yourself up for question 7.

taxa <- c('Coral', 'fish', 'Fish',
          'Phytoplankton', 'coral', 'phytoplankton',
          'zooplankton', 'Zooplankton',
          'Echinoderms', 'echinoderms', 
          'Cephalopods', 'cephalopods')

taxa_values <- sample(taxa, size = 100, replace = TRUE)
set.seed(1)
counts <- round(runif(min = 10, max = 500, n = 100))

taxa_counts <- data.frame(taxa = taxa_values, abundance = counts)


#a) Using dplyr, calculate the mean abundance of each taxonomic group, what do you notice about the output? Is it what you would expect?



#b) Hopefully not. What’s going on? Can you fix it?

#If you’re struggling to figure out what to do, think about how you would go about solving the problem in excel, this might help you figure out what you should try and google. (otherwise I’ll give you a hint about a useful function to use here).



#### October 27, 2015 ####

# Functions? 

# DISCRETE TIME LOGISTIC POPULATION

# EQUATION

# Nt=Nt−1+r∗Nt−1∗(1−Nt−1/K)

# Given a starting population N1 of 2 individuals, a per capita growth rate of r=1, and carrying capacity of k=1000, how many individuals should there be in the next generation? I’ll give you a start for how I’d start to set myself up to calculate this in R.

N_1 <- 2
r <- 1
K <- 1000
N_2 <- N_1 + (r*N_1) * (1-(N_1/K))
N_2
# Now calculate the population when t = 3 (generation 3)

N_3 <- N_2 + (r*N_2) * (1-(N_2/K))
N_3

# Now calculate the population when t = 4 (generation 4)

N_4 <- N_3 + (r*N_3) * (1-(N_3/K))
N_4


# MAKE IT A FUNCITON 

dgrowth <- function(ninit, r, K, ngen) {
  n <- rep(NA, ngen)
  n[1] <- ninit
  
  for (i in 2:ngen) {
    n[i] = n[i-1] + (r*n[i-1]) * (1-(n[i-1]/K)) 
    # calculate population size - hint eqn!
  }
  return(n) # return value
}

dgrowth(ninit = 1, r = 1, K = 1000, ngen = 100)

# First, let’s make a data frame where we bind one column of time values with our vector/column of population size at time step t (calcuated using our handy function). This time let’s look at 100 time steps.

ngen = 100
pop_100 <- dgrowth(ninit = 1, r = 1, K = 1000, ngen = 100)
time_100 <- rep(1:ngen)
pop_df <- data.frame(pop_100, time_100)

library(ggplot2)
ggplot(data = pop_df, aes(x = time_100, y = pop_100)) + 
  geom_line()


#Set your input values in preparation for an upcoming for loop! We will be calculating a set of predicted populations based on values of r from 0.7 to 3 by an increment of 0.1. (remember to look up functions you haven’t seen before!)

ninit = 1
ngen  = 100
K = 1000
r_vals = seq(from = 0.7, to = 3, by = 0.1)

# Before we begin, let’s take a detour to learn a new function that can be super handy. Sometimes you create one vector or dataframe and then want to add rows to it. Describe what is happening in the code below (add comments as notes for future you!)

pop1 <- dgrowth(r = 1, ninit = 1, K = 1000, ngen = 4)
pop_df1 <- data.frame(N = pop1, time = 1:4)

pop2 <- dgrowth(r = 2, ninit = 1, K = 1000, ngen = 4)
pop_df2 <- data.frame(N = pop2, time = 1:4)

pops_df <- rbind(pop_df1, pop_df2)
?rbind
pops_df

# Ok, back to looping. Let’s start by just making a loop that calculates the populations for 100 generations using our different values of r. You can look at the example for loops

for (i in 1:length(r_vals)) {
  dgrowth(ninit = 1, ngen = 100, K = 1000, r =  r_vals[i])
}

# Now let’s store each new set of values in a data.frame called pops.

# Create a dataframe with an initial population using r = 1. This is the data.frame that we will add rows to.
pops <- data.frame(r = 0.6, t = 1:ngen,
                   N = dgrowth(r = 0.6, ninit = ninit,
                               K = K, ngen = ngen))


for(i in 1:length(r_vals)){
  N    <- dgrowth(ninit = 1, ngen = 100, K = 1000, r =  r_vals[i])
  popr <- data.frame(r = r_vals[i], t = 1:ngen, N = N)
  pops <- rbind(pops, popr)
}

## Another way of doing this for loop with r in r_vals that 
### simplifies the loops 
## Changing r = r instead of r = r_vals[i] for i in legnth(r_vals)
for(r in r_vals){
  N <- dgrowth(ninit = 1, ngen = 100, K = 1000, r = r)
  popr <- data.frame(r = r, t = 1:ngen, N = N)
  pops <- rbind(pops, popr)
}
head(pops, n = 10L)
tail(pops, n =)

ggplot(data = pops, aes(x = t, y = N)) + 
  geom_line() + facet_wrap(~ r)

library(dplyr)

pops %>%
  group_by(r) %>%
  slice(91:100) %>%
  ggplot(aes(x = r, y = N)) + geom_point(fill = as.factor(r),
                                         color = rainbow(250)) 


#### November 3, 2015 ####

# Tidy Data
library(tidyr)
library(gapminder)

gap <- 
  filter(gapminder, grepl("^N", country)) %>% 
  filter(year %in% c(1952, 1977, 2007)) %>% 
  slice(1:9) %>% 
  select(-continent, -gdpPercap, -pop) %>% 
  mutate(lifeExp = round(lifeExp))

life_exps <- spread(gap, key = year, value = lifeExp)

life_exps


long_form <- gather(data = life_exps, key = country, value = year)
long_form


gap_with_cont <- 
  filter(gapminder, grepl("^N", country)) %>% 
  filter(year %in% c(1952, 1977, 2007)) %>% 
  slice(1:9) %>% 
  select(-gdpPercap, -pop) %>% 
  mutate(lifeExp = round(lifeExp))

life_exps <- spread(gap_with_cont, key = year, value = lifeExp)
life_exps



long_form <- gather(data = life_exps, key = country, value = year, 3:5)
long_form

#long_form <- gather(data = life_exps, key = country, value = year, '1952', '1977', '2007')


set.seed(1)
counts <- 
  data.frame(site = c(1, 1, 2, 3, 3, 3),
             taxon = c("A", "B", "A", "A", "B", "C"),
             abundance = round(runif(n = 6,
                                     min = 0, max = 20), 0))

counts

counts_wide <- spread(counts, key = taxon, value = abundance)
counts_wide

counts_long <- gather(counts_wide, key = site, value = abundance)
counts_long


#### November 10, 2015 ####

library(tidyr)
library(dplyr)
library(gapminder)


gap <- 
  filter(gapminder, grepl("^N", country)) %>% 
  filter(year %in% c(1952, 1977, 2007)) %>% 
  slice(1:9) %>% 
  select(-continent, -gdpPercap, -pop) %>% 
  mutate(lifeExp = round(lifeExp)) %>% 
  spread(key = year, value = lifeExp)

gap

gap_with_cont <- 
  filter(gapminder, grepl("^N", country)) %>% 
  filter(year %in% c(1952, 1977, 2007)) %>% 
  slice(1:9) %>% 
  select(-gdpPercap, -pop) %>% 
  mutate(lifeExp = round(lifeExp)) %>%
  spread(key = year, value = lifeExp)

gap_with_cont


# when using slice, need to have . in front of data
# because normal data is a function and it would get confused
slice(.data = gapminder, c(1,2, 50:55, 100)) 
# slice gapminder, rows 1&2



tidied_gap <- gather(data = gap, key = year,
                     value = lifeExp, -country)
tidied_gap2 <- gather(data = gap, key = year,
                     value = lifeExp, 2:4)
# these both do the same thing. the indeces tell you which
# columns to look at when tidying. The -country tells you to
# not look at that column specifically 


# data is the data you are wrangling
# key is how you want to separate data
## the new column we are going to create!
# value is the data that was under the key in the previous df

# key is the name of the column that contains the values
# that were across the top of your data.frame. 
# Value is the name of the values that populated all of 
# the cells that you gathered.

tidied_gap


tidied_gap_with_cont <- gather(data = gap_with_cont,
                               key = year, value = lifeExp, 3:5)
tidied_gap_with_cont


tidied_gap_with_cont <- gather(data = gap_with_cont,
                               key = year, value = lifeExp,
                               -country, -continent)
tidied_gap_with_cont


spread_gap <- 
  spread(data = tidied_gap, key = year, value = lifeExp)

tidied_gap
gap

spread_gap <- 
  spread(data = tidied_gap, key = year, value = lifeExp)
# key in this instance is what you want to *spread* across
# the top (the top columns) and value is the values you want
# under those columns 

spread_gap


set.seed(1)
counts <- 
  data.frame(site = c(1, 1, 2, 3, 3, 3),
             taxon = c("A", "B", "A", "A", "B", "C"),
             abundance = round(runif(n = 6, min = 0, max = 20),
                               0))
counts

# Use spread to put counts into wide form. E.g.,
counts_wide <- spread(counts, key = taxon, value = abundance,
                      fill = 0)

counts_wide2 <- spread(counts, key = taxon, value = abundance)


counts_wide


# Use gather to return counts_wide to long form!

counts_long <- gather(counts_wide, key = site, value = abundance)
counts_long

counts_long2 <- gather(data = counts_wide, key = column_names,
                       value = all_the_cells, A, B, C)
counts_long2



# In this case when a species wasn’t observed at a site it was because it was never observed. Read the help file for spread to see if there is a simple way to have those NA values fill with zeroes. Then try it!

counts_long_NA <- gather(counts_wide2,
                         key = site, value = abundance,
                         na.rm = T)

# With gather it just gets rid of the NAs. No way as of now to 
# just return them as zeroes. 
counts_long_NA





#What about when I want to spread data but have
# multiple columns that I want to retain?

# Remind ourselves what the data.frame looks like
tidied_gap_with_cont

spread_gap <- 
  spread(data = tidied_gap_with_cont, key = year, value = lifeExp)

# Let's look at it
spread_gap


united_gap <- 
unite(data = tidied_gap_with_cont,
      col = location_key, country:continent, sep = "__")

separated_gap <-
  separate(data = united_gap, col = location_key,
           into = c('country', 'continent'), sep = "__")
separated_gap
