#View(gapminder) ####
str(gapminder)
#1704 observations (rows) with 6 variables (columns)

# country, continent, year, lifeExp, population, gdpPercap

head(gapminder) # first 6 rows
tail(gapminder) # last 6 rows

dim(gapminder) # the dimensions

life <- gapminder$lifeExp
dim(life)
# NULL - because it might not be a data.frame
str(life)
# doesn't say anything about being a data.frame
is.data.frame(life)
# NOPE, not a data.frame
is.vector(life)
# it turned into a vector

life[1]
life[1704]
life[1:10]; life[1694:1704]


life[c(1,2)]
life[c(1,2,1704)]

gapminder[1, 'country'] #ooooh

gapminder[1, c('country','year')]

gapminder[1, ] #get the whole first row
gapminder[1:10, ]


gapminder[100, ]
fortyfifty <- gapminder[40:50, c('country', 'continent', 'lifeExp')]
fortyfifty

life[c(1, 2, 3, 1700:1704)]

plot(gapminder$pop ~ gapminder$year)

library(ggplot2)
#data: as data.frame ####

#aesthetic(aes): mapping variables to visual properties -
  # position, color, line type, size

#geom: actual visualization of the data

#scale: map values to aes, color, size, shape (show up as legends and axes)

#stat: statistical transformations, summaries of data

#facet: different subsets of the data

# Basic Scatter

ggplot(data = gapminder, aes(x = year, y = lifeExp)) + 
  geom_point()

#### adding color ####

ggplot(data = gapminder, aes(x = year, y = lifeExp, color = continent)) + 
  geom_point()

# Layering ####

linedYL <- ggplot(data = gapminder, aes(x = year, y = lifeExp)) + 
  geom_point()
linedYL + geom_line(aes(by = country, color = continent))




