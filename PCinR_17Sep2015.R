#View(gapminder)
str(gapminder)
#1704 observations (rows) with 6 variables (columns)

# country, continent, year, lifeExp, population, gdpPercap

head(gapminder) # first 6 rows
tail(gapminder) # last 6 rows

dim(gapminder) # the dimensions

life <- gapminder$lifeExp

