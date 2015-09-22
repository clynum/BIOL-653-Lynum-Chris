#View(gapminder) ####
library(gapminder)
library(ggplot2)
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


#data: as data.frame ####

#aesthetic(aes): mapping variables to visual properties -
  # position, color, line type, size

#geom: actual visualization of the data

#scale: map values to aes, color, size,
##shape (show up as legends and axes)

#stat: statistical transformations, summaries of data

#facet: different subsets of the data

# Basic Scatter

ggplot(data = gapminder, aes(x = year, y = lifeExp)) + 
  geom_point()

#### adding color ####

ggplot(data = gapminder, aes(x = year, y = lifeExp,
                             color = continent)) + 
  geom_point()

# Layering ####
dev.new()
linedYL <- ggplot(data = gapminder, aes(x = year, y = lifeExp)) + 
  geom_point()
linedYL + geom_line(aes(by = country, color = continent))

dev.new()
pr <- ggplot(data = gapminder, aes(x = year, y = lifeExp))
pr + geom_point(aes(color = continent))
pr + geom_point()
pr + geom_point(aes(color = continent)) + geom_line(aes(by = country))
pr <- pr + geom_line(aes(by = country, color = continent))
  + geom_point() 

# panel = background
# data point = layer
# lines = layer
# axes = layers
# etc. 


pr + xlab('Year') + ylab('Life Expactancy')

dev.new()
prac2 <- ggplot(data = gapminder, aes(x = year, y = lifeExp,
                                      by = country,
                                      color = log(gdpPercap)))
prac2 + geom_point()
prac2 + geom_point() + scale_color_gradientn(colours = topo.colors(10))
# color HAS to be spelled colour for scale_color_gradientn

#??dev.new()
#dev.list()
#dev.set


g <- ggplot(data = mpg, aes(x = class, y = hwy))
g + geom_point()
g + geom_bar(stat = 'identity')
g + geom_boxplot()
g + geom_dotplot(binaxi = 'y', stackdir = 'center')
g + geom_violin(scale = 'area', aes(color = class))

f <- ggplot(data = mpg, aes(x = cty, y = hwy))
f + geom_blank()
f + geom_jitter()
f + geom_point()
f + geom_quantile()
f + geom_rug(sides = 'bl')
#f + geom_smooth(model = lm)
f + geom_text(aes(color = manufacturer,label = manufacturer))


a <- ggplot(data = mpg, aes(x = hwy))
a + geom_area(stat = 'bin')
a + geom_density(kernel = 'gaussian')
a + geom_dotplot()
a + geom_freqpoly()
a + geom_histogram(binwidth = 5)

b <- ggplot(data = mpg, aes(x = fl))
b + geom_bar()

c <- ggplot(map, aes(x = long, y = lat))

d <- ggplot(economics, aes(date, unemploy))
d + geom_path(lineend = 'butt', linejoin = 'round', linemitre = 1)
d + geom_ribbon(aes(ymin = unemploy -900, ymax = unemploy + 900))


e <- ggplot(seals, aes(x = long, y = lat))
e + geom_segment(aes(xend = long + delta_long,
                     yend = lat + delta_lat))
e + geom_rect(aes(xmin = long, ymin = lat, xmax = long + delta_long,
                  ymax = lat + delta_lat))

h <- ggplot(diamonds, aes(cut, color))
h + geom_jitter()

i <- ggplot(movies, aes(x = year, y = rating))
i + geom_bin2d(binwidth = c(5, 0.5))
i + geom_density2d()
i + geom_hex()


j <- ggplot(economics, aes(x = date, y = unemploy))
j + geom_area()
j + geom_line()
j + geom_step(ditection = 'hv')

df <- data.frame(grp = c('A', 'B'), fit = 4:5, se = 1:2)
k <- ggplot(df, aes(x = grp, y = fit,
                    ymin = fit-se, ymax = fit + se))
k + geom_crossbar(fatten = 2)
k + geom_errorbar()
k + geom_linerange()
k + geom_pointrange()


data <- data.frame(murder = USArrests$Murder,
                   state = tolower(rownames(USArrests)))
data2 <- data.frame(rape = USArrests$Rape,
                   state = tolower(rownames(USArrests)))
data3 <- data.frame(assault = USArrests$Assault,
                    state = tolower(rownames(USArrests)))
map <- map_data('state')
l <- ggplot(data, aes(fill = murder))
l + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat)

seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2))
m <- ggplot(seals, aes(long, lat))
m + geom_contour(aes(z = z))
m + geom_raster(aes(fill = z), hjust = 0.5,
                vjust = 0.5, interpolate = F)
m + geom_tile(aes(fill = z)) # The same as raster


n <- b + geom_bar(aes(fill = fl))
n
n + scale_fill_manual(
  values = c('skyblue', 'royalblue', 'blue', 'navy'),
  limits = c('d','e','p','r'), breaks = c('d','e','p','r'),
  name = 'fuel', labels = c('D', 'E', 'P', 'R'))

#library(RColorBrewer)
#display.brewer.all()
n + scale_fill_brewer(palette = "Spectral")


p <- f + geom_point(aes(shape = fl))
p
p + scale_shape(solid = F)

q <- f + geom_point(aes(size = cyl, color = class))
q

s <- ggplot(mpg, aes(fl, fill = drv))
s + geom_bar(position = position_dodge(width = 1))
