# Script: NDVI point analysis
# author: AS Louw
# enquiry: jasahovercar@gmail.com

# Setup environment
library(terra)
library(sf)
library(data.table)
library(ggplot2)
library(ggthemes)
theme_set(theme_economist())

#-------------------------------------------------------------------------------
# paths
point.paths <- "data/vectordata/random_points/points_5000_clean_data_area.shp"


r.paths <- list.files(path = "data/ndvimaps/",
                      full.names = TRUE,
                      pattern = ".tif")

r.names <- list.files(path = "data/ndvimaps/",
                      pattern = ".tif") |> (\(x) substr(x,start = 1, stop = nchar(x)-4))() # this takes the name but drops the .tif extension
                                                                                           # more info on the syntax https://www.r-bloggers.com/2021/05/the-new-r-pipe/

#-------------------------------------------------------------------------------
# read data
vi.raster <- rast(r.paths)  
names(vi.raster) <- r.names

rpoints.vec <- vect(point.paths)


#-------------------------------------------------------------------------------
#extract ndvi at the random points in the field:
vi.points <- extract(x = vi.raster,
                     y = rpoints.vec)
head(vi.points)
setDT(vi.points)


#-------------------------------------------------------------------------------
# Correlation: 
## mica ~ mapir 2022-07-05
cor.0705 <- cor(x = vi.points$ndvimaps_mica0705,
                y = vi.points$ndvimaps_mapir0705)

## mica ~ mapir 2022-08-17
cor.0817 <- cor(x = vi.points$ndvimaps_mica0817,
                y = vi.points$ndvimaps_mapir0817)


#-------------------------------------------------------------------------------
# Comparing in graph:
ggplot(vi.points, aes(x = ndvimaps_mica0705, y = ndvimaps_mapir0705))+
  geom_point() + geom_smooth()
# comment: We can see a rather linear pattern for this data
# check linear model estimates for this.

ggplot(vi.points, aes(x = ndvimaps_mica0817, y = ndvimaps_mapir0817))+
  geom_point() + geom_smooth()
# for this day the imagery is more convex




# If we compare the points one for one on line:
vi.long <- melt(vi.points, id.vars = 1,
                measure.vars = 2:5)
ggplot(vi.long,aes(color = variable,x = 1:2e4, y = value)) + geom_point()
# We can see that the 


# to just show a readable subset. generate an index
set.seed(1)
n.obs <- 30
rnd.index <- runif(n = n.obs,
                   min = 1,
                   max = nrow(vi.points))

readable.0705 <- melt(vi.points[rnd.index,c(1,2,4)], id.vars = 1,
                      measure.vars = 2:3)
readable.0817 <- melt(vi.points[rnd.index,c(1,3,5)], id.vars = 1,
                      measure.vars = 2:3)

ggplot(readable.0705, aes(x = ID, color = variable, y = value)) + 
  geom_point()










