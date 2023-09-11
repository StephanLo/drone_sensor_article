### Figure 6 alternative:


r_paths <- list.files(path = "data/ndvimaps/",
                      full.names = TRUE,
                      pattern = ".tif")

r_names <- list.files(path = "data/ndvimaps/",
                      pattern = ".tif") |> (\(x) substr(x,start = 1, stop = nchar(x)-4))() 
point_paths <- "data/vectordata/random_points/points_5000_clean_data_area.shp"

# read data
vi_raster <- rast(r_paths)
names(vi_raster) <- r_names
points_vec <- vect(point_paths)
buf_vec <- buffer(points_vec,0.1) # 10 cm radius buffer (0.1 meter)
#extract ndvi at the random points in the field:
vi_points <- extract(x = vi_raster,
                           y = buf_vec,
                           fun = mean,
                            na.rm = TRUE)
setDT(vi_points)



names(vi_points)
# I'm using column indices here, which is very dangerous but cant get other stuff to work. so so so 
df_longer <- rbindlist(list(vi_points[, c(2,4)],
                                 vi_points[, c(3,5)]
                                ),
                        use.names = FALSE
)
names(df_longer) <- c("mapir","mica")
model_together <- lm(mica ~ mapir, data = df_longer)


# Correlation: 
## mica ~ mapir 2022-07-05
cor_0705 <- cor(x = vi_points$ndvimaps_mica0705,
                y = vi_points$ndvimaps_mapir0705)

## mica ~ mapir 2022-08-17
cor_0817 <- cor(x = vi_points$ndvimaps_mica0817,
                y = vi_points$ndvimaps_mapir0817)

##  Analysis 1.2: Test models ---"---"---"---"
model_day_1 <- lm(formula = ndvimaps_mica0705 ~ ndvimaps_mapir0705, data = vi_points) 
model_day_2 <- lm(formula = ndvimaps_mica0817 ~ ndvimaps_mapir0817, data = vi_points)
 

modelSum_5k <- rbind(tidy(model_day_1),tidy(model_day_2))
setDT(modelSum_5k)
modelSum_5k$term <- c("intercept","slope","intercept","slope")
modelSum_5k$day <- c("Survey 1", "Survey 1", "Survey 2","Survey 2") 



## can the data be scaled?
scale_ndvi1 <- function(ndvi_in){
  return(ndvi_in*round(model_day_1$coefficients[2],2) + round(model_day_1$coefficients[1],2) ) # ndvi_out <- ndvi*mod_slope + mod_intercept
}
scale_ndvi2 <- function(ndvi_in){
  return(ndvi_in* round(model_day_2$coefficients[2],2) + round(model_day_2$coefficients[1],2)) # ndvi_out <- ndvi*mod_slope + mod_intercept
}


vi_points[, c("scaled_mapir0705","scaled_mapir0817") := list(scale_ndvi1(ndvimaps_mapir0705),scale_ndvi2(ndvimaps_mapir0817)) ] 

vi_points[complete.cases(vi_points),][]


p1 <- ggplot(vi_points, aes(x = ndvimaps_mica0705, y = scaled_mapir0705)) +
  geom_point(size = 2,alpha = 0.6, color = "darkgreen") + 
  #geom_smooth(method = "lm") +
  geom_abline(slope = 1,intercept = 0,linewidth = 1) + 
  xlim(-0.25,1.12) + 
  ylim(-0.25,1.12) + 
  labs(x = "NDVI: MicaSense RedEdge-MX",
       y = "MAPIR Scaled NDVI",
       caption = expression("scaling equation:" ~ 3.97*NDVI[mapir] + 0.79 ) )

p1

p2 <- ggplot(vi_points, aes(x = ndvimaps_mica0817, y = scaled_mapir0817)) +
  geom_point(size = 2,alpha = 0.6, color = "darkgreen") + 
  geom_abline(slope = 1,intercept = 0) + 
  xlim(-0.25,1.12) + 
  ylim(-0.25,1.12) +
  labs(x = "NDVI: MicaSense RedEdge-MX",
       y = "MAPIR Scaled NDVI",
       caption = expression("NDVI scaling equation:" ~ ndvi[scaled] ~ "=" ~ 3.4*NDVI[mapir] + 0.56 ))

p1+p2 + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect")


### About image histograms (fig 8) 

# First for scene 1:
# Individual Images --
image_roi <- vect("data/vectordata/indiv_scene_1.gpkg")
ground_soil <- vect("data/vectordata/ground_soil.gpkg")
init_mica <- rast("data/individualimages/together-calibrated/1_mica_modified.tif") |> mask(image_roi[1,])
init_mapir <- rast("data/individualimages/together-calibrated/1_mapir_modified.tif") |> project(init_mica) |> mask(image_roi[1,]) 


names(init_mica) <- c("mic_Blue", "mic_Green", "mic_Red", "mic_NIR", "mic_RedEdge")
names(init_mapir) <- c("map_Red", "map_Cyan", "map_NIR")
# These images are 16 bit integer reflectance images. scale to reflectance
# by dividing with 2^16

refl_mica <- init_mica/(2^16)
refl_mapir <- init_mapir/(2^16)


## divide ground and soil. 
mic_ground <- mask(refl_mica, ground_soil[ground_soil$Crop == FALSE,])
mic_crop <- mask(refl_mica, ground_soil[ground_soil$Crop == TRUE,])

map_ground <- mask(refl_mapir, ground_soil[ground_soil$Crop == FALSE,])
map_crop <- mask(refl_mapir, ground_soil[ground_soil$Crop == TRUE,])

# save it all into a data
ground_df <- c(map_ground[[c("map_Red","map_NIR")]],
               mic_ground[[c("mic_Red","mic_NIR")]]) |> 
  as.data.table() |> melt(measure.vars = 1:4)  

ground_df$type = "ground"

## DO the same thing for plants, and add together in data.frame
crop_df <- c(map_crop[[c("map_Red","map_NIR")]],
             mic_crop[[c("mic_Red","mic_NIR")]]) |> 
  as.data.table() |> melt(measure.vars = 1:4)  



crop_df$type = "crop"

camera_reflectances <- rbind(ground_df,crop_df)
  
## Add column for sensor, use partial string match (grepl) to return mapir or mica. with a if else statement.
camera_reflectances[, camera := fifelse(grepl("map",variable), "mapir","mica" )]
## do same for color (band)
camera_reflectances[, band := fifelse(grepl("Red",variable), "Red","NIR")]


band_summaries <- camera_reflectances[, list(refl_av = mean(value, na.rm = T), refl_sd = sd(value,na.rm = T))
                                      , by =  list(type,camera,band)]


ggplot(camera_reflectances, aes(x=value, colour= camera, fill = camera)) + 
  geom_density(alpha = 0.7) +
  scale_colour_discrete(labels = c("Infrared converted", "Multispectral"))+
  scale_fill_discrete(labels = c("Infrared converted", "Multispectral"))+
  facet_grid(rows = vars(band), cols = vars(type),scales = "free") + 
  geom_vline(data = band_summaries, aes(xintercept = refl_av)) +
    labs(x = "Reflectance",
       y = "Density",
       colour = "Sensor",
       fill = "Sensor")

ggplot(camera_reflectances, aes(x=value, fill = camera)) + 
  geom_density(alpha = 0.7) +
#  scale_colour_discrete(labels = c("Infrared converted", "Multispectral"))+
  scale_fill_discrete(labels = c("Infrared converted", "Multispectral"))+
  facet_grid(rows = vars(band), cols = vars(type),scales = "free") + 
  geom_vline(data = band_summaries, aes(xintercept = refl_av)) +
  labs(x = "Reflectance",
       y = "Density",
 #      colour = "Sensor",
       fill = "Sensor")    
    


# rowwe idee:
    # is the mapir orange influenced by green? So that mapir is measuring some green in orange band?
    # if we assume that 
    # take mapir$orange - mica$red 
    # is the difference correlated with mica$green?
mapir_green <- refl_mapir$map_Red - refl_mica$mic_Red

bandfig_titles <- c("(a) IC Red - MS Red", "(b) MS: Red band", "(c) IC: Red Band" )
plot(c( mapir_green, refl_mica$mic_Red, refl_mapir$map_Red),
     main = bandfig_titles, 
     range = c(0,0.3),
     axes = FALSE,
     nc =3)

png(filename = "processing/scene_1_RGB.png")
plotRGB(refl_mica, r = 3, g = 2, b = 1, scale = 0.4)
dev.off()

# red difference:
png(filename = "processing/scene_1_reddifdf.png")
plot(mapir_green, axes = FALSE, range = c(0,0.3), legend = FALSE)
dev.off()

# Mapir red:
png(filename = "processing/scene_1_Map_red.png")
plot(refl_mapir$map_Red, axes = FALSE, range = c(0,0.3), legend = FALSE)
dev.off()

# Mic red:
png(filename = "processing/scene_1_Mic_red.png")
plot(refl_mica$mic_Red, axes = FALSE, range = c(0,0.3), legend = FALSE)
dev.off()

# Mic red:
e <- ext(refl_mapir) + 0.00001
png(filename = "processing/scene_1_legend.png", res = 96)
plot(refl_mica$mic_Red, axes = FALSE, 
     range = c(0,0.3),
     plg = list(loc = "bottom"))
dev.off()


plot(refl_mica$mic_Red)
plot(refl_mapir$map_Red, range = c(0, 0.3))
plet(refl_mica$mic_Green)


plot(focalPairs(c(refl_mapir$map_Red,refl_mica$mic_Red), fun = "pearson"))

    
    plot(refl_mapir$map_Red)
  plot(focalPairs(c(refl_mapir$map_Red,refl_mica$mic_Red), fun = "pearson"))
plot(focalPairs(c(mapir_green,refl_mica$mic_Green), fun = "pearson"))    
      


# To show different lines in different facets, use aesthetics
p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ cyl)

mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
p + geom_hline(aes(yintercept = wt), mean_wt)

v <- vect(system.file("ex/lux.shp", package="terra"))
p <- spatSample(as.polygons(v, ext=T), 10)
values(p) = data.frame(id=11:20, name=letters[1:10])

m <- plet(v, "NAME_1", alpha=.5, tiles="")
m <- points(m, p, col="gray", cex=2, popup=T)
lines(m, v)

plet(v, "NAME_1", split=TRUE, alpha=.2) |> 
  points(p, col="gray", cex=2, popup=T) |> lines(v)
