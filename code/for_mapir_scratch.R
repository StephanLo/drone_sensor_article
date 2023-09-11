# survey 1
sen_labs <- c("Survey 3W OCN", "MicaSense RedEdge-MX", "Handheld Trimble Greenseeker")

p1 <- ggplot(ndvi_long[day=="survey 1",], aes(x = factor(ID),y = ndvi, colour = factor(sensor))) + 
  geom_point( alpha = 1, size = 2) +
  scale_colour_discrete(labels = sen_labs)+
  xlab("Sample ID") + ylab("NDVI") + 
  labs(colour = "Sensor",
       caption = "Survey 1") + theme_clean()
# survey 2
p2 <- ggplot(ndvi_long[day=="survey 2",], aes(x = factor(ID),y = ndvi, colour = factor(sensor))) + 
  geom_point( alpha = 1, size = 2) +
  scale_colour_discrete(labels = sen_labs)+
  xlab("Sample ID") + ylab("NDVI") +
  labs(colour = "Sensor",
       caption = "Survey 2") + theme_clean()

p1 + p2  + 
  plot_annotation(title = "NDVI measured by 3 sensors at sample points in a potato and soybean field" ) +
  plot_layout(guides = "collect") &
  theme(legend.position='bottom')



## For individual images.

rgbpath <- c(
  "data/individualimages/mapirraw/Processed_1/Calibrated_1/2022_0705_131608_453_CALIBRATED_red.tif",
  "data/individualimages/mapirraw/Processed_1/Calibrated_1/2022_0705_131608_453_CALIBRATED_green.tif",
  "data/individualimages/mapirraw/Processed_1/Calibrated_1/2022_0705_131608_453_CALIBRATED_blue.tif")

r_photo <- rast(rgbpath)

hist(r_photo[[1]])
hist(mapir_im1[[1]])

plot(r_photo[[1]])
plot(mapir_im1[[1]])


library(tidyterra)

ggplot() + geom_spatraster_rgb(data =  r_photo,r = 2, g = 1, b = 3, max_col_value = 0.5 )

ggplot() + geom_spatraster_rgb(data =  refl_mapir,  r = 2, g = 1, b = 3, max_col_value = 0.5)




# First for scene 1:
# Individual Images --
image_roi <- vect("data/vectordata/indiv_scene_1.gpkg")
ground_soil <- vect("data/vectordata/ground_soil.gpkg")
init_mica <- rast("data/individualimages/together-calibrated/1_mica_modified.tif") |> mask(image_roi[1,])
init_mapir <- rast("data/individualimages/together-calibrated/1_mapir_modified.tif") |> project(init_mica) |> mask(image_roi[1,]) 


names(init_mica) <- c("mic_Blue", "mic_Green", "mic_Red", "mic_NIR", "mic_RedEdge")
names(init_mapir) <- c("map_Cyan", "map_Red", "map_NIR")
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

## Do the same thing for plants, and add together in data.frame
crop_df <- c(map_crop[[c("map_Red","map_NIR")]],
             mic_crop[[c("mic_Red","mic_NIR")]]) |> 
  as.data.table() |> melt(measure.vars = 1:4)  

crop_df$type = "crop"

camera_reflectances <- rbind(ground_df,crop_df)

## Add column for sensor, use partial string match (grepl) to return mapir or mica. with an if else statement.
camera_reflectances[, camera := fifelse(grepl("map",variable), "mapir","mica" )]
## do same for color (band)
camera_reflectances[, band := fifelse(grepl("Red",variable), "Red","NIR")]

# Get mean and sd of each band in the plant and soil areas.
band_summaries <- camera_reflectances[, list(refl_av = mean(value, na.rm = T), refl_sd = sd(value,na.rm = T))
                                      , by =  list(type,camera,band)]






## 
unique(field_samples$band)
ggplot(field_samples[band %in% c("mic_NIR", "map_NIR")], aes(x = ID, y = value, colour = band)) + geom_point()


ggplot(field_samples[band %in% c("mic_Red", "map_Orange")], aes(x = ID, y = value, colour = band)) + geom_point()













