# Script: NDVI point analysis: paper targets
# author: AS Louw
# enquiry: jasahovercar@gmail.com

# Setup environment
library(terra)
library(sf)
library(tmap)
library(data.table)
library(ggplot2)
library(ggthemes)
theme_set(theme_base())

#-------------------------------------------------------------------------------
# paths
point_path <- c("data/vectordata/targetsamples_0705.gpkg", "data/vectordata/targetsamples0817.gpkg")

survey_path <- "data/Yao_farm_surveys_2022.xlsx" 
survey_sheet <- c("20220705_trimble_NDVI", "20220817_trimble_NDVI") #sheet name


r_path <- list.files(path = "data/ndvimaps/",
                      full.names = TRUE,
                      pattern = ".tif")

r_names <- list.files(path = "data/ndvimaps/",
                      pattern = ".tif") |> (\(x) substr(x,start = 1, stop = nchar(x)-4))() # this takes the name but drops the .tif extension
# more info on the syntax https://www.r-bloggers.com/2021/05/the-new-r-pipe/

#-------------------------------------------------------------------------------
# read data
## if first date, use path index 1&3 and surveysheet 1, if second date, 2&4 and survey sheet 2, 

vi_raster <- rast(r_path[c(1,3)])  
names(vi_raster) <- c("sensor_1", "sensor_2")

rpoints_vec <- vect(point_path[1])

survey_df <- readxl::read_excel(path = survey_path,
                                sheet = survey_sheet[1])

#-------------------------------------------------------------------------------
# Make buffer around point. Of 10 cm radius (0.1 meter)
p_buf <- buffer(rpoints_vec, width = 0.1) 

#extract ndvi at the points in the field:
vi_points <- extract(x = vi_raster,
                     y = p_buf)

head(vi_points)
setDT(vi_points)

vi_points <- merge(vi_points, values(rpoints_vec),by.x = "ID", by.y = "no")

# Join field and other data
all_df <- merge(vi_points, survey_df, by.x = "ID", by.y = "ID")

names(all_df)[names(all_df) == "NDVI"] <- "trimble"
# Drop the missing values.
all_df <- all_df[!is.na(sensor_1),]

## If we aggregate the measurements in the buffer
all_agg <- all_df[, .(sensor_1 = mean(sensor_1),
                      sensor_2 = mean(sensor_2),
                      trimble = mean(trimble)),
                  by = ID]



## FIGURE 1 VIEW ---"---"---"
# rearrange for plotting:
ndvi_long <- all_df[,c("ID", "sensor_1", "sensor_2","trimble")] |>
  melt(id.vars = "ID",
       variable.name = "sensor",
       value.name = "ndvi")

ndvi_agg_long <- all_agg[,c("ID", "sensor_1", "sensor_2","trimble")] |>
  melt(id.vars = "ID",
       variable.name = "sensor",
       value.name = "ndvi")  

## Plot 1
ggplot(ndvi_long, aes(x = ID,y = ndvi, colour = factor(sensor))) + 
  geom_point( alpha = 0.5,size = 2) +
  scale_colour_discrete(labels = c("Modified RGB", "Multispectral", "Handheld NDVI"))+
  xlab("Sample ID") + ylab("NDVI") +
  labs(caption = "\n Modified RGB: MAPIR Survey3W OCN   ||   Multispectral: MicaSense RedEdge-MX   ||   Handheld: Trimble Greenseeker \n Note: Multiple points for each Sample ID, since all NDVI values in 10 cm radius of sample point locations were sampled",
       colour = "Sensor")

## Plot variation 2 (aggregate ndvi readings)
ggplot(ndvi_agg_long, aes(x = factor(ID),y = ndvi, colour = factor(sensor))) + 
  geom_point( alpha = 1,size = 3) +
  scale_colour_discrete(labels = c("Modified-RGB", "Multispectral", "Handheld NDVI"))+
  xlab("Sample ID") + ylab("NDVI") +
  labs(caption = "\n Modified RGB: MAPIR Survey3W OCN   ||   Multispectral: MicaSense RedEdge-MX   ||   Handheld: Trimble Greenseeker" ,
       colour = "Sensor")


## FIGURE 2 VIEW ---"---"---"
#rearrange data for plotting. 
ndvi_v2 <- all_df[,c("ID", "sensor_1", "sensor_2", "trimble")] |>
  melt(id.vars = c("ID","trimble"),
       variable.name = "sensor",
       value.name = "drone_ndvi")

ggplot(ndvi_v2, aes(x = trimble, y = drone_ndvi, colour = sensor)) + 
  geom_point(size = 2) +
  geom_abline(slope = 1,intercept = 0) + 
  xlim(0,1) + 
  ylim(0,1) + 
  scale_colour_discrete(labels = c("Modified-RGB", "Multispectral")) +
  xlab("Handheld Sensor NDVI") + 
  ylab ("Drone Sensor NDVI") + 
  labs(colour = "Drone Sensor")

ggplot(all_agg, aes(x = trimble, y = sensor_2)) + 
  geom_point(size = 2)+ geom_label(aes(label = ID)) +
  geom_abline(slope = 1,intercept = 0) + xlim(0,1) + ylim(0,1) +
  xlab("Handheld Sensor NDVI") + 
  ylab ("Drone Sensor NDVI")

ggplot(all_agg, aes(x = trimble, y = sensor_1)) + 
  geom_point(size = 2)+ geom_label(aes(label = ID))
  22#geom_abline(slope = 1,intercept = 0) + xlim(0,1) + ylim(0,1) +
  xlab("Handheld Sensor NDVI") + 
  ylab ("Drone Sensor NDVI")


## Test models ---"---"---"---"
model_sensor_1 <- lm(formula = trimble ~ sensor_1, data = all_agg) 
model_sensor_2 <- lm(formula =  trimble ~ sensor_2, data = all_agg)


sink(file = "processing/mod_out.txt")
print("Mapir Camera 17 August 2022")
summary(model_sensor_1)

print("MicaSense Camera 17 August 2022")
summary(model_sensor_2)
sink()









