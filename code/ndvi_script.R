## For mapir study we need to

# -- calculate NDVI for four rasters, masked by roi
# -- sample NDVI at points.


library(terra)
# For individual images
mapir <- list.files(
  path = "data/individualimages/together-calibrated/",
  pattern = "*mapir_modified.tif",
  full.names = TRUE
)

mica <- list.files(
  path = "data/individualimages/together-calibrated/",
  pattern = "*mica_modified.tif",
  full.names = TRUE
)

for (i in 1:2) {
  mic_r <- rast(mica[i])
  map_r <- rast(mapir[i]) |> crop(mic_r)
  # work out NDVI
  mic_vi <- (mic_r[[4]] - mic_r[[3]]) / (mic_r[[4]] + mic_r[[3]])
  map_vi <- (map_r[[3]] - map_r[[1]]) / (map_r[[3]] + map_r[[1]])

  writeRaster(mic_vi, filename = paste0("data/individualimages/ndvi/mica_scene_", i, ".tif"),overwrite = TRUE)
  writeRaster(map_vi, filename = paste0("data/individualimages/ndvi/mapir_scene_", i, ".tif"),overwrite = TRUE)
}


# load rasters
mapir <- list.files(path = "data/georeferenceddata/for_datum_JGD2011",
                      pattern = "mapir",
                      full.names = TRUE)

mica <- list.files(path = "data/georeferenceddata/for_datum_JGD2011",
                    pattern = "mica",
                    full.names = TRUE)
#
roi <- vect("data/vectordata/clean_data_area.gpkg") #   "/data/vectordata/clean_data_area.gpkg"


samplepoints <- vect("data/vectordata/pointbuffers/10cmbuffers5000.shp")
#
#   r.temp <- rast(mica[2]) |> mask(roi)
#   # work out NDVI
#   ndvi.temp <- (r.temp[[4]] - r.temp[[3]])/(r.temp[[4]] + r.temp[[3]])
#   writeRaster(ndvi.temp,filename = "C:/Users/Ram/Documents/Stephan/temp/ndvimaps_mica0817.tif")
#
#
#
#
#   #roi <- vect() #   "/data/vectordata/clean_data_area.gpkg"
#
#   file_paths <- "data/individualimages/together-calibrated/1_mapir_modified.tif"
#
#   r.temp <- rast(fp) |> mask(roi)
#   # work out NDVI
#   ndvi.temp <- (r.temp[[3]] - r.temp[[1]])/(r.temp[[3]] + r.temp[[1]])
#   writeRaster(ndvi.temp,filename = "C:/Users/Ram/Documents/Stephan/temp/ndvimaps_mapirJPG0817.tif", overwrite = T)
#   plot(ndvi.temp)
#
