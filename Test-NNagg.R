library(sf)
library(raster)
library(stars)
library(dplyr)
library(tmap)


#NN Data Loading
nn.raster <- stack("Data/NN/nn_21_base.grd")
nn.spatial <- stack("Data/NN/nn_21_spatialcv.grd")
nn.out <- stack("Data/NN/nn_21_outlier.grd")
nn.names <- read.csv("Data/NN_Raster_Names_New.csv")
names(nn.raster) <- nn.names$nn_names
names(nn.spatial) <- nn.names$nn_names
names(nn.out) <- nn.names$nn_names

head(nn.out)
plot(nn.out)
str(nn.out)
names(nn.out)
summer <- c("NN_6_14","NN_7_14","NN_8_14",
            "NN_6_15","NN_7_15","NN_8_15",
            "NN_6_16","NN_7_16","NN_8_16",
            "NN_6_17","NN_7_17","NN_8_17",
            "NN_6_18","NN_7_18","NN_8_18")
summer.nn <- subset(nn.out,summer)
names(summer.nn)
str(summer.nn$NN_6_14)
summer.test <- mean(summer.nn)

plot(summer.test)

large.area <- st_read("Data/LargeAreaCounties")

tm_shape(summer.test) + tm_raster() +
  tm_shape(large.area) + tm_borders(alpha = 0.5)


pmMean = raster::extract(summer.test, large.area, fun = mean, na.rm = TRUE)
head(pmMean)

large.area <-
  large.area %>% mutate(
    pmMeanSmr = raster::extract(summer.test, large.area, fun = mean, na.rm = TRUE),
    pmMaxSmr = raster::extract(summer.test, large.area, fun = max, na.rm = TRUE),
    pmMinSmr = raster::extract(summer.test, large.area, fun = min, na.rm = TRUE)
  )

head(large.area)
str(large.area$pmMax)
range(large.area$pmMax)
range(large.area$pmMean)

tm_shape(large.area) + tm_polygons("pmMeanSmr")
  