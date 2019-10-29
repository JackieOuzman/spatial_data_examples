

#Instead of the painful process of performing your spatial analysis in GIS systems like ArcGIS or QGIS 
#and then shuffling your results into another system for analysis you can move your entire spatial analysis workflow into R.

#install.packages("sf")
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)

########################### VECTORS ######################################################################################
#Read in a simple vector

#as shapefile
eden_valley <- st_read(
  "EdenValley_site1GDA_a.shp")
#as kml
fence2_kml <- st_read("fence2.kml")



#Quick plot of all the data 
plot(eden_valley)

#quick plot of one attribute
plot(eden_valley["SymbolID"])

#quick plot of just the coodinates
eden_valley_geo <- st_geometry(eden_valley)
plot(eden_valley_geo)

st_area(eden_valley_geo)
#be careful the results is a UNIT object that requires additional procesing unclass()


#what is the coordinates of my vector?
eden_valley
st_crs(eden_valley)
#you can get the coordinates from a vector to be used to project other data
the_crs <- st_crs(eden_valley, asTEXT = TRUE)
the_crs

#project this onto a new shapefile
fence1 <- st_read(
  "Fence1.shp")

fence1 <- st_transform(fence1, the_crs)


ggplot()+
  geom_sf(data = eden_valley, size = 1 , colour = "black")+
  coord_sf()


some_pts <- read.csv(file = "graph1_data.csv")
str(some_pts)
#trun my cvs file into spatial data
plot_some_pts <- st_as_sf(some_pts, coords = c("POINT_X", "POINT_Y"), crs = the_crs)

plot_some_pts

ggplot() +
  geom_sf(data = eden_valley, size = 1 , colour = "black")+
  geom_sf(data = plot_some_pts) +
  coord_sf()+
  ggtitle("Map")


#can use dplyr function to look at data..
group_by(plot_some_pts, hour ) %>% 
count()

#this is messy lets drop geom
group_by(plot_some_pts, hour ) %>% 
  count() %>% 
  st_set_geometry(NULL)

#another example

regions <- st_read(
  "SLA_selection_Region.shp")
regions

Av_area_state <- group_by(regions, STATE_CODE ) %>%
  summarise(
  count = n(), 
  Ave_Area = round(mean(Area_dd2_), 2)) %>%
  st_set_geometry(NULL)
Av_area_state

Av_area_SLA <- group_by(regions, SLA_NAME11) %>% 
  summarise(
    count = n(),
    Ave_Area = round(mean(Area_dd2_), 2)) %>% 
  st_set_geometry(NULL) %>% 
  arrange(SLA_NAME11)

Av_area_SLA


# Other things...
#1) can join no spatial data to spatial data - need and ID value to do this, eg SLA_Names11



#2) can simply vector - I havent used this...This can speed up analysis or display
regions_simple_tran <- st_transform(regions, the_crs)
regions_simple <- st_simplify(regions_simple_tran, dTolerance = 500, preserveTopology = TRUE) #large number means great simplification

plot(st_geometry(regions))
plot(st_geometry(regions_simple))


#3) sf package is great but the older version is sp and you might want to use functions in sp

# convert between the two
str(regions_simple)
regions_simple_sp <- as(regions_simple, Class = "Spatial")
#str(regions_simple_sp)
#and the other way
str(st_as_sf(regions_simple_sp))


#write out to csv file or other...if will drop the geometry clm

st_write(plot_some_pts, "plot_some_pts.csv")

st_write(plot_some_pts, "plot_some_pts_geo.csv",  layer_options = "GEOMETRY=AS_XY", delete_dsn = TRUE)


##### cal distance

#bring in the VF 

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)
#Try distance tool

plot_some_pts_1 <- mutate(plot_some_pts, 
                                    dist = st_distance(plot_some_pts, fence1))
head(plot_some_pts_1)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)+
  geom_sf(data = plot_some_pts_1)

#### gganimate
library(gganimate)
library(png)
library(gifski)
#install.packages("transformr")
library(transformr)
library(lubridate)

#example data set with more data than before

VF1_InclusionBord_animalID <- read.csv("sp_VF1_InclusionBord_animalID.csv")
#head(VF1_InclusionBord_animalID)
VF1_InclusionBord_animalID$date <- as_date(VF1_InclusionBord_animalID$date)
VF1_InclusionBord_animalID$time <- as_datetime(VF1_InclusionBord_animalID$time, tz="GMT")
VF1_InclusionBord_animalID <- mutate(VF1_InclusionBord_animalID,
                                     hms = hms::as.hms(time, tz="GMT"))
#assign  coords for each of the VF dataframes
sp_VF1_InclusionBord_animalID <- st_as_sf(VF1_InclusionBord_animalID, 
                                          coords = c("X", "Y"), 
                                          crs = the_crs)
sp_VF1_InclusionBord_animalID_20 <- sp_VF1_InclusionBord_animalID %>% 
  filter(date == "2019-05-20") 

#plot my data as a normal map 
p4a <- ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = sp_VF1_InclusionBord_animalID_20, aes(colour = animal_ID))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p4a  

#cool graphing option in R allows you to do a facet wrap
p4a_a <- ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = sp_VF1_InclusionBord_animalID_20, aes(colour = animal_ID))+
  facet_wrap(.~collar)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p4a_a

#cool graphing option in R allows you to animate your graph/map
p4b <- p4a +
  labs( title =   'Date:  {format(as_datetime(frame_time ), tz="GMT")}',
        #title =   'Date:  {format(as_datetime(frame_time, "%b %e"), tz="GMT")}',
        #subtitle = 'Hour: {format(as_datetime(frame_time, "%H"), tz="GMT")}',
        caption = "Frame {frame} of {nframes} ({progress * 100}%)") +
  transition_time(time) +
  shadow_wake(0.3)
animation_20thVf1 <- animate(p4b, duration = 60) 
animation_20thVf1
#anim_save(animation = animation_20thVf1 , filename = "animation_20thVf1.gif")

library(tmap)
#another option here



###########################    Raster data examples    ##################################################################

#With rasters you will aggregate, reclassify, crop, mask and extract.

library(raster)
library(sp)
library(ncdf4)


GS_rain <- raster("GS_rain.tif")
class(GS_rain)
nlayers(GS_rain)
plot(GS_rain)

nc_rain_2018 <- brick("2018.monthly_rain.nc")
nc_rain_2018
class(nc_rain_2018)
nlayers(nc_rain_2018)
plot(nc_rain_2018)

# Get the extent 
extent(GS_rain)
# Determine the number of grid cells 
ncell(GS_rain)



#Raster data can be very big depending on the extent and resolution (grid size). 
#In order to deal with this the raster() and brick() 
#functions are designed to only read in the actual raster values as needed


#The raster values will be read in by default if you perform spatial analysis operations 
#that require it or you can read in the values from a raster manually with the function getValues()

vals <- getValues(GS_rain)
vals
hist(vals)

# Determine the raster resolution
res(GS_rain)

####CROP and MASK#####

#you will want your raster to share an extent with another layer and this is where crop() comes in handy. 
#With crop() you are cropping the raster so that the extent (the bounding box) of the raster matches the extent of the input crop layer. 
#But within the bounding box no masking is done (no raster cells are set to NA).

###bring in data and assign names

nc_rain_2018
mons <- c("Jan", "Feb", "March","April", "May","June", "July", "Aug","Sep", "Oct", "Nov", "Dec")
names(nc_rain_2018) <- mons
nc_rain_2018

#bring in shapefile with extent I want
barrossa_st <- st_read("baroosa_ext_WGS_buff3.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object

plot(nc_rain_2018)
rain_crop <- crop(nc_rain_2018, barrossa_sf)
plot(rain_crop)


#extrcat only values between 1st Oct and 30April
Oct_dec_ <-   subset(rain_crop, 10:12) 
jan_april <- subset(rain_crop, 1:4) 
GS_rain <- stack(Oct_dec_, jan_april) 
plot(GS_rain)

#use function to run raster maths
#sum all the layers in the raster stack
nlayers(GS_rain)
GS_rain


sum_GS_rain1 <- overlay(GS_rain, fun=sum) #can be used to specify layers to use
plot(sum_GS_rain1)
sum_GS_rain1

sum_GS_rain2 <- stackApply(GS_rain, indices = 1, fun=sum) 
#function applied subset return a single value number of layers in the output ratser equals the number of layers in indices
plot(sum_GS_rain2)
sum_GS_rain2

GS_rain
indices <- c(1,1,1,2,2,2,1) #oct nov dec april and jan feb march
indices
sum_GS_rain2_a <- stackApply(GS_rain, indices = indices, fun=sum) 
sum_GS_rain2_a



sum_GS_rain3 <- calc(GS_rain, fun=sum) #cal new raster from another raster using formula 
plot(sum_GS_rain3)
sum_GS_rain3

##### raster examples with larger datasets #######

###create function to help me ####
##function one ####
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}
#function two
function_jan_mean_temp_by_yr <- function(year_input) {
  
  min_1 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          year_input, ".min_temp.nc", sep = ""),varname = "min_temp")
  max_1 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          year_input , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  min <- crop(min_1, barrossa_sf)
  max <- crop(max_1, barrossa_sf)
  
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  daily_mean_temp_jan <- subset(daily_mean_temp, 1:30) #pull out the first 30 days of mean temp ? should this be 31??
  av_jan_mean_temp <- mean(daily_mean_temp_jan)
}
### list of years ####
jax_list <- as.character(c(2000:2018)) #data as string
### loop through my list###
for (i in jax_list) {
  assign(paste0("jan_temp", i), function_jan_mean_temp_by_yr(i))
}  
  
STACK1 <- stack( jan_temp2000,jan_temp2001, jan_temp2002, jan_temp2003, jan_temp2004, jan_temp2005, jan_temp2006,
                  jan_temp2007, jan_temp2008, jan_temp2009, jan_temp2010, jan_temp2011, jan_temp2012,
                  jan_temp2013, jan_temp2014, jan_temp2015, jan_temp2016, jan_temp2017, jan_temp2018)

means_jan_temp <- calc(STACK1, fun = mean, na.rm = T)
means_jan_temp
plot(means_jan_temp)


barrossa_st_extract <- st_read("extract_jan_temp_yrs_WGS.shp")
barrossa_extract_sf <- as(barrossa_st_extract, "Spatial") #convert to a sp object

mean_jan_temp_extract <- extract(STACK1, barrossa_extract_sf, method="simple")

pts_jan_temp_wide <- data.frame(barrossa_extract_sf$POINT_X, barrossa_extract_sf$POINT_Y, mean_jan_temp_extract)
head(pts_jan_temp_wide)

names(pts_jan_temp_wide) <- c("POINT_X", "POINT_Y",  "2000",
                              "2001", "2002", "2003", "2004", "2005", "2006",
                              "2007", "2008", "2009", "2010", "2011", "2012",
                              "2013", "2014", "2015", "2016", "2017", "2018")

pts_jan_temp_narrow <- gather(pts_jan_temp_wide, key = "year", value = "Mean_Jan_temp", `2000`:`2018` )
pts_jan_temp_narrow <- mutate(pts_jan_temp_narrow, year_as_double = as.double(year))
ggplot(pts_jan_temp_narrow, aes(factor(year_as_double), Mean_Jan_temp))+
  geom_boxplot()+
  #geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ #straight line regression
  geom_smooth(color="black", aes(group=1))+ #smooth line
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Mean Jan temperature",
       title = "Sample points over the Barossa",
       caption = "First the mean January temperature is calculated for each pixel by year, then the values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
       ")
