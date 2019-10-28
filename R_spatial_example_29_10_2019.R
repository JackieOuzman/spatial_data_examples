
#install.packages("sf")
library(sf)
library(ggplot2)
library(dplyr)

#Read in a simple vector
eden_valley <- st_read(
  "W:/VF/Eden_Valley/VF_Boundary/EdenValley_site1GDA_a.shp")

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
  "W:/VF/Eden_Valley/VF_Boundary/Fence1.shp")

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


# Other things...
#1) can join no spatial data to spatial data - need and ID value to do this



#2) can simply vector - I havent used this...This can speed up analysis or display
regions <- st_read(
  "SLA_selection_Region.shp")
regions
regions_simple_tran <- st_transform(regions_simple, the_crs)
regions_simple <- st_simplify(regions_simple_tran, dTolerance = 100, preserveTopology = TRUE) #large number means great simplification

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
