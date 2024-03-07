#For the %>% operator
library (magrittr)


#Get world map
#install.packages("rnaturalearth")
library (rnaturalearth)

world_map <- ne_countries(returnclass = "sf",scale = 50) 

# Plot world coastlines 
library (ggplot2)
ggplot(world_map)+
  geom_sf()

#Define our projection for the map (Lambert Azimuthal Equal Area projection). This projection is suitable for mapping areas with minimal distortion around a central point.
Projection3D<-"+proj=laea +lat_0=0 +lon_0=-30 +x_0=43210000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


#Get bathymetric data for the world from NOAA as a data frame, and then transform it as a raster file. 
#We'll get a warning ababout invalid latitudes for some values. This is because aour projection doesn't work well for specific regions, since it is designed for our target area (Atlantic Ocean) 
#install.packages("marmap")
#install.packages("terra")
#install.packages("tidyterra")
#install.packages(sf)
library(marmap)
library(terra)
library(tidyterra)
library (sf)


bat_whole <- getNOAA.bathy(lon1=-180,lon2=180,lat1=90,lat2=-90, res = 10)

Bathy_Whole_World <- as.xyz(bat_whole) %>% 
  rename(Longitude=V1,Latitude=V2,Depth=V3) %>% 
  filter(Depth<0.1) %>% 
  as_spatraster(xycols = c(1:2),crs=4326) %>%  
  project(Projection3D)

#Let's zoom in on the mid atlantic ridge
#install.packages("sfheaders")
library(sfheaders)
library(ggforce)

Crop_MAR<-data.frame(
  lon = c(-80,50,-80,50),  
  lat = c(-45,-45,45,45)) %>% 
  st_as_sf(coords=c("lon","lat"), crs = 4326) %>% 
  st_transform(Projection3D) %>% 
  sf_to_df(fill=T)

#Combine bathymetry data and our world map, but already for the cropped area
world_map %>% 
  st_transform(Projection3D) %>%
  ggplot()+
  geom_spatraster(data=Bathy_Whole_World,aes(fill=Depth))+
  geom_sf()+
  scale_fill_viridis_c(na.value = NA) +
  coord_sf(xlim=c(min(Crop_MAR$x),max(Crop_MAR$x)),
           ylim=c(min(Crop_MAR$y),max(Crop_MAR$y)))+
  theme_classic()

#Time to get our study area (REDUCE area)
# Read the shapefile
study_area <- st_read("C:/Users/paoga/Documents/AÑO 2023-2024/REDUCE/study_area/REDUCE_NEW_study_area.shp")
 
 # Add it to the map
  world_map %>% 
  st_transform(Projection3D) %>%
  ggplot()+
  geom_spatraster(data=Bathy_Whole_World,aes(fill=Depth))+
  geom_sf(data=world_map, fill= "black", colour = "black")+
  geom_sf (data=study_area, fill="transparent", linewidth= 1)+
  scale_fill_viridis_c(na.value = NA) +
  coord_sf(crs= Projection3D,xlim=c(min(Crop_MAR$x),max(Crop_MAR$x)),
           ylim=c(min(Crop_MAR$y),max(Crop_MAR$y)))+
  theme_classic()

  # Add interesting points
  InterestingPoints<-data.frame(
    lon = c(-27.862,-14.3737,-15.7315,-16.751,
            -23.777,6.739,-5.705),  
    lat = c(38.723,-7.9481,28.620,32.860,15.9495
            ,0.4535,-15.9697),
    Islands= c("Azores","Ascension","Canarias","Madeira"
               ,"Cabo Verde","São Tomé and Príncipe","Saint Helena"))%>% 
    st_as_sf(coords=c("lon","lat"), crs = 4326) %>% 
    st_transform(Projection3D) %>% 
    sf_to_df(fill=T)
  
  
  world_map %>% 
    st_transform(Projection3D) %>%
    ggplot()+
    geom_spatraster(data=Bathy_Whole_World,aes(fill=Depth))+
    geom_sf(data=world_map, fill= "black", colour = "black")+
    geom_sf (data=study_area, fill= "black", alpha=0.2, colour = "black", linewidth=0.8)+
    geom_mark_ellipse(data=InterestingPoints,
                      aes(x=x,
                          y=y,
                          label = Islands,
                          group=Islands),show.legend=F,
                      alpha=0.8,
                      label.buffer = unit(1, "mm"),
                      label.fill = "grey80", label.fontsize = 8)+
    scale_fill_viridis_c(na.value = NA) +
    coord_sf(crs= Projection3D, xlim=c(min(Crop_MAR$x),max(Crop_MAR$x)),
             ylim=c(min(Crop_MAR$y),max(Crop_MAR$y)))+
    scale_fill_gradientn(colours=c("#5e24d6","#22496d","#042f66","#054780","#1074a6",
                                   "#218eb7","#48b5d2","#72d0e1","#9ddee7","#c6edec"),
                         breaks=c(0,-2500,-5000,-7500),
                         labels=c("0","2,500","5,000","7,500"),
                         na.value = NA)+
    labs(x="Longitude",y="Latitude", 
         caption = "Bathymetry data retrieved from NOAA database",
         title = "Most Important Islands of the REDUCE Project Study Area",fill="Depth (m)")+
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 1),
          panel.background = element_blank(), # bg of the panel
          panel.grid.major = element_line(linetype = "dotted",
                                          colour="grey30",
                                          linewidth=0.25),
          panel.ontop = TRUE,
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
 

  
