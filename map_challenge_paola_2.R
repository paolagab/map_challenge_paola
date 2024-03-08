# map_challenge_paola_2
# Author: Paola Gabasa (@paolagab)
# Date: March 8, 2024
# Description: This script demonstrates how to create a map of the Atl. Ocean combining bathymetric data and other elements using R.

#--------------------------------------------------------------------------------
#                                   Setup
#--------------------------------------------------------------------------------

# Set computer
cpu <- "pc-paola" 

# Load required packages
pacman::p_load("magrittr", "rnaturalearth", "ggplot2","marmap", "terra", "tidyterra", "sf", "sfheaders", "ggforce", install=FALSE)

# Set main data path
if(cpu == "pc-paola") main_dir <- "C:/Users/paoga/Documents/AÑO 2023-2024/REDUCE/map_challenge_paola2/map_challenge_paola"

# Create data paths
input_dir <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)

output_dir <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)



#--------------------------------------------------------------------------------
#                             Download needed files
#--------------------------------------------------------------------------------

# There is custom-made shape file that we need for this project that corresponds to a specific study area (REDUCE project)
# All the files needed to read this shape file can be downloaded from github (https://github.com/paolagab/map_challenge_paola/tree/main/study_area).
# You should download the files directly from github (it's easier) and save them to the local input folder you just created.
# We'll use them in step 5


#--------------------------------------------------------------------------------
#                                 Create map
#--------------------------------------------------------------------------------

# See README.md file for map description




# 1) Get map of the world in our desired projection

  # Get world map
  world_map <- ne_countries(returnclass = "sf", scale = 50) 

  # Take a look  
  ggplot(world_map)+
    geom_sf()+
    theme_classic()

  # Define our projection for the map (Lambert Azimuthal Equal Area projection). This projection is suitable for mapping areas with minimal distortion around a central point.
  projection3D<-"+proj=laea +lat_0=0 +lon_0=-30 +x_0=43210000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  # Transform the projection of our map of the world (in this case,from crs = 4326 -> crs = projection3D)
  world_map <- st_transform(world_map, projection3D)
  
  # Take a look. It should give the impression of a 3D globe  
  ggplot(world_map)+
    geom_sf()+
    theme_classic()

# 2) Add bathymetry layer

  #Get bathymetric data for the world from NOAA database 
  #We'll most likely get a warning about invalid latitudes for some values. This is because our projection doesn't work well for specific regions, since it is designed for our target area (Atlantic Ocean) 
  bat_whole <- getNOAA.bathy(lon1=-180,lon2=180,lat1=90,lat2=-90, res = 10)
 
  # Transform it into a dataframe, rename columns, and filter by depth<0.1 so we only get depth.This may not be ideal for inland areas that are below 0.
  bat_whole <- as.xyz(bat_whole) %>% 
  rename(longitude=V1,latitude=V2,depth=V3) %>% 
  filter(depth<0.1) 
  
  # Transform it into a raster file with our custom projection (in this case, from crs = 4326 -> crs = projection3D)
  # We'll most likely get a warning about invalid latitudes for some values. This is because our projection doesn't work well for specific regions, since it is designed for our target area (Atlantic Ocean) 
  bat_whole <- as_spatraster(bat_whole, xycols = c(1:2), crs=4326) %>%  
  project(Projection3D)
 
  # Take a look 
  ggplot()+
  geom_spatraster(data=bat_whole, aes(fill=depth))+
  theme_classic()
  
# 3) Let's zoom in on a specific area

  # Create a dataframe containing the bounding box coordinates of our target area (in this case, the mid Atlantic ridge)
  zoom_map<-data.frame(
  lon = c(-80,50,-80,50),  
  lat = c(-45,-45,45,45))
  
  # Transform its projection. To perform spatial operations or manipulations, we need to convert the df containing the spatial data to an sf object before converting it back to a data frame
  zoom_map <- st_as_sf(zoom_map, coords=c("lon","lat"), crs = 4326) %>% 
  st_transform(projection3D) %>% 
  sf_to_df(fill=T)

# 4) Combine different layers
  
  # Combine bathymetry data and our world map, but already for the cropped area
  ggplot()+
  geom_spatraster(data=bat_whole, aes(fill=depth))+
  geom_sf(data = world_map)+
  coord_sf(xlim=c(min(zoom_map$x),max(zoom_map$x)),  # to specify the limits of our map, based on our zoomed-in area
           ylim=c(min(zoom_map$y),max(zoom_map$y)))+
  theme_classic()

# 5) Read and add our specific study area to the map. 
  
  # Read the shape file that we previously downloaded and that is now located in our input folder
  study_area <- st_read(paste0(input_dir, "/REDUCE_NEW_study_area.shp"))

  # Transform the projection of the file into our custom projection (in this case, from crs = 4326 -> crs = projection3D)
  study_area <- st_transform(study_area, projection3D)

 # Add study area to the map. Take a look.
  ggplot()+
  geom_spatraster(data= bat_whole,aes(fill=depth))+
  geom_sf(data=world_map) +
  geom_sf (data=study_area, fill="transparent", linewidth= 0.8) +
  coord_sf(xlim=c(min(zoom_map$x),max(zoom_map$x)),
             ylim=c(min(zoom_map$y),max(zoom_map$y)))+
  theme_classic()

# 6) Add interesting points to the map
  
  # Create a dtaframe containing specific geographical points andt heir coordinates
  interesting_points<-data.frame(
    lon = c(-27.862,-14.3737,-15.7315,-16.751,
            -23.777,6.739,-5.705),  
    lat = c(38.723,-7.9481,28.620,32.860,15.9495
            ,0.4535,-15.9697),
    islands= c("Azores","Ascension","Canarias","Madeira"
               ,"Cabo Verde","São Tomé and Príncipe","Saint Helena"))
  
  # Transform its projection. To perform spatial operations or manipulations, we need to convert the df containing the spatial data to an sf object before converting it back to a data frame
    interesting_points <- st_as_sf(interesting_points, coords=c("lon","lat"), crs = 4326) %>% 
    st_transform(projection3D) %>% 
    sf_to_df(fill=T)
  
  
# 7) Plot everything together (world map, bathymetry layer, study area, interesting points), add labels and improve the aesthetics

    # To export it as a pdf file, run the following two comment lines, run the plot, and then the one after the plot (dev.off()). The pdf containing the map should be appear in the output folder we created at the beginning
    graphics.off()
    pdf( paste0(output_dir, "/REDUCE_study_area.pdf"))

    
    ggplot()+
    geom_spatraster(data=bat_whole,aes(fill=depth))+
    geom_sf(data=world_map, fill= "black", colour = "black")+
    geom_sf (data=study_area, fill= "black", alpha=0.2, colour = "black", linewidth=0.8)+ #alpha is used for the level of transparency
    geom_mark_ellipse(data=interesting_points,          # geo_mark_ellipse is used to circle our interesting points and label them
                      aes(x=x,
                          y=y,
                          label = islands,
                          group=islands), linewidth = 0.75,
                      show.legend=F, alpha=0.8,
                      label.buffer = unit(0, "mm"),     # label.buffer is used to adjust the size of the region around the mark where labels cannot be placed
                      label.fill = "grey80", label.fontsize = 7,
                      con.type = "none") +
    coord_sf(xlim=c(min(zoom_map$x),max(zoom_map$x)),
             ylim=c(min(zoom_map$y),max(zoom_map$y)))+
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
 
  dev.off() # close the pdf device
