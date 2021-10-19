library(osmdata)
library(sf)
library(ggplot2)
library(ggmap)

# general london boundary box
bbx <- getbb("London, UK")
bbx <- getbb("Oval, London, UK")
bbx <- getbb("London Borough of Lambeth, UK")

#test the bb size with ggmap
my_map <- get_map(bbx, maptype = "toner-background")
ggmap(my_map)

#If you want to make your own boundry box
# -0.13, 51.51, -0.11, 51.52
# min_lon <- -0.510375; max_lon <--0.099715 
# min_lat <- 51.286760; max_lat <- 51.476302
# bbx <- rbind(x = c(min_lon, max_lon),y = c(min_lat,max_lat))
# colnames(bbx) <- c("min","max")


available_tags("highway")

highways <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("primary","secondary", 
                          "tertiary")) %>%
  osmdata_sf()

highways <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()



ggplot() +
  geom_sf(data = highways$osm_lines,
          aes(color=highway),
          size = .4,
          alpha = .65)#+
  #theme_void()



streets <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          aes(color=highway),
          size = .4,
          alpha = .65)#+
  #theme_void()

color_roads <- rgb(0.42,0.449,0.488)
ggplot() +
  geom_sf(data = streets$osm_lines,
          col = color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          col = color_roads,
          size = .6,
          alpha = .8)#+
  # coord_sf(xlim = c(min_lon,max_lon),
  #          ylim = c(min_lat,max_lat),
  #          expand = FALSE)+
 # theme(legend.position = F) #+ theme_void()


#############################################
require(tigris)
counties_MA <- counties(state="MA",cb=T,class="sf",)
counties_MA <- st_crop(counties_MA,
                       xmin=min_lon,xmax=max_lon,
                       ymin=min_lat,ymax=max_lat)
ggplot() + 
  geom_sf(data=counties_MA,fill="gray",lwd=0)+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()
