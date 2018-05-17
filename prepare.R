
library(readr) #data import
library(dplyr) #data munging
library(ggplot2)
# library(bcmaps) #for BC regional district map
library(sf) #sf map object
library(stringr) #modifying character strings
library(units) #unit conversion
library(rmapshaper)
library(RColorBrewer) #for colour palette
# library(rnaturalearth)
library(sp)
library(mapview)

# library(raster)

# crop spatial object
# https://stackoverflow.com/questions/13982773/crop-for-spatialpolygonsdataframe/13986029#13986029

## college regions
cr_simp <- st_read("CR/CR_2012.shp")
cr_simp <- ms_simplify(cr_simp)
# cr_simp <- ms_simplify(nc)
# add projection
p4s <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
st_crs(cr_simp) <- p4s

## Colleges
inst <- read_csv("publicandprivateandnonbcdegreegrantingpostsecondaryinstitutions.csv")


# convert to sf data frame
inst <- st_as_sf(
  inst, 
  coords = c('Longitude', 'Latitude'),
  crs = "+init=epsg:4326",
  remove = FALSE
) 

# Data exploration
mapview(cr_simp['CR_NUM'])

inst <- inst %>%   
  filter(`Institution Type` == "B.C. Public")


# BC Albers projection
cr_simp_alb<-st_transform(cr_simp, 3005)
inst_alb<-st_transform(inst, 3005) 

# This is the key!!. Join the two
#http://www.hafro.is/~einarhj/education/tcrenv2017/b_points_in_polygons.html

# cr_inst <- st_join(cr_simp_alb, inst_alb,join = st_intersects,left = TRUE, suffix = c(".x", ".y")) %>% 
#   ms_simplify()

cr_inst1 <- st_join(inst_alb, cr_simp_alb,join = st_intersects) 

# get coordinates to plot as geom_point
seal_coords <- do.call(rbind, st_geometry(cr_inst1)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

cr_inst1 <- dplyr::bind_cols(cr_inst1,seal_coords)

# %>% 
#   ms_simplify()

# but not we need those point locations for the colleges
# cd_plot <- left_join(cr_inst, inst_alb, by = c("Address"))



# converting sf to sp and then to df
all <- as(cr_inst, "Spatial")
plotalldf <- fortify(all,region = "CR_NAME") 

## joining population summary tabular and spatial data
cd_plot <- inner_join(plotalldf, as.data.frame(cr_inst), by = c("id" = "CR_NAME"))


## converting sf to sp and then to df
plotmap <- as(cr_simp_alb, "Spatial")
plotmapdf <- fortify(plotmap, region = "CR_NAME") 


plotlocs <- as(inst_alb, "Spatial")
# plotlocsdf <- fortify(plotlocs, region = "Location") 
plotlocsdf <- as.data.frame(plotlocs)
# plotlocsdf <- distinct(plotlocsdf,City, .keep_all = TRUE)

#Institution labels
# unis <- c("UBC \n UVIc \n SFU") # for text labels
# unis.coords <- data.frame(x=c(270571), y=c(1069045))
# n <- length(nyteams)
# r <- 0.3
# uni.coords <- data.frame(x=+unis.coords$x,y=unis.coords$y)
#create abbreviations
cr_inst1 <-  cr_inst1 %>% 
  mutate(Short = ifelse(Institution == 'British Columbia Institute of Technology', 'BCIT',
                        ifelse(Institution == 'Camosun College', 'CAM',
                               ifelse(Institution == 'Capilano University', 'CAPU',
                                      ifelse(Institution == 'College of New Caledonia', 'CNC',
                                             ifelse(Institution == 'College of the Rockies', 'COTR',
                                                    ifelse(Institution == 'Douglas College', 'DOUG',
                                                           ifelse(Institution == 'Emily Carr University of Art and Design', 'ECUAD',
                                                                  ifelse(Institution == 'Kwantlen Polytechnic University', 'KWN',
                                                                         ifelse(Institution == 'Justice Institute of British Columbia', 'JIBC',
                                                                                ifelse(Institution == 'Langara College', 'LANG',
                                                                                       ifelse(Institution == 'North Island College', 'NIC',
                                                                                              ifelse(Institution == 'Northern Lights College', 'NLC',
                                                                                                     ifelse(Institution == 'Nicola Valley Institute of Technology', 'NVIT',
                                                                                                            ifelse(Institution == 'Northwest Community College', 'NWCC',
                                                                                                                   ifelse(Institution == 'Okanagan College', 'OKAN',
                                                                                                                          ifelse(Institution == 'Royal Roads University', 'RRU',
                                                                                                                                 ifelse(Institution == 'Selkirk College', 'SEL',
                                                                                                                                        ifelse(Institution == 'Thompson Rivers University', 'TRU',
                                                                                                                                               ifelse(Institution == 'University of the Fraser Valley', 'UFV',
                                                                                                                                                      ifelse(Institution == 'Vancouver Community College', 'VCC',
                                                                                                                                                             ifelse(Institution == 'Vancouver Island University', 'VIU',
                                                                                                                                                                    ifelse(Institution == 'University of British Columbia', 'UBC',
                                                                                                                                                                           ifelse(Institution == 'University of Northern British Columbia', 'UNBC',
                                                                                                                                                                                  ifelse(Institution == 'Simon Fraser University', 'SFU',"UVIC")))))))))))))))))))))))))
#make Main campus type
cr_inst1 <- cr_inst1 %>% 
  mutate(`Location Description` =ifelse(grepl('Main Campus', Location), "Main Campus", `Location Description`))
# 
inst_list <- distinct(cr_inst1,Institution, .keep_all = TRUE) %>% 
  select(Short,Institution) %>% 
  st_set_geometry(NULL)

# inst_list$Short <- c('BCIT','CAM','CAPU','CNC','COTR','DOUG','ECUAD','KWN','JIBC','LANG','NIC','NLC','NVIT','NWCC','OKAN','RRU','SEL','TRU','UFV','VCC','VIU','UBC','UNBC','SFU','UVIC')

## Combine regions and loctations into one df  

# %>% 
  # mutate(VI = ifelse(id== 'Vancouver Island','Vancouver Island','other'))


#Get centroids of regions for labels
# centroids.df <- as.data.frame(coordinates(plotmap))
# names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column names
# idList <- plotmap@data$CR_NAME
# places <- data.frame(id = idList, centroids.df)
## Plot as sp -> data.frame objects
ggplot(data = plotalldf, aes(x = long, y = lat,group = group, fill=id))+
  geom_polygon(alpha = 0.9) +
  geom_path(colour = "grey50", size = 0.3) +
  # coord_fixed(xlim = c(1370571.8, 1003875.7),  ylim = c(369045, 649045), ratio = 0.79)  + 
  scale_fill_manual(values = pal,guide = guide_legend(order = 1, title.position = "top")) +
  guides(fill=FALSE)  +
  geom_point(data=subset(plotlocsdf,Economic.Development.Region== 'Mainland/Southwest'), aes(x = coords.x1, y = coords.x2), shape=1, color="red", size=2, alpha=0.8,inherit.aes = FALSE) +
  geom_text(data = (distinct(plotlocsdf,City, .keep_all = TRUE) %>% 
                      subset(Economic.Development.Region != 'Mainland/Southwest')) ,aes(label = City, x = coords.x1, y = coords.x2),size=2.5,color='red',inherit.aes = FALSE) +
  labs(title = "British Columbia Public Post-Secondary \n Institutions") +
  theme_minimal() +
  theme(
    # panel.background = element_rect(fill = 'lightblue', colour = 'lightblue'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    # legend.title = element_text(size = 14, face = "bold"),
    # legend.text = element_text(size = 13),
    # legend.position = c(0.15, 0.2),
    text = element_text(family = "Verdana"),
    plot.title = element_text(vjust=-13,hjust = .99,size = 12),
    plot.margin = unit(c(5, 5, 5, 5), "mm"))
# geom_polygon(data=subset(plotmapdf,id='Vancouver Island'),fill='blue')

# Data exploration
mapview(cr_inst1['City'])

## plotting chloropleth
## creating a colour brewer palette from http://colorbrewer2.org/
cols <- c("FALSE"='lightblue',"other"="#FFFFD4",'TRUE'='white',"Vancouver Island"="#FED98E")
pal <- c(brewer.pal(12, "Set3")[12:1],brewer.pal(3, "Set3")[1:3])


# regions to include/exclude
lm <- c(15,7,4)
cty <- c('North Vancouver', 'Abbotsford', 'Mission')

#data = filter(cr_inst1,(CR_NUM %in% lm) | (City %in% cty))

# Plot as sf
ggplot(cr_simp_alb) +
  geom_sf(aes(fill=CR_NAME),size=.1) +
  geom_point(data = cr_inst1,aes(x = lon, y = lat),
             shape=1, color="red", size=1, alpha=0.8,inherit.aes = FALSE) +
  geom_text(data = (distinct(cr_inst1,City, .keep_all = TRUE) %>% 
                      filter(!(CR_NUM %in% lm) & !(City %in% cty) )) ,
            aes(label = City, x = lon, y = lat),size=2.5,color='red',inherit.aes = FALSE,hjust = 1.1)+

  # geom_polygon(data = ocean, aes(long, lat, group = group,fill=hole),inherit.aes = FALSE) +
    # geom_sf(alpha = 0.9) +
  # geom_path(colour = "grey50", size = 0.3) +
  # coord_fixed(xlim = c(1370571.8, 1003875.7),  ylim = c(369045, 649045), ratio = 0.79)  +
  scale_fill_manual(values = pal,guide = guide_legend(order = 1, title.position = "top")) +
  guides(fill=FALSE)   +
  theme_minimal() +
  theme(
    # panel.background = element_rect(fill = 'lightblue', colour = 'lightblue'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_line(colour = 'transparent'),
    # legend.title = element_text(size = 14, face = "bold"),
    # legend.text = element_text(size = 13),
    # legend.position = c(0.15, 0.2),
    text = element_text(family = "Verdana"),
    plot.title = element_text(vjust=-13,hjust = .99,size = 12),
    plot.margin = unit(c(5, 5, 5, 5), "mm"))
  
## inset map
# Extent rectangle for inset map(-129.45,-120.45,44.49, 55.99)
pol<-data.frame(xmin=-130,xmax=120 ,ymin=48 ,ymax=52)
ggplot(cr_simp_alb) +
  geom_sf(aes(fill=CR_NAME),size=.1) +
  geom_point(data = cr_inst1,aes(x = lon, y = lat),
             shape=1, color="red", size=1, alpha=0.8,inherit.aes = FALSE) +
  geom_text(data = (distinct(cr_inst1,City, .keep_all = TRUE) %>% 
                      filter(!(CR_NUM %in% lm) & !(City %in% cty) )) ,
            aes(label = City, x = lon, y = lat),size=2.5,color='red',inherit.aes = FALSE,hjust = 1.1)+
  coord_sf(xlim = c(1370571.8, 1003875.7),  ylim = c(369045, 649045))  + 
  
  # geom_polygon(data = ocean, aes(long, lat, group = group,fill=hole),inherit.aes = FALSE) +
  # geom_sf(alpha = 0.9) +
  # geom_path(colour = "grey50", size = 0.3) +
  # coord_fixed(xlim = c(1370571.8, 1003875.7),  ylim = c(369045, 649045), ratio = 0.79)  +
  scale_fill_manual(values = pal,guide = guide_legend(order = 1, title.position = "top")) +
  guides(fill=FALSE)   +
  theme_minimal() +
  theme(
    # panel.background = element_rect(fill = 'lightblue', colour = 'lightblue'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_line(colour = 'transparent'),
    # legend.title = element_text(size = 14, face = "bold"),
    # legend.text = element_text(size = 13),
    # legend.position = c(0.15, 0.2),
    text = element_text(family = "Verdana"),
    plot.title = element_text(vjust=-13,hjust = .99,size = 12),
    plot.margin = unit(c(5, 5, 5, 5), "mm"))


# plot(rd_plot)

## saving plots as SVG

## create a folder to store the output plots
if (!exists("out")) dir.create('out', showWarnings = FALSE)

ggsave("out/colleges.png", width = 40, height = 35.8, units = "cm")

# svg_px("out/VI_rd.svg", width = 650, height = 550)
plot(rd_plot)
dev.off()


