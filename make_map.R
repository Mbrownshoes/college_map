library(gridExtra)
library(grid)
library(ggrepel)
library(png)
library(tidyverse)
library(magick)


cols <- c("FALSE"='lightblue',"other"="#FFFFD4",'TRUE'='white',"Vancouver Island"="#FED98E")
pal <- c(brewer.pal(12, "Set3")[12:1],brewer.pal(3, "Set3")[1:3])


# regions to include/exclude
lm <- c(15,7,4)



#data = filter(cr_inst1,(CR_NUM %in% lm) | (City %in% cty))
## preparing image to insert to BC line graph
img_path <- paste0("img/", "BCID_H_rgb_pos.png")
img <- readPNG(img_path)
g <- rasterGrob(img, interpolate = TRUE)

# mytable <- 
#   cbind(sites=c("site 1","site 2","site 3","site 4"), mydata[10:13,])

tabletheme <- gridExtra::ttheme_minimal(
  core = list(fg_params=list(hjust=0, x=0,cex = 2.0)),
  colhead = list(fg_params=list(cex = 0)),
  rowhead = list(fg_params=list(cex = 0)))

cty_insts <- c('North Vancouver', 'Abbotsford', 'Mission', 'Chilliwack',
              'Agassiz', 'Vancouver','Burnaby')


cty <- c('North Vancouver', 'Abbotsford', 'Mission', 'Chilliwack', 'Victoria','Parksville','Ashcroft'
         , 'Agassiz', 'Vancouver','Burnaby')

s <- cr_inst1 %>% distinct(City,Institution, .keep_all = TRUE) %>% filter(!(CR_NUM %in% lm) & !(City %in% cty_insts) )


# Plot as sf
p1 <- ggplot(cr_simp_alb) +
  geom_sf(aes(fill=CR_NAME),size=.1) +
  geom_point(data = cr_inst1 %>% subset(`Location Description` == 'Main Campus'),aes(x = lon, y = lat),
             shape=5, color="black", size=3, alpha=0.8, stroke = 2, inherit.aes = FALSE) +
  geom_point(data = cr_inst1 %>% subset(`Location Description` != 'Main Campus'),aes(x = lon, y = lat),
             shape=1, color="black", size=3, alpha=0.8, stroke = 2,inherit.aes = FALSE) +
  # geom_label(data=uni.coords,aes(label = unis,x = 270571, y = 1069045, hjust=.2, vjust=.8), size = 10) +
  annotate("text", x = 1670500, y = 1639000, label = "British Columbia Public Post-Secondary \n Institutions",size = 16) +
  annotate("text", x = 1670500, y = 339000, label = "Colours reporesent college regions",size = 9) +
  annotate("text", x = 1670500, y = 279000, label = "Created by BC Stats, Ministry of Jobs, Trade and Technology, April 2018",size = 9) +
  # geom_point(data=NULL,aes(x = 10000, y = 742500),
   # shape=5, color="black", size=6, alpha=0.8,inherit.aes = FALSE) +
  # annotation_custom(tableGrob(data.frame(c('Main Campus', 'Campus')), rows=NULL, theme = tabletheme), xmin=70500, xmax=70600, ymin=739000, ymax=749000) +
  annotation_custom(tableGrob(inst_list, rows=NULL, theme = tabletheme), xmin=10500, xmax=270500, ymin=1139000, ymax=1439000) +

  geom_text(data = (distinct(cr_inst1,City, .keep_all = TRUE) %>%
                      filter(!(CR_NUM %in% lm) & !(City %in% cty) )) ,
            aes(label = City, x = lon, y = lat),size=6,color='black',inherit.aes = FALSE,hjust = 1.1, vjust=1)+
  geom_text_repel(
    data =  cr_inst1 %>% distinct(City,Institution, .keep_all = TRUE) %>% filter(!(CR_NUM %in% lm) & !(City %in% cty_insts) ),
    mapping=aes(label = Short, x = lon, y = lat),
    size=4, box.padding = .5,
    nudge_x =2,
    # nudge_y=9,
    nudge_y = ifelse(s$City == 'Agassiz' | s$City == 'Oliver'| s$City == 'Kamloops'| s$City == 'Lillooet' | s$City == 'Chetwynd', -9, 9)
  ) +
  scale_fill_manual(values = pal,guide = guide_legend(order = 1, title.position = "top")) +
  guides(fill=FALSE)   +
  # labs(title = "British Columbia Public Post-Secondary \n Institutions")+
  theme_minimal() +
  theme(
    # panel.background = element_rect(fill = 'lightblue', colour = 'lightblue'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_line(colour = 'transparent'),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    legend.position = c(0.15, 0.2),
    text = element_text(family = "Verdana"),
    plot.title = element_text(vjust=20,hjust = .89,size = 40),
    # panel.border = element_rect(colour = "black", fill=NA, size=2),
    plot.margin = unit(c(5, 5, 5, 5), "mm")) 
  


cty <- c('North Vancouver', 'Abbotsford', 'Mission', 'Chilliwack', 'Victoria','Parksville'
         , 'Vancouver','Burnaby')

## inset map
# Extent rectangle for inset map(-129.45,-120.45,44.49, 55.99)
p2 <- ggplot(cr_simp_alb) +
  geom_sf(aes(fill=CR_NAME),size=.1) +
  geom_point(data = cr_inst1 %>% subset(`Location Description` == 'Main Campus'),aes(x = lon, y = lat),
             shape=5, color="black", size=3,stroke = 2, alpha=0.8,inherit.aes = FALSE) +
  geom_point(data = cr_inst1 %>% subset(`Location Description` != 'Main Campus'),aes(x = lon, y = lat),
             shape=1, color="black", size=3,stroke = 2, alpha=0.8,inherit.aes = FALSE) +
  geom_text(data = (distinct(cr_inst1,City, .keep_all = TRUE)) %>% 
              filter(!(City %in% c('North Vancouver','Vancouver', 'Burnaby','Agassiz','Langley', 'Richmond'))),
            aes(label = City, x = lon, y = lat),size=6,color='black',inherit.aes = FALSE,hjust = 1.1)+
  # geom_text(data = (distinct(cr_inst1,City,Institution, .keep_all = TRUE)) ,
  #           aes(label = Short, x = lon, y = lat),size=4,color='black',inherit.aes = FALSE,hjust = 0.1)+
  # geom_text_repel(
  #   data =  (distinct(cr_inst1,City, .keep_all = TRUE))  ,
  #   mapping=aes(label = City, x = lon, y = lat),
  # size=6, color='red',box.padding = unit(0.5, "lines")
  # )+
  geom_text_repel(
              data =  cr_inst1 %>%
                filter((CR_NUM %in% lm) | (City %in% cty) & !(CR_NUM %in% c(1,8))),
              mapping=aes(label = Short, x = lon, y = lat),
              size=4, box.padding =0.25,
              nudge_x =2,
              nudge_y = 9
              # nudge_y=9
            )+
  coord_sf(xlim = c(1297571.8, 1193875.7),  ylim = c(449045, 490045))  + 
  labs(title = "Lower Mainland")+
  scale_fill_manual(values = pal,guide = guide_legend(order = 1, title.position = "top")) +
  guides(fill=FALSE)   +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_line(colour = 'transparent'),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    text = element_text(family = "Verdana"),
    plot.margin = unit(c(5, 5, 5, 5), "mm"))

# ggsave("out/colleges.png", width = 40, height = 35.8, units = "in")
plot <- image_read("colleges.png")
# And bring in a logo
logo_raw <- image_read(paste0("img/", "BCID_H_rgb_pos.png"))


png(file="colleges.png", width = 44, height = 34, units = "in", res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.55, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.4, height = 0.4, x = 0.222, y = 0.15) #plot area for the inset map
print(p1,vp=v1) 
grid.raster(logo_raw,x = unit(0.84, "npc"), y = unit(0.96, "npc"), height=unit(.11, "npc"))
print(p2,vp=v2)
dev.off()
