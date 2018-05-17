s <- cr_inst1 %>% distinct(City,Institution, .keep_all = TRUE) %>% filter(!(CR_NUM %in% lm) & !(City %in% cty) )

ggplot(cr_simp_alb) +
  geom_sf(aes(fill=CR_NAME),size=.1) +
  geom_point(data = cr_inst1 %>% subset(`Location Description` == 'Main Campus'),aes(x = lon, y = lat),
             shape=5, color="black", size=2, alpha=0.8, stroke = 3, inherit.aes = FALSE) +
  geom_point(data = cr_inst1 %>% subset(`Location Description` != 'Main Campus'),aes(x = lon, y = lat),
             shape=1, color="black", size=2, alpha=0.8, stroke = 3,inherit.aes = FALSE) +
  geom_text(data = (distinct(cr_inst1,City, .keep_all = TRUE) %>%
                      filter(!(CR_NUM %in% lm) & !(City %in% cty) )) ,
            aes(label = City, x = lon, y = lat),size=2,color='black',inherit.aes = FALSE,hjust = 1.3, vjust=1)+
  geom_text_repel(
    data =  cr_inst1 %>% distinct(City,Institution, .keep_all = TRUE) %>% filter(!(CR_NUM %in% lm) & !(City %in% cty) ),
    mapping=aes(label = Short, x = lon, y = lat),
    size=2, box.padding = .5,
    nudge_x =2,
    nudge_y = ifelse(s$City == 'Salmon Arm', -9, 9)
  )
