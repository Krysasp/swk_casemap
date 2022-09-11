library(sf)
library(glue)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(ggforce)
library(readr)
library(dplyr)
library(scales)
library(ggtext)
library(ggthemes)
library(gridExtra)
library(viridis)
library(colorspace)
options(scipen = 999)
options(digits=6)


swk_sf <- st_read("swk_divmap_2022.shp")
metamap <- read_csv("metadata.csv")
# Map base

#write.csv(case_merge,"swk_maps/swk_sf_details.csv", row.names = FALSE)

# CENTROIDES: Colocar el nombre de cada departamento en el mapa
## Creamos el centroide
swk_sf <- swk_sf %>% mutate(centroid = map(geometry, st_centroid), 
                            coords = map(centroid, st_coordinates), 
                            coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 2))
swk_sf <- swk_sf[-c(6:15)]
target <- c("Kuching","Samarahan","Serian","Sri Aman","Betong","Sarikei","Sibu","Mukah","Bintulu","Kapit","Miri","Limbang")
swk_sf <- swk_sf[match(target, swk_sf$ADM2_EN),]
swk_sf2 <- do.call("rbind", replicate(17, swk_sf, simplify = FALSE))
swk_sf2 <- tibble::rowid_to_column(swk_sf2, "ID")
metamap <- metamap[-c(2)]
swk_tab <- left_join(swk_sf2, metamap, by = "ID") %>%
  select(OBJECTID,ADM2_EN,coords,case_prop,coords_x,coords_y,case_per100k,col_date,month,seq_sum,population,geometry) %>%
  mutate(col_date = as.Date(col_date, format = "%m/%d/%Y"))
#swk_tab$month<-month(swk_tab$col_date, label = TRUE)


#glimpse(swk_tab)
#swk_sf[order(swk_sf$ADM2_EN), ]
#swk_sf <- do.call("rbind", replicate(4, swk_sf, simplify = FALSE))

ggplot(data = swk_sf) +
  geom_sf(fill="skyblue3", color="black", alpha = 0.7)+ 
  geom_text_repel(force=1.0,mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5)
ggsave("swk_maps/swk_map_centroid.png")
swk_tab1 <- subset(swk_tab, col_date == as.Date("2021/1/31"))
swk_tab2 <- subset(swk_tab, col_date == as.Date("2021/2/28"))
swk_tab3 <- subset(swk_tab, col_date == as.Date("2021/3/31"))
swk_tab4 <- subset(swk_tab, col_date == as.Date("2021/4/30"))
swk_tab5 <- subset(swk_tab, col_date == as.Date("2021/5/31"))
swk_tab6 <- subset(swk_tab, col_date == as.Date("2021/6/30"))
swk_tab7 <- subset(swk_tab, col_date == as.Date("2021/7/31"))
swk_tab8 <- subset(swk_tab, col_date == as.Date("2021/8/31"))
swk_tab9 <- subset(swk_tab, col_date == as.Date("2021/9/30"))
swk_tab10 <- subset(swk_tab, col_date == as.Date("2021/10/31"))
swk_tab11 <- subset(swk_tab, col_date == as.Date("2021/11/30"))
swk_tab12 <- subset(swk_tab, col_date == as.Date("2021/12/31"))
swk_tab13 <- subset(swk_tab, col_date == as.Date("2022/1/31"))
swk_tab14 <- subset(swk_tab, col_date == as.Date("2022/2/28"))
swk_tab15 <- subset(swk_tab, col_date == as.Date("2022/3/31"))
swk_tab16 <- subset(swk_tab, col_date == as.Date("2022/4/28"))

#### mapa con colores por grupos

case_ranges <- c(
  "<100", "101-500","501-1,000","1,001-5,000", "5,001-10,000", "10,000-50,000","50,000-100,000",">100,000")

swk_mapdata <- 
  swk_tab %>%
  mutate(
    case_range = factor(case_when(
      `case_prop` < 100 ~ case_ranges[1],
      between(`case_prop`, 1001, 500) ~ case_ranges[2],
      between(`case_prop`, 501, 1000) ~ case_ranges[3],
      between(`case_prop`, 1001, 5000) ~ case_ranges[4],
      between(`case_prop`, 5001, 10000) ~ case_ranges[5],
      between(`case_prop`, 10000, 50000) ~ case_ranges[6],
      between(`case_prop`, 50000, 100000) ~ case_ranges[7],
      `case_prop` >= 100000 ~ case_ranges[8]
    ), levels = case_ranges))
colors_map <- diverging_hcl(7, palette = "Blue-Red 3", rev = F)

swk_mapdata %>% 
  filter(between(col_date, as.Date('2021-01-01'), as.Date('2022-04-28'))) %>%
  ggplot() +
  aes(fill = case_range) +
  geom_sf(color = "black", size = 0.1) +
  scale_fill_manual(values = colors_map, name = NULL) +
  facet_wrap("col_date", ncol = 2) +
  ggtitle("Cummulative daily cases")
swk_mapdata


#my_breaks <- c(1000, 3000, 6000, 9000, 12000, 15000, 17000)
swk_mapdata %>% 
  #filter(col_date %in% seq(as.Date('2021-01-01'), as.Date('2021-06-01'),as.Date('2021-12-01'),as.Date('2022-04-28'))) %>%
  filter(col_date == as.Date('2021-01-01') | col_date == as.Date('2021-06-01') | col_date == as.Date('2021-12-01') | col_date == as.Date('2022-04-29')) %>%
  ggplot() +  geom_sf(color = "black", size = 0.1) +
  aes(fill = case_range) +
  #coord_sf(datum = NA)+
  scale_fill_manual(values = colors_map, name = NULL) +
  #geom_point(data = swk_tab1, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  #geom_label_repel(data = swk_tab1, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "January 2021")+
  #geom_text_repel(force=1.5,data = swk_tab1, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  theme(legend.position="right", legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"),
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),plot.margin = unit(c(5, 5, 5, 0), "cm"))+theme_void()+ facet_wrap("col_date", ncol = 4) +
  ggtitle("Cummulative daily cases")
swk_mapdata

tab2 <-ggplot() +
  geom_sf(data = swk_tab2, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab2, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab2, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab2, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "February 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab3 <-ggplot() +
  geom_sf(data = swk_tab3, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab3, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab3, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab3, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "March 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab4 <-ggplot() +
  geom_sf(data = swk_tab4, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab4, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab4, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab4, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "April 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab5 <-ggplot() +
  geom_sf(data = swk_tab5, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab5, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab5, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab5, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "May 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab6 <-ggplot() +
  geom_sf(data = swk_tab6, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab6, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_label_repel(data = swk_tab6, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "June 2021")+
  geom_text_repel(force=1.5,data = swk_tab6, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab7 <-ggplot() +
  geom_sf(data = swk_tab7, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab7, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab7, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab7, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "July 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab8 <-ggplot() +
  geom_sf(data = swk_tab8, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab8, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab8, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab8, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "August 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))

tab9 <-ggplot() +
  geom_sf(data = swk_tab9, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab9, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab9, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab9, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "September 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab10 <-ggplot() +
  geom_sf(data = swk_tab10, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab10, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab10, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab10, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "October 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab11 <-ggplot() +
  geom_sf(data = swk_tab11, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab11, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab11, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab11, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "November 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab12 <-ggplot() +
  geom_sf(data = swk_tab12, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab12, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab12, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab12, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "December 2021")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab13 <-ggplot() +
  geom_sf(data = swk_tab13, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab13, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab13, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab13, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "January 2022")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab14 <-ggplot() +
  geom_sf(data = swk_tab14, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab14, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab14, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab14, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "February 2022")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab15 <-ggplot() +
  geom_sf(data = swk_tab15, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab15, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab15, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab15, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "March 2022")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
tab16 <-ggplot() +
  geom_sf(data = swk_tab16, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab16, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_text_repel(force=1.5,data = swk_tab16, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  geom_label_repel(data = swk_tab16, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "Cumulative cases", title = "April 2022")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.title = element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))




g <- arrangeGrob(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,tab10,tab11,tab12,tab13,tab14,tab15,tab16, nrow = 4, ncol = 4)
ggsave(file="swk_maps/Epid_multi_4x4_maps.png", g, width = 46, height = 35, units = "cm" )
g <- arrangeGrob(tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,tab10,tab11,tab12,tab13,tab14,tab15,tab16, nrow = 3, ncol = 5)
ggsave(file="swk_maps/Epid_multi_9_maps.png", g, width = 59, height = 27, units = "cm" )

#For_legens_only!
tab16 <-ggplot() +
  geom_sf(data = swk_tab16, aes(fill = case_per100k))+ coord_sf(expand = FALSE)+ 
  scale_fill_gradientn(colours = c("#d7ffd7","#e8f7a3","#e4f031", "#d9e61a","#fcc17d","#fca368","#fa8657","#ee504a","#e03b50","#ca2d59","#b02363","#81176d","#57096e"),limits=c(0, 18000), breaks = c(1000, 3000, 6000, 9000, 12000, 15000, 17000), labels = c("1,000","3,000","6,000", "9,000", "12,000", "15,000", "17,000"), oob=squish)+
  geom_point(data = swk_tab16, aes(coords_x, coords_y, size = population, label = "Population size"), color = "#2c9264")+
  geom_label_repel(data = swk_tab16, aes(coords_x, coords_y, label = seq_sum), size = 4, color = "#9e3566",fontface = "bold",nudge_y=0.9)+
  labs(fill = "cumulative cases per 100,000 population", title = "April 2022")+
  geom_text_repel(force=1.5,data = swk_tab16, mapping = aes(coords_x, coords_y, label = ADM2_EN), size = 3.5,fontface = "bold")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), strip.text = element_text(size = 14, face="bold"))+
  theme(legend.position = "right",legend.direction = "vertical", legend.key.size =unit(1.5, "cm"), legend.key = element_rect(color = "white"),legend.title.align = 0.5,
        legend.text = ggtext::element_markdown(size = 14),legend.key.width =unit(1.5, "cm"), legend.title = element_text(size = 18, hjust = 0.5, face="bold"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_text(hjust = 0.5,size = 15, face="bold"))
ggsave(tab16, file="swk_maps/Epid_only_map16v2.png", width = 59, height = 27, units = "cm" )