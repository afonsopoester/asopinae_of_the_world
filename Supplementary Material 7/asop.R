library(tidyverse)
library(patchwork)
library(ggspatial)
library(sf)
library(scales)
library(showtext)
library(png)
library(grid)
library(speciesgeocodeR)


font_add_google("Kanit", "kanit", bold.wt = 600)
showtext_auto()
showtext_opts(dpi = 1200)
north <- readPNG("north.png")
pngob <- rasterGrob(north)

custom_plasma <- function(n) {
  plasma_colors <- rev(viridis::plasma(n))
  
  custom_colors <- c("grey35", plasma_colors[-1])
  
  return(custom_colors)
}
scale_fill_custom_plasma <- function(limits = NULL) {
  scale_fill_gradientn(colors = custom_plasma(256), limits = limits)
}



PlotRichness <- function(data, polygons, areanames, column_name, limit_x = c(-180, 180), limit_y = c(-90,90), overlap = FALSE, threshold = 1, breaksx = seq(-180, 180, by = 30), breaksy = seq(-60, 75, by = 20), axis_size = 10, font_size = 4.6, font_color = "white", numbers = TRUE) {
  richness <- data %>%
    group_by(!!sym(column_name)) %>%
    summarise(sppol = length(unique(Species)))
  
  colnames(richness) <- c(paste(areanames), "sppol")  
  
  polygons$centroids <- st_point_on_surface(polygons$geometry)

  
  plot_data <- left_join(polygons, richness, by = paste(areanames))
  
  plot_data <- plot_data %>%
    mutate(sppol = coalesce(sppol, 0))
    
  
  if(numbers == TRUE) {
      plot <- ggplot(plot_data) +
        geom_sf(aes(fill = sppol)) +
        geom_sf_text(aes(label = ifelse(sppol > threshold, sppol, ""), geometry = centroids), size = font_size, colour = font_color,  family = "kanit", fontface = "bold" , check_overlap = overlap) +
        scale_fill_custom_plasma(limits = c(0, max(plot_data$sppol))) +
        coord_sf() +
        theme_bw() +
        labs(fill = "Richness") +
        theme(axis.ticks = element_line(linewidth = .45),
              axis.text = element_text(size = axis_size),
              axis.title = element_blank(),
              panel.background = element_rect(color = "#c8e1fa",
                                              fill = "#c8e1fa"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) +
        scale_x_continuous(sec.axis = dup_axis(),
                           breaks = breaksx,  
                           limits = limit_x, 
                           expand = c(0,0)) +
        scale_y_continuous(breaks = breaksy, 
                           limits = limit_y, 
                           expand = c(0,0))
  } else {
    plot <- ggplot(plot_data) +
      geom_sf(aes(fill = sppol)) +
      scale_fill_custom_plasma() +
      coord_sf() +
      theme_bw() +
      labs(fill = "Richness") +
      theme(axis.ticks = element_line(linewidth = .45),
            axis.text = element_text(size = axis_size),
            axis.title = element_blank(),
            panel.background = element_rect(color = "#c8e1fa",
                                            fill = "#c8e1fa"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      scale_x_continuous(sec.axis = dup_axis(),
                         breaks = breaksx,  
                         limits = limit_x, 
                         expand = c(0,0)) +
      scale_y_continuous(breaks = breaksy, 
                         limits = limit_y, 
                         expand = c(0,0))
  }
  
  
  plot
  
}


cntr <- st_read("Shapes/GADM_COUNTRIES.shp")
cntr_spat <- as_Spatial(cntr)
regions <- st_read("Shapes/Ecoregions2017.shp", options = "ENCODING=WINDOWS-1252")
regions_spat <- as_Spatial(regions)
realms <- st_read("Shapes/Ecoregions_Realms.shp", options = "ENCODING=WINDOWS-1252")
realms_spat <- as_Spatial(realms)

asop <- read_csv("Asopinae_Analises_R_Preparado.csv") %>%
  as.data.frame()

fig1 <- ggplot() +
  geom_sf(data = cntr, color = "black", fill = "white") +
  geom_point(data = asop, aes(x = decimalLongitude, y = decimalLatitude),
             color = "red", size = 0.6, shape = 19) +
  coord_sf() +
  theme_bw() +
  theme(axis.ticks = element_line(linewidth = 0.9),
        axis.text = element_text(size = 6),
        axis.title = element_blank(),
        panel.background = element_rect(color = "#c8e1fa",
                                        fill = "#c8e1fa"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_continuous(sec.axis = dup_axis(),
                     breaks = seq(-180, 180, by = 30),  
                     limits = c(-180, 180), 
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-60, 75, by = 20), 
                     limits = c(-70,80), 
                     expand = c(0,0)) +
  annotation_custom(pngob, 
                    xmin = -150, xmax = -175, 
                    ymin = -42, ymax = -62)



figtwoa <- asop %>%
  group_by(Species) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(Species, -n), y = n)) +
    geom_col(fill = "black") +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_y_continuous(name = "Number of records", limits = c(0, 500)) +
    scale_x_discrete(name = "Species") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 28, hjust = 0.5, vjust = 0.6),
      panel.grid = element_blank()
    )

figtwob <- asop %>% 
  group_by(country) %>%
  summarise(spp = n_distinct(Species)) %>%
  arrange(desc(spp)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(country, -spp), y = spp)) +
  geom_col(fill = "black") +
  geom_text(aes(label = spp), vjust = -0.5) +
  scale_y_continuous(name = "Number of species", limits = c(0, 55)) +
  scale_x_discrete(name = "Country") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 28, hjust = 0.5, vjust = 0.6),
    panel.grid = element_blank()
  )

countries_endemic <- read.csv("asop_end_cntr.csv")
colnames(countries_endemic) <- c("x", "country", "endemic_species_count")



figtwoc <- countries_endemic %>%
  top_n(5) %>%
  ggplot(aes(x = reorder(country, -endemic_species_count), y = endemic_species_count)) +
  geom_col(fill = "black") +
  geom_text(aes(label = endemic_species_count), vjust = -0.5) +
  scale_y_continuous(name = "Number of restricted species", limits = c(0,30)) +
  scale_x_discrete(name = "Country") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 28, hjust = 0.5, vjust = 0.6),
    panel.grid = element_blank()
  )



fig2 <- figtwoa/figtwob/figtwoc + plot_annotation(tag_levels = "a") + plot_layout(heights = c(10,10,10))



fig3a <- asop %>%
  filter(continent == "North America"| continent == "South America") %>%
  PlotRichness(cntr, areanames = "NAME_0", column_name = "country",
               limit_x = c(-140, -30),
               limit_y = c(-50, 65),
               threshold = 4,
               font_size = 3.7) +
  annotation_scale(location = "bl")


fig3b <- asop %>%
  filter(continent == "Europe", country != "Russia") %>%
  PlotRichness(cntr, areanames = "NAME_0", column_name = "country",
               limit_x = c(-25, 40),
               limit_y = c(33, 70),
               threshold = 1) +
  annotation_scale(location = "bl") 

fig3c <- asop %>%
  filter(continent == "Africa") %>%
  PlotRichness(cntr, areanames = "NAME_0", column_name = "country",
               limit_x = c(-20, 54),
               limit_y = c(-37, 38),
               threshold = 2) +
  annotation_scale(location = "bl") +
  annotation_custom(pngob, xmin = -17, xmax = -10, ymin = -35, ymax = -25) 

fig3d <- asop %>%
  filter(continent == "Asia"| continent == "Oceania" | continent == "Australia" | country == "Russia") %>%
  PlotRichness(cntr, areanames = "NAME_0", column_name = "country",
               limit_x = c(17, 179),
               limit_y = c(-49, 70),
               threshold = 2,
               font_size = 3.75) +
  annotation_scale(location = "bl") 

fig3 <- (fig3a|fig3b) / (fig3c|fig3d) + plot_annotation(tag_levels = "a") 

fig4a <- asop %>%
  filter(continent == "North America"| continent == "South America") %>%
  PlotRichness(regions, areanames = "ECO_NAME", column_name = "ecoregion",
               limit_x = c(-140, -30),
               limit_y = c(-50, 65),
               threshold = 3,
               numbers = FALSE) +
  annotation_scale(location = "bl") 

fig4b <- asop %>%
  filter(continent == "Europe") %>%
  PlotRichness(regions, areanames = "ECO_NAME", column_name = "ecoregion",
               limit_x = c(-25, 40),
               limit_y = c(33, 70),
               threshold = 1,
               numbers = FALSE) +
  annotation_scale(location = "bl") 

fig4c <- asop %>%
  filter(continent == "Africa") %>%
  PlotRichness(regions, areanames = "ECO_NAME", column_name = "ecoregion",
               limit_x = c(-20, 54),
               limit_y = c(-37, 38),
               threshold = 2,
               numbers = FALSE) +
  annotation_scale(location = "bl") +
  annotation_custom(pngob, xmin = -17, xmax = -10, ymin = -35, ymax = -25) 

fig4d <- asop %>%
  filter(continent == "Asia"| continent == "Oceania" | continent == "Australia" | country == "Russia") %>%
  PlotRichness(regions, areanames = "ECO_NAME", column_name = "ecoregion",
               limit_x = c(17, 179),
               limit_y = c(-49, 70),
               threshold = 2,
               numbers = FALSE) +
  annotation_scale(location = "bl") 

fig4 <- (fig4a|fig4b) / (fig4c|fig4d) + plot_annotation(tag_levels = "a") 




fig5a <- PlotRichness(asop, realms, areanames = "REALM", column_name = "realm", limit_x = c(-180, 185), limit_y = c(-70,80), font_color = 
                       "white", axis_size = 7.5)



asop_grid <- RichnessGrid(asop, reso = 4, type = "spnum")
asop_grid_df <- raster::as.data.frame(asop_grid, xy = TRUE) %>% 
  na.omit()
fig5b <- ggplot(cntr) +
  geom_sf(fill = "white") +
  geom_raster(asop_grid_df, mapping = aes(x=x, y=y, fill = layer)) + 
  scale_fill_custom_plasma(limits = c(0, max(asop_grid_df$layer))) +
  coord_sf() +
  theme_bw() +
  labs(fill = "Richness") +
  annotation_custom(pngob, xmin = -175, xmax = -155, ymin = -60, ymax = -35)  +
  theme(axis.ticks = element_line(linewidth = .9),
        axis.text = element_text(size = 7.5),
        axis.title = element_blank(),
        panel.background = element_rect(color = "#c8e1fa",
                                        fill = "#c8e1fa"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_continuous(sec.axis = dup_axis(),
                     breaks = seq(-180, 185, by = 30),  
                     limits = c(-180, 185), 
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-60, 75, by = 20), 
                     limits = c(-70,80), 
                     expand = c(0,0))

fig5 <- fig5a / fig5b + plot_annotation(tag_levels = "a") 






