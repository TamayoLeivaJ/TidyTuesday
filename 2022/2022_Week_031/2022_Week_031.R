#### (TidyTuesday - 2022 - Week 31) #### 
#### ----------------------------------------------------------------------------------------- ####
## 
###  Week     Date	           Data	                           Source	                                 Article
###  - 31     - 2022-08-02	   - Oregon Spotted Frog	         - usgs.gov spotted frog data	           - usgs.gov spotted-frog-article


#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(systemfonts)

#### Libraries (This Plot) ####


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_031/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')


#### Data wrangling #### 
##### Where are frogs most likely to be seen in Oregon?
##### Counts #####
Frogs <- frogs %>%
         group_by(HabType, Detection) %>% 
         dplyr::summarise(Count = n())

##### Transform UTM to lat/long #####
frogs_coord <- tibble(frogs$UTME_83, frogs$UTMN_83)
frogs_coord <- sf::st_as_sf(x = frogs_coord, coords = c(1,2), crs = "+proj=utm +zone=10") # Conversion of data frame to sf object
frogs_coord <- sf::st_transform(frogs_coord, crs = "+proj=longlat +datum=WGS84")          # Projection transformation
frogs_coord <- tibble(frogs_coord)                                                        # Convert it to tibble
frogs_location <- tibble(Detection = frogs$Detection,
                         lat = unlist(map(frogs_coord$geometry, 2)),
                         long = unlist(map(frogs_coord$geometry, 1)))
  
##### Map #####
water <- osmdata::opq(bbox = c(-121.84, 43.76, -121.76, 43.81)) %>% 
         osmdata::add_osm_feature(key = "natural", value = "water") %>% 
         osmdata::osmdata_sf()

river <- osmdata::opq(bbox = c(-121.84, 43.76, -121.76, 43.81)) %>% 
         osmdata::add_osm_feature(key = "waterway", value = "river") %>% 
         osmdata::osmdata_sf() 

#### Plot aesthetics ####
background     <- "#B8ECD7"
lines_color    <- "#DADADA"
title_color    <- "#262F48"
subtitle_color <- "#444853"
text_color     <- "#444853"
caption_color  <- "#262F48"
palette_f      <- c("#D5434D", "#FDA83E", "#605376")


#### Annotation ####
annotation_title_text <- c("Oregon Spotted Frog")
annotation_subtitle_text <- c("Telemetry was used to study the movement and habitat use of the spotted frog (*Rana pretiosa*) at Crane Prairie Reservoir in Oregon, USA. The study was conducted between September and the end of November 2018. The total number of frogs detected is shown by habitat type, location, and detection status. Frogs were tracked within the main reservoir and in surrounding pond and river habitats.")
annotation_subtitle_text <- stringr::str_wrap(annotation_subtitle_text, 102) %>% 
                            stringr::str_replace_all("\n","<br>")


#### Plot ####
##### Map #####
(map <- ggplot() +
       geom_rect(aes(xmin = -121.824775, xmax = -121.764923, ymin = 43.764375, ymax = 43.814821), color = "#155362", fill = "transparent", alpha = 0) + 
       geom_sf(data = water$osm_polygons, fill = "#74B8D6", colour = "#74B8D6", alpha = 0.9, inherit.aes = FALSE) +
       geom_sf(data = river$osm_lines, fill = "#74B8D6", colour = "#74B8D6", alpha = 1.0, inherit.aes = FALSE) +
       geom_point(data = frogs_location, aes(x = long, y = lat, color = forcats::fct_infreq(Detection), size = forcats::fct_infreq(Detection)), alpha = .8) +
       scale_color_manual(values = palette_f) +
       scale_size_manual(values = seq(1, 3, 1)) +
       guides(color = "none", size = "none") +
       coord_sf(xlim = c(-121.84, -121.76), ylim = c(43.76, 43.82), expand = TRUE) +
       theme_void() +
       theme(
         ## Axis ##
         axis.text.y  = element_text(size = 10, family = "Syne", color = text_color),
         axis.text.x  = element_text(size = 10, family = "Syne", color = text_color, angle = 90),
         ## Plot Aesthetic ##
         panel.background = element_rect(fill = "transparent", color = NA),
         plot.background  = element_rect(fill = "transparent", color = NA),
         ## Titles & Caption ##
         plot.title.position = "panel",
         plot.title = ggtext::element_markdown(color = title_color, family = "Antonio", face = "plain", size = 12, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5)) +
       ### Labels ###
       labs(title = "Crane Prairie Reservoir<br>Oregon, USA"))

##### Main #####
Plot <- Frogs %>% 
        ggplot(aes(x = forcats::fct_reorder(HabType, Count, .desc=TRUE), y = Count, fill = forcats::fct_reorder(Detection, Count, .desc=TRUE))) +
        ggchicklet::geom_chicklet(width = 0.75, radius = grid::unit(4.5, "pt")) +
        ### Annotations ###
        geom_text(data = Frogs %>% filter(Detection == "No visual"), aes(label = HabType, y = Count/2), nudge_x = .5, size = 7, family = "Syne", color = text_color) +
        ### Scales ###
        scale_fill_manual(values = palette_f) +
        scale_y_continuous(limits = c(0, 200), breaks = seq(0, 180, 30)) +
        coord_flip() +
        ### Theme ###
        theme_minimal() +
        theme(
          ## Text ##
          text = element_text(face = "plain", family = "Syne", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
          ## Axis ##
          axis.title.x = element_text(size = 16, family = "Syne", color = text_color, margin = unit(c(0.25, 0.25, 0.75, 0.25), "cm")),
          axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.text.x  = element_text(size = 14, color = text_color),
          axis.line.x  = element_blank(),
          axis.ticks.x = element_line(size = 0.1, color = lines_color),
          axis.line.y  = element_blank(),
          axis.ticks.y = element_blank(),
          ## Lines ##
          panel.grid.major.x = element_line(size = 1.0, color = lines_color),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          ## Legend ##
          legend.position = c(.945, .20),
          legend.text = element_text(face = "plain", family = "Syne", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0, size = 20),
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background  = element_rect(fill = background, color = NA),
          ## Titles & Caption ##
          plot.title.position = "panel",
          plot.title = ggtext::element_markdown(color = title_color, family = "Belleza", face = "plain", size = 24, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5, margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")),
          plot.subtitle = ggtext::element_markdown(color = title_color, family = "Belleza", face = "plain", size = 16, hjust = 0.5, halign = 0, vjust = 0.5, valign = 0.5, margin = margin(t = 0.1, r = 0.1, b = 0.5, l = 0.1, unit = "cm")),
          plot.caption.position = "panel",
          plot.caption = ggtext::element_markdown(color = caption_color, family = "Menlo", size = 10, hjust = 1, vjust = 0, margin = margin(t = -0.5, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.25, r = 0.75, b = 0.75, l = 0.25, unit = "cm")) +
        ### Labels ###
        labs(x = "",
             y = "Frogs",
             fill = "",
             title = annotation_title_text,
             subtitle = annotation_subtitle_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>  TamayoLeivaJ<br><br> 
                        Source: usgs.gov")

Plot_Final <- Plot + patchwork::inset_element(map, left = 0.55, bottom = 0.35, right = 1, top = 1)

#### Progress ####
ggsave("./2022/2022_Week_031/Plots/2022_Week_031.png", Plot_Final, dpi = 326, scale = 1, width = 10, height = 10, units = c("in"))
