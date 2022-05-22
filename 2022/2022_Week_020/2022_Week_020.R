#### (TidyTuesday - 2022 - Week 20) #### 
#### ----------------------------------------------------------------------------------------- ####
## Eurovision
###  Week     Date	           Data	                 Source	                 Article
###  - 20     - 2022-05-17	   - Eurovision	         - Eurovision	           - Tanya Shapiro


#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)

#### Libraries (This Plot) ####
library(sf)
library(rnaturalearthdata)
library(BBmisc)
library(ggbump)
library(ggflags)
library(MetBrewer)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_020/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')


#### Data wrangling #### 
##### The winners of the Eurovision Song Contest
eurovision_winner <- eurovision %>% 
                     filter(section %in% c("final","grand-final") & winner == "TRUE") %>% 
                     mutate(artist_country = if_else(artist_country == "Yugoslavia", "Serbia", artist_country))  %>%
                     group_by(year, winner) %>% 
                     add_count()

countries <- rnaturalearthdata::countries50 %>%
             st_as_sf() %>% 
             sf::st_make_valid(geol) %>%   # Important to change the projection from S2 (spherical) to R2 (flat space)                    
             st_crop(xmin = -20, xmax = 48, ymin = 27, ymax = 70) %>% 
             mutate(admin = case_when(admin == "Republic of Serbia" ~ "Serbia", T ~ admin))  %>%
             right_join(eurovision_winner, by = c("admin" = "artist_country"))

ranking <- st_geometry(countries) %>% 
           st_point_on_surface() %>% 
           st_coordinates() %>% 
           as_tibble() %>% 
           bind_cols(tibble(ranking_lat = normalize(rank(-.$Y), range = c(60, 30), method = "range"),               # Ranking Countries by Lat in reverse order
                            year = countries$year,
                            country = countries$admin,
                            subregion = countries$subregion,
                            ctry = countries$adm0_a3,
                            ctry_2_Lttrs = countries$iso_a2 %>% tolower(),
                            song = countries$song,
                            artist = countries$artist,
                            xend = if_else(subregion %in% c("Northern Europe","Western Europe","Southern Europe"), -10, 48),
                            x_axis_start = xend + if_else(xend < 0, 0, 2),
                            xend_2 = x_axis_start,
                            x_axis_start_2 = xend_2 + if_else(xend_2 < 0, -5, 5)))  %>%
           mutate(subregion = if_else(country == "Israel", "Western Asia", subregion),
                  subregion = fct_relevel(subregion, "Northern Europe","Eastern Europe","Western Europe","Western Asia","Southern Europe"),
                  Song_txt = if_else(subregion %in% c("Northern Europe","Western Europe","Southern Europe"), 
                                     paste0(.$song, " - ", .$artist, " | ",.$year), 
                                     paste0(.$year, " | ", .$song, " - ",.$artist))) %>% 
           arrange(subregion, desc(year)) %>% 
           mutate(row_n = row_number(),
                  ranking_song = normalize(rank(row_n), range = c(70, 27), method = "range")) %>%    # Ranking Year in reverse order
           group_by(subregion) %>%
           mutate(ranking_lat = case_when(subregion == "Southern Europe" ~ 30,
                                          subregion == "Western Europe" ~ 45,
                                          T ~ max(ranking_lat))) %>% 
           ungroup()

data_map <- rnaturalearthdata::countries50 %>% 
            st_as_sf() %>% 
            sf::st_make_valid(geol) %>%
            st_crop(xmin = -20, xmax = 48, ymin = 27, ymax = 70) %>% 
            filter(!admin %in% c("Greenland", countries$admin))

labels <- tibble(subregion = c("Northern Europe","Eastern Europe","Western Europe","Western Asia","Southern Europe"),
                 label = c("Northern Europe","Eastern Europe","Western Europe","Asia","Southern Europe"),
                 X = c(-22,60,-22,60,-22),
                 Y = c(72,55,52,38,34))

#### Plot aesthetics ####
background     <- "#F2F2F2"
lines_color    <- ""
title_color    <- "#4F0259"
subtitle_color <- "#8C4272"
text_color     <- ""
caption_color  <- "#39054D"
Palette <- met.brewer("Johnson", 5, type = "discrete")[c(5,2,1,3,4)]


#### Annotation ####
annotation_title_text <- c("Eurovision")
annotation_subtitle_text <- c("All time contest winners (1956 - 2022)")


#### Plot ####
Plot <- ggplot() + 
        geom_sf(data = data_map, size = .3, fill = "grey90", color = background) +
        geom_sigmoid(data = ranking,  aes(x = X, y = Y, xend = x_axis_start, yend = ranking_lat, group = country, color = subregion), alpha = .3, smooth = 10, size = .4) + 
        geom_sf(data = countries, size = .1, aes(fill = subregion), color = background) +
        geom_point(data = ranking,  aes(x = x_axis_start, y = ranking_lat, color = subregion), size = 2.5) +
        geom_sigmoid(data = ranking %>% filter(year != 1969), aes(x = xend_2, y = ranking_lat, xend = x_axis_start_2, yend = ranking_song, group = year, color = subregion), alpha = .3, smooth = 10, size = .4) + 
        geom_sigmoid(data = ranking %>% filter(year == 1969), aes(x = xend_2, y = ranking_lat, xend = x_axis_start_2, yend = ranking_song, group = country, color = subregion), alpha = .3, smooth = 10, size = .4) + 
        ### Annotations ###
        geom_flag(data = ranking, aes(x = x_axis_start_2 + if_else(x_axis_start > 0, 1, -1), y = ranking_song, country = ctry_2_Lttrs), size = 2) +
        geom_text(data = ranking, aes(x = x_axis_start_2 + if_else(x_axis_start > 0, 2, -2), y = ranking_song, label = Song_txt, color = subregion, hjust = if_else(x_axis_start > 0, 0, 1)), size = 2.0, nudge_x = 0, nudge_y = 0, family = "Bebas Neu") +  # Song text
        geom_text(data = labels, aes(x = X, y = Y, label = label, color = subregion), size = 4.0, family = "Koulen") +  # 
        ### Scales ###
        scale_color_manual(values = Palette, breaks = c("Northern Europe","Eastern Europe","Western Europe","Western Asia","Southern Europe")) +
        scale_fill_manual(values = Palette, breaks = c("Northern Europe","Eastern Europe","Western Europe","Western Asia","Southern Europe")) +
        coord_sf(clip = "off") +
        ### Theme ###
        theme_void() +
        theme(
          ## Legend ##
          legend.position = "none",
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background = element_rect(fill = background, color = NA),
          ## Titles & Caption ##
          plot.title.position = "plot",
          plot.title    = element_markdown(color = title_color, family = "Koulen", face = "plain", size = 28, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5, margin = margin(t = 0.5, r = 0.1, b = -0.25, l = 0.0, unit = "cm")),
          plot.subtitle = element_markdown(color = title_color, family = "Bebas Neue", face = "plain", size = 16, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5, margin = margin(t = 0.5, r = 0.1, b = -0.5, l = 0.0, unit = "cm")),
          plot.caption.position = "panel",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", size = 6, hjust = 1, vjust = 0, margin = margin(t = -0.5, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm")) +
        ### Labels ###
        labs(x = "",
             y = "",
             subtitle = annotation_subtitle_text,
             title = annotation_title_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>  TamayoLeivaJ<br><br> 
                        Source: Eurovision by Tanya Shapiro | <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span> tashapiro")


#### Progress ####
ggsave("./2022/2022_Week_020/Plots/2022_Week_020.png", Plot, dpi = 326, scale = 1, width = 10, height = 6, units = c("in"))
