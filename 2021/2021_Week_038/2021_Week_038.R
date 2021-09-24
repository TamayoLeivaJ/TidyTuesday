#### (TidyTuesday - 2021 - Week 38) #### 
#### ----------------------------------------------------------------------------------------- ####
## Billboard Top 100 data set
###  Week     Date	          Data	                 Source	                 Article
###  - 38     - 2021-09-14     - Billboard Top 100   - Data.World            - ThePudding

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(viridis)
library(lubridate)
library(ggfx)
library(ggtext)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_038/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_038/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
tt_data <- tt_load(2021, week = 38)

billboard <- tt_data$billboard
audio_features <- tt_data$audio_features %>% distinct() # Some rows are duplicated
  
#### Data wrangling #### 
##### Which songs have have been #1 for the longest time?
billboard_audio_features <- billboard %>% 
                            select(-url) %>% 
                            left_join(audio_features %>% distinct(), by = c("song_id","song","performer"))

billboard_song_longest_1 <- billboard_audio_features %>% 
                            mutate(week_id = parse_date_time(week_id, "mdy"),
                                   year_id = year(week_id)) %>% 
                            group_by(song_id, song, performer) %>%
                            arrange(week_id) %>%                                                                       # Arrange the df by weeks to avoid count problems
                            mutate(Counter_weeks = sequence(rle(week_position)$lengths),                               # Count the number of consecutive #? positions
                                   End_weeks = ifelse(lead(week_position) == week_position, "No", "Yes")) %>%          # Was this week the last on the current position? 
                            group_by(song_id, song, performer) %>%
                            arrange(desc(Counter_weeks)) %>% 
                            group_by(song_id, song, performer) %>% 
                            mutate(weeks_on_chart = n(), alpha = 1/peak_position) %>% 
                            group_by(song_id) %>% 
                            slice_min(peak_position, n = 1, with_ties = FALSE) %>% 
                            filter(peak_position <= 1)
   
#### Annotation List ####


#### Text ####
annotation_title_text <- c("BILLBOARD THE HOTEST 100")
annotation_subtitle_text <- c("Songs that have been #1 for the longest time")

#### Images ####
##### Team Logo #####


##### Team Cars #####

#### Plot aesthetics ####
background  <- c("#1E212F")
lines_color <- c("#0AD89D")
title_color <- c("#FFFFFF")
subtitle_color <- c("#FEC803")
text_color  <- c("#FEC803")
caption_color  <- c("#4E4D73")

#### Plot ####
billboard_song_longest_1 %>%
     filter(Counter_weeks < 13) %>% 
     ### Layers base ###
     ggplot(aes(x = Counter_weeks, y = year_id, color = as.character(energy))) + 
     geom_jitter(size = 3, height = 0.5, width = 0.5, alpha = 0.50) + 
     with_outer_glow(geom_point(data = billboard_song_longest_1 %>% filter(Counter_weeks >= 13), size = 4, alpha = 0.75), colour = "#3D4675", sigma = 2, expand = 5) +
     ### Annotations ###
     ### Clusters Annotations ###
     #geom_mark_ellipse(aes(label = "(1975–1977)", filter = constructor_name %in% constructor_teams[1] & between(year, 1975, 1977)), label.fill = "transparent", color = constructor_color[1], label.colour = constructor_color[1], con.colour = constructor_color[1], label.buffer = unit(50, 'mm'), expand = unit(2.0, "mm")) +
     ### Layers extra ###
     #geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[1] & between(year, 1999, 2004)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     ### image Annotations ###
     #annotation_custom(rasterGrob(image = img_Formula1_logo, width = 0.15, x = 0.05, y = -0.1)) +
     ### Scales ###
     scale_y_continuous(limits = c(1965,2022), breaks = seq(1970, 2020, 10)) +
     scale_x_continuous(expand = c(0, 1), breaks = c(seq(1, 13, 4), 16, 19), limits = c(0, 25)) +
     scale_color_manual(values = viridis::inferno(n = length(unique(billboard_song_longest_1$energy)), begin = 0.4, end = 0.9, direction = 1)) +
     coord_cartesian(expand = TRUE, clip = "off") +
     ### Guides ###
     #guides(color = guide_legend(title.position = "top", override.aes = list(size = 4)), order=2) +
     ### Theme ###
     theme_classic() +
     theme(
       ## Text ##
       ## Axis ##
       axis.title.x = element_text(face = "plain", family = "Basique", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(t = 10, r = 0, b = 10, l = 0)),
       axis.title.y = element_text(face = "plain", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 90),
       axis.text.x = element_text(face = "plain", family = "Basique", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
       axis.text.y = element_text(face = "plain", family = "Basique", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
       axis.line.x = element_line(colour = "transparent"),
       axis.ticks.x = element_line(colour = lines_color),
       axis.line.y = element_line(colour = "transparent"),
       axis.ticks.y = element_line(colour = lines_color),
       ## Panel Grid ##
       ## Plot Aesthetic ##
       panel.background = element_rect(fill = background, color = NA),
       plot.background = element_rect(fill = background, color = NA),
       legend.background = element_rect(fill = background, color = NA),
       legend.key = element_rect(fill = background, color = NA),
       ## Legend ##
       legend.position = "none",
       ## Titles & Caption ##
       plot.title.position = "plot",
       plot.title = element_markdown(color = title_color, family = "Basique", face = "bold", size = 16),
       plot.subtitle = element_markdown(color = subtitle_color, family = "Basique", face = "plain", size = 14),
       plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 0.5),
       ## Margin ##
       plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
     ### Labels ###
     labs(title = annotation_title_text,
          subtitle = annotation_subtitle_text,
          x = "Consecutive Weeks",
          y = NULL,
          caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                     <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                     Source: Data.World")
   
#### Progress ####
ggsave("./2021/2021_Week_038/Plots/2021_Week_038.png", dpi = 320, scale = 1, width = 8, height = 12, units = c("in"))