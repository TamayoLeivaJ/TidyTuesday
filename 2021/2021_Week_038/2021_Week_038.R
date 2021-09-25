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
   
#### Annotation ####
Label <- tibble(x = seq(20, 26, length.out = 10),
                y = rep(1975, 10),
                color = seq(min(billboard_song_longest_1$energy, na.rm = TRUE), 
                            max(billboard_song_longest_1$energy, na.rm = TRUE), length.out = 10))

#### Text ####
annotation_title_text <- c("BILLBOARD HOTEST TOP 100")
annotation_subtitle_text <- c("Which songs have have been #1 for the longest time?")

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
     ### Labels Annotations ###
     annotate(geom = "text", x = 24.0, y = 2019, color = "#EF6E21FF", label = "Old Town Road · Lil Nas X", family = "Basique") +
     annotate(geom = "text", x = 23.5, y = 2017, color = "#FC9F07FF", label = "Despacito · Luis Fonsi & Daddy Yankee", family = "Basique") +
     annotate(geom = "text", x = 24.5, y = 1996, color = "#7A7A7DFF", label = "One Sweet Day · Mariah Carey & Boyz II Men", family = "Basique") +
     annotate(geom = "text", x = 20.5, y = 1998, color = "#7A7A7DFF", label = "Candle In The Wind · Elton John", family = "Basique") +
     annotate(geom = "text", x = 21.0, y = 2009, color = "#F8850FFF", label = "I Gotta Feeling · The Black Eyed Peas", family = "Basique") +
     annotate(geom = "text", x = 22.0, y = 1993, color = "#A92E5EFF", label = "I Will Always Love You · Whitney Houston", family = "Basique") +
     annotate(geom = "text", x = 20.5, y = 1994, color = "#E35833FF", label = "I'll Make Love To You · Boyz II Men", family = "Basique") +
     annotate(geom = "text", x = 20.5, y = 1997, color = "#7A7A7DFF", label = "Macarena · Los Del Rio", family = "Basique") +
     annotate(geom = "curve", x = 16.5, y = 1997, xend = 14.5, yend = 1996.5, curvature = +.1, color = "#7A7A7DFF", arrow = arrow(length = unit(0.05, "inches"))) +
     annotate(geom = "text", x = 22.0, y = 2015, color = "#EF6E21FF", label = "Uptown Funk! · Mark Ronson & Bruno Mars", family = "Basique") +
     annotate(geom = "text", x = 23.5, y = 1992, color = "#7A7A7DFF", label = "End Of The Road (From 'Boomerang') · Boyz II Men", family = "Basique") +
     annotate(geom = "text", x = 22.0, y = 2000, color = "#F8850FFF", label = "The Boy Is Mine · Brandy & Monica", family = "Basique") +
     annotate(geom = "curve", x = 16.0, y = 2000, xend = 13.5, yend = 1998.5, curvature = +.2, color = "#F8850FFF", arrow = arrow(length = unit(0.05, "inches"))) +
     ### Color Scale Annotations ###
     geom_point(data = Label, aes(x = x, y = y, color = as.character(color)), size = 4.5, alpha = 0.50) +
     annotate(geom = "curve", x = 18.5, y = 1973, xend = 19.5, yend = 1975, curvature = -.4, color = "#932667FF", arrow = arrow(length = unit(0.05, "inches"))) +
     annotate(geom = "text", x = 18.5, y = 1972, color = "#932667FF", label = "Less energy", family = "Basique") +
     annotate(geom = "curve", x = 27.5, y = 1977, xend = 26.5, yend = 1975, curvature = -.4, color = "#F6D645FF", arrow = arrow(length = unit(0.05, "inches"))) +
     annotate(geom = "text", x = 27.5, y = 1978, color = "#F6D645FF", label = "More energy", family = "Basique") +
     ### Scales ###
     scale_y_continuous(limits = c(1966,2021), breaks = seq(1970, 2020, 10)) +
     scale_x_continuous(expand = c(0, 1), breaks = c(seq(1, 13, 4), 16, 19), limits = c(0, 32)) +
     scale_color_manual(values = viridis::inferno(n = length(unique(c(billboard_song_longest_1$energy, Label$color))), begin = 0.4, end = 0.9, direction = 1)) +
     coord_cartesian(expand = TRUE, clip = "on") +
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
