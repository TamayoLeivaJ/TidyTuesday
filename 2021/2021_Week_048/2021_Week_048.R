#### (TidyTuesday - 2021 - Week 48) #### 
#### ----------------------------------------------------------------------------------------- ####
## Dr. Who
###  Week     Date	           Data	                    Source	                   Article
###  - 48     - 2021-11-23	   - Dr. Who	              - datardis Pkg	           - R and Omics, datardis package

#### set up ####
#### Libraries ####
library(here)
library(tidyverse, quietly = TRUE)
library(tidytuesdayR)
library(scales)
library(ggtext)
library(systemfonts)
library(png)
library(ggimage)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_048/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_048/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')


#### Data wrangling #### 
##### What is the average star rating of Doctor Who?
doctor_who <- tibble(doctor = c("Christopher Eccleston", rep("David Tennant", 3), rep("Matt Smith", 3), rep("Peter Capaldi", 3), rep("Jodie Whittaker", 3)),
                     doctor_n = c("The Ninth Doctor", rep("The Tenth Doctor", 3), rep("The Eleventh Doctor", 3), rep("The Twelfth Doctor", 3), rep("The Thirteenth Doctor", 3)),
                     season_number = c(1,2:4,5:7,8:10,11:13),
                     doctor_color = c("#DA4D40",rep("#518F4A", 3),rep("#F27979", 3),rep("#5590A8", 3),rep("#EF9E0E", 3))) 

episodes <- episodes %>% 
            left_join(doctor_who, by = "season_number") %>% 
            filter(type != "special" & !is.na(rating) & season_number < 13) %>% 
            arrange(season_number, episode_number) %>% 
            mutate(episode_id = row_number()) %>% 
            group_by(season_number) %>% 
            mutate(rating_avg = mean(rating),
                   x = min(episode_id),
                   xend = max(episode_id),
                   season_label = min(episode_id) + (max(episode_id) - min(episode_id))/2) %>% 
            group_by(doctor) %>%
            mutate(doctor_label = min(episode_id) + (max(episode_id) - min(episode_id))/2,
                   doctor_image_label = min(episode_id) + (max(episode_id) - min(episode_id))/2) %>% 
            ungroup() %>% 
            mutate(season_number = factor(season_number)) %>% 
            mutate(doctor_image =  case_when(doctor_n == "The Ninth Doctor" ~ "./2021/2021_Week_048/Images/Doctor_Who_09.png",
                                             doctor_n == "The Tenth Doctor" ~ "./2021/2021_Week_048/Images/Doctor_Who_10.png",
                                             doctor_n == "The Eleventh Doctor" ~ "./2021/2021_Week_048/Images/Doctor_Who_11.png",
                                             doctor_n == "The Twelfth Doctor" ~ "./2021/2021_Week_048/Images/Doctor_Who_12.png",
                                             doctor_n == "The Thirteenth Doctor" ~ "./2021/2021_Week_048/Images/Doctor_Who_13.png"))

#### Plot aesthetics ####
background  <- c("#011126")
lines_color <- c("#310F3E")
title_color <- c("#F6F6F8")
text_color  <- c("#DA4D40")
caption_color  <- c("#EF9E0E")


#### Annotation ####
annotation_title_text <- c("Who is your favorite Doctor Who?")


#### Plot ####
Plot <- episodes %>% 
        ggplot(aes(episode_id, rating, group = season_number)) +
              geom_point(aes(color = season_number)) +
              geom_segment(aes(x = x - 0.2, xend = xend + 0.2, y = rating_avg, yend = rating_avg, color = season_number)) +
              geom_segment(aes(x = episode_id, xend = episode_id, y = rating, yend = rating_avg, color = season_number)) +
        ### Annotations ###
        #### Add y-axis information
              geom_point(data = tibble(x = -2, y = seq(76, 91, 1)), aes(x, y), shape = "-", color = text_color, inherit.aes = FALSE) +
              geom_text(data = tibble(x = -4, y = seq(76, 91, length.out = 6)), aes(x, y, label = y), color = text_color, size = 2, family = "Neuropol", inherit.aes = FALSE) +
              geom_text(data = tibble(x = -7, y = 83.5), aes(x, y), label = "Rating", color = text_color, size = 2.0, angle = 90, family = "Neuropol", inherit.aes = FALSE) +
              geom_point(data = tibble(x = 149, y = seq(76, 91, 1)), aes(x, y), shape = "-", color = caption_color, inherit.aes = FALSE) +
              geom_text(data = tibble(x = 152, y = seq(76, 91, length.out = 6)), aes(x, y, label = y), color = caption_color, size = 2, family = "Neuropol", inherit.aes = FALSE) +
        ### Image Annotations ###
              geom_image(aes(x = doctor_image_label, y = 95, image = doctor_image), asp = 1.5)+
        ### Text Annotations ###
              geom_label(aes(x = season_label, y = 75, label = paste0("S", season_number), color = season_number), fill = background, size = 2.0, angle = 90, family = "Neuropol") + 
              geom_label(aes(x = doctor_label, y = 92, label = doctor_n, color = season_number), fill = background, size = 2.0, angle = 0, family = "Neuropol") +
              annotate(geom = "curve", x = 70, y = 80, xend = 91, yend = 83.33333, curvature = -.4, arrow = arrow(length = unit(0.2, "lines"), type = "closed"), color = "#5590A8") +
              geom_text(data = tibble(x = 70, y = 79), aes(x, y), color = "#5590A8", label = "Middle line represent\nthe Season mean", size = 2.0, angle = 0, family = "Neuropol", hjust = 0.5, vjust = 0.5,inherit.aes = FALSE) +
        ### Scales ###
        scale_x_continuous(limits = c(-7, 152)) +
        scale_y_continuous(expand = c(0,0), limits = c(74, 98)) +       
        scale_color_manual(values = doctor_who$doctor_color) +
        coord_cartesian(expand = TRUE, clip = "on") +
        ### Theme ### 
        theme_classic() +
        theme(
          ## Text ##
          text = element_text(face = "plain", family = "Neuropol", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
          ## Axis ##
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
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
          plot.title = element_markdown(color = title_color, family = "Doctor Who", face = "plain", size = 30),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 6),
          ## Margin ##
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
        ### Labels ###
        labs(title = annotation_title_text,
             caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                              <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                              Source: Dr. Who {datardis}<br>")


#### Progress ####
ggsave("./2021/2021_Week_048/Plots/2021_Week_048.png", Plot, dpi = 320, scale = 1, width = 9, height = 6, units = c("in"))
