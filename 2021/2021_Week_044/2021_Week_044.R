#### (TidyTuesday - 2021 - Week 44) #### 
#### ----------------------------------------------------------------------------------------- ####
## Ultra Trail Running data set
###  Week     Date	           Data	                    Source	                   Article
###  - 44     - 2021-10-26	   - Ultra Trail Running	  - BjnNowak-Github Repo	   - RunRepeat.com

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(scales)
library(ggtext)
library(gapminder)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_044/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_044/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

#### Data wrangling #### 
##### Which are the most popular Ultra Trail Running events in the world?
race_plot <- race %>% 
             mutate(country = if_else(country == "Hong Kong, China", "China", as.character(country)),
                    event = if_else(event == "Grand Raid De La Réunion", "Le Grand Raid De La Réunion", as.character(event))) %>% 
             left_join(gapminder %>% select(country, continent), by = c("country")) %>%        # We will use Gapminder dataset to extract countries continent information
             filter(participants != 0) %>% 
             arrange(desc(participants)) %>% 
             group_by(event) %>% 
             mutate(max_participants = max(participants)) %>% 
             filter(participants == max_participants) %>% 
             dplyr::slice_head(n = 1) %>% 
             ungroup() %>% 
             slice_max(order_by = max_participants, n = 40, with_ties = FALSE) %>% 
             mutate(continent = case_when(country %in% c("Russia","Ukraine") ~ "Europe", TRUE ~ as.character(continent))) %>%  # In gapminder there is no Russia and Ukraine
             add_count(country) %>% 
             add_count(continent, name = "continent_n") %>%
             arrange(continent_n, desc(n), max_participants) %>% 
             ungroup() %>%
             mutate(possition = seq(1:n())) %>% 
             mutate(event = case_when(event == "Le Grand Raid De La Réunion" ~ "Le Grand Raid", 
                                      TRUE ~ as.character(event)))


labels_barplot <- race_plot %>% select(event, city, country, continent, max_participants, possition)  

angle <-  90 - 360 * (labels_barplot$possition -0.5)/nrow(labels_barplot)     # Alignment to the center of the bars: substract 0.5 to center (extreme right = 1; extreme left = 0)
labels_barplot$hjust <- if_else(angle < -90, 1, 0)                            # Labels alignment: right or left (If I am on the left part of the plot, my labels have currently an angle < -90)
labels_barplot$angle <- if_else(angle < -90, angle + 180, angle)              # flip angle BY to make them readable

# Prepare a data frame for base lines
base_data <- race_plot %>% 
             filter(country %in% c("France", "China", "United States", "Spain")) %>% 
             group_by(country) %>% 
             summarize(start = min(possition), end = max(possition)) %>% 
             rowwise() %>% 
             mutate(title = mean(c(start, end)),
                    continent = case_when(country %in% c("France", "Spain") ~ "Europe",
                                          country %in% c("United States")~ "Americas",
                                          country %in% c("China")~ "Asia"))

# Prepare a data frame for colors code
colors_barplot <- tibble(continent = c("Europe", "Asia", "Americas"),
                                 x = c(35,2,11),
                                 y = rep(3000,3))

#### Plot aesthetics ####
background  <- c("#E4F2F2")
lines_color <- c("#310F3E")
title_color <- c("#261D1A")
subtitle_color <- c("#261D1A")
text_color  <- c("#261D1A")
caption_color  <- c("#667339")
pallete_color  <- c("Europe" = "#8C3041", "Asia" = "#037994", "Americas" = "#60A62E")

#### Annotation ####
annotation_title_text <- c("Popular Ultra Trail Running Events")
annotation_subtitle_text <- c("If you are a trail running lover you should definitely travel to these four countries which concentrate some of the most popular races. And go to **Le Grand Raid de la Réunion**, which since 1989 take place at the Réunion island (departments of France), and has 168.1 km and 9610 m of elevation gain and is crowned as the most popular trail running race with up to 2900 participants in 2019")
annotation_subtitle_text <- str_wrap(annotation_subtitle_text, 105) %>% str_replace_all("\n","<br>") # Helps to separate long texts into lines with the same maximum number of characters

#### Plot ####
Plot <- 
  race_plot %>%
  ### Layers base ###
  ggplot(aes(x = possition, y = max_participants, fill = continent)) + 
      ### Geom Layers ###
      geom_col() +
      ### Annotations ###
      ### Text Annotations ###
      geom_text(data = labels_barplot, aes(x = possition, y = max_participants + 100, label = event, hjust = hjust), color = "black", family = "Alumni Sans", fontface = "bold", alpha = if_else(labels_barplot$max_participants >= 1000, 0.8, 0.6), size = if_else(labels_barplot$max_participants >= 1000, 4.0, 3.0), angle = labels_barplot$angle, inherit.aes = FALSE) +  # I use conditional statements within layers such as if_else() and case_when() to assign differential values of y-axis position, alpha, size and labels to each value based on conditions.
      geom_text(data = labels_barplot, aes(x = possition, y = max_participants - case_when(max_participants >= 1000 & max_participants < 2900 ~ 600, max_participants == 2900 ~ 2000, T ~ max_participants * 0.82), label = if_else(max_participants == 2900, paste0(max_participants, " Participants"), as.character(max_participants)), hjust = hjust), color = background, family = "Alumni Sans", fontface = "bold", alpha = if_else(labels_barplot$max_participants >= 1000, 0.8, 0.6), size = if_else(labels_barplot$max_participants >= 1000, 3.5, 2.5), angle = labels_barplot$angle, inherit.aes = FALSE) +
      geom_text(data = colors_barplot, aes(x = x, y = y, label = continent, color = continent), hjust=c(2,0,0), family = "Alumni Sans", fontface = "bold", alpha = 0.8, size = 5.5, angle = 0, inherit.aes = FALSE) +
      # Add base line information
      geom_segment(data = base_data, aes(x = start, y = - 80, xend = end, yend = - 80, colour = continent), alpha = 0.8, size = 0.6 , inherit.aes = FALSE)  +
      geom_text(data = base_data, aes(x = title, y = if_else(country == "United States", -1200, -600), label = country, colour = continent), alpha = 0.8, size = 5, family = "Alumni Sans", fontface = "bold", inherit.aes = FALSE) +
      ### Scales ###
      scale_y_continuous(limits = c(-3000, 3000)) +       # The  negative value control the size of the internal circle
      scale_fill_manual(values = pallete_color) +
      scale_color_manual(values = pallete_color) +
      coord_polar(start = 0, clip = "off") +
      ### Theme ### 
      theme_classic() +
      theme(
            ## Text ##
            text = element_text(face = "plain", family = "Alumni Sans", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
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
            plot.title = element_markdown(color = title_color, family = "Big Shoulders Stencil Text", face = "plain", size = 24),
            plot.subtitle = element_markdown(color = subtitle_color, family = "Alumni Sans", face = "plain", size = 14),
            plot.caption.position = "plot",
            plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 7),
            ## Margin ##
            plot.margin = margin(t = 0.1, r = -1, b = -0.1, l = -1, unit = "cm")) +      # It is complicated to manage the size of the plot, so we use negative values to enlarge the plot.
  ### Labels ###
  labs(title = annotation_title_text,
       subtitle = annotation_subtitle_text,
       caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                        Source: International Trail Running Association (ITRA)<br>")
#### Progress ####
ggsave("./2021/2021_Week_044/Plots/2021_Week_044.png", Plot, dpi = 320, scale = 1, width = 7, height = 8, units = c("in"))

