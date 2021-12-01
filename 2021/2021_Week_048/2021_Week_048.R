#### (TidyTuesday - 2021 - Week 49) #### 
#### ----------------------------------------------------------------------------------------- ####
## Dr. Who
###  Week     Date	           Data	                    Source	                   Article
###  - 49     - 2021-11-30	   - World Cup Cricket	    - ESPN Cricinfo	           - Wikipedia

#### set up ####
#### Libraries ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)
library(lubridate)
library(png)
library(ggfx)
library(ggimage)
library(patchwork)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_049/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_049/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')


#### Data wrangling #### 
##### Top cricket countries and their streaks
matches <- matches %>% 
           mutate(match_date_str = str_replace(match_date, pattern = "(\\-[0-9]+\\,)", replacement = ","),  # Some dates are periods (e.g., January 8-9, 2000) so we look for the pattern "(\\-[0-9]+\\,)" and substitute "," for it (i.e., "-9," to ","). The "+" means that there may be one or more numbers within the pattern.                  
                  match_date_str = parse_date_time(match_date_str, orders = c("mdy")),
                  day = day(match_date_str),
                  month = month(match_date_str),
                  year = year(match_date_str)) %>% 
           group_by(winner) %>% 
           add_count(name = "total_wins") %>%
           group_by(year, winner) %>% 
           add_count(name = "annual_wins") %>% 
           arrange(-annual_wins, -total_wins) %>% 
           #mutate(year = as.factor(as.numeric(year))) %>% 
           ungroup()

countries <- matches %>%
             select(winner, year, annual_wins, total_wins) %>% 
             distinct() %>%
             group_by(winner) %>% 
             filter(n() == 10) %>% 
             mutate(annual_wins_Levels = case_when(annual_wins >= 1 & annual_wins <= 10 ~ "1-10",
                                                   annual_wins >= 11 & annual_wins <= 20 ~ "10-20",
                                                   annual_wins >= 21 & annual_wins <= 30 ~ "20-30")) %>% 
             ungroup() 

countries <- countries %>%
             bind_rows(tibble(winner = rep(sort(unique(countries$winner)),3),
                              year = sort(rep(1993:1995,9)))) %>% 
             mutate(img = "./2021/2021_Week_049/Images/cricket_bat.png")
             

#1993-2005


#### Plot aesthetics ####
background  <- c("#91D9C4")
lines_color <- c("#FFFFE0")
title_color <- c("#2A2740")
subtitle_color <- c("#2A2740")
text_color  <- c("#2A2740")
caption_color  <- c("#023373")


#### Annotation ####
annotation_title_text <- c("ICC Men's Cricket World Cup")
annotation_subtitle_text <- c("Teams of the major cricket nations and their annual and total streaks from 1996 to 2005")

#### Plot Annotation ####
Plot_Annotation <- ggplot(tibble(x = 1, y = 1), aes(x=x, y=y)) +
                   geom_point(color = lines_color, fill = "#46ACC8", shape = 21, size = 40) +
                   geom_text(aes(x = 1, y = 1, label = "Total Wins\n1996-2005"), size = 5.2, color = lines_color, family = "Playball") +
                   ggpubr::theme_transparent()

#### Plot ####
Plot <- countries %>% 
        ggplot() +
        geom_image(aes(x = 1997.8, y = 1, image = img), stat = "unique", size = 1.0, asp = 1.1) +
        as_reference(geom_image(aes(x = 1997.8, y = 1, image = img), stat = "unique", size = 1.0, asp = 1.1), id = "bat") +
        with_blend(geom_tile(aes(x = year, y = 0, width = 1, height = 11, fill = annual_wins_Levels)), bg_layer = "bat", blend_type = "in") +
        ### Annotations ###
        geom_point(aes(x = 2003.5, y = -3.5, size = total_wins), color = lines_color, fill = "#46ACC8", shape = 21) +
        geom_text(aes(x = 2003.5, y = -3.5, label = total_wins), size = 3, color = lines_color) +
        ### Text Annotations ###
        ### Scales ###
        scale_size(range = c(8, 12), guide = "none") +
        scale_x_continuous(limits = c(1992, 2006), breaks = c(1996, 1999, 2002, 2005)) +
        scale_fill_manual(values = c("#E2D100", "#E58600","#B41921"), na.value = "black", breaks = c("1-10","10-20","20-30"), #colours = c("#F2B705","#F27405","#D95204")
                          guide = guide_legend(keyheight = unit(1, "lines"), keywidth = unit(2, "lines"), show.limits = TRUE, title.position = "top", label.position = "bottom", title.hjust = 0.5, nrow = 1)) +
        coord_cartesian(expand = TRUE, clip = "off") +
        facet_wrap(~winner, ncol = 3) +
        ### Theme ### 
        theme_classic() +
        theme(
          ## Text ##
          text = element_text(face = "plain", family = "Playball", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
          ## Axis ##
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10, color = text_color),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          ## Panel Grid ##
          panel.grid.major.x = element_line(size = 0.2, color = lines_color),
          panel.spacing = unit(2, "lines"),
          ## Strip ##
          strip.background = element_rect(fill = background, color = NA),
          strip.text = element_text(size = 24, color = text_color, family = "Playball"),
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background = element_rect(fill = background, color = NA),
          legend.background = element_rect(fill = background, color = NA),
          legend.key = element_rect(fill = background, color = NA),
          legend.text = element_text(face = "plain", family = "Playball", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0, size = 12),
          legend.title = element_text(face = "plain", family = "Playball", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0, size = 18),
          ## Legend ##
          legend.position = "top",
          legend.direction = "horizontal",
          ## Titles & Caption ##
          plot.title.position = "panel",
          plot.title = element_markdown(color = title_color, family = "Playball", face = "plain", size = 30, hjust = 0.5),
          plot.subtitle = element_markdown(color = subtitle_color, family = "Playball", face = "plain", size = 14, hjust = 0.5),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 9, margin = margin(t = 1.0, r = 0.5, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 1.5, unit = "cm")) +
        ### Labels ###
        labs(fill = "Annual Wins",
             title = annotation_title_text,
             subtitle = annotation_subtitle_text,
             caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                              <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                              Source: ESPN Cricinfo<br>")

#### Final Plot ####
Plot_F <- Plot + 
          inset_element(Plot_Annotation, left = 0, bottom = 1, right = 0.3, top = 0.8, align_to = "full") + 
          plot_annotation(theme = theme(plot.background = element_rect(fill = background, color = NA)))

#### Progress ####
ggsave("./2021/2021_Week_049/Plots/2021_Week_049.png", Plot_F, dpi = 320, scale = 1, width = 12, height = 9, units = c("in"))
