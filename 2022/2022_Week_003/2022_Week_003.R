#### (TidyTuesday - 2022 - Week 03) #### 
#### ----------------------------------------------------------------------------------------- ####
## Chocolate Bar ratings
###  Week     Date	           Data	                    Source	                   Article
###  - 02     - 2022-01-18	   - Chocolate Bar ratings	- Flavors of Cacao	       - Will Canniford on Kaggle

#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)

#### Libraries (Plot) ####
library(png)
library(ggimage)
library(ggdist)
library(MetBrewer)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_003/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')


#### Data wrangling #### 
##### Chocolate Bar ratings by country
chocolate_rating <- chocolate %>% 
                    mutate(country_of_bean_origin = case_when(country_of_bean_origin %in% c("Trinidad","Tobago") ~ "Trinidad and Tobago", T ~ country_of_bean_origin)) %>% 
                    group_by(country_of_bean_origin) %>% 
                    add_count(name = "Counts") %>% 
                    mutate(rating_median = median(rating),
                           rating_Q4 = quantile(rating, probs = 1),
                           x_possition = min(rating)) %>% 
                    filter(rating_median > 3.2 & Counts >= 10) %>% 
                    left_join(gapminder::gapminder_unfiltered %>% select(country, continent) %>% distinct(), by = c("country_of_bean_origin"="country")) %>%  # We will use Gapminder dataset to extract countries continent information
                    mutate(continent = case_when(country_of_bean_origin == "U.S.A." ~ "Americas", 
                                                 country_of_bean_origin == "Congo" ~ "Africa",
                                                 T ~ as.character(continent))) %>% 
                    ungroup()

cont_labels <- chocolate_rating %>% 
               select(continent) %>% 
               distinct() %>% 
               mutate(x = 0,
                      y = seq(7, 22, 5))

#### Plot aesthetics ####
background     <- "#004F63"
lines_color    <- "#223240"
title_color    <- "#FFFFFF"
subtitle_color <- "#7C9AA6"
text_color     <- "#FFFFFF"
caption_color  <- "#FFFFFF"
color_palette  <- met.brewer("Cross", 6)


#### Annotation ####
annotation_title_text <- c("Good chocolate beans come<br>from many places")
annotation_subtitle_text <- c("Only countries of origin of the bean with an<br>median value >3.2 and at least 10 evaluations are shown.")


#### Plot ####
Plot <- chocolate_rating %>% 
        ggplot(aes(x = rating, y = fct_reorder(country_of_bean_origin, rating_median, .desc = FALSE), fill = continent, group = country_of_bean_origin)) +
        stat_halfeye(adjust = .4, width = .4, justification = .25, .width = 0, point_colour = NA ) +
        geom_boxplot(width = .4, outlier.size = 1.0, color = "white", position = position_nudge(y = -.25)) +
        ### Annotations ###
        ### Text Annotations ###
        geom_text(aes(x = x_possition, y = country_of_bean_origin, label = country_of_bean_origin, color = continent), size = 7, family = "Anton", nudge_x = -0.1, hjust = 1, vjust = 0.5, alpha = 0.8) +
        geom_text(data = cont_labels, aes(x = x, y = y, label = continent, color = continent), size = 12, family = "Anton", angle = 90, inherit.aes = FALSE) +
        ### Scales ###
        guides(color = "none", fill = "none") +
        scale_x_continuous(breaks = seq(1,4,1)) +
        scale_color_manual(values = color_palette) +
        scale_fill_manual(values = color_palette) +
        coord_cartesian(xlim = c(-0.2, 4), clip = 'off') +
        ### Theme ###
        theme_classic() +
        theme(
          ## Text ##
          text = element_text(face = "plain", family = "Anton", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
          ## Axis ##
          axis.title.x = element_text(size = 18, color = text_color),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14, color = text_color),
          axis.line.x = element_blank(),
          axis.ticks.x = element_line(size = 0.1, color = lines_color),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          ## Panel Grid ##
          panel.grid.major.x = element_line(size = 0.1, color = lines_color, linetype = "longdash"),
          panel.spacing = unit(2, "lines"),
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background = element_rect(fill = background, color = NA),
          ## Titles & Caption ##
          plot.title.position = "panel",
          plot.title = element_markdown(color = title_color, family = "Pattaya", face = "plain", size = 36, hjust = 0.50, halign = 0.5),
          plot.subtitle = element_markdown(color = subtitle_color, family = "Pattaya", face = "plain", size = 18, hjust = 0.5, halign = 0.5, margin = margin(t = 0.0, r = 0.0, b = 0.2, l = 0.0, unit = "cm")),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 12, margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
        ### Labels ###
        labs(x = "Chocolate Bars Rating",
             y = "",
             fill = "",
             subtitle = annotation_subtitle_text,
             title = annotation_title_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                              <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                              Source: Flavors of Cacao")


#### Progress ####
ggsave("./2022/2022_Week_003/Plots/2022_Week_003.png", Plot, dpi = 320, scale = 1, width = 8, height = 16, units = c("in"))