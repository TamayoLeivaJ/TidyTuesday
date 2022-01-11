#### (TidyTuesday - 2022 - Week 02) #### 
#### ----------------------------------------------------------------------------------------- ####
## Starbucks drinks
###  Week     Date	           Data	                    Source	                   Article
###  - 02     - 2022-01-11	   - Bee Colony losses	    - USDA	                   - Bee Informed

#### set up ####
#### Libraries ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)
library(png)
library(ggimage)
library(MetBrewer)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_002/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2022/2022_Week_002/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

#### Data wrangling #### 
##### Bees





#### Plot aesthetics ####
background     <- "#F2F2F2"
lines_color    <- "#BFBFBD"
title_color    <- "#395938"
subtitle_color <- "#395938"
text_color     <- "#395938"
caption_color  <- "#2D402C"
color_palette  <- met.brewer(name="Signac", n=14, type="discrete")[c(9,3,11,12,13)]


#### Annotation ####
annotation_title_text <- c("Bees")
annotation_subtitle_text <- c("What the bee?")


#### Plot ####
Plot <- df %>% 
        ggplot(aes(x = x, y = y, size = size, fill = size)) +
        geom_point(shape = 21, color = "white") +
        ### Annotations ###
        geom_text() +
        ### Text Annotations ###
        geom_text() +
        ### Scales ###
            scale_x_continuous(limits = c(0, 830), breaks = seq(0, 700, 100)) +
            scale_y_continuous(breaks = seq(0, 500, 100), labels = scales::label_number(suffix = "mg")) +
            scale_size(range = c(4, 6), guide = "none") +
            scale_fill_manual(values = color_palette, na.value = "black", breaks = c(""), guide = "none") +
            scale_color_manual(values = color_palette, na.value = "black", breaks = c(""), guide = "none") +
            coord_cartesian(ylim = c(0, 500), clip = 'off') +
        ### Theme ### 
        theme_classic() +
        theme(
          ## Text ##
          text = element_text(face = "plain", family = "Oooh Baby", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
          ## Axis ##
          axis.title.x = element_text(size = 18, color = text_color),
          axis.title.y = element_text(size = 18, color = text_color),
          axis.text.y = element_text(size = 12, color = text_color),
          axis.text.x = element_text(size = 12, color = text_color),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          ## Panel Grid ##
          panel.grid.major.x = element_line(size = 0.2, color = lines_color),
          panel.spacing = unit(2, "lines"),
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background = element_rect(fill = background, color = NA),
          legend.background = element_rect(fill = background, color = NA),
          legend.key = element_rect(fill = background, color = NA),
          legend.text = element_text(face = "plain", family = "Oooh Baby", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0, size = 12),
          legend.title = element_text(face = "plain", family = "Merienda", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0, size = 18),
          ## Legend ##
          legend.position = "top",
          legend.direction = "horizontal",
          ## Titles & Caption ##
          plot.title.position = "panel",
          plot.title = element_markdown(color = title_color, family = "Montserrat", face = "plain", size = 30, hjust = 0.5),
          plot.subtitle = element_markdown(color = subtitle_color, family = "Merienda", face = "plain", size = 14, hjust = 0.5, margin = margin(t = 0.0, r = 0.0, b = 4.5, l = 0.0, unit = "cm")),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 9, margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) + 
        ### Labels ###
        labs(x = "",
             y = "",
             fill = "",
             size = "",
             title = annotation_title_text,
             subtitle = annotation_subtitle_text,
             caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                              <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                              Source: Starbucks Coffee Company Beverage Nutrition Information & PythonCoderUnicorn <br>")


#### Progress ####
ggsave("./2022/2022_Week_002/Plots/2022_Week_002.png", Plot, dpi = 700, scale = 1, width = 12, height = 10, units = c("in"))
