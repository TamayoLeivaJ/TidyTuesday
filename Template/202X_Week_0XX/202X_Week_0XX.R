#### (TidyTuesday - 2022 - Week XX) #### 
#### ----------------------------------------------------------------------------------------- ####
## 
###  Week     Date	           Data	       Source	       Article
###  - XX     - 2022-05-00	   - 	         - 	           - 


#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)

#### Libraries (This Plot) ####


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_0XX/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####



#### Data wrangling #### 
##### ?



#### Plot aesthetics ####
background     <- "#F2F2F2"
lines_color    <- "#3C5473"
title_color    <- "#39054D"
subtitle_color <- "#DB004C"
text_color     <- "#DB004C"
caption_color  <- "#39054D"


#### Annotation ####
annotation_title_text <- c("")
annotation_subtitle_text <- c("")
annotation_subtitle_text <- str_wrap(annotation_subtitle_text, 90) %>% 
                            str_replace_all("\n","<br>")


#### Plot ####
Plot <- df %>% 
        ### Annotations ###
        ### Scales ###
        ### Theme ###
        theme_minimal() +
        theme(
          ## Text ##
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
             y = "",
             fill = "",
             subtitle = annotation_subtitle_text,
             title = annotation_title_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                              <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>  TamayoLeivaJ<br><br> 
                              Source: Kaggle")


#### Progress ####
ggsave("./2022/2022_Week_0XX/Plots/2022_Week_0XX.png", Plot, dpi = 326, scale = 1, width = 6, height = 12, units = c("in"))
