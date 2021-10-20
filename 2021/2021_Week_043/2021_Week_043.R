#### (TidyTuesday - 2021 - Week 43) #### 
#### ----------------------------------------------------------------------------------------- ####
## Big Pumpkins data set
###  Week     Date	           Data	                 Source	                 Article
###  - 43     - 2021-10-19	   - Big Pumpkins	       - BigPumpkins.com	     - Great Pumpkin Commonwealth

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(scales)
library(ggtext)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_043/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_043/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
tt_data <- tt_load(2021, week = 43)

#### Data wrangling #### 
##### The impact of global seafood production on biological sustainability.

                        
#### Plot aesthetics ####
background  <- c("#023859")
lines_color <- c("#EBF0F2")
title_color <- c("#E6AC1C")
subtitle_color <- c("#EBF0F2")
text_color  <- c("#EBF0F2")
caption_color  <- c("#E6EDF2")
pallete_color  <- c(redmonder.pal(name = "qPBI", n = 8),"#387AA8","#C75153")[c(1,3:8,9:10)]

#### Annotation ####
annotation_title_text <- c("Seafood & fish production vs biological sustainability")
annotation_subtitle_text <- c("The production of millions of tons, within the seafood category, has tripled from 1961 to 2013. As a result, <br>
                              the balance between <b style='color:#387AA8;'>Sustainable</b> and <b style='color:#C75153;'>Overexploited</b> biological production has a growing biological unsustainability.")

#### Plot ####
seafood_prod_th_tonnes %>%
  ### Layers base ###
  ggplot(aes(x = Year, y = Production_tonnes, group = Food, fill = Food)) +
  geom_area() 
  ### Annotations ###
  ### Labels Annotations ###

  ### Scales ###

  ### Theme ###
  theme_classic() +
  theme(
        ## Text ##
        text = element_text(face = "plain", family = "BenchNine", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
        ## Axis ##
        axis.title.x = element_text(face = "plain", family = "BenchNine", color = text_color, size = 16, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.title.y = element_text(face = "plain", family = "BenchNine", color = text_color, size = 16, hjust = 0.5, vjust = 0.5, angle = 90, margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.text.x = element_text(face = "plain", family = "BenchNine", color = text_color, size = 14, hjust = 0.5, vjust = 0.5, angle = 0),
        axis.text.y = element_text(face = "plain", family = "BenchNine", color = text_color, size = 14, hjust = 0.5, vjust = 0.5, angle = 0),
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
        plot.title = element_markdown(color = title_color, family = "BenchNine", face = "bold", size = 20),
        plot.subtitle = element_markdown(color = subtitle_color, family = "BenchNine", face = "plain", size = 18),
        plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 0.5),
        ## Margin ##
        plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
        ### Labels ###
        labs(title = annotation_title_text,
             subtitle = annotation_subtitle_text,
             x = "Year",
             y = "Production (tonnes)",
             caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                        Source: OurWorldinData.org")

#### Progress ####
ggsave("./2021/2021_Week_043/Plots/2021_Week_043.png", dpi = 320, scale = 1, width = 12, height = 9, units = c("in"))
