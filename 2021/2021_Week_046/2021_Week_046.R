#### (TidyTuesday - 2021 - Week 46) #### 
#### ----------------------------------------------------------------------------------------- ####
## Learning with afrilearndata
###  Week     Date	           Data	                    Source	                   Article
###  - 46     - 2021-11-09	   - afrilearndata	        - afrilearndata	           - afrilearndata

#### set up ####
#### Libraries ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)
library(afrilearndata)
library(raster)
library(ggthemes)
library(patchwork)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_046/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
data(afripop2000)
data(afripop2020)

#### Data wrangling #### 
##### Which are the most popular Ultra Trail Running events in the world?
africa_2000 <- afripop2000 %>% 
               as.data.frame(xy=TRUE) %>% 
               rename("Pop" = "ppp_2000_1km_Aggregated") %>% 
               filter(!is.na(Pop))

africa_2000 <- africa_2000 %>% 
               mutate(Pop_Levels = case_when(Pop < 1 ~ "0",
                                             Pop > 0 & Pop < 10 ~ "1-10",
                                             Pop >= 10 & Pop < 100 ~  "10-100",
                                             Pop >= 100 & Pop < 1000 ~ "100-1k",
                                             Pop >= 1000 & Pop < 10000 ~ "1k-10k",
                                             Pop >= 10000 & Pop < 20000 ~ "10k-20k",
                                             Pop >= 20000 ~ "≥ 20k")) 

africa_2020 <- afripop2020 %>% 
               as.data.frame(xy=TRUE) %>% 
               rename("Pop" = "ppp_2020_1km_Aggregated") %>% 
               filter(!is.na(Pop))

africa_2020 <- africa_2020 %>% 
               mutate(Pop_Levels = case_when(Pop < 1 ~ "0",
                                             Pop > 0 & Pop < 10 ~ "1-10",
                                             Pop >= 10 & Pop < 100 ~  "10-100",
                                             Pop >= 100 & Pop < 1000 ~ "100-1k",
                                             Pop >= 1000 & Pop < 10000 ~ "1k-10k",
                                             Pop >= 10000 & Pop < 20000 ~ "10k-20k",
                                             Pop >= 20000 ~ "≥ 20k")) 

#### Plot aesthetics ####
background  <- c("#E4F2F2")
lines_color <- c("#310F3E")
title_color <- c("#261D1A")
subtitle_color <- c("#261D1A")
text_color  <- c("#261D1A")
caption_color  <- c("#667339")

#### Annotation ####
annotation_title_text <- c("Rising population density in Africa")

#### Plot ####
##### Africa Map 2000 #####
Plot_2000 <- 
  africa_2000 %>% 
  ### Layers base ###
  ggplot(aes(x=x, y=y, fill = Pop_Levels %>% fct_reorder(Pop), color=Pop_Levels %>% fct_reorder(Pop))) +  
      ### Geom Layers ###
      geom_tile() +
      ### Annotations ###
      ### Text Annotations ###
      geom_text(aes(x = 0, y = -15, label = "2000"), color = text_color, family = "Oswald", alpha = 1, size = 6) +
      ### Scales ###
      scale_fill_manual(values = viridis::inferno(n=7), name = NULL, guide = "none") +
      scale_color_manual(values = viridis::inferno(n=7), name = NULL, guide = "none") +
      coord_map() +
      ### Theme ### 
      theme_map() +
      theme(
            ## Text ##
            text = element_text(face = "plain", family = "Oswald", color = text_color, size = 14, hjust = 0.5, vjust = 0.5, angle = 0),
            ## Axis ##
            ## Panel Grid ##
            ## Plot Aesthetic ##
            panel.background = element_rect(fill = background, color = NA),
            plot.background = element_rect(fill = background, color = NA),
            ## Legend ##
            legend.text = element_text(face = "plain", family = "Oswald", color = text_color, size = 14, hjust = 0.5, vjust = 0.5, angle = 0),
            legend.background = element_rect(fill = background, color = NA),
            legend.key = element_rect(fill = background, color = NA),
            legend.box.background  = element_rect(fill = background, color = NA),
            legend.position = "top",
            ## Margin ##
            plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"))

##### Africa Map 2020 #####
Plot_2020 <- 
  africa_2020 %>% 
  ### Layers base ###
  ggplot(aes(x=x, y=y, fill = Pop_Levels %>% fct_reorder(Pop), color=Pop_Levels %>% fct_reorder(Pop))) +  
      ### Geom Layers ###
      geom_tile() +
      ### Annotations ###
      ### Text Annotations ###
      geom_text(aes(x = 0, y = -15, label = "2020"), color = text_color, family = "Oswald", alpha = 1, size = 6) +
      ### Scales ###
      scale_fill_manual(values = viridis::inferno(n=7), name = "Population\n(people/km²)", 
                        guide = guide_legend(keyheight = unit(1, "lines"), keywidth = unit(2, "lines"), show.limits = TRUE, label.position = "bottom", title.position = "top", title.hjust = 0.5, nrow = 1)) +
      scale_color_manual(values = viridis::inferno(n=7), name = NULL, guide = "none") +
      coord_map() +
      ### Theme ### 
      theme_map() +
      theme(
            ## Text ##
            text = element_text(face = "plain", family = "Oswald", color = text_color, size = 14, hjust = 0.5, vjust = 0.5, angle = 0),
            ## Axis ##
            ## Panel Grid ##
            ## Plot Aesthetic ##
            panel.background = element_rect(fill = background, color = NA),
            plot.background = element_rect(fill = background, color = NA),
            ## Legend ##
            legend.text = element_text(face = "plain", family = "Oswald", color = text_color, size = 14, hjust = 0.5, vjust = 0.5, angle = 0),
            legend.background = element_rect(fill = background, color = NA),
            legend.key = element_rect(fill = background, color = NA),
            legend.box.background  = element_rect(fill = background, color = NA),
            legend.position = "top",
            ## Margin ##
            plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) 

##### Final Plot #####
design <- c("AB")

Plot <- 
  Plot_2000/Plot_2020 +
  plot_layout(design = design, guides = "collect") &
  plot_annotation(title = annotation_title_text,
                  caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                             <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                             Source: afrilearndata",
                  theme = theme(
                    ## Plot Aesthetic ##
                    panel.background = element_rect(fill = background, color = NA),
                    plot.background = element_rect(fill = background, color = NA),
                    ## Legend ##
                    legend.position = "top",
                    ## Titles & Caption ##
                    plot.title.position = "plot",
                    plot.title = element_text(color = title_color, family = "Oswald", face = "plain", size = 24, hjust = 0, margin = unit(c(t = 0.1, r = 0.0, b = 0.1, l = 0.0), unit = "cm")),
                    plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 0.5, size = 9),
                    ## Margin ##
                    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")))

#### Progress ####
ggsave("./2021/2021_Week_046/Plots/2021_Week_046.png", Plot, dpi = 320, scale = 1, width = 10, height = 8, units = c("in"))

