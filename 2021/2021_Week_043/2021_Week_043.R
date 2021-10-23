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
library(gghalves)
library(scales)
library(ggtext)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_043/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_043/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
tt_data <- tt_load(2021, week = 43)
pumpkins <- tt_data$pumpkins

#### Data wrangling #### 
##### How gigantically scary can a giant pumpkin be?
# Types: F = "Field Pumpkin", P = "Giant Pumpkin", S = "Giant Squash", W = "Giant Watermelon", L = "Long Gourd" (length in inches, not weight in pounds),  = Tomato
Pumpkins <- pumpkins %>% 
  separate(col = id, into = c("Year", "Type"), sep = "-") %>% 
  mutate(Type = case_when(Type == "F" ~ "Field\nPumpkin",
                          Type == "P" ~ "Giant\nPumpkin",
                          Type == "S" ~ "Giant\nSquash",
                          Type == "W" ~ "Giant\nWatermelon",
                          Type == "L" ~ "Long\nGourd",
                          Type == "T" ~ "Tomato"),
         weight_lbs = str_remove(weight_lbs, pattern = ","),      # The column is character class and the numbers have "," as thousands separator. It is necessary to change it before coerce the column as numeric.
         weight_lbs = as.numeric(as.character(weight_lbs))) %>%   # Now we transform the column. To avoid stochastic behavior we use as.character to specify the transformation. 
  filter(!is.na(weight_lbs)) %>%                                  # Now we filter some rows of annotations (characters) in the weight_lbs column.
  filter(Type != "Tomato")                                        

# Get outliers values 
Outliers_GP <- sort(boxplot.stats(Pumpkins$weight_lbs[Pumpkins$Type %in% c("Giant\nPumpkin")])$out, decreasing = TRUE)
Outliers_GS <- sort(boxplot.stats(Pumpkins$weight_lbs[Pumpkins$Type %in% c("Giant\nSquash")])$out, decreasing = TRUE)

#### Plot aesthetics ####
background  <- c("#010326")
lines_color <- c("#310F3E")
title_color <- c("#D9DBD7")
subtitle_color <- c("#D9DBD7")
text_color  <- c("#D9DBD7")
caption_color  <- c("#3E688C")
pallete_color  <- c("#F2E30F","#96111C","#A68F1B","#FF5E35","#FF7F00")

#### Annotation ####
annotation_title_text <- c("Fear the giant pumpkins")
annotation_subtitle_text <- c("How gigantically scary can a giant pumpkin be?")

#### Plot ####
Plot <- 
Pumpkins %>%
  ### Layers base ###
  ggplot(aes(x = Type %>% fct_rev(), y = weight_lbs, color = Type, fill = Type)) +
  ### Secondary Layers ###
  geom_boxplot(width = .25, alpha = 0.75, size = .5, outlier.shape = NA, position = position_nudge(x = 0.25)) +
  geom_half_point(data = Pumpkins %>% group_by(Type) %>% filter(!Type %in% c("Giant\nPumpkin","Giant\nSquash")) %>% slice_sample(prop = 0.1), side = "l", range_scale = .75, alpha = 1/10, size = 2.5) +
  geom_half_point(data = Pumpkins %>% group_by(Type) %>% filter(Type %in% c("Giant\nSquash") & weight_lbs < min(Outliers_GS)) %>% slice_sample(prop = 0.1), side = "l", range_scale = .75, alpha = 1/10, size = 2.5) +
  geom_half_point(data = Pumpkins %>% group_by(Type) %>% filter(Type %in% c("Giant\nPumpkin") & weight_lbs < min(Outliers_GP)) %>% slice_sample(prop = 0.1), side = "l", range_scale = .75, alpha = 1/10, size = 2.5) +
  geom_segment(aes(x = "Giant\nPumpkin", xend = "Giant\nPumpkin", y = 2218, yend = 2703), color = text_color, arrow = arrow(ends = "both", angle = 90, length = unit(.1, "cm")), position = position_nudge(x = -0.20)) +
  geom_half_point(data = Pumpkins %>% filter(Type %in% c("Giant\nPumpkin") & weight_lbs >= min(Outliers_GP)), side = "l", range_scale = .90, alpha = 1/2, size = 2.5) +
  geom_half_point(data = Pumpkins %>% filter(Type %in% c("Giant\nSquash") & weight_lbs >= min(Outliers_GS)), side = "l", range_scale = .90, alpha = 1/2, size = 2.5) +
  ### Annotations ###
  ### Labels Annotations ###
  annotate(geom = "segment", x = -Inf, xend = Inf, y = 2204, yend = 2204, color = "#96111C", size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  geom_text(aes(x = "Giant\nPumpkin", y = 2800), color = text_color, size = 5, family = "Kaushan Script", label = "The biggest are between\n2200 ~ 2700 lbs\n(1.0 ~ 1.2 ton)", nudge_x = 0.50) +
  annotate(geom = "curve", x = "Long\nGourd", y = 1300, xend = "Giant\nWatermelon", yend = 2200, curvature = -.5, color = text_color, arrow = arrow(length = unit(0.4, "lines"), type = "closed")) +
  geom_text(aes(x = "Long\nGourd", y = 1200), color = text_color, size = 5, family = "Kaushan Script", label = "The heaviest vegetable in the world,\nmore than 1 ton (2204 pounds),\n the same as ~11 average men", nudge_x = -0.25) +
  ### Scales ###
  scale_y_continuous(breaks = c(0,1000,2000,3000), limits = c(0, 3200), labels = label_number(suffix = " lbs")) +
  scale_color_manual(values = pallete_color) +
  scale_fill_manual(values = pallete_color) +
  coord_flip(clip = "off") +
  ### Theme ### 
  theme_classic() +
  theme(
    ## Text ##
    text = element_text(face = "plain", family = "Kaushan Script", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
    ## Axis ##
    axis.title.x = element_text(face = "plain", family = "Kaushan Script", color = text_color, size = 18, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(t = 10, r = 0, b = 10, l = 0)),
    axis.title.y = element_text(face = "plain", family = "Kaushan Script", color = text_color, size = 18, hjust = 0.5, vjust = 0.5, angle = 90, margin = margin(t = 0, r = 10, b = 0, l = 10)),
    axis.text.x = element_markdown(face = "plain", family = "Kaushan Script", color = text_color, size = 14, hjust = 0.5, vjust = 0.5, angle = 0),
    axis.text.y = element_text(face = "plain", family = "Kaushan Script", color = text_color, size = 18, hjust = 0.5, vjust = 0.5, angle = 0),
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
    plot.title = element_markdown(color = title_color, family = "IM FELL English SC", face = "plain", size = 32),
    plot.subtitle = element_markdown(color = subtitle_color, family = "IM FELL English SC", face = "plain", size = 26),
    plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 0.5),
    ## Margin ##
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
  ### Labels ###
  labs(title = annotation_title_text,
       subtitle = annotation_subtitle_text,
       y = "Weigth",
       x = "Giant Pumpkin Types",
       caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                        Source: BigPumpkins.com")

#### Progress ####
ggsave("./2021/2021_Week_043/Plots/2021_Week_043.png", Plot, dpi = 320, scale = 1, width = 9, height = 12, units = c("in"))

