#### (TidyTuesday - 2021 - Week 39) #### 
#### ----------------------------------------------------------------------------------------- ####
## Emmy Awards data set
###  Week     Date	           Data	                 Source	                 Article
###  - 39     - 2021-09-21     - Emmy Awards         - Emmys                 - Susie Lu

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(grid)
library(ggforce)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_039/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_039/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
tt_data <- tt_load(2021, week = 39)


#### Data wrangling #### 
##### Who are the Formula One constructor with more consecutive World Championship wins?

                       
#### Annotation List ####


#### Text ####
annotation_title_text <- c("Who are the Formula One constructor with more consecutive World Championship victories?")
annotation_subtitle_text <- c("The Constructors' Championship was not awarded until 1958. Since then, \nfive constructors' teams have won three or more world championships.")

#### Images ####
##### Team Logo #####


##### Team Cars #####

#### Plot aesthetics ####
background  <- c("#F2F2F3")
lines_color <- c("#15151E")
text_color  <- c("#15151E")
constructor_color <- c("Ferrari" = "#D90404", "McLaren" = "#F28705", "Mercedes" = "#02D2BE", "Red Bull" = "#1600EF", "Williams" = "#0059FF")

#### Plot ####
     #ggplot(aes(x = year, y = constructor_points)) + 
     ### Layers base ###
     #geom_point(aes(fill = constructor_name), size = 2, color = "#696969", alpha = 1/4, shape = 21) +
     ### Annotations ###
     ### Clusters Annotations ###
     #geom_mark_ellipse(aes(label = "(1975–1977)", filter = constructor_name %in% constructor_teams[1] & between(year, 1975, 1977)), label.fill = "transparent", color = constructor_color[1], label.colour = constructor_color[1], con.colour = constructor_color[1], label.buffer = unit(50, 'mm'), expand = unit(2.0, "mm")) +
     ### Layers extra ###
     #geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[1] & between(year, 1999, 2004)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     ### image Annotations ###
     #annotation_custom(rasterGrob(image = img_Formula1_logo, width = 0.15, x = 0.05, y = -0.1)) +
     ### Scales ###
     #scale_x_continuous(breaks = seq(1950, 2020, 5), limits =(c(1950, 2021))) +
     #scale_y_continuous(expand = c(0, 0.01), breaks = seq(0, 900, 300), limits =(c(0, 950))) +
     #scale_fill_manual(values = constructor_color) +
     #coord_cartesian(clip = "off") +
     ### Guides ###
     guides(color = guide_legend(title.position = "top", override.aes = list(size = 4)), order=2) +
     ### Theme ###
     theme_classic() +
     theme(
       ## Text ##
       ## Axis ##
       axis.title.x = element_text(face = "plain", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
       axis.title.y = element_text(face = "plain", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 90, margin = margin(t = 0, r = 10, b = 0, l = 0)),
       axis.text.x = element_text(face = "plain", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
       axis.text.y = element_text(face = "plain", color = text_color, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
       axis.line.x = element_line(colour = lines_color),
       axis.ticks.x = element_line(colour = lines_color),
       axis.line.y = element_line(colour = lines_color),
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
       plot.title = element_markdown(color = "firebrick4", family = "Avenir", face = "bold"),
       plot.subtitle = element_markdown(color = "firebrick4", family = "Avenir", face = "plain"),
       plot.caption = element_markdown(color = "firebrick4", family = "Menlo", hjust = 1, vjust = -55),
       ## Margin ##
       plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
     ### Labels ###
     labs(title = annotation_title_text,
          subtitle = annotation_subtitle_text,
          x = "Season",
          y = "Constructor Points by Season",
          caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                     <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                     Source: Ergast API")

#### Progress ####
ggsave("./2021/2021_Week_039/Plots/2021_Week_039.png", dpi = 300, scale = 1, width = 12, height = 8, units = c("in"))