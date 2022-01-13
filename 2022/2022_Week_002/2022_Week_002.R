#### (TidyTuesday - 2022 - Week 02) #### 
#### ----------------------------------------------------------------------------------------- ####
## Bee Colony losses
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
library(geomtextpath)
library(patchwork)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_002/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2022/2022_Week_002/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

#### Data wrangling #### 
##### Bees
colonies <- colony %>% 
            mutate(colony_lost_pct = -colony_lost_pct/100,
                   colony_reno_pct =  colony_reno_pct/100) %>%  
            pivot_longer(names_to = "colony_pct", cols = c(colony_lost_pct, colony_reno_pct), values_to = "pct") %>%
            mutate(months_order = case_when(months == "January-March" ~ 1,
                                            months == "April-June" ~ 2,
                                            months == "July-September" ~ 3,
                                            months == "October-December" ~ 4)) %>% 
            arrange(year, months_order, state) %>% 
            filter(!is.na(pct) & state != "United States") %>% 
            ungroup()


stressor_USA <- stressor %>% 
                filter(!is.na(stress_pct) & state != "United States") %>% 
                group_by(year, stressor) %>% 
                mutate(stressor_mean = mean(stress_pct/100),
                       stressor = if_else(stressor %in% c("Other","Unknown","Disesases"), "Disesases, unknown cause or other cause", stressor)) %>%
                summarise(stress_pct = mean(stress_pct/100)) %>% 
                arrange(year, stress_pct) %>% 
                ungroup()
                  
                
bees <-  tibble(img = c("./2022/2022_Week_002/Images/bee_left.png","./2022/2022_Week_002/Images/bee_right.png"),
                colony_pct = c("colony_lost_pct","colony_reno_pct"),
                label = c("Lost\nColonies","Renovated\nColonies"),
                x = c(2019, 2020),
                y = c(0.43, 0.43),
                x_possition = x - if_else(label == "Lost\nColonies", 0.6, -0.6),
                y_possition = y)


#### Plot aesthetics ####
background     <- "#FFFFFF"
lines_color    <- "#BFBFBD"
title_color    <- "#58555A"
subtitle_color <- "#58595B"
text_color     <- "#012E40"
caption_color  <- "#2D402C"
color_palette  <- c("#110066","#549DA6","#038ABF","#F21B1B","#BF9004","#58555A")


#### Annotation ####
annotation_title_text <- c("Loss of honey bee colonies in the U.S.")
annotation_subtitle_text <- c("The Varroa mite is an external parasite that attacks and feeds on honey bees *Apis cerana* and *Apis mellifera*. The Varroa mite can only reproduce in a honey bee colony, and the disease caused by the mite is called varroosis. During infection, the parasitic mite attaches itself to the bee's body and weakens it by sucking out the fat bodies. The Varroa mite has the greatest negative impact among honey bee colonies in the U.S. state and worldwide. Since 2015 and through 2021, Varroa mite have affected about 30% of honey bee colonies in U.S. states, and although efforts have been made to renew honey bee colonies, lost colonies in U.S. states shows higher median values (50th percentile) than renovated colonies, even when some states show outliers.")
annotation_subtitle_text <- str_wrap(annotation_subtitle_text, 68) %>% str_replace_all("\n","<br>") # Helps to separate long texts into lines with the same maximum number of characters
annotation_subtitle_text <- annotation_subtitle_text %>% 
                            str_replace_all("Varroa mite","<b><i style='color:#110066'>Varroa mite</i></b>") %>% 
                            str_replace_all("lost colonies","<b style='color:#58555A'>lost colonies</b>") %>% 
                            str_replace_all("renovated colonies","<b style='color:#BF9004'>renovated colonies</b>") # Exchange normal strings for specially formatted HTML strings

#### Theme ####
theme_set(theme_classic() +
          theme(
            ## Text ##
            text = element_text(face = "plain", family = "Alegreya Sans", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
            ## Axis ##
            axis.title.x = element_text(size = 18, color = text_color),
            axis.title.y = element_text(size = 18, color = text_color, margin = margin(t = 0.0, r = 20.0, b = 0.0, l = 0.0)),
            axis.text.y = element_text(size = 14, color = text_color),
            axis.text.x = element_text(size = 14, color = text_color),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            ## Panel Grid ##
            panel.grid.major.x = element_line(size = 0.1, color = lines_color),
            panel.spacing = unit(2, "lines"),
            ## Plot Aesthetic ##
            panel.background = element_rect(fill = background, color = NA),
            plot.background = element_rect(fill = background, color = NA),
            ## Titles & Caption ##
            plot.title.position = "panel",
            plot.title = element_markdown(color = title_color, family = "Anton", face = "plain", size = 24, hjust = 0.90, halign = 0.5),
            plot.subtitle = element_markdown(color = subtitle_color, family = "Alegreya Sans", face = "plain", size = 14, hjust = 0.90, halign = 0.5, margin = margin(t = 0.0, r = 0.0, b = 3.0, l = 0.0, unit = "cm")),
            plot.caption.position = "plot",
            plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 9, margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
            ## Margin ##
            plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")))


#### Plot ####
Plot <- colonies %>% 
        ggplot(aes(x = year, y = pct, color = colony_pct, group = colony_pct)) +
        annotate(geom = "segment", x = 2014.5, xend = 2021.5, y = 0, yend = 0, color = lines_color, size = 0.1) + 
        geom_point(position = position_jitter(seed = 707), alpha = 0.25) +
        stat_summary(geom="line", aes(color = colony_pct, group = colony_pct), fun = function(x) quantile(x, 0.5)) +
        stat_summary(fun = function(x) quantile(x, 0.5), size = 1.5, shape = 18) +
        ### Annotations ###
        geom_image(data = bees, aes(x = x, y = y, image = img, color = colony_pct), size = 0.10, asp = 2.0, inherit.aes = FALSE) +
        geom_text(data = bees, aes(x = x_possition, y = y_possition, label = label, color = colony_pct), size = 5, family = "Alegreya Sans", inherit.aes = FALSE) +
        ### Text Annotations ###
        stat_summary(geom = "text", fun = function(x) quantile(x, 0.5), aes(label = scales::percent(abs(round(..y.., 2)), accuracy = 1), color = colony_pct, hjust = 1.5, vjust = if_else(..y.. > 0, -2.0, 2.0)), size = 6, family = "Alegreya Sans", fontface = "bold") +
        ### Scales ###
            scale_x_continuous(breaks = seq(2015, 2021, 1)) +
            scale_y_continuous(breaks = seq(-0.3,0.3, 0.1), labels = function(x) percent(abs(x))) +
            scale_color_manual(values = color_palette[c(6,5)], na.value = "black", guide = "none") +
            coord_cartesian(ylim = c(-0.3,0.3), clip = 'off') +
        ### Labels ###
        labs(x = "Year",
             y = "Percentage with respect to \nthe total number of colonies",
             fill = "",
             size = "",
             title = annotation_title_text,
             subtitle = annotation_subtitle_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                              <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                              Source: USDA")

#### Plot Annotation ####
Plot_Annotation <- stressor_USA %>% 
                   ggplot(aes(x = year, y = stress_pct, color = stressor, label = stressor)) +
                   geom_line(size = 2) +
                   geom_textline(vjust = if_else(stressor_USA$stressor == "Disesases, unknown cause or other cause", 1.5, -0.5), size = 4.5) +
                   ### Scales ###
                       scale_x_continuous(breaks = seq(2015, 2021, 1)) +
                       scale_y_continuous(breaks = seq(0, 0.5, 0.1), labels = scales::label_percent(accuracy = 1)) +
                       scale_color_manual(values = color_palette[c(1:4)], breaks = c("Varroa mites","Other pests/parasites","Pesticides","Disesases, unknown cause or other cause"), na.value = "black", guide = "none") +
                       coord_cartesian(ylim = c(0, 0.4), clip = 'off') +
                   theme(panel.background = element_rect(fill = "transparent", color = NA),
                         plot.background = element_rect(fill = "transparent", color = NA)) +
                   ### Labels ###
                   labs(x = "",
                        y = "Percent of colonies\naffected by stressors")

#### Final Plot ####
Plot <- Plot + inset_element(Plot_Annotation, left = 0, bottom = 0.6, right = 0.5, top = 1, align_to = 'full')

#### Progress ####
ggsave("./2022/2022_Week_002/Plots/2022_Week_002.png", Plot, dpi = 320, scale = 1, width = 12, height = 10, units = c("in"))
