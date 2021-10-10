#### (TidyTuesday - 2021 - Week 41) #### 
#### ----------------------------------------------------------------------------------------- ####
## Registered Nurses data set
###  Week     Date	           Data	                 Source	                 Article
###  - 41     - 2021-10-05      - Registered Nurses     - Data.World            - BLS

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(broom)
library(geojsonio)
library(rgdal)
library(rgeos)
library(ggtext)
library(patchwork)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_041/Data"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_041/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_041/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
tt_data <- tt_load(2021, week = 41)
nurses <- tt_data$nurses # Extract key data

us_states <- geojson_read("./2021/2021_Week_041/Data/us_states_hexgrid.geojson",  what = "sp") # Get the geoJSON file to plot the hexbin map

#### Data wrangling #### 
##### The impact of COVID-19 on U.S. nurses
###### Map data wrangling ######
us_states@data = us_states@data %>% 
                 # Remove the "(United States)" of all state names, then remove white space at the right
                 mutate(google_name = str_remove(google_name, pattern = "\\(United States\\)") %>% str_trim(side = "right")) 

# Obtain the centroid of each hexbin in order to plot centered text labels
centers <- cbind.data.frame(data.frame(gCentroid(us_states, byid=TRUE), id = us_states@data$iso3166_2, State = us_states@data$google_name)) 

# We need to transform it from an "sp" object to a "data frame" in order to use it in ggplot2 
us_states_df <- tidy(us_states, region = "google_name") 


nurses_us_states <- nurses %>%
                    janitor::clean_names(case = "none") %>% # Remove spaces in column names, but keep case unchanged
                    right_join(us_states_df, by = c("State" = "id")) %>% 
                    left_join(centers %>% select(id, State), by = c("State"))
 
 
#### Plot aesthetics ####
background  <- c("#4B83A6")
lines_color <- c("#DFEDF2")
title_color <- c("#F2F2F2")
subtitle_color <- c("#E4F2F1")
text_color  <- c("#DFEDF2")
caption_color  <- c("#D9D9D9")
  
#### Annotation ####
annotation_title_text <- c("Annual Salary and Number of Registered Nurses on the U.S.")
annotation_subtitle_text <- c("The state of California has the highest number of registered nurses and the highest annual<br> 
                              (median) salary. Although it has always been the state with the most RNs, since 1998 the<br>
                              number has increased by more than half, from nearly 172 k to 307 k, while the median annual<br>
                              salary has also increased in two decades from less than $50 K to more than $110 K.")

#### Plot ####
##### Hexbin Map Plot #####
Plot_Hexbin <-
nurses_us_states %>%   
   filter(Year == 2020) %>%
   ggplot() +
   geom_polygon(aes(fill = Annual_Salary_Median, x = long, y = lat, group = group), color="white") +
   geom_text(data = centers %>% filter(id %in% c(nurses_us_states %>% filter(Annual_Salary_Avg <  102000) %>% pull(id) %>% unique())), aes(x = x, y = y, label = id), color = text_color) +
   geom_text(data = centers %>% filter(id %in% c(nurses_us_states %>% filter(Annual_Salary_Avg >= 102000) %>% pull(id) %>% unique())), aes(x = x, y = y, label = id), color = "black") +
   scale_fill_stepsn(colours = viridis::inferno(n = 5, begin = 0.15, end = 0.85, direction = 1), labels = scales::label_dollar(scale = 1/1000, accuracy = 1, suffix = " k"), limits = c(30000, 120000), breaks = seq(30000, 120000, length.out = 6)) +
   guides(fill = guide_colorsteps(barwidth = unit(20, "lines"), barheight = unit(0.5, "lines"), show.limits = TRUE, frame.colour = "black", frame.linewidth = unit(0.2, "lines"), title.position = "top", title.hjust = 0.5, label.position = "bottom")) +
   coord_map(clip = "on") +
   theme_void() +
   theme(## Text ##
      text = element_text(face = "plain", family = "Oswald", color = text_color, size = 12),
      ## Plot Aesthetic ##
      panel.background = element_rect(fill = background, color = NA),
      plot.background = element_rect(fill = background, color = NA),
      ## Legend ##
      legend.position = "top",
      ## Margin ##
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
   ### Labels ###
   labs(fill = "Annual Salary (Median) of Nurses in 2020")

##### Line Plot #####
Plot_line <- 
nurses_us_states %>%   
   ggplot() +
   geom_line(data = nurses_us_states %>% group_by(State) %>% filter(max(Total_Employed_RN) >= 150000), aes(x = Year, y = Total_Employed_RN, group = State, color = Annual_Salary_Median)) +
   geom_line(data = nurses_us_states %>% group_by(State) %>% filter(max(Total_Employed_RN) <  150000), aes(x = Year, y = Total_Employed_RN, group = State, color = Annual_Salary_Median), alpha = 1/10) +
   geom_text(data = nurses_us_states %>% group_by(State) %>% filter(id %in% c("CA","TX","FL") & Year == 2020), aes(x = Year, y = Total_Employed_RN, color = Annual_Salary_Median, label = id), size = 4, nudge_x = -1, nudge_y = 15000, hjust = 0.5, fontface = "bold") +
   geom_text(data = nurses_us_states %>% group_by(State) %>% filter(id %in% c("NY") & Year == 2020), aes(x = Year, y = Total_Employed_RN, color = Annual_Salary_Median, label = id), size = 4, nudge_x = 0.0, nudge_y = -15000, hjust = 1.0, fontface = "bold") +
   scale_x_continuous(expand = c(0,0), breaks = c(seq(1998, 2018, 4), 2020)) +
   scale_y_continuous(labels = scales::label_comma(scale = 1/1000, accuracy = 1, suffix = " k")) +
   scale_color_stepsn(colors = viridis::inferno(n = 5, begin = 0.15, end = 0.85, direction = 1), limits = c(30000, 120000), breaks = seq(30000, 120000, length.out = 6)) +
   guides(color = "none") +
   coord_cartesian(expand = TRUE, clip = "off") +
   theme_classic() +
   theme(## Text ##
      text = element_text(face = "plain", family = "Oswald", color = text_color, size = 12),
      axis.text.x = element_text(face = "plain", family = "Oswald", color = text_color, size = 12),
      axis.text.y = element_text(face = "plain", family = "Oswald", color = text_color, size = 12),
      axis.title.x = element_text(face = "plain", family = "Oswald", color = text_color, size = 12, margin = unit(c(t = 0.2, r = 0.5, b = 0.2, l = 0.5),"cm")),
      axis.title.y = element_text(face = "plain", family = "Oswald", color = text_color, size = 12, margin = unit(c(t = 0.5, r = 0.2, b = 0.5, l = 0.2),"cm")),
      ## Plot Aesthetic ##
      panel.background = element_rect(fill = background, color = NA),
      plot.background = element_rect(fill = background, color = NA),
      axis.line = element_line(color = lines_color),
      axis.ticks = element_line(color = lines_color),
      ## Margin ##
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
   ### Labels ###
   labs(x = "Year",
        y = "Total Number of Registered Nurses")

##### Final Plot #####
design <- c("AAA
             AAA
             BBB")

Plot_Week_041 <- 
   Plot_Hexbin/Plot_line +
   plot_layout(design = design) &
   plot_annotation(title = annotation_title_text,
                subtitle = annotation_subtitle_text,
                 caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                         <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                         Source: Data.World",
                   theme = theme(
      ## Plot Aesthetic ##
      panel.background = element_rect(fill = background, color = NA),
      plot.background = element_rect(fill = background, color = NA),
      ## Titles & Caption ##
      plot.title = element_text(color = title_color, family = "Oswald", face = "plain", size = 24, hjust = 0.5, margin = unit(c(t = 0.1, r = 0.0, b = 0.1, l = 0.0), unit = "cm")),
      plot.subtitle = element_markdown(color = subtitle_color, family = "Oswald", face = "plain", size = 16, hjust = 0.5, vjust = 0.5, margin = unit(c(t = 0.5, r = 0.0, b = 0.5, l = 0.0), unit = "cm")),
      plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 0.5, size = 9),
      ## Margin ##
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")))
   

#### Progress ####
ggsave("./2021/2021_Week_041/Plots/2021_Week_041.png", Plot_Week_041, dpi = 320, scale = 1, width = 8, height = 10, units = c("in"))
