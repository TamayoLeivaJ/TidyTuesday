#### (TidyTuesday - 2022 - Week 06) #### 
#### ----------------------------------------------------------------------------------------- ####
## Tuskegee Airmen
###  Week     Date	           Data	                    Source	                                               Article
###  - 06     - 2022-02-08	   - Tuskegee Airmen	      - Commemorative Airforce (CAF) by way of the VA-TUG	   - Wikipedia & Air Force Historical Research Agency


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
dir.create(here("2022/2022_Week_006/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')


#### Data wrangling #### 
##### 
airmen_clean <- airmen %>% 
                mutate(graduation_date = lubridate::ymd(graduation_date),
                       graduation_date_y = lubridate::year(graduation_date),
                       graduation_date_yq = lubridate::quarter(graduation_date, with_year = T),
                       graduation_date_ym = lubridate::format_ISO8601(graduation_date, precision = "ym"),
                       pilot_type = if_else(pilot_type == "Liason pilot", "Liaison pilot", as.character(pilot_type)),
                       number_of_aerial_victory_credits = as.numeric(as.character(number_of_aerial_victory_credits)),
                       total_aerial_victory_credits = sum(number_of_aerial_victory_credits)) %>% 
                group_by(pilot_type) %>% 
                add_count(name = "Count") %>% 
                ungroup()


annotations <- tibble(label = c("**The history of Red Tails** <br><br>The *Tuskegee Airmen* were the first group of black military pilots and aviators in the United States Armed Forces, who fought in World War II and trained at Tuskegee Institute (now Tuskegee University), located near Tuskegee, Alabama, in the United States. <br><br>The *Tuskegee Airmen* formed the *332nd Fighter Group*, which had four fighter squadrons. The *99th Pursuit Squadron* -a year older than the group- was the first flying squadron composed of black military pilots, and the first to deploy overseas (in North Africa in April 1943, and later in Sicily and other parts of Italy). The *332nd Fighter Group* (originally consisting of the *100th*, *301st* and *302nd Fighter Squadrons*), was the first black flying group, and was deployed to Italy in early 1944. On May 1 of the same year, the *99th Pursuit Squadron* was added to the group, bringing the *332nd Fighter Group* to four fighter squadrons. When the pilots of the *332nd Fighter Group* painted the tails of their P-47 Thunderbolt aircraft red, the nickname *Red Tails* was coined. The red markings that distinguished the *Tuskegee Airmen*.",
                                "1006 Graduates Pilots",
                                "Between 1942 and 1948",
                                "697 Single engine",
                                "247 Twin engine",
                                "51 Liaison",
                                "11 Service",
                                "WWII",
                                "Deploy at the<br>Mediterranean theater<br>Italy, mainly",
                                "One flying Group",
                                "332nd Fighter Group",
                                "Four flying squadron",
                                "99th, 100th, 301st, and 302nd Fighter Squadrons",
                                "112 Aerial Victories"),
                      x = c(0.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 4.3, 4.3, 2.0, 2.0, 3.2, 3.2, 2.5),
                      y = c(8.5, 16, 15, 14, 12.5, 11, 9.5, 8.2, 7.0, 7, 6, 4, 3, 0),
                      alpha = c(0.6, 1, 0.4, 0.6, 0.6, 0.6, 0.6, 1, 0.5, 1, 0.6, 1, 0.6, 1), 
                      size = c(4, 20, 10, 16, 16, 16, 16, 12, 6, 13, 8, 13, 8, 28),
                      color = c("#4E1A36", rep("#F21B42", 6), rep("#8C5889", 2), rep("#03588C", 2), rep("#2E838C", 2), "#F2B705"),
                      hjust = rep(0.5, 14),
                      vjust = rep(0.5, 14),
                      id = letters[1:14])


#### Plot aesthetics ####
background     <- "#EDEFF2"
lines_color    <- "#3C5473"
title_color    <- "#594A4A"
subtitle_color <- "#FA141D"
text_color     <- "#594A4A"
caption_color  <- "#8C4272"


#### Annotation ####
annotation_title_text <- c("Tuskegee Airmen")
annotation_subtitle_text <- c("Red Tails")


#### Plot ####
Plot <- annotations  %>% 
        filter(!id == "a") %>% 
        ggplot() +
        geom_richtext(aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust), alpha = annotations$alpha[2:14], size = annotations$size[2:14], color = annotations$color[2:14], label.color = NA, fill = NA, family = "Saira Stencil One") +
        geom_textbox(data = annotations %>% filter(id == "a"), aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust), alpha = annotations$alpha[1], size = annotations$size[1], color = annotations$color[1], fill =  NA, width = unit(0.27, "npc"), family = "Yuji Syuku", inherit.aes = FALSE) +
        ### Scales ###
        scale_x_continuous(limits = c(-0.5, 5)) +
        scale_y_continuous(limits = c(0, 16.5)) +
        coord_cartesian(expand = TRUE, clip = "off") +
        ### Theme ###
        theme_void() +
        theme(
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background = element_rect(fill = background, color = NA),
          ## Titles & Caption ##
          plot.title.position = "plot",
          plot.title    = element_markdown(color = background, fill = title_color, family = "Saira Stencil One", face = "plain", size = 44, hjust = 0.0, halign = 0.0, vjust = 0.5, valign = 0.5, padding = unit(c(0.25, 0.25, 0.25, 0.25), "cm")),
          plot.subtitle = element_markdown(color = background, fill = subtitle_color, family = "Saira Stencil One", face = "plain", size = 48, hjust = 0.0, halign = 0.0, vjust = 0.5, valign = 0.5, padding = unit(c(0.25, 0.25, 0.25, 0.25), "cm")),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 10, margin = margin(t = 0.2, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
        ### Labels ###
        labs(x = "",
             y = "",
             fill = "",
             subtitle = annotation_subtitle_text,
             title = annotation_title_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>  TamayoLeivaJ<br><br> 
                        Source: Commemorative Airforce (CAF) by way of the Veterans Advocacy Tableau User Group (VA-TUG)")

#### Progress ####
ggsave("./2022/2022_Week_006/Plots/2022_Week_006.png", Plot, dpi = 326, scale = 1, width = 12, height = 12, units = c("in"))
