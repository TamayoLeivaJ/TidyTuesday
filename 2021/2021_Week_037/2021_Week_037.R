#### (TidyTuesday - 2021 - Week 37) #### 
#### ----------------------------------------------------------------------------------------- ####
## Formula 1 Races data set
###  Week     Date	           Data	                 Source	                 Article
###  - 37     - 2021-09-07     - Formula 1 Races     - ergast.com/mrd/db     - FiveThirtyEight

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
dir.create(here("2021/2021_Week_037/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_037/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
tt_data <- tt_load(2021, week = 37)

races <- tt_data$races
constructor <- tt_data$constructors
constructor_results <- tt_data$constructor_results
constructor_standings <- tt_data$constructor_standings

#### Data wrangling #### 
##### Who are the Formula One constructor with more consecutive World Championship wins?
race_constructor_res <- races %>% select(-c(url)) %>% rename(circuit_name = name, race_time = time) %>% 
                        left_join(constructor_results %>% rename(race_points = points), by = c("raceId")) %>% 
                        left_join(constructor_standings %>% rename(season_points = points), by = c("raceId","constructorId")) %>% 
                        left_join(constructor %>% select(-url) %>% rename(constructor_name = name, constructor_nationality = nationality), by = "constructorId") %>% 
                        mutate_at(vars(-date), na_if, "\\N") # Transform any "\N" to NA

race_won_const_year <- race_constructor_res %>% 
                       filter(!is.na(constructor_name)) %>% 
                       group_by(year, constructor_name) %>%
                       mutate(constructor_points = sum(race_points),
                              n_wins = sum(position == 1, na.rm = TRUE)) %>% 
                       distinct(year, constructor_name, constructor_points, n_wins)
                       
#### Annotation List ####
constructor_teams <- c("Ferrari", "McLaren", "Mercedes", "Red Bull", "Williams")

#### Text ####
annotation_title_text <- c("Who are the Formula One constructor with more consecutive World Championship victories?")
annotation_subtitle_text <- c("The Constructors' Championship was not awarded until 1958. Since then, five constructors' teams have won three or more consecutive world championships.")

#### Images ####
##### Team Logo #####
img_Formula1_logo <- magick::image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/3/33/F1.svg/800px-F1.svg.png")
img_ferrari_logo  <- magick::image_read("./2021/2021_Week_037/Images/Logo-F1-2021-ferrari.png")
img_mclaren_logo  <- magick::image_read("./2021/2021_Week_037/Images/Logo-F1-2021-mclaren.png")
img_mercedes_logo <- magick::image_read("./2021/2021_Week_037/Images/Logo-F1-2021-mercedes.png")
img_redbull_logo  <- magick::image_read("./2021/2021_Week_037/Images/Logo-F1-2021-redbull.png")
img_williams_logo <- magick::image_read("./2021/2021_Week_037/Images/Logo-F1-2021-williams.png")

##### Team Cars #####
img_ferrari <- magick::image_read("https://www.formula1.com/content/dam/fom-website/teams/2021-Team-Pages/teamcar-ferrari.png.transform/2col-retina/image.png")
img_mclaren <- magick::image_read("https://www.formula1.com/content/dam/fom-website/teams/2021/teamcar-mclaren.png.transform/2col-retina/image.png")
img_mercedes <- magick::image_read("https://www.formula1.com/content/dam/fom-website/teams/2021/teamcar-mercedes.png.transform/2col-retina/image.png")
img_redbull <- magick::image_read("https://www.formula1.com/content/dam/fom-website/teams/2021-Team-Pages/teamcar-redbull.png.transform/2col-retina/image.png")
img_williams <- magick::image_read("https://www.formula1.com/content/dam/fom-website/teams/2021-Team-Pages/teamcar-williams.png.transform/2col-retina/image.png")

#### Plot aesthetics ####
background  <- c("#F2F2F3")
lines_color <- c("#15151E")
text_color  <- c("#15151E")
constructor_color <- c("Ferrari" = "#D90404", "McLaren" = "#F28705", "Mercedes" = "#02D2BE", "Red Bull" = "#1600EF", "Williams" = "#0059FF")

#### Plot ####
race_won_const_year %>% 
  filter(constructor_points >= 10) %>% 
     ggplot(aes(x = year, y = constructor_points)) + 
     ### Layers base ###
     geom_point(aes(fill = constructor_name), size = 2, color = "#696969", alpha = 1/4, shape = 21) +
     ### Annotations ###
     ### Clusters Annotations ###
     geom_mark_ellipse(aes(label = "(1975–1977)", filter = constructor_name %in% constructor_teams[1] & between(year, 1975, 1977)), label.fill = "transparent", color = constructor_color[1], label.colour = constructor_color[1], con.colour = constructor_color[1], label.buffer = unit(50, 'mm'), expand = unit(2.0, "mm")) +
     geom_mark_ellipse(aes(label = "(1988-1991)", filter = constructor_name %in% constructor_teams[2] & between(year, 1988, 1991)), label.fill = "transparent", color = constructor_color[2], label.colour = constructor_color[2], con.colour = constructor_color[2], label.buffer = unit(90, 'mm'), expand = unit(2.5, "mm")) +
     geom_mark_ellipse(aes(label = "(1992-1994)", filter = constructor_name %in% constructor_teams[5] & between(year, 1992, 1994)), label.fill = "transparent", color = constructor_color[5], label.colour = constructor_color[5], con.colour = constructor_color[5], expand = unit(2.0, "mm")) +
     geom_mark_hull(aes(label = "(1999–2004)", filter = constructor_name %in% constructor_teams[1] & between(year, 1999, 2004)), label.fill = "transparent", color = constructor_color[1], label.colour = constructor_color[1], con.colour = constructor_color[1], label.buffer = unit(75, 'mm'), expand = unit(3.0, "mm")) +
     geom_mark_hull(aes(label = "(2010-2013)", filter = constructor_name %in% constructor_teams[4] & between(year, 2010, 2013)), label.fill = "transparent", color = constructor_color[4], label.colour = constructor_color[4], con.colour = constructor_color[4], label.buffer = unit(20, 'mm'), expand = unit(3.0, "mm")) +
     geom_mark_hull(aes(label = "", filter = constructor_name %in% constructor_teams[3] & between(year, 2014, 2020)), label.fill = "transparent", color = constructor_color[3], label.colour = constructor_color[3], con.colour = constructor_color[3], expand = unit(2.5, "mm"), con.type = "none") +
     ### Layers extra ###
     geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[1] & between(year, 1999, 2004)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[1] & between(year, 1975, 1977)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[2] & between(year, 1988, 1991)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[3] & between(year, 2014, 2020)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[4] & between(year, 2010, 2013)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     geom_point(data = race_won_const_year %>% filter(constructor_name %in% constructor_teams[5] & between(year, 1992, 1994)), aes(x = year, y = constructor_points, fill = constructor_name), size = 3, shape = 21) +
     ### image Annotations ###
     annotation_custom(rasterGrob(image = img_Formula1_logo, width = 0.15, x = 0.05, y = -0.1)) +
     annotation_custom(rasterGrob(image = img_ferrari_logo,  width = 0.05, x = 27/71, y = 590/950)) +
     annotation_custom(rasterGrob(image = img_ferrari,  width = 0.1, x = 27/71, y = 510/950)) +
     annotation_custom(rasterGrob(image = img_mclaren_logo,  width = 0.1, x = 39/71, y = 890/950)) +
     annotation_custom(rasterGrob(image = img_mclaren,  width = 0.1, x = 39/71, y = 850/950)) +
     annotation_custom(rasterGrob(image = img_mercedes_logo,  width = 0.15, x = 63/71, y = 900/950)) +
     annotation_custom(rasterGrob(image = img_mercedes, width = 0.1, x = 63/71, y = 840/950)) +
     annotate(geom = "text", x = 2016, y = 800, label = "(2014-2020)", color = constructor_color[3], fontface = "bold", size = 4) +
     annotation_custom(rasterGrob(image = img_redbull_logo,  width = 0.15, x = 48/71, y = 710/950)) +
     annotation_custom(rasterGrob(image = img_redbull,  width = 0.1, x = 48/71, y = 640/950)) +
     annotation_custom(rasterGrob(image = img_williams_logo, width = 0.1, x = 42/71, y = 370/950)) +
     annotation_custom(rasterGrob(image = img_williams, width = 0.1, x = 42/71, y = 340/950)) +
     ### Scales ###
     scale_x_continuous(breaks = seq(1950, 2020, 5), limits =(c(1950, 2021))) +
     scale_y_continuous(expand = c(0, 0.01), breaks = seq(0, 900, 300), limits =(c(0, 950))) +
     scale_fill_manual(values = constructor_color) +
     coord_cartesian(clip = "off") +
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
ggsave("./2021/2021_Week_037/Plots/2021_Week_037.png", dpi = 300, scale = 1, width = 12, height = 8, units = c("in"))