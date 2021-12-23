#### (TidyTuesday - 2021 - Week 52) #### 
#### ----------------------------------------------------------------------------------------- ####
## Starbucks drinks
###  Week     Date	           Data	                    Source	                   Article
###  - 52     - 2021-12-21	   - Starbucks drinks	    - Starbucks drinks	           - Behance - Starbucks infographics

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
dir.create(here("2021/2021_Week_052/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_052/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')


#### Data wrangling #### 
##### Calories or Caffeine
starbucks <- starbucks %>% 
             filter(serv_size_m_l != 0 & size %in% c("short","tall","grande","venti","trenta")) %>%
             filter(calories != 0 & caffeine_mg != 0) %>%
             group_by(product_name, size, serv_size_m_l) %>% 
             dplyr::slice_max(calories) %>% 
             arrange(desc(serv_size_m_l)) %>% 
             ungroup()
             
cup_sizes <- starbucks %>%
             select(size, serv_size_m_l) %>% 
             distinct() %>% 
             mutate(img = "./2021/2021_Week_052/Images/coffee_cup.png",
                    img_size = 0.04 * (serv_size_m_l/max(serv_size_m_l))) %>% 
             group_by(size) %>% 
             mutate(label = if_else(size == "venti", paste0(str_to_title(size), "\n", min(serv_size_m_l), "-", max(serv_size_m_l), " ml"), 
                                                     paste0(str_to_title(size), "\n", serv_size_m_l, " ml"))) %>% 
             select(size, img, img_size, label) %>% 
             group_by(size) %>%
             dplyr::slice_max(img_size) %>% 
             arrange(desc(img_size)) %>% 
             mutate(x_possition = case_when(size == "short" ~ 200, size == "tall" ~ 300, size == "grande" ~ 400, size == "venti" ~ 500, size == "trenta" ~ 600),
                    y_possition = 650) %>% 
             ungroup()

cup_stain <- tibble(img = "./2021/2021_Week_052/Images/coffee_cup_stain.png")
cup_stain2 <- tibble(img = "./2021/2021_Week_052/Images/coffee_stain.png")

starbucks <- starbucks %>%
             left_join(cup_sizes, by = "size") %>% 
             mutate(size = size %>% fct_reorder(serv_size_m_l),
                    product_name = str_to_title(product_name)) 

# Calculate quantile
quantile(starbucks$caffeine_mg, probs = seq(.1, .9, by = .1))
quantile(starbucks$calories, probs = seq(.1, .9, by = .1))
        
#### Plot aesthetics ####
background     <- "#F2F2F2"
lines_color    <- "#BFBFBD"
title_color    <- "#395938"
subtitle_color <- "#395938"
text_color     <- "#395938"
caption_color  <- "#2D402C"
color_palette  <- met.brewer(name="Signac", n=14, type="discrete")[c(9,3,11,12,13)]


#### Annotation ####
annotation_title_text <- c("Starbucks")
annotation_subtitle_text <- c("What type of coffee are you looking for today?")

#### Plot ####
Plot <- starbucks %>% 
        ggplot(aes(x = calories, y = caffeine_mg, size = serv_size_m_l, fill = size)) +
        annotate(geom = "rect", xmin = -Inf, xmax = 390, ymin = 249, ymax = Inf, fill = "#F2E205", alpha = 0.1) +
        annotate(geom = "rect", xmin = 390, xmax = 830, ymin = -Inf, ymax = 249, fill = "#FC0D0D", alpha = 0.1) +
        geom_point(shape = 21, color = "white") +
        ### Annotations ###
        geom_image(data = cup_stain, aes(x = 700, y = 400, image = img), size = 0.25, asp = 1.8, inherit.aes = FALSE) +
        geom_image(data = cup_stain2, aes(x = 0, y = 600, image = img), size = 0.15, asp = 1.8, inherit.aes = FALSE) +
        geom_image(aes(x = x_possition, y = y_possition, image = img, color = size), size = unique(cup_sizes$img_size), stat = "unique", asp = 1.5) +
        geom_text(aes(x = x_possition, y = y_possition - 50, label = label, color = size), size = 5, family = "Oooh Baby") +
        ### Text Annotations ###
        geom_text(data = starbucks %>% filter(calories >= 550 | caffeine_mg >= 400), aes(x = calories, y = caffeine_mg + case_when(product_name %in% c("Java Chip Frappuccino Blended","Brewed Coffee - Medium Roast") ~ -10, product_name == "Brewed Coffee - True North Blend Blonde Roast" ~ 20, T ~ 0), label = product_name, color = size), size = 5, family = "Oooh Baby", nudge_x = 10, hjust = 0) +
        geom_text(aes(x = 380, y = 400, label = "Top 10% Caffeine Drinks"), color = "#735724", size = 4, family = "Montserrat", inherit.aes = FALSE, angle = 270) +
        geom_text(aes(x = 820, y = 100, label = "Top 10% Calories Drinks"), color = "#D94C1A", size = 4, family = "Montserrat", inherit.aes = FALSE, angle = 270) +
        ### Scales ###
            scale_x_continuous(limits = c(0, 830), breaks = seq(0, 700, 100)) +
            scale_y_continuous(breaks = seq(0, 500, 100), labels = scales::label_number(suffix = "mg")) +
            scale_size(range = c(4, 6), guide = "none") +
            scale_fill_manual(values = color_palette, na.value = "black", breaks = c("short","tall","grande","venti","trenta"), guide = "none") +
            scale_color_manual(values = color_palette, na.value = "black", breaks = c("short","tall","grande","venti","trenta"), guide = "none") +
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
        labs(x = "Calories",
             y = "Caffeine",
             fill = "Drink Size",
             size = "Serving size (ml)",
             title = annotation_title_text,
             subtitle = annotation_subtitle_text,
             caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                              <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                              Source: Starbucks Coffee Company Beverage Nutrition Information & PythonCoderUnicorn <br>")

#### Progress ####
ggsave("./2021/2021_Week_052/Plots/2021_Week_052.png", Plot, dpi = 700, scale = 1, width = 12, height = 10, units = c("in"))
