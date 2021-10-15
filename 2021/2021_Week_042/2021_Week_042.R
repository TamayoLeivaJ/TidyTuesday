#### (TidyTuesday - 2021 - Week 42) #### 
#### ----------------------------------------------------------------------------------------- ####
## Global Seafood data set
###  Week     Date	           Data	                 Source	                 Article
###  - 42     - 2021-10-12     - Global Seafood      - OurWorldinData.org    - OurWorldinData.org

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(scales)
library(BBmisc)
library(gggibbous)
library(Redmonder)
library(ggtext)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_042/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_042/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
if(rate_limit_check(quiet = TRUE) > 10){
tt_data <- tt_load(2021, week = 42)
fish_stocks_within_sust_levels <- tt_data$`fish-stocks-within-sustainable-levels`
seafood_and_fish_prod_t_tonnes <- tt_data$`seafood-and-fish-production-thousand-tonnes`
}

if(rate_limit_check(quiet = TRUE) < 10){
fish_stocks_within_sust_levels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
seafood_and_fish_prod_t_tonnes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')
}

#### Data wrangling #### 
##### The impact of global seafood production on biological sustainability.
seafood_prod_th_tonnes <- seafood_and_fish_prod_t_tonnes %>% 
                          clean_names(case = "none") %>% 
                          rename_at(vars(starts_with("Commodity_Balances_Livestock_and_Fish_Primary_Equivalent_")), ~ str_remove_all(., pattern = "Commodity_Balances_Livestock_and_Fish_Primary_Equivalent_")) %>%
                          rename_at(vars(ends_with("_tonnes")), ~ str_remove_all(., pattern = "_276[1-7]_Production_5510_tonnes")) %>%
                          adorn_totals(Pelagic_Fish:Marine_Fish_Other, where = "col", fill = "-", na.rm = TRUE, name = "Total") %>%
                          filter(Entity %in% c("World")) %>% 
                          pivot_longer(cols = Pelagic_Fish:Marine_Fish_Other, names_to = "Food", values_to = "Production_tonnes") %>% 
                          arrange(desc(Year,Production_tonnes)) %>% 
                          mutate(Food = case_when(Food == "Marine_Fish_Other" ~ "Marine Fish",
                                                  Food == "Molluscs_Other" ~ "Molluscs",
                                                  Food == "Demersal_Fish" ~ "Demersal Fish",
                                                  Food == "Pelagic_Fish" ~ "Pelagic Fish",
                                                  Food == "Freshwater_Fish" ~ "Freshwater Fish",
                                                  T ~ Food),
                                 Food = Food %>% fct_reorder2(Year,Production_tonnes) %>% fct_rev()) %>% 
                          group_by(Year) %>%
                          arrange(desc(Production_tonnes)) %>% 
                          mutate(Cumsum = cumsum(Production_tonnes),
                                 Label_Possition = Cumsum - (Production_tonnes/2))

Yl <- c(min(seafood_prod_th_tonnes$Year), max(seafood_prod_th_tonnes$Year))

fish_stocks_sust_level <- fish_stocks_within_sust_levels %>% 
                          clean_names(case = "none") %>% 
                          rename("Sustainable" = "Share_of_fish_stocks_within_biologically_sustainable_levels_FAO_2020",
                                 "Overexploited" = "Share_of_fish_stocks_that_are_overexploited") %>% 
                          mutate(Sustainable = Sustainable/100, 
                                 Overexploited = Overexploited/100) %>% 
                          filter(Entity %in% c("World")) %>% 
                          pivot_longer(cols = Sustainable:Overexploited, names_to = "Levels", values_to = "Ratio") %>% 
                          mutate(right = if_else(Levels == "Sustainable", TRUE, FALSE)) 

fish_stocks_sust_level <- fish_stocks_sust_level %>% 
                          filter(between(Year, Yl[1], Yl[2])) %>% 
                          left_join(
                            tibble(
                              Year = fish_stocks_sust_level$Year[which(fish_stocks_sust_level$right == FALSE)],
                              yend = 180000000,
                              y_axis_start = yend - 40000000,
                              possition_y = normalize(fish_stocks_sust_level$Ratio[which(fish_stocks_sust_level$right == FALSE)], range = c(first(y_axis_start), first(yend)), method = "range")))
                        
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
  geom_area() +
  geom_text(data = seafood_prod_th_tonnes %>% filter(Year == 2013), aes(y = Label_Possition, label = Food, color = Food), size = 6, nudge_x = +4, fontface = "bold", family = "BenchNine") +
  geom_line(data = fish_stocks_sust_level %>% filter(between(Year, min(Yl), max(Yl))), aes(x = Year, y = possition_y), color = lines_color, inherit.aes = FALSE) +
  geom_moon(data = fish_stocks_sust_level %>% filter(between(Year, min(Yl), max(Yl))), aes(x = Year, y = possition_y, ratio = Ratio, right = right, fill = right), color = background, inherit.aes = FALSE, size = 10) +
  ### Annotations ###
  ### Labels Annotations ###
  annotate(geom = "text", x = 1974, y = 135000000, color = "#C75153", label = "10.0%", fontface = "bold") +
  annotate(geom = "text", x = 1974, y = 150000000, color = "#387AA8", label = "90.0%", fontface = "bold") +
  annotate(geom = "text", x = 1978, y = 133000000, color = "#C75153", label = "8.5%", hjust = 0.5, fontface = "bold") +
  annotate(geom = "text", x = 2008, y = 185000000, color = "#C75153", label = "32.9%", fontface = "bold") +
  annotate(geom = "text", x = 2013, y = 183000000, color = "#C75153", label = "32.0%", fontface = "bold") +
  ### Scales ###
  scale_x_continuous(expand = c(0,0), limits = c(min(Yl), max(Yl) + 7), breaks = seq(min(Yl), max(Yl), length.out = 5)) +
  scale_y_continuous(expand = c(0,0), labels = label_number(scale = 1/1000000, suffix = " M")) +
  scale_fill_manual(values = pallete_color, breaks = c(levels(seafood_prod_th_tonnes$Food),"TRUE","FALSE")) +
  scale_color_manual(values = pallete_color[1:7]) +
  coord_cartesian(clip = "off") +
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
ggsave("./2021/2021_Week_042/Plots/2021_Week_042.png", dpi = 320, scale = 1, width = 12, height = 9, units = c("in"))
