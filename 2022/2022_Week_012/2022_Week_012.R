#### (TidyTuesday - 2022 - Week 12) #### 
#### ----------------------------------------------------------------------------------------- ####
## Baby names
###  Week     Date	           Data	          Source	                          Article
###  - 12     - 2022-03-22 	   - Baby names 	- US babynames & nzbabynames 	    - Emily Kothe's nzbabynames vignette


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
dir.create(here("2022/2022_Week_012/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


#### Data wrangling #### 
##### Top 20 baby names by the first letter
Names <- babynames %>% 
         filter(year >= 2000 & name != "Unknown") %>% 
         group_by(name, sex) %>% 
         add_count(wt = n, name = "Count") %>% 
         select(sex, name, Count) %>% 
         distinct() %>% 
         mutate(name_initial = str_extract(name, "^.{1}")) %>%  # Extract the name first letter
         group_by(name) %>%
         add_count(wt = Count, name = "Total_Count") %>%
         mutate(prop = Count/Total_Count,
                Total_Count_label = map(Total_Count, function (x){number_format(accuracy = 1, scale = 1/1000, suffix = "k")(x)})) %>% 
         group_by(name_initial) %>% 
         arrange(desc(Total_Count), .by_group = TRUE) %>%
         mutate(id = cumsum(!duplicated(name)))  %>%       # Give the position of baby names in each letter group avoiding double counting of duplicate names (names used for male and female)
         group_by(name_initial) %>% 
         filter(id <= 20)

# Create dataset for space columns 
data <- tibble(name_initial = rep(unique(Names$name_initial), 1),
               id = c(rep(21, 26)))

Names <- rbind(Names, data)


##### Data labels barplots #####
labels <- Names %>%  
          mutate(possition = id)

angle <-  80 - 360 * (labels$possition-0.5)/(max(labels$possition))    # Alignment to the center of the bars: substract 0.5 to center (extreme right = 1; extreme left = 0) // As it does not start from zero but from one, we have had to modify the initial angle from 90 to 80.
labels$hjust <- 0.5                                                    # Labels alignment: 0.5 center
labels$angle <- if_else(angle < -90, angle + 180, angle)               # flip angle BY to make them readable

#### Plot aesthetics ####
background     <- "#F2F2F2"
lines_color    <- "#3C5473"
title_color    <- "#39054D"
subtitle_color <- "#DB004C"
text_color     <- "#DB004C"
caption_color  <- "#39054D"


#### Annotation ####
annotation_title_text <- c("Don't call me baby")
annotation_subtitle_text <- c("The 20 most frequent baby names for each letter since 2000. The names show the proportion in which they are used according to male or female baby sex.")
annotation_subtitle_text <- str_wrap(annotation_subtitle_text, 70) %>% str_replace_all("\n","<br>")
annotation_subtitle_text <- annotation_subtitle_text %>% 
                            str_replace_all("The 20 most","<span class='text-nowrap'>The 20 most") %>%
                            str_replace_all("to male","to <p style='color:#BF04B3'>male</p>") %>% 
                            str_replace_all("female","<p style='color:#F2B705'>female</p>") %>%
                            str_replace_all("baby sex\\.","baby sex.</span>")


#### Plot ####
Plot <- Names  %>% 
        ggplot() +
        geom_segment(data = data.frame(y = c(0.25, 0.5, 0.75, 0.99)), aes(x = 1 - 0.7, xend = 20 + 0.7, y = y, yend = y), linetype="dashed", color = lines_color, size = rep(c(0.05, .1, 0.05, .1), 26), inherit.aes = FALSE) +
        geom_text(data = data.frame(x = 0, y = c(0.25, 0.5, 0.75, 1), label = c("25%","50%","75%","100%")), aes(x=x, y=y, label=label), size = rep(c(1.5, 1.8, 2, 2.2),26), color = text_color, family = "Ranga", fontface='bold', inherit.aes = FALSE) +
        geom_segment(data = data.frame(y = -0.1), aes(x = 2, xend = 19, y = y, yend = y), linetype="solid", color = lines_color, size = 0.05, arrow = arrow(length = unit(0.05, "cm")), inherit.aes = FALSE) +
        geom_text(data = data.frame(x = c(1,20), y = -0.1), aes(x=x, y=y, label=x), size = 2, color = text_color, family = "Ranga", fontface='bold', inherit.aes = FALSE) +
        geom_text(data = Names  %>% filter(id != 21) %>% group_by(name) %>% slice(n = 1), aes(x = id, y = 1.17, label = if_else(id == 20, paste0("Total Count\n",Total_Count_label), as.character(Total_Count_label))), size = 1.5, alpha = 0.7, color = text_color, family = "Ranga", fontface='bold', inherit.aes = FALSE) +
        geom_col(aes(x = id, y = prop, fill = sex)) +
        ### Annotations ###
        geom_text(data = labels, aes(x = possition, y = 0.5, label = name, hjust = hjust), color = background, family = "Ranga", fontface = "plain", size = 3.0, angle = labels$angle, inherit.aes = FALSE) + # I use conditional statements within layers such as if_else() and case_when() to assign differential values of y-axis position, alpha, size and labels to each value based on conditions.
        geom_text(aes(x = 1, y = -0.5, label = name_initial), size = 5.5, color = text_color, family = "Ranga", fontface = "plain", hjust = 0.5, vjust = 0.5)  +
        ### Scales ###
        scale_y_continuous(limits = c(-0.5, 1.17)) +
        scale_x_continuous(limits = c(0,21), expand = c(0,0)) +
        scale_fill_manual(values = c("#BF04B3", "#F2B705"), breaks = c("M","F")) + #c("#FFCA01", "#F56E00")
        coord_polar() +
        facet_wrap(~name_initial, strip.position = "bottom", ncol = 6) +
        ### Theme ###
        theme_void() +
        theme(
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background = element_rect(fill = background, color = NA),
          ## Strip ##
          strip.text = element_blank(),
          ## Legend ##
          legend.position = "none",
          ## Titles & Caption ##
          plot.title.position = "plot",
          plot.title    = element_markdown(color = title_color, family = "Antonio", face = "plain", size = 44, hjust = 0.5, halign = 0.0, vjust = 0.5, valign = 0.5, padding = unit(c(0.25, 0.25, 0.25, 0.25), "cm")),
          plot.subtitle = element_markdown(color = subtitle_color, family = "Antonio", face = "plain", size = 24, hjust = 0.5, halign = 0.0, vjust = 0.5, valign = 0.5, padding = unit(c(0.25, 0.25, 0.25, 0.25), "cm")),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 10, margin = margin(t = 0.2, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm")) +
        ### Labels ###
        labs(x = "",
             y = "",
             fill = "",
             subtitle = annotation_subtitle_text,
             title = annotation_title_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>  TamayoLeivaJ<br><br> 
                        Source: US babynames & nzbabynames")

#### Progress ####
ggsave("./2022/2022_Week_012/Plots/2022_Week_012.png", Plot, dpi = 600, scale = 1, width = 12, height = 12, units = c("in"))
