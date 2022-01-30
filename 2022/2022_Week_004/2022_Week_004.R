#### (TidyTuesday - 2022 - Week 04) #### 
#### ----------------------------------------------------------------------------------------- ####
## Board games
###  Week     Date	           Data	                    Source	                   Article
###  - 04     - 2022-01-25	   - Board games           	- BoardGameGeek (Kaggle)   - Alyssa Goldberg on Kaggle


#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)

#### Libraries (This Plot) ####
library(ggrepel)
library(ggfx)
library(patchwork)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_004/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')


#### Data wrangling #### 
##### Are the most popular board games related to the most common board game categories and mechanics?
details_board <- details %>%
                 select(id, boardgamecategory, boardgamemechanic) %>% 
                 mutate(boardgamecategory = str_remove_all(boardgamecategory, paste(c("\\[","\\]",'\\"',"\\'"), collapse = "|")),
                        boardgamemechanic = str_remove_all(boardgamemechanic, paste(c("\\[","\\]",'\\"',"\\'"), collapse = "|"))) %>% 
                 separate_rows(boardgamecategory, sep = ",") %>% 
                 separate_rows(boardgamemechanic, sep = ",") %>%
                 mutate(boardgamecategory = str_trim(boardgamecategory),
                        boardgamemechanic = str_trim(boardgamemechanic)) %>% 
                 distinct() %>% 
                 left_join(details %>% select(id, primary, yearpublished, playingtime, owned), by = "id")

details_board_category <- details %>% 
                          pull(boardgamecategory) %>% 
                          str_remove_all(paste(c("\\[","\\]",'\\"',"\\'"), collapse = "|")) %>% 
                          str_split(pattern = ",") %>% 
                          unlist() %>% 
                          str_trim() %>% 
                          tibble(boardgamecategory = .) %>% 
                          group_by(boardgamecategory) %>% 
                          add_count(name = "Count_Category", sort = TRUE) %>% 
                          ungroup() %>% 
                          distinct() %>% 
                          filter(!is.na(boardgamecategory))

level_category <- details_board_category %>% 
                  filter(Count_Category >= 1000) %>%
                  pull(boardgamecategory) %>% 
                  str_trim() %>% 
                  list()
                  
details_board_mechanic <- details %>% 
                          pull(boardgamemechanic) %>% 
                          str_remove_all(paste(c("\\[","\\]",'\\"',"\\'"), collapse = "|")) %>% 
                          str_split(pattern = ",") %>% 
                          unlist() %>% 
                          str_trim() %>% 
                          tibble(boardgamemechanic = .) %>% 
                          group_by(boardgamemechanic) %>% 
                          add_count(name = "Count_Mechanic", sort = TRUE) %>% 
                          ungroup() %>%
                          distinct() %>% 
                          filter(!is.na(boardgamemechanic))

level_mechanic <- details_board_mechanic %>% 
                  filter(Count_Mechanic >= 1000) %>%
                  pull(boardgamemechanic) %>% 
                  str_trim() %>%
                  list()

details_board_ratings <- details_board %>% 
                         left_join(details_board_category) %>% 
                         left_join(details_board_mechanic) %>% 
                         arrange(desc(owned)) %>% 
                         left_join(ratings %>% select(id, rank, average, bayes_average, users_rated))

#### Data Plot ####
##### Data main Plot #####
details_board_ratings_main_plot <- details_board_ratings %>% 
                                   select(id, primary, average, owned) %>% 
                                   distinct()

Top25 <- details_board_ratings_main_plot %>% dplyr::slice_head(n=25) %>% pull(id)

##### Data grid Plot #####
details_board_ratings_grid_plot <- details_board_ratings %>% 
                                   select(id, primary, owned, yearpublished, boardgamecategory, Count_Category, boardgamemechanic, Count_Mechanic) %>% 
                                   distinct() %>% 
                                   mutate(boardgamecategory = case_when(Count_Category < 1000 ~ "Other", T ~ boardgamecategory),
                                          boardgamemechanic = case_when(Count_Mechanic < 1000 ~ "Other", T ~ boardgamemechanic)) %>% 
                                   filter(id %in% Top25) %>% 
                                   pivot_longer(cols = starts_with("boardgame"), names_to = "boardgame", values_to = "category_mechanic") %>%
                                   mutate(boardgame = str_remove(boardgame, "boardgame") %>% str_to_sentence()) %>% 
                                   filter(category_mechanic != "Other") %>% 
                                   mutate(boardgame_n = if_else(boardgame == "Category", 2, 1)) %>%
                                   mutate(primary_label = paste0(primary," (",yearpublished,")"),
                                          category_mechanic_label = if_else(boardgame == "Category", paste0(category_mechanic," (",Count_Category,")"), paste0(category_mechanic," (",Count_Mechanic,")"))) %>% 
                                   select(id, primary, owned, yearpublished, boardgame, category_mechanic, boardgame_n, primary_label, category_mechanic_label) %>%
                                   distinct()

  
##### Data labels barplots #####
###### Category ######
labels_barplot_category <- details_board_category %>% 
                           filter(Count_Category >= 1000) %>% 
                           mutate(possition = seq(1:n()))

angle <-  90 - 360 * (labels_barplot_category$possition -0.5)/nrow(labels_barplot_category)     # Alignment to the center of the bars: substract 0.5 to center (extreme right = 1; extreme left = 0)
labels_barplot_category$hjust <- if_else(angle < -90, 1, 0)                                     # Labels alignment: right or left (If I am on the left part of the plot, my labels have currently an angle < -90)
labels_barplot_category$angle <- if_else(angle < -90, angle + 180, angle)                       # flip angle BY to make them readable

###### Mechanic ######
labels_barplot_mechanic <- details_board_mechanic %>% 
                           filter(Count_Mechanic >= 1000) %>% 
                           mutate(possition = seq(1:n())) %>% 
                           mutate(boardgamemechanic = case_when(boardgamemechanic == "Hand Management" ~ "Hand Mgmt", T ~ boardgamemechanic))

angle <-  90 - 360 * (labels_barplot_mechanic$possition -0.5)/nrow(labels_barplot_mechanic)     # Alignment to the center of the bars: substract 0.5 to center (extreme right = 1; extreme left = 0)
labels_barplot_mechanic$hjust <- if_else(angle < -90, 1, 0)                                     # Labels alignment: right or left (If I am on the left part of the plot, my labels have currently an angle < -90)
labels_barplot_mechanic$angle <- if_else(angle < -90, angle + 180, angle)                       # flip angle BY to make them readable


#### Plot aesthetics ####
background     <- "#293B58"
lines_color    <- "#2E4CA6"
title_color    <- "#FFFFFF"
subtitle_color <- "#7C9AA6"
text_color     <- "#FFFFFF"
caption_color  <- "#FFFFFF"


#### Annotation ####
annotation_title_text <- c("What Board Games do you most enjoy playing?")
annotation_subtitle_text <- c("Do the most popular board games (top 25) belong to the most common categories and mechanics among board games?")
annotation_subtitle_text <- str_wrap(annotation_subtitle_text, 60) %>% str_replace_all("\n","<br>") # Helps to separate long texts into lines with the same maximum number of characters
annotation_subtitle_text <- annotation_subtitle_text %>% 
                            str_replace_all("Do the most","<span class='text-nowrap'>Do the most") %>%
                            str_replace_all("top 25","<p style='color:#F2E205'>Top 25</p>") %>% 
                            str_replace_all("most<br>common","most</span><br><span class='text-nowrap'><p>common") %>% 
                            str_replace_all("categories","<p style='color:#05F2DB'>Categories</p>") %>% 
                            str_replace_all("mechanics among board games\\?","<p style='color:#F25C05'>Mechanics</p> among board games?</p></span>") 
#### Plots ####
##### Theme Plot 1, 4 #####
theme_set(### Theme ###
          theme_classic() +
          theme(
            ## Text ##
            text = element_text(face = "plain", family = "Bangers", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
            ## Axis ##
            axis.title.x = element_text(size = 18, color = text_color),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 14, color = text_color),
            axis.line.x = element_blank(),
            axis.ticks.x = element_line(size = 0.1, color = lines_color),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            ## Panel Grid ##
            panel.grid.major.x = element_line(size = 0.1, color = lines_color, linetype = "longdash"),
            panel.spacing = unit(2, "lines"),
            ## Plot Aesthetic ##
            panel.background = element_rect(fill = background, color = NA),
            plot.background = element_rect(fill = background, color = NA),
            ## Margin ##
            plot.margin = margin(t = 0.5, r = 0.0, b = 0.5, l = 0.5, unit = "cm")))

##### Plot 1 #####
Plot1 <- details_board_ratings_main_plot %>% filter(owned < 70830) %>% 
         ggplot(aes(x = average, y = owned)) +
         geom_point(color = "#F2D129", alpha = 0.8, size = 1) +
         ### Annotations ###
         geom_text_repel(data = details_board_ratings_main_plot %>% dplyr::slice_head(n=25), aes(x = average, y = owned, label = primary), direction = "both", force = 4, force_pull = 2, family = "Bangers", size = 4, color = text_color, seed = 707, inherit.aes = FALSE) +
         geom_line(data = details_board_ratings_main_plot %>% dplyr::slice_head(n=25), aes(x = average, y = owned), orientation = "y", linetype = "dashed", color = "#F2D129", alpha = 0.5, size = 0.2, inherit.aes = FALSE) +
         with_outer_glow(geom_point(data = details_board_ratings_main_plot %>% dplyr::slice_head(n=25), aes(x = average, y = owned), color = "#F2E205", alpha = 1, size = 2.0, inherit.aes = FALSE), colour = "#F2BF27", sigma = 2, expand = 5) +
         ### Text Annotations ###
         geom_text(x = 3.0, y = 10000, label = "10K owned", size = 4, color = "#F2D129", family = "Acme") +
         geom_text(x = 4.0, y = 50000, label = "50K owned", size = 4, color = "#F2D129", family = "Acme") +
         geom_text(x = 4.5, y = 70000, label = "70K owned", size = 4, color = "#F2D129", family = "Acme") +
         geom_text(x = 5.0, y = 100000, label = "100K owned", size = 4.5, color = "#F2E205", family = "Acme") +
         geom_text(x = 5.5, y = 120000, label = "120K owned", size = 4.5, color = "#F2E205", family = "Acme") +
         geom_text(x = 6.0, y = 160000, label = "160K owned", size = 4.5, color = "#F2E205", family = "Acme") +
         geom_text(x = 7.5, y = 190000, label = "Top Board Games", size = 5.5, color = "#F2E205", family = "Bangers") +
         geom_text(x = 7.5, y = -12000, label = "Board Game Average Rating 1-10", size = 5.5, color = "#F2E205", family = "Bangers") +
         geom_text(x = 1.5, y = 200000, label = "Board Game Mechanic\n(Number of games)", size = 6.0, color = "#F25C05", family = "Bangers") +
         geom_text(x = 1.5, y = 25000, label = "Board Game Category\n(Number of games)", size = 6.0, color = "#05F2DB", family = "Bangers") +
         ### Scales ###
         scale_x_continuous(breaks = c(1, seq(2.5, 10, 2.5))) +
         coord_cartesian(xlim = c(-1, 10), clip = 'off', expand = FALSE) +
         ### Theme ###
         theme(axis.text.x = element_text(size = 14, color = "#F2E205", family = "Bangers", margin = margin(t = 0.5, r = 0.0, b = 0.0, l = 0.0, unit = "cm")))

##### Plot 4 #####
Plot4 <- details_board_ratings_grid_plot %>%
         ggplot(aes(x = fct_relevel(category_mechanic, c(level_mechanic, level_category)), y = fct_reorder(primary_label, owned, .desc = FALSE), color = boardgame)) +
         geom_point(alpha = 0.3, size = 5) +
         geom_point(alpha = 0.8, size = 3) +
         ### Scales ###
         guides(color = "none") +
         scale_x_discrete(position = "top") +
         scale_y_discrete(position = "right") +
         scale_color_manual(values = c("#F25C05","#05F2DB"), breaks = c("Mechanic","Category")) +
         coord_cartesian(clip = 'off', expand = TRUE) +
         ### Theme ###
         theme(## Axis ##
           axis.title.x = element_text(size = 14, family = "Bangers", color = text_color, margin = margin(t = 0.2, r = 0.0, b = 0.5, l = 0.0, unit = "cm")),
           axis.title.y = element_text(size = 14, family = "Bangers", color = text_color, angle = 270),
           axis.text.y = element_text(size = 12, family = "Bangers", color = text_color),
           axis.text.x = element_text(size = 12, family = "Bangers", color = text_color, angle = 90, hjust = 0),
           axis.ticks.x = element_blank(),
           ## Panel Grid ##
           panel.grid.major.x = element_line(size = 0.1, color = lines_color, linetype = "longdash"),
           panel.grid.major.y = element_line(size = 0.1, color = lines_color, linetype = "longdash"),
           ## Plot Aesthetic ##
           panel.background = element_rect(fill = "transparent", color = NA),
           plot.background = element_rect(fill = "transparent", color = NA)) +
         ### Labels ###
         labs(x = "Top Board Games Mechanics & Categories",
              y = "Top 25 Board Games")

###### Theme Plot 2, 3 ######
theme_set(theme_void() +
          theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, "cm"),
                ## Plot Aesthetic ##
                panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA)))

##### Plot 2 #####
Plot2 <- details_board_category %>% filter(Count_Category >= 1000) %>%
         ggplot(aes(x = fct_reorder(boardgamecategory, Count_Category, .desc = TRUE), y = Count_Category)) +
         geom_bar(stat="identity", fill = alpha("#05F2DB", 1.0)) +
         scale_y_continuous(limits = c(-1000, 6500)) + 
         coord_polar(start = 0) +
         geom_text(data = labels_barplot_category, aes(x = possition, y = Count_Category + if_else(Count_Category > 3800, -Count_Category*0.8, 300), label = boardgamecategory, hjust = hjust), color = if_else(labels_barplot_category$Count_Category > 3800, background, text_color), family = "Bangers", fontface = "plain", alpha = if_else(labels_barplot_category$Count_Category >= 1000, 0.8, 0.0), size = if_else(labels_barplot_category$Count_Category >= 4000, 4.8, 3.8), angle = labels_barplot_category$angle, inherit.aes = FALSE) + # I use conditional statements within layers such as if_else() and case_when() to assign differential values of y-axis position, alpha, size and labels to each value based on conditions.
         geom_text(data = labels_barplot_category %>% filter(Count_Category > 1500), aes(x = possition, y = Count_Category + case_when(Count_Category == 6402 ~ -1500, Count_Category == 3820 ~ 300, T ~ -Count_Category*0.7), label = Count_Category, hjust = hjust), color = if_else(labels_barplot_category$Count_Category[which(labels_barplot_category$Count_Category > 1500)] == 3820, text_color, background), family = "Bangers", fontface = "plain", alpha = 0.8, size = 4.0, angle = labels_barplot_category$angle[which(labels_barplot_category$Count_Category > 1500)], inherit.aes = FALSE)  


##### Plot 3 #####
Plot3 <- details_board_mechanic %>% filter(Count_Mechanic >= 1000) %>% 
         ggplot(aes(x = fct_reorder(boardgamemechanic, Count_Mechanic, .desc = TRUE), y = Count_Mechanic)) + 
         geom_bar(stat="identity", fill = alpha("#F25C05", 1.0)) +
         ### Scales ###
         scale_y_continuous(limits = c(-1000, 6500)) + # The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
         coord_polar(start = 0) +
         geom_text(data = labels_barplot_mechanic, aes(x = possition, y = Count_Mechanic + if_else(Count_Mechanic > 4000, -Count_Mechanic*0.7, 300), label = boardgamemechanic, hjust = hjust), color = text_color, family = "Bangers", fontface = "plain", alpha = if_else(labels_barplot_mechanic$Count_Mechanic >= 1000, 0.8, 0.0), size = if_else(labels_barplot_mechanic$Count_Mechanic >= 4000, 4.8, 3.8), angle = labels_barplot_mechanic$angle, inherit.aes = FALSE) +  # I use conditional statements within layers such as if_else() and case_when() to assign differential values of y-axis position, alpha, size and labels to each value based on conditions.
         geom_text(data = labels_barplot_mechanic %>% filter(Count_Mechanic > 1500), aes(x = possition, y = Count_Mechanic + if_else(Count_Mechanic > 4000, 300, -Count_Mechanic*0.7), label = Count_Mechanic, hjust = hjust), color = text_color, family = "Bangers", fontface = "plain", alpha = 0.8, size = 4.0, angle = labels_barplot_mechanic$angle[which(labels_barplot_mechanic$Count_Mechanic > 1500)], inherit.aes = FALSE)  

#### Final Plot ####
layout_A <- c(area(t = 3, l = 1, b = 6, r = 4),
              area(t = 0, l = 2, b = 3, r = 5))

Plot23 <- Plot2/Plot3 +
          plot_layout(design = layout_A) 
          
Plot123 <- Plot1 + inset_element(Plot23, left = 0, bottom = 0.0, right = 0.6, top = 1, align_to = 'full')

Plot <- 
  Plot123 + Plot4 + 
  plot_layout(widths = c(6.5, 3.5)) +
  plot_annotation(subtitle = annotation_subtitle_text,
                  title = annotation_title_text,
                  caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                             <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                              Source: Board Game Geek (Kaggle)",
                  theme = theme(
                    ## Plot Aesthetic ##
                    panel.background = element_rect(fill = background, color = NA),
                    plot.background = element_rect(fill = background, color = NA),
                    ## Legend ##
                    legend.position = "top",   
                    ## Titles & Caption ##
                    plot.title.position = "panel",
                    plot.title = element_markdown(color = title_color, family = "Bangers", face = "plain", size = 30, hjust = 0.50, halign = 0.7),
                    plot.subtitle = element_markdown(color = subtitle_color, family = "Bangers", face = "plain", size = 18, hjust = 0.5, halign = 0.7, margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm")),
                    plot.caption.position = "plot",
                    plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 12, margin = margin(t = -0.2, r = -1.0, b = 0.5, l = 0.0, unit = "cm")),
                    ## Margin ##
                    plot.margin = margin(t = 0.5, r = 2.0, b = 0.0, l = -2.0, unit = "cm"))) # When working with patchwork, the axes (text and titles) can be modified a lot, and it is necessary to adjust the margins (even using negative values) according to the case, in order to keep the correct position. 

#### Progress ####
ggsave("./2022/2022_Week_004/Plots/2022_Week_004.png", Plot, dpi = 326, scale = 1, width = 18, height = 12, units = c("in"))
