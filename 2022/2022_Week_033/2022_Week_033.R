#### (TidyTuesday - 2022 - Week 33) #### 
#### ----------------------------------------------------------------------------------------- ####
## 
###  Week     Date	           Data	                          Source	                              Article
###  - 33     - 2022-08-16	   - Open Source Psychometrics	  - Open-Source Psychometrics Project	  - Character Personality


#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)


#### Libraries (This Plot) ####
library(igraph)
library(ggraph)
library(tidygraph)
library(ggforce)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_033/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
psychometrics <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')


#### Data wrangling #### 
##### Network to see similarities between GOT characters?
###### set seed
set.seed(4321)

#### Intersection #### 
psycho_df <- psychometrics %>%
             dplyr::filter(uni_name == "Game of Thrones" & avg_rating >= 80) %>% 
             dplyr::group_by(char_name, question) %>%
             dplyr::slice_max(order_by = avg_rating, n = 1, with_ties = FALSE) %>% 
             dplyr::ungroup() %>% 
             dplyr::group_by(question) %>%
             dplyr::mutate(question_id = dplyr::cur_group_id(),
                           personality = stringr::str_c(question_id, personality, sep = "_")) %>% 
             dplyr::ungroup() %>%
             dplyr::select(char_name, question, personality) %>% 
             dplyr::group_by(char_name) %>%
             dplyr::summarise(personality = list(as.character(personality))) %>%
             dplyr::rename("from" = "char_name") %>% 
             tidyr::crossing(dplyr::rename(., "to" = "from", "personality_to" = "personality")) %>% 
             dplyr::mutate(frequency = purrr::map2_dbl(personality, personality_to, ~length(base::intersect(.x, .y)))) %>% 
             dplyr::select(from, to, frequency) %>% 
             dplyr::filter(from != to) %>% 
             dplyr::mutate(similarity = case_when(frequency <= quantile(frequency, probs = 0.80) ~ "q80",
                                                  frequency <= quantile(frequency, probs = 0.90) ~ "q90",
                                                  frequency <= quantile(frequency, probs = 0.95) ~ "q95",
                                                  frequency <= quantile(frequency, probs = 0.99) ~ "q99",
                                                  TRUE ~ "Top1")) %>% 
             dplyr::arrange(frequency)
  
             
#### Network #### 
##### Network vertices #####
psycho_ve <- psycho_df %>%
             dplyr::group_by(to) %>%
             dplyr::summarise(links = sum(frequency)) %>%
             dplyr::select(to, links)


##### Network object #####
psycho_ig <- igraph::graph_from_data_frame(d = psycho_df, 
                                           vertices = psycho_ve, 
                                           directed = FALSE)


##### Network tidy #####
psycho_tg <- tidygraph::as_tbl_graph(psycho_ig) %>% 
             tidygraph::activate(nodes) %>% 
             dplyr::mutate(label = name)

size <- igraph::V(psycho_tg)$links

#### Legend Annotation ####
data_label <- tibble(x = seq(1.75, 2.25, length.out = 30),
                     y = seq(-1.35, -1.35, length.out = 30),
                     color  = psycho_ve %>% pull(links) %>% sort(),
                     alpha  = psycho_ve %>% pull(links) %>% sort(),    
                     size   = psycho_ve %>% pull(links) %>% sort())


#### Plot aesthetics ####
background     <- "#141C26"
lines_color    <- "#A6444C"
title_color    <- "#F2C230"
subtitle_color <- "#FFAB16"
text_color     <- "#F2F2F2"
caption_color  <- "#6C7D8C"


#### Annotation ####
annotation_title_text <- c("Game of Thrones")
annotation_text <- c("The Open-Source Psychometrics Project, it has recruited more than 3 million volunteers to rate characters from different series, according to 400 descriptive adjectives and other properties. The responses can be aggregated to create profiles that users can match as part of a personality test. In total, the project collects information on more than 2,000 characters, with 400 descriptive adjectives rated on a scale of 1 to 100, depending on how strongly one would identify the character's particular characteristic. From the data set, GOT characters have been selected, to investigate which characters share the greatest number of the strongest characteristics (rating >80%) in common or similarity.")
annotation_text <- stringr::str_wrap(annotation_text, 70) %>% 
                   stringr::str_replace_all("\n","<br>")

#### Plot ####
Plot <- psycho_tg %>%
        ggraph::ggraph(layout = "igraph", algorithm = 'fr') +
        ggraph::geom_edge_arc(aes(edge_width = similarity, alpha = similarity, colour = similarity), lineend = "round", strength = .1) +
        ggraph::geom_node_point(aes(colour = links), fill = background, size = log(size) * 1.5, alpha = 1.0, shape = 21) +
        ggraph::geom_node_point(aes(colour = links), size = log(size) * 1.0, alpha = 1.0) +
        ### Annotations ###
        ggraph::geom_node_label(aes(label = label), colour = text_color, size = log(size) * 0.60, family = "Cinzel Decorative", fontface = "bold", repel = FALSE, nudge_y = -0.04, nudge_x = 0.00, alpha = 0.6, fill = background, label.size = NA) +
        ggtext::geom_richtext(aes(x = 1.26, y = -1.33), label = annotation_text, color = subtitle_color, size = 4, family = "Cinzel", face = "plain", fill = "transparent", label.size = NA, hjust = 0.5, halign = 0, vjust = 0.5, valign = 0.5, margin = margin(t = 0.1, r = 0.1, b = 0.5, l = 0.1, unit = "cm")) +
        ### Scales ###
        ggraph::scale_edge_width_manual(values = c(seq(0.2, 1.0, length.out = 4), 2.0), breaks = c("q80","q90","q95","q99","Top1")) +
        ggraph::scale_edge_alpha_manual(values = c(seq(0.2, 0.5, length.out = 4), 0.8), breaks = c("q80","q90","q95","q99","Top1")) +
        ggraph::scale_edge_colour_manual(values = viridis::inferno(n = length(unique(psycho_df$similarity)), begin = 0.5, end = 0.9, direction = 1)) +
        scale_colour_gradientn(colors = viridis::plasma(n = length(unique(psycho_ve$links)), begin = 0.5, end = 0.9, direction = 1)) +
        guides(edge_width = "none", edge_alpha = "none", colour = "none", edge_colour = "none") +
        coord_cartesian(ylim = c(-1.4, 0.2)) +
        ### Theme ###
        theme_void() +
        theme(
          ## Plot Aesthetic ##
          panel.background = element_rect(fill = background, color = NA),
          plot.background  = element_rect(fill = background, color = NA),
          ## Titles & Caption ##
          plot.title.position = "panel",
          plot.title = ggtext::element_markdown(color = title_color, family = "Cinzel", face = "plain", size = 42, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5, margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")),
          plot.caption.position = "panel",
          plot.caption = ggtext::element_markdown(color = caption_color, family = "Menlo", size = 10, hjust = 1, vjust = 0, margin = margin(t = -0.5, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 1.5, unit = "cm")) +
        ### Labels ###
        ggplot2::labs(x = "",
                      y = "",
                      title = annotation_title_text,
                      caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                                 <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>  TamayoLeivaJ<br><br> 
                                 Source: Open-Source Psychometrics Project courtesy of <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @tanya_shapiro")


Final_Plot <- Plot +
              ### Legend Annotation ###
              ggforce::geom_link2(data = data_label, aes(x = x, y = y, colour = color, alpha = alpha, size = size, group = 1), lineend = 'round') +
              geom_point(data = data_label[c(1, 30),], aes(x = x, y = y, colour = color), fill = background, alpha = 1.0, size = range(log(size) * 1.5), shape = 21) +
              geom_point(data = data_label[c(1, 30),], aes(x = x, y = y, colour = color), alpha = 1.0, size = range(log(size))) +
              geom_text(data = data_label[c(1, 30),], aes(x = x, y = y + 0.002), label = c("-","+"), colour = background, alpha = 1.0, size = sort(log(size))[c(1,30)], family = "Cinzel Decorative", fontface = "bold", hjust = 0.5, vjust = 0.5) +
              geom_text(data = data_label[15,], aes(x = x, y = y + 0.100), label = "How to read the network?", colour = subtitle_color, alpha = 1.0, size = sort(log(size) * 1.5)[1], family = "Cinzel", fontface = "plain") +
              geom_text(data = data_label[15,], aes(x = x, y = y + 0.025), label = "Connections Scale", colour = text_color, alpha = 1.0, size = sort(log(size) * 0.8)[1], family = "Cinzel Decorative", fontface = "bold") +
              geom_text(data = data_label[15,], aes(x = x, y = y - 0.025), label = "Similarity Scale", colour = text_color, alpha = 1.0, size = sort(log(size) * 0.8)[1], family = "Cinzel Decorative", fontface = "bold") +
              scale_alpha_continuous(range = c(0.0, 1.0)) +
              scale_size_continuous(range = c(0.2, 2.0)) +
              guides(colour = "none", alpha = "none", size = "none")

#### Progress ####
ggsave("./2022/2022_Week_033/Plots/2022_Week_033.png", Final_Plot, dpi = 326, scale = 1, width = 12, height = 14, units = c("in"))
