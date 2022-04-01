#### (TidyTuesday - 2022 - Week 13) #### 
#### ----------------------------------------------------------------------------------------- ####
## Collegiate sports
###  Week     Date	           Data	                           Source	                                Article
###  - 13     - 2022-03-29 	   - Collegiate Sports Budgets 	   - Equity in Athletics Data Analysis 	  - NPR


#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)

#### Libraries (This Plot) ####
library(packcircles)
library(viridis)
library(patchwork)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_013/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')


#### Data wrangling #### 
##### Where do the sports funds go?
Sports <- sports %>% 
          group_by(year, institution_name) %>% 
          mutate(total_exp_all_sports = sum(total_exp_menwomen, na.rm = TRUE)) %>% 
          group_by(institution_name) %>%
          mutate(max_total_exp_all_sports = max(total_exp_all_sports)) %>% 
          arrange(desc(max_total_exp_all_sports)) %>% 
          group_by(year) %>% 
          #mutate(q_total_exp_all_sports = quantile(total_exp_all_sports, 0.99, na.rm = TRUE)) %>% 
          filter(total_exp_all_sports >= quantile(total_exp_all_sports, 0.99, na.rm = TRUE)) %>% 
          #filter(total_exp_all_sports >= head(unique(sort(total_exp_all_sports, decreasing = TRUE)), 10)) 
          ungroup()

Sports_int <- Sports %>%
              filter(year == max(year)) %>%
              select(year, institution_name, sports) %>% 
              distinct() %>% 
              group_by(year, institution_name) %>% 
              group_split(.keep = FALSE)

Sports_int_list <- Reduce(intersect, Sports_int) %>% unlist() 

Sports_top <- Sports %>%
              mutate(sports = fct_other(sports, keep = Sports_int_list, other_level = "Other sports")) %>% 
              group_by(year, institution_name, sports) %>% 
              summarise(across(where(is.numeric) & !c(total_exp_all_sports, max_total_exp_all_sports), sum, na.rm = TRUE), .groups = "keep") %>%
              group_by(year, institution_name) %>% 
              mutate(total_exp_all_sports = sum(total_exp_menwomen, na.rm = TRUE),
                     total_rev_all_sports = sum(total_rev_menwomen, na.rm = TRUE)) %>% 
              group_by(year, sports) %>% 
              mutate(total_exp_all_sports_all_top1 = sum(total_exp_menwomen, na.rm = TRUE),
                     total_rev_all_sports_all_top1 = sum(total_rev_menwomen, na.rm = TRUE)) %>%
              ungroup()

## Generate circle layout
Sports_top_circle <- Sports_top %>% 
                     filter(year == max(year)) %>% 
                     select(year, sports, total_exp_all_sports_all_top1) %>% 
                     distinct() %>% 
                     arrange(desc(total_exp_all_sports_all_top1)) %>% 
                     mutate(id = row(.),
                            total_exp_all_sports_all_top1_M = round(total_exp_all_sports_all_top1/1000000),
                            label = if_else(sports %in% c("All Track Combined"), paste0("USD ", total_exp_all_sports_all_top1_M,"M"), paste0(sports, "\nUSD ", total_exp_all_sports_all_top1_M,"M")))

  
packing <- circleProgressiveLayout(as.numeric(Sports_top_circle$total_exp_all_sports_all_top1), sizetype='area')
packing$radius <- 0.95 * packing$radius
Sports_top_circle <- cbind(Sports_top_circle, packing)
dat.gg <- circleLayoutVertices(packing, npoints = 100)


#### Plot aesthetics ####
background     <- "#F2F2F2"
lines_color    <- "#3C5473"
title_color    <- "#39054D"
subtitle_color <- "#DB004C"
text_color     <- "#DB004C"
caption_color  <- "#39054D"

Palette <- inferno(begin = 0.1, end = 0.4, direction = -1, n=7)

#### Annotation ####
annotation_title_text <- c("Collegiate sports")
annotation_subtitle_text <- c("For the 2019-2020 season, the colleges with the highest expenditures in sports -top 1%- invested more than 80 million each. While there are more than 30 registered sports, only 8 are played at all of these universities. Despite this sport variety, a single sport takes -by far- the largest investment, a figure even larger than all others combined. And explained by the high level of income generated")
annotation_subtitle_text <- str_wrap(annotation_subtitle_text, 90) %>% str_replace_all("\n","<br>")


#### Plot ####
Plot_A <- Sports_top %>% 
          filter(year == max(year)) %>% 
          ggplot(aes(x = total_exp_menwomen, y = fct_reorder(institution_name, total_exp_all_sports), fill = fct_other(sports, keep = "Football", other_level = "All others") %>% fct_relevel("Football", after = Inf), color = fct_other(sports, keep = "Football", other_level = "All others") %>% fct_relevel("Football", after = Inf))) +
          geom_col(position = "stack") +
          ### Annotations ###
          geom_text(aes(label = institution_name, x = 500000), size = 3.8, hjust = 0, color = background, family = "Graduate", fontface = "plain") +
          ### Scales ###
          scale_fill_manual(values = c("Football" = "#D90404", "All others" = "#023859")) +
          scale_color_manual(values = c("Football" = "#D90404", "All others" = "#023859")) +
          scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "USD ", suffix = " M", decimal.mark = "."), breaks = seq(0,120e6, 40e6), limits = c(0,120e6)) +
          coord_cartesian(expand = FALSE) +
          guides(fill = guide_legend(title = "", label.position = "top"), color = "none") +
          ### Theme ###
          theme_minimal() +
          theme(
            ## Text ##
            text = element_text(face = "plain", family = "Antonio", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
            ## Axis ##
            axis.title.x = element_text(size = 18, color = text_color, margin = unit(c(0.25, 0.25, 0.75, 0.25), "cm")),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 16, color = text_color),
            axis.line.x = element_blank(),
            axis.ticks.x = element_line(size = 0.1, color = lines_color),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            ## Legend ##
            legend.position = c(.9,.35),
            legend.text = element_text(face = "plain", family = "Antonio", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0, size = 20),
            ## Plot Aesthetic ##
            panel.background = element_rect(fill = background, color = NA),
            plot.background = element_rect(fill = background, color = NA),
            ## Titles & Caption ##
            plot.title.position = "panel",
            plot.title    = element_markdown(color = title_color, family = "Graduate", face = "plain", size = 18, hjust = 0, halign = 0.0, vjust = 0.5, valign = 0.5, margin = unit(c(0.25, 0.25, 0.75, 0.25), "cm")),
            ## Margin ##
            plot.margin = margin(t = 0.25, r = 0.75, b = 0.75, l = 0.25, unit = "cm")) +
          labs(title = "Colleges with the highest expenditures (Top 1%)<br>for all sports in season 2019-2020.",
               x = "Total expenditure")

Plot_B <- ggplot() +
          geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.character(id), color = as.character(id)), alpha = 1.0) +
          ### Annotations ###
          geom_text(data = Sports_top_circle, aes(x, y, size = total_exp_all_sports_all_top1, label = label), color = background, family = "Antonio", fontface = "plain") +
          geom_text(data = Sports_top_circle %>% filter(sports %in% c("All Track Combined")), aes(x, y, size = total_exp_all_sports_all_top1, label = sports), color = Palette[4],  family = "Antonio", fontface = "plain", nudge_y = 7000, nudge_x = 0, inherit.aes = FALSE) +
          ### Scales ###
          scale_fill_manual(values = c("#D90404", Palette, "#023859"), breaks = c(1:2,4:9,3)) +
          scale_color_manual(values = c("#D90404", Palette, "#023859"), breaks = c(1:2,4:9,3)) +
          scale_size_continuous(range = c(4.2,15)) +
          coord_equal(expand = TRUE) +
          ### Theme ###
          theme_void() +
          theme(
            ## Text ##
            text = element_text(face = "plain", family = "Antonio", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
            ## Axis ##
            axis.title.y = element_markdown(color = subtitle_color, family = "Antonio", face = "plain", size = 38, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5, angle = 90),
            ## Plot Aesthetic ##
            panel.background = element_rect(fill = background, color = NA),
            plot.background = element_rect(fill = background, color = NA),
            ## Titles & Caption ##
            plot.title.position = "panel",
            plot.title    = element_markdown(color = title_color, family = "Graduate", face = "plain", size = 24, hjust = 1, halign = 1.0, vjust = 0.5, valign = 0.5, margin = unit(c(0.25, 0.25, 0.75, 0.25), "cm")),
            ## Legend ##
            legend.position = "none",
            ## Margin ##
            plot.margin = margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm")) +
          labs(title = "Colleges (Top 1%)<br>cumulative total expenditure by sport.",
               y = "Season 2019-2020")

Plot_C <- Sports_top %>% 
          filter(year == max(year)) %>% 
          select(sports, total_rev_all_sports_all_top1) %>% 
          distinct() %>% 
          ggplot(aes(x = total_rev_all_sports_all_top1, y = fct_other(sports, keep = "Football", other_level = "All others") %>% fct_rev(), fill = fct_other(sports, keep = "Football", other_level = "All others"))) +
          geom_col(position = "stack") +
          ### Annotations ###
          geom_text(aes(label = fct_other(sports, keep = "Football", other_level = "All others"), x = 5e7), size = 8, hjust = 0, color = background, family = "Graduate", fontface = "plain") +
          ### Scales ###
          scale_fill_manual(values = c("Football" = "#D90404", "All others" = "#023859")) +
          scale_color_manual(values = c("Football" = "#D90404", "All others" = "#023859")) +
          scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "USD ", suffix = " M", decimal.mark = "."), breaks = seq(0,2e9, 5e8), limits = c(0,2e9)) +
          coord_cartesian(expand = FALSE) +
          guides(fill = "none") +
          ### Theme ###
          theme_minimal() +
          theme(
            ## Text ##
            text = element_text(face = "plain", family = "Antonio", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
            ## Axis ##
            axis.title.x = element_text(size = 18, color = text_color, margin = unit(c(0.25, 0.25, 0.75, 0.25), "cm")),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 16, color = text_color),
            axis.line.x = element_blank(),
            axis.ticks.x = element_line(size = 0.1, color = lines_color),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            ## Legend ##
            legend.position = c(.9,.35),
            legend.text = element_text(face = "plain", family = "Antonio", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0, size = 20),
            ## Plot Aesthetic ##
            panel.background = element_rect(fill = background, color = NA),
            plot.background = element_rect(fill = background, color = NA),
            ## Titles & Caption ##
            plot.title.position = "panel",
            plot.title    = element_markdown(color = title_color, family = "Graduate", face = "plain", size = 18, hjust = 0, halign = 0.0, vjust = 0.5, valign = 0.5, margin = unit(c(0.25, 0.25, 0.75, 0.25), "cm")),
            ## Margin ##
            plot.margin = margin(t = 0.25, r = 0.75, b = 0.75, l = 0.25, unit = "cm")) +
          labs(title = "Colleges (Top 1%) cumulative total revenue<br>Football vs All others (Season 2019-2020)",
               x = "Total revenue")

#### Final Plot ####
Plot <-  Plot_A + Plot_B + Plot_C +
         plot_layout(heights = c(1.20, 1.60, 0.20)) +
         plot_annotation(subtitle = annotation_subtitle_text,
                         title = annotation_title_text,
                         caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                                    <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                                     Source: Equity in Athletics Data Analysis",
                         theme = theme(
                           ## Plot Aesthetic ##
                           panel.background = element_rect(fill = background, color = NA),
                           plot.background = element_rect(fill = background, color = NA),
                           ## Legend ##
                           legend.position = "top",   
                           ## Titles & Caption ##
                           plot.title.position = "plot",
                           plot.title    = element_markdown(color = title_color, family = "Graduate", face = "plain", size = 44, hjust = 0.5, halign = 0.0, vjust = 0.5, valign = 0.5, padding = unit(c(0.25, 0.25, 0.25, 0.25), "cm")),
                           plot.subtitle = element_markdown(color = subtitle_color, family = "Antonio", face = "plain", size = 19, hjust = 0.5, halign = 0.0, vjust = 0.5, valign = 0.5, padding = unit(c(0.25, 0.25, 0.25, 0.25), "cm")),
                           plot.caption.position = "plot",
                           plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 10, margin = margin(t = 0.2, r = -1.0, b = 0.1, l = 0.0, unit = "cm")),
                           ## Margin ##
                           plot.margin = margin(t = 0.5, r = 0.75, b = 0.5, l = 0.5, unit = "cm"))) # When working with patchwork, the axes (text and titles) can be modified a lot, and it is necessary to adjust the margins (even using negative values) according to the case, in order to keep the correct position. 


#### Progress ####
ggsave("./2022/2022_Week_013/Plots/2022_Week_013.png", Plot, dpi = 326, scale = 1, width = 10, height = 20, units = c("in"))
