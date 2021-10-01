#### (TidyTuesday - 2021 - Week 39) #### 
#### ----------------------------------------------------------------------------------------- ####
## Emmy Awards data set
###  Week     Date	           Data	                 Source	                 Article
###  - 39     - 2021-09-21         - Emmy Awards         - Emmys                 - Susie Lu

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(circlize)
library(grid)
library(patchwork)
library(systemfonts)

#### Directory ####
# Create directory to save images and plots
dir.create(here("2021/2021_Week_039/Images"), recursive = TRUE, mode = "0755")
dir.create(here("2021/2021_Week_039/Plots"), recursive = TRUE, mode = "0755")

#### Read data ####
tt_data <- tt_load(2021, week = 39)

nominees <- tt_data$nominees

#### Data wrangling #### 
##### Which ...?
emmys <- nominees %>% 
         separate(category, sep = " - ", into = c("category", "year")) %>% 
         filter(category %in% c("Outstanding Drama Series","Outstanding Comedy Series","Outstanding Animated Program","Outstanding Variety Talk Series","Outstanding Limited Series")) %>% 
         mutate(category = str_remove(category, pattern = "Outstanding ")) %>% 
         filter(distributor %in% c("HBO","NBC","ABC","Netflix","FOX","Hulu","Prime Video")) %>% 
         group_by(category, distributor, type) %>% 
         summarise(Counts = n(), .groups = "keep") %>%  
         group_by(category, distributor) %>% 
         mutate(Total = sum(Counts),
                Prop = Counts/Total)

emmys_chord  <- emmys %>% 
                select(distributor, category, Counts, type) %>% 
                arrange(type) # So the "Winner" links will be the last on be plotted (on top)
#### Annotation ####



#### Text ####
annotation_title_text <- c("Emmy Awards")
annotation_subtitle_text <- c("")

#### Plot aesthetics ####
background  <- c("#E1E8EB")
lines_color <- c("#0AD89D")
title_color <- c("#152D35")
subtitle_color <- c("#112031")
text_color  <- c("#112031")
caption_color  <- c("#345B63")

#### Plot ####
p1 <- wrap_elements(full = ~ c(
              circos.par(gap.after = c("Hulu" = 5, "HBO" = 5, "NBC" = 5, "ABC" = 5, "Netflix" = 5, "Prime Video" = 5, "FOX" = 35,
                                       "Limited Series" = 5, "Drama Series" = 5, "Comedy Series" = 5, "Animated Program" = 5, "Variety Talk Series" = 35),
                         start.degree = 67.5),
              chordDiagram(emmys_chord,
                           order = c("Limited Series", "Drama Series","Comedy Series","Animated Program","Variety Talk Series", "Hulu","HBO","NBC","ABC","Netflix", "Prime Video","FOX"),
                           grid.col = c("Hulu" = "#7FC8A9", "HBO" = "#231151FF", "NBC" = "#F47800", "ABC" = "#B3B3B3", "Netflix" = "#CD113B", "Prime Video" = "#0D79AF", "FOX" = "#000000", 
                                        "Limited Series" = "#E1E8EB", "Drama Series" = "#E1E8EB", "Comedy Series" = "#E1E8EB", "Animated Program" = "#E1E8EB", "Variety Talk Series" = "#E1E8EB"),
                           transparency = ifelse(emmys_chord$type == "Winner", 0.00, 0.75),
                           scale = TRUE,
                           link.sort = TRUE, 
                           link.decreasing = TRUE,
                           directional = -1,
                           diffHeight = mm_h(1),
                           link.largest.ontop = FALSE)
)
                )


circos.clear()

plot_emmys  <- 
p1 + theme(## Plot Aesthetic ##
           panel.background = element_rect(fill = background, color = NA),
           plot.background = element_rect(fill = background, color = NA),
           ## Titles & Caption ##
           plot.title.position = "plot",
           plot.title = element_markdown(color = title_color, family = "Basique", face = "bold", size = 16),
           plot.subtitle = element_markdown(color = subtitle_color, family = "Basique", face = "plain", size = 14),
           plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 0.5),
           ## Margin ##
           plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
     ### Labels ###
     labs(title = annotation_title_text,
          subtitle = annotation_subtitle_text,
          caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                  Source: Emmys")

#### Progress ####
ggsave("./2021/2021_Week_039/Plots/2021_Week_039.png", plot_emmys, dpi = 320, scale = 1, width = 8, height = 10, units = c("in"))