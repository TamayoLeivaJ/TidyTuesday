#### (TidyTuesday - 2021 - Week 39) #### 
#### ----------------------------------------------------------------------------------------- ####
## Emmy Awards data set
###  Week     Date	           Data	                 Source	                 Article
###  - 39     - 2021-09-21     - Emmy Awards         - Emmys                 - Susie Lu

#### set up ####
#### Libraries ####
library(here)
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(circlize)
library(grid)
library(gridtext)
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
##### How well streaming producers performed in the programs categories at the Emmys?
emmys <- nominees %>% 
         separate(category, sep = " - ", into = c("category", "year")) %>% 
         filter(category %in% c("Outstanding Drama Series","Outstanding Comedy Series","Outstanding Television Movie","Outstanding Variety Talk Series","Outstanding Limited Series","Outstanding Competition Program")) %>% 
         mutate(category = str_remove(category, pattern = "Outstanding ")) %>% 
         filter(distributor %in% c("HBO","Apple TV+","Disney+","Netflix","Hulu","Prime Video")) %>% 
         group_by(category, distributor, type) %>% 
         summarise(Counts = n(), .groups = "keep") %>%  
         group_by(category, distributor) %>% 
         mutate(Total = sum(Counts),
                Prop = Counts/Total)
  
emmys_chord  <- emmys %>% 
                select(distributor, category, Counts, type) %>% 
                arrange(distributor, category, desc(type)) # So the "Winner" links will be the last on be plotted (on top)
  
#### Plot aesthetics ####
background  <- c("#E1E8EB")
lines_color <- c("#0AD89D")
title_color <- c("#152D35")
subtitle_color <- c("#261C2C")
text_color  <- c("#261C2C")
caption_color  <- c("#345B63")
  
#### Annotation ####
annotation_title_text <- c("Emmy Awards")
annotation_subtitle_text <- c("How well **streaming producers** performed in the **programs categories** at the Emmys? <br><br>
                              Among producers with streaming services, currently <b style='color: #231151FF;'>HBO</b> has won in almost every of this six category, <br>
                              but **Competition program**. However, <b style='color: #000000;'>Apple TV+</b> has won its entire nomination in 2021 with <br>
                              **Ted Lasso** in **Comedy Series**.<br><br>
                              <b style='color: #231151FF; font-size:25px;'>Winners</b> and <b style='color: #23115180; font-size:25px;'>Nominess</b>")
  
#### Plot ####
p1 <- wrap_elements(plot = ~ c(
              circos.par(gap.after = c("Hulu" = 5, "HBO" = 5, "Apple TV+" = 5, "Disney+" = 5, "Netflix" = 5, "Prime Video" = 35,
                                       "Limited Series" = 5, "Drama Series" = 5, "Comedy Series" = 5, "Television Movie" = 5, "Competition Program" = 5, "Variety Talk Series" = 35),
                         start.degree = 72),
              chordDiagram(emmys_chord,
                           order = c("Limited Series", "Drama Series","Comedy Series","Television Movie","Competition Program","Variety Talk Series", "Hulu","HBO","Apple TV+","Disney+","Netflix", "Prime Video"),
                           grid.col = c("Hulu" = "#7FC8A9", "HBO" = "#231151FF", "Apple TV+" = "#000000", "Disney+" = "#B3B3B3", "Netflix" = "#CD113B", "Prime Video" = "#0D79AF", 
                                        "Limited Series" = "#E1E8EB", "Drama Series" = "#E1E8EB", "Comedy Series" = "#E1E8EB", "Television Movie" = "#E1E8EB", "Competition Program" = "#E1E8EB", "Variety Talk Series" = "#E1E8EB"),
                           transparency = ifelse(emmys_chord$type == "Winner", 0.00, 0.75),
                           scale = TRUE,
                           link.sort = FALSE, 
                           link.decreasing = FALSE,
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
           plot.title = element_markdown(color = title_color, family = "Economica", face = "bold", size = 32, margin = unit(c(t = 0.5, r = 0.0, b = 0.5, l = 0.0), unit = "cm")),
           plot.subtitle = element_markdown(color = subtitle_color, family = "Economica", face = "plain", size = 16, hjust = 0.5, halign = 0.5, valign = 0.5, margin = unit(c(t = 0.5, r = 0.0, b = 0.5, l = 0.0), unit = "cm")),
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
