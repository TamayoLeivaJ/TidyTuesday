#### (TidyTuesday - 2022 - Week 05) #### 
#### ----------------------------------------------------------------------------------------- ####
## Dog breeds
###  Week     Date	           Data	                    Source	                   Article
###  - 05     - 2022-02-01	   - Dog breeds	            - American Kennel Club	   - Vox


#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)

#### Libraries (This Plot) ####
library(magick)
library(png)
library(ggimage)
library(ggbump)
library(MetBrewer)
library(patchwork)


#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_005/Plots"), recursive = TRUE, mode = "0755")
dir.create(here("2022/2022_Week_005/Images"), recursive = TRUE, mode = "0755")


#### Read data ####
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')


#### Function ####
###### Images Function ######
### URL Ref: https://themockup.blog/posts/2021-01-28-removing-image-backgrounds-with-magick/
img_color <- function(img_url, color, geometry, side, border, background, flop){
  # find the name of the img and extract it
  img_name <- str_replace(img_url, ".*[/]([^.]+)[.].*", "\\1")
  
  circle <- image_draw(image_blank(side, side))                                                 # Create a circle that will be use as frame
  symbols(side/2, side/2, circles = side/2, bg='black', inches=FALSE, add=TRUE)
  dev.off()
  
  if(isTRUE(flop))
    {
            img_url %>% 
              image_read() %>%
              image_flop() %>%                                                                            # Invert image horizontally
              image_scale(geometry = geometry) %>%                                                        # Scale all images to the same size
              image_border(color = "white", border) %>%                                                   # Add an images border to avoid cutting the image
              image_colorize(color = color, opacity = 20) %>%                                             # Change images color
              image_composite(circle, operator='copyopacity', offset = "+0+0") %>%                        # Create image with the two images. The top image -the circle- is center at 0,0     
              image_background(background) %>%                                                            # Set image background
              image_write(path = paste0("2022/2022_Week_005/Images/", img_name, ".png"), format = "png")  # Save image locally
  } else {
           img_url %>% 
             image_read() %>% 
             image_scale(geometry = geometry) %>%                                                        # Scale all images to the same size
             image_border(color = "white", border) %>%                                                   # Add an images border to avoid cutting the image
             image_colorize(color = color, opacity = 20) %>%                                             # Change images color
             image_composite(circle, operator='copyopacity', offset = "+0+0") %>%                        # Create image with the two images. The top image -the circle- is center at 0,0     
             image_background(background) %>%                                                            # Set image background
             image_write(path = paste0("2022/2022_Week_005/Images/", img_name, ".png"), format = "png")  # Save image locally
  }
}

img_traits_color <- function(img_url, geometry, color){
                    # find the name of the img and extract it
                    img_name <- str_replace(img_url, ".*[/]([^.]+)[.].*", "\\1")
                    
                    img_url %>% 
                      image_read() %>% 
                      image_scale(geometry = geometry) %>%
                      image_crop() %>% 
                      image_fill(color = "transparent", refcolor = "white", fuzz = 2, point = "+1+1") %>% # Set image background
                      image_colorize(color = color, opacity = 100) %>% 
                      image_write(path = paste0("2022/2022_Week_005/Images/", img_name, ".png"), format = "png")  # Save image locally

}



#### Data wrangling #### 
##### How is the popularity of breeds growing?
###### Bump plot ######
breed_rank_long <- breed_rank %>% 
                   pivot_longer(cols = ends_with("Rank"), values_to = "Rank", names_to = "year") %>% 
                   mutate(year = str_remove(year, " Rank") %>% as.numeric(.)) %>% 
                   filter(!is.na(Rank)) %>% 
                   group_by(Breed) %>%
                   arrange(year, .by_group = TRUE) %>% 
                   mutate(R_min = min(Rank),
                          R_diff = max(Rank) - min(Rank),
                          R_diff_dir = case_when(first(Rank)-last(Rank) > 0 ~ "Increases",  
                                                 first(Rank)-last(Rank) < 0 ~ "Decrease", 
                                                 T ~ "Constant")) %>% 
                   ungroup()

###### Images plot ######  
breeds <- breed_rank_long %>% 
          filter(R_diff_dir %in% c("Increases","Decrease") | Breed == "Retrievers (Labrador)") %>% 
          distinct(Breed, R_min, R_diff, R_diff_dir, Image) %>%
          group_by(R_diff_dir) %>% 
          arrange(desc(R_diff), .by_group = TRUE) %>%
          slice_head(n = 5) %>% 
          ungroup() %>% 
          select(Breed, R_min, R_diff_dir, img_url = Image) %>% 
          mutate(color = case_when(R_diff_dir == "Constant" ~ "#F4C40F", 
                                   R_diff_dir == "Decrease" ~ "#D8443C", 
                                   R_diff_dir == "Increases" ~ "#1F6E9C"),
                 geometry = "250x250",
                 border = "25x25",
                 side = 300,
                 background = "#F2F2F2",
                 flop = if_else(R_diff_dir == "Decrease", FALSE, TRUE)) 

breeds %>% select(img_url, color, geometry, side, border, background, flop) %>% pwalk(img_color) # Process dog images

breeds <- breeds %>% 
          group_by(R_diff_dir) %>%
          arrange(R_min, .by_group = TRUE) %>% 
          ungroup() %>% 
          mutate(img = paste0("2022/2022_Week_005/Images/", str_replace(img_url, ".*[/]([^.]+)[.].*", "\\1"), ".png"),
                 label = str_wrap(Breed, 18) %>% str_replace_all(" ","\n"),
                 label = case_when(Breed == "Miniature Bull Terriers" ~ "Miniature\nBull Terriers",
                                   Breed == "Black and Tan Coonhounds" ~ "Black & Tan\nCoonhounds",
                                   Breed == "Spaniels (Irish Water)" ~ "Spaniels\n(Irish Water)", 
                                   T ~ label),
                 x = case_when(R_diff_dir == "Constant" ~ 2031, 
                               R_diff_dir == "Decrease" ~ 2029.6,
                               R_diff_dir == "Increases" ~ 2032.4),
                 y = c(2.5, rep(seq(40, 190, length.out = 5), 2)),  # The y-axis is inverted on the plot
                 x_possition = if_else(R_diff_dir == "Decrease", x - 2.4, x + 2.2),
                 y_possition = y) %>% 
          left_join(breed_rank_long %>%
                    group_by(Breed) %>% 
                    mutate(label_rank = if_else(Breed != "Retrievers (Labrador)", paste0(first(Rank)," - ", last(Rank)), as.character(paste0("Top ",last(Rank))))) %>% 
                    select(Breed, label_rank)) %>% 
          distinct() %>% 
          mutate(label = paste0(label,"\n",label_rank))

breeds_inc <- breeds %>% filter(R_diff_dir == "Increases") %>% pull(Breed)
breeds_dec <- breeds %>% filter(R_diff_dir == "Decrease") %>% pull(Breed)

###### Annotations ######
breeds_ranks <- breed_rank_long %>% 
                select(Breed, year, Rank, R_diff, R_diff_dir) %>% 
                filter(Breed %in% breeds$Breed & year %in% c(min(year), max(year))) %>% 
                distinct() %>% 
                mutate(label = if_else(year ==  2013, paste0(Breed, " - ", Rank), as.character(Rank)),
                       x = if_else(year == 2013, 2012.8, 2020.5),
                       y = if_else(Rank %in% c(101,185), Rank -3, Rank))

image_traits <- tibble(trait = c("Affectionate With Family","Good With Other Dogs","Coat Grooming Frequency","Playfulness Level",
                                 "Watchdog/Protective Nature","Trainability Level","Energy Level","Barking Level"),
                       img_url = paste0("2022/2022_Week_005/Images/img_", seq(1:8), ".png"),
                       geometry = "250x250",
                       color = met.brewer("Signac", 14, "discrete")[c(3,5:6,8,10:11,13:14)],
                       x = rep(1:2, each = 4),
                       y = rep(4:1, 2),
                       x_possition = x,
                       y_possition = y,
                       label = trait)

image_traits %>% select(img_url, geometry, color) %>% pwalk(img_traits_color) # Process traits images

###### Traits Images ######
breed_traits_images <- breed_traits %>% 
                       mutate(Breed = str_squish(Breed)) %>% 
                       filter(Breed %in% breeds$Breed) %>% 
                       left_join(breeds_ranks %>% select(Breed, R_diff_dir)) %>% 
                       pivot_longer(cols = c(2:7,10:17), names_to = "trait", values_to = "Index") %>% 
                       select(Breed, trait, Index, R_diff_dir) %>% 
                       distinct() %>% 
                       left_join(image_traits %>% select(trait, img_url)) %>% 
                       filter(!is.na(img_url)) %>% 
                       left_join(breeds %>% select(Breed, y)) %>% 
                       mutate(img = img_url,
                              label = Index,
                              y = if_else(trait %in% c("Affectionate With Family","Good With Other Dogs","Coat Grooming Frequency","Playfulness Level"), y-5, y+5),
                              x =  c(rep(seq(2035.0, 2037.0, length.out = 4),2),
                                     rep(seq(2036.5, 2038.5, length.out = 4),10),
                                     rep(seq(2023.5, 2025.5, length.out = 4),10))) %>% 
                       filter(trait %in% c("Affectionate With Family", "Good With Other Dogs","Playfulness Level","Watchdog/Protective Nature","Trainability Level","Energy Level") & Index >= 4 | 
                              trait %in% c("Coat Grooming Frequency", "Barking Level") & Index <= 2)



#### Plot aesthetics ####
background     <- "#F2F2F2"
lines_color    <- "#F2F2F2"
title_color    <- "#0F6CA6"
subtitle_color <- "#554A71"
text_color     <- "#979DA6"
caption_color  <- "#979DA6"


#### Annotation ####
annotation_title_text <- c("Dog Breed Popularity")
annotation_subtitle_text <- c("Over the 7 years from 2013 to 2020, the American Kennel Club (AKC) breed popularity ranking has expanded the number of registered breeds from 177 to 195. However, some dog breeds - for many possible reasons - have seen a greater variation in popularity than others. Here, we analyze the 5 breeds that have changed the most - increased or decreased - according to the AKC breed popularity ranking, and particularly review what characteristics they have in common that may or may not explain this variation. In addition, we include the breed that has been the favorite throughout this period, probably for many good reasons.")
annotation_subtitle_text <- str_wrap(annotation_subtitle_text, 100) %>% str_replace_all("\n","<br>") # Helps to separate long texts into lines with the same maximum number of characters
annotation_subtitle_text <- annotation_subtitle_text %>% 
                            str_replace_all("Over the 7 years","<span class='text-nowrap'>Over the 7 years") %>%
                            str_replace_all("increased","<p style='color:#1F6E9C'>increased</p>") %>% 
                            str_replace_all("decreased","<p style='color:#D8443C'>decreased</p>") %>%
                            str_replace_all("the favorite","<p style='color:#F4C40F'>the favorite</p>") %>%
                            str_replace_all("many good reasons\\.","many good reasons.</span>")

#### Plot Legend ####
Plot_legend <- image_traits %>% 
               ggplot() +
               ### Annotations ###
               geom_image(aes(x = x, y = y, image = img_url), size = 0.05, asp = 3.5, inherit.aes = FALSE) +
               geom_text(aes(x = x_possition, y = y_possition, label = label), color = image_traits$color, hjust = 0.0, size = 5.0, family = "Pattaya", nudge_x = 0.1, inherit.aes = FALSE) +
               ### Scales ###
               coord_cartesian(xlim = c(1,3), ylim = c(1,5), expand = FALSE, clip = "off") +
               ### Theme ###
               theme_void() +
               theme(
                 ## Plot Aesthetic ##
                 panel.background = element_rect(fill = "transparent", color = NA),
                 plot.background = element_rect(fill = "transparent", color = NA),
                 ## Titles ##
                 plot.title.position = "plot",
                 plot.title = element_markdown(color = title_color, family = "Pattaya", face = "plain", size = 18, hjust = 0.5, halign = 0.5, margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm")),
                 ## Margin ##
                 plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
               ### Labels ###
               labs(title = "Good scores of:")
  
  
#### Plot ####
Plot <- breed_rank_long %>% 
        filter(!Breed %in% breeds$Breed) %>% 
        ggplot() +
        geom_bump(aes(x = year, y = Rank, group = Breed, color = R_diff_dir), alpha = 0.1, size = 0.5) +
        geom_bump(data = breed_rank_long %>% filter(Breed == "Retrievers (Labrador)"), aes(x = year, y = Rank, color = R_diff_dir, group = Breed), alpha = 0.8, size = 1.5) +
        geom_bump(data = breed_rank_long %>% filter(Breed %in% breeds_dec), aes(x = year, y = Rank, color = R_diff_dir, group = Breed), alpha = 0.8, size = 1.5) +
        geom_bump(data = breed_rank_long %>% filter(Breed %in% breeds_inc), aes(x = year, y = Rank, color = R_diff_dir, group = Breed), alpha = 0.8, size = 1.5) +
        geom_point(data = breed_rank_long %>% filter(Breed %in% breeds$Breed), aes(x = year, y = Rank, group = Breed, color = R_diff_dir), alpha = 0.8, size = 2.0) +
        ### Annotations ###
        geom_image(data = breeds, aes(x = x, y = y, image = img), size = 0.08, asp = 1.8, inherit.aes = FALSE) +
        geom_text(data = breeds, aes(x = x_possition, y = y_possition, label = label, color = R_diff_dir), hjust = 0.5, size = 4.5, family = "Pattaya", nudge_x = 0.1, inherit.aes = FALSE) +
        ### Image Annotations ###
        geom_image(data = breed_traits_images, aes(x = x, y = y, image = img), size = 0.015, asp = 1.8, inherit.aes = FALSE) +
        ### Text Annotations ###
        geom_text(data = breeds_ranks, aes(x = x, y = y, label = label, color = R_diff_dir), hjust = 1, size = 3.8, family = "Anton", inherit.aes = FALSE) +
        geom_text(data = tibble(y = c(177,195), x = c(2012.8, 2020.5), label = c(177,195)), aes(x = x, y = y, label = label), color = text_color, hjust = 1, size = 3.8, family = "Anton", inherit.aes = FALSE) +
        ### Scales ###
        guides(color = "none") +
        scale_y_reverse() + 
        scale_x_continuous(limits = c(2009.8, 2038.0), breaks = seq(2013, 2020, 1)) +
        scale_color_manual(values = c("#F4C40F","#D8443C","#1F6E9C"), breaks = c("Constant","Decrease","Increases")) +
        coord_cartesian(expand = TRUE, clip = "off") +
        ### Theme ###
        theme_classic() +
        theme(
          ## Text ##
          text = element_text(face = "plain", family = "Anton", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
          ## Axis ##
          axis.title.x = element_text(size = 16, color = text_color),
          axis.title.y = element_text(size = 16, color = text_color, angle = 90),
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
          ## Titles & Caption ##
          plot.title.position = "panel",
          plot.title = element_markdown(color = title_color, family = "Pattaya", face = "plain", size = 34, hjust = 0.18, halign = 0.0),
          plot.subtitle = element_markdown(color = subtitle_color, family = "Pattaya", face = "plain", size = 16, hjust = 0.0, halign = 0.0, margin = margin(t = 0.0, r = 0.0, b = 0.2, l = 0.0, unit = "cm")),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = caption_color, family = "Menlo", hjust = 1, halign = 1, size = 12, margin = margin(t = 0.2, r = 0.1, b = 0.1, l = 0.0, unit = "cm")),
          ## Margin ##
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
        ### Labels ###
        labs(x = "",
             y = "Breed popularity ranking based on the AKC",
             fill = "",
             subtitle = annotation_subtitle_text,
             title = annotation_title_text,
             caption = "<span style='font-family: \"Font Awesome 6 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                        <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>  TamayoLeivaJ<br><br> 
                        Source: American Kennel Club (AKC)")

#### Final Plot ####
Plot_Final <- Plot + inset_element(Plot_legend, left = 0.6, bottom = 0.82, right = 1, top = 1, align_to = 'full')

#### Progress ####
ggsave("./2022/2022_Week_005/Plots/2022_Week_005.png", Plot_Final, dpi = 326, scale = 1, width = 16, height = 12, units = c("in"))
