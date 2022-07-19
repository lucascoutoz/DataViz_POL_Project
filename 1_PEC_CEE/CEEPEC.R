library(camcorder)
library(ggtext)
library(ragg)
library(readxl)
library(showtext)
library(tidyverse)

ceepec <- read_excel("S0007123414000544sup001.xlsx", 2)
codebook <- read_excel("S0007123414000544sup001.xlsx")

##Overview: How many pre-electoral coalitions(PECs) can be found there? 
ceepec %>% filter(PEC == 1) %>%
           select(PEC, coalition_name, el_index) %>%
           distinct()

##PECs per country and year 
ceepec1 <- ceepec %>%
           filter(PEC == 1) %>%
           select(country, coalition_name, el_year) %>%
           distinct() %>%
           group_by(country, el_year) %>%
           count() %>%
           ungroup() 
           

##Note that not all elections had PECs. We should take them into account
list <- ceepec %>% select(country, el_year) %>% unique()


happybthday = 
  tribble(
    ~country, ~el_year, ~n,
    "Slovenia", 1996, 0,
    "Slovenia", 2000, 0,
    "Slovenia", 2004, 0,
    "Slovenia", 2011, 0,
    "Slovakia", 2012, 0,
    "Poland", 2011, 0,
    "Lithuania", 2008, 0,
    "Lithuania", 2012, 0,
    "Hungary", 1994, 0,
    "Hungary", 1998, 0,
    "Estonia", 2003, 0,
    "Estonia", 2007, 0,
    "Czech Republic", 1996, 0,
    "Czech Republic", 1998, 0,
    "Czech Republic", 2006, 0,
    "Croatia", 1992, 0)

ceepec2 <- ceepec1 %>% 
           rbind(happybthday) %>%
           group_by(el_year) %>%
           mutate(meani = mean(n))
  
ceepec3 <- ceepec2 %>%
           select(el_year, meani) %>%
           unique() %>%
           mutate(country = case_when(meani > -0.1 ~ "Average")) %>%
           arrange(el_year) %>%
           rename(n = meani)


ceepec4 <- ceepec2 %>% rbind(ceepec3) %>% select(-meani)

#fonts
font_add(family = "regular", "Barlow Semi Condensed-Regular.ttf")
font_add(family = "semibold", "BarlowSemiCondensed-SemiBold.ttf")
showtext_auto() 

Plot <- ceepec4 %>% ggplot(aes(el_year, n, group = country)) +
            geom_line(data = ceepec4 %>% 
                      filter(!country %in% c("Average", "Croatia")), 
                      colour = "grey75", alpha = .50, size = .25) +
            geom_line(data = ceepec4 %>% 
                      filter(country == "Croatia"), 
                      colour = "#131394", size = .50) +
            geom_line(data = ceepec4 %>% 
                      filter(country == "Average"), 
                      colour = "white", size = 1.25) +
            geom_line(data = ceepec4 %>% 
                      filter(country == "Average"),
                      colour = "#e71b1b", size = 1) +
            scale_x_continuous(limits = c(1990, 2015),
                               breaks = seq(1991, 2012, by = 3)) +
            scale_y_continuous(limits = c(0, 6.5),
                               breaks = seq(0, 7, by = 1)) +
            geom_text(data = ceepec4 %>%
                        filter(country == "Average", el_year == 2012),
                        aes(label = paste(country, "\n", round(n, 1))),
                      x = 2012.25, colour = "#e71b1b", family = "semibold",
                      hjust = 0, vjust = .75, size = 10, lineheight = .3) +
            geom_text(data = ceepec4 %>%
                        filter(country == "Croatia", el_year == 2011),
                      aes(label = paste(country, "\n", round(n, 1))),
                      x = 2011.25, colour = "#131394", family = "semibold",
                      hjust = 0, vjust = .75, size = 10, lineheight = .3) +
            coord_cartesian(expand=FALSE) +
            xlab("") + ylab("Number of PECs") + 
  labs(title = "Number of Pre-Electoral Coalitions (PECs) in Central and Eastern Europe",
       subtitle = "The number of PECs in the region decreased from 1991 to 2012",
       caption= "Lucas Couto | Data: Ibenskas (2016) | DataViz Inspiration: @pablo_alvrez") +
  theme(
    #Title, Subtitle, Caption
    plot.title = element_text(family = "semibold", hjust = 0.5, vjust = 0.5, size = 60, color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="semibold", size = 45, hjust = 0.5, color = "black"),
    plot.caption = element_text(family="semibold", size = 35, color = "black", hjust = 1),
    plot.caption.position = "plot",
    #Panel and Background
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#EEEEEE"),
    plot.background = element_rect(fill = "#EEEEEE", linetype = 'blank'),
    #Axes
    axis.title = element_text(size = 60, family = "semibold", color = "black"),
    axis.text.y = element_text(size = 40, family = "regular", color = "black"),
    axis.text.x = element_text(size = 40, family = "regular", color = "black"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    #Plus
    text = element_text(family = "regular", size = 45))

ggsave("CEEPEC.png",
       plot = Plot,
       device = agg_png(width = 10, height = 8, units = "in", res = 300))