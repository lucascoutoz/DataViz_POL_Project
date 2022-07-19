library(ggrepel)
library(ggtext)
library(haven)
library(ragg)
library(showtext)
library(stringr)
library(tidyverse)

govt <- read_dta('Africa_govt_obs.dta') 
main <- read_dta('Africa.dta') 
sub <- read_dta('Africa_govt-party_obs.dta')

govt1 <- govt %>%
         filter(cangetdata == 1,
                govtcoalition == 1,
                regimetype == 2) 
sub1 <- sub %>%
        mutate(lccode = paste0(countryname, year, party_seats, portfolios, cabinetcode)) %>%
        select(lccode, partyname)

main1 <- main %>%
         filter(presidential == 1) %>%
         mutate(lccode = paste0(countryname, year, party_seats, portfolios, cabinetcode)) %>%
         left_join(sub1) %>%
         unique() 
         #mutate(name = paste0(partyname, "_", countryname, "_", year)) 


msubset <- main1 %>%
           filter(formateur == 1) %>%
           mutate(name = paste0(partyname, " ", countryname, " ", year)) %>%
           select(lccode, name)

main2 <- main1 %>% left_join(msubset) %>% mutate(name = str_replace_na(name, "")) %>% unique()

main2[21,19] <- "PNU Kenya 2008.1"
main2[24,19] <- "PNU Kenya 2008.2"

#fonts
font_add(family = "regular", "ShipmatesRegular-R7P6.ttf")
font_add(family = "bold", "ThisflagBold-MzVx.ttf")
font_add(family = "regulartext", "KdamThmorPro-Regular.ttf")
font_add(family = "barlow", "Barlow Semi Condensed-Regular.ttf")
showtext_auto() 



presafrica <- main2 %>% mutate(formateur = cut(formateur, breaks = 2)) %>%
          ggplot(aes(seatshare, portfolioshare)) +
          geom_point(aes(colour = formateur, fill = formateur), size = 4) +
          geom_text_repel(aes(label = name), size = 8, family = "barlow") +
          geom_abline(intercept = 0, slope = 1) +
          scale_color_manual(values = c("#47B5FF", "#1363DF")) +
          scale_fill_manual(values = c("#47B5FF", "#1363DF")) +
          coord_cartesian(expand=FALSE) +
          scale_x_continuous(limits = c(0, 1.1),
                             breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
          scale_y_continuous(limits = c(0, 1),
                             breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
          xlab("Coalition Seat Share") + ylab("Portfolio Share") +
  labs(title = "<span style='color:#1363DF'>Formateur advantage</span> in Portfolio Allocation in African Presidential Regimes",
       subtitle = "Presidents tend to assign more portfolios to their parties than to <span style='color:#47B5FF'>junior partners</span>",
       caption= "@lucas_coutoz with data from ariotti and golder (2018)") +
  theme(
    #Title, Subtitle, Caption
    plot.title = element_markdown(family = "regular", hjust = 0.5, vjust = 0.5, size = 30, color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family="regular", size = 25, hjust = 0.5, color = "black"),
    plot.caption = element_text(family="bold", size = 35, color = "black", hjust = 1),
    plot.caption.position = "plot",
    #Panel and Background
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DFF6FF"),
    plot.background = element_rect(fill = "#DFF6FF", linetype = 'blank'),
    #Axes
    axis.title = element_text(size = 40, family = "regulartext", color = "black"),
    axis.text.y = element_text(size = 25, family = "regulartext", color = "black"),
    axis.text.x = element_text(size = 25, family = "regulartext", color = "black"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    #Plus
    text = element_text(family = "regulartext", size = 45),
    legend.position = "none")

https://colorhunt.co/palette/06283d1363df47b5ffdff6ff

ggsave("presafrica.png",
       plot=presafrica,
       device = agg_png(width = 8, height = 6, units = "in", res = 300))
