rm(list = ls())
library(tidyverse)
library(gganimate)
library(tidyr)
library(lubridate)


data_file <- "all-states-history.csv"
df <- read.csv(data_file)

#positiveIncrease is the daily positive test increase
#positive for cumuluative sum (positiveCaseViral has blanks, is lower across board)

#parse date, convert date, map date to week of year
df$date_l <- mdy(as.character(df$date))
df$week <- week(df$date_l)

df <- df %>% separate(date, into =c("day","month","year"),sep="/")

subset <- df %>% 
  select(state, week, day, month, year, date_l, positiveIncrease, positive) %>%
  group_by(state, week) %>%
  mutate(cases = sum(positiveIncrease)) %>%
  filter(row_number() == n())

subset <- subset  %>%
  filter(week > 10 & state != "AS" & state != "GU" & state != "PR" & state != "VI" & state != "MP")

#now need cumulative sum by week
subset <- subset %>%
  group_by(state) %>%
  arrange(date_l) %>% 
  mutate(cumsum = cumsum(cases)) %>%
  ungroup()

subset_formatted <- subset %>%
  group_by(date_l) %>%
  mutate(rank = rank(-cumsum),
         relative_rank = cumsum/cumsum[rank==1],
         label = paste0(" ",round(cumsum/1e3))) %>%
  group_by(state) %>%
  filter(rank <= 10) %>%
  ungroup()


staticplot = ggplot(subset_formatted, aes(rank, group = state,
                                          fill = as.factor(state), color = as.factor(state))) +
  geom_tile(aes(y = cumsum/2,
                height = cumsum,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(state, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=cumsum,label = label, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=15, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(3,3,3,5, "cm"))


anim = staticplot + transition_states(date_l, transition_length = 4, state_length = 3) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Coronavirus Cases by Week : {closest_state}',  
       subtitle  =  "Top 10 States",
       caption  = "Cases in Thousands")

#good
animate(anim, duration = 20, fps = 15,  width = 1200, height = 1000, end_pause = 30,
        renderer = gifski_renderer("gganim4.gif"))
