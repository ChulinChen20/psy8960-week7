# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(GGally)



# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
  mutate(timeStart = ymd_hms(timeStart),
         condition = factor(condition, levels=c("A","B","C"), labels=c("Block A","Block B","Control")),
         gender = factor(gender, levels=c("M","F"), labels=c("Male","Female")),
         timeSpent = timeEnd - timeStart) %>%
  dplyr::filter(q6 == 1) %>%
  select(-q6)





# Visualization
week7_tbl %>%
  select(q1:q10) %>%
  ggpairs
(ggplot(week7_tbl,aes(timeStart,q1)) +
geom_point() +
labs(x="Date of Experiment", y="Q1 Score")) %>% 
ggsave(filename="../figs/fig1.png", units="px", width=1920, height=1080)
  

(ggplot(week7_tbl,aes(q1,q2,color=gender)) +
geom_jitter() +
labs(color="Participant Gender") + 
coord_fixed(ratio=0.75)) %>% 
ggsave(filename="../figs/fig2.png", units="px", width=1920, height=1080)
(ggplot(week7_tbl,aes(q1,q2)) +
geom_jitter() +
facet_grid(cols=vars(gender))+
labs(x="Score on Q1", y="Score on Q2")+ 
coord_fixed()) %>% 
ggsave(filename="../figs/fig3.png", units="px", width=1920, height=1080)

(ggplot(week7_tbl,aes(gender,timeSpent)) +
geom_boxplot() +
ylab("Time Elapsed (mins)")) %>%
ggsave(filename="../figs/fig4.png", units="px", width=1920, height=1080)

(ggplot(week7_tbl,aes(q5,q7,color=condition)) +
geom_point(position=position_jitter(width=0.1)) +
geom_smooth(method = "lm", se = FALSE) +
coord_fixed(ratio=0.4) +
labs(x="Score on Q5", y="Score on Q7",color="Experimental Condition") +
theme(legend.position="bottom",
      legend.background=element_rect("#E0E0E0"))) %>%
ggsave(filename="../figs/fig5.png", units="px", width=1920, height=1080)

