# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)
library(GGally)


# Data Import and Cleaning
week7_tbl <-read_csv("../data/week3.csv",col_types = cols(timeStart=col_datetime(format="%Y-%m-%d %H:%M:%S"), timeEnd=col_datetime(format = "%Y-%m-%d %H:%M:%S"),condition="f",gender="f",q1="i",q2="i",q3="i",q4="i",q5="i",q6="i",q7="i",q8="i",q9="i",q10="i")) %>%
  rename(Condition=condition, Gender=gender) %>%
  mutate(Condition= factor(Condition, levels = c("A","B","C"),labels=c("Block A","Block B","Control")))%>%
  mutate(Gender= factor(Gender, levels = c("M","F"),labels=c("Male","Female"))) %>%
  filter(q6==1) %>%
  select(-q6) %>%
  mutate(timeSpent=timeEnd-timeStart) 




 
# Visualization
week7_tbl %>%
  ggpairs(columns=c(paste0("q",c(1:5,7:10)))) 

week7_tbl %>% 
  ggplot(aes(timeStart,q1)) +
  geom_point() +
  labs(x="Date of Experiment", y="Q1 Score")
  

week7_tbl %>% 
  ggplot(aes(q1,q2,color=Gender)) +
  geom_jitter() +
  labs(color="Participant Gender") + 
  coord_fixed(ratio=0.75) 
week7_tbl %>% 
  ggplot(aes(q1,q2)) +
  geom_jitter() +
  facet_grid(cols=vars(Gender))+
  labs(x="Score on Q1", y="Score on Q2")+ 
  coord_fixed()  

week7_tbl %>% 
  ggplot(aes(Gender,timeSpent)) +
  geom_boxplot() +
  ylab("Time Elapsed (mins)")


week7_tbl %>%
  ggplot(aes(q5,q7,color=Condition)) +
  geom_point(position=position_jitter(width=0.1)) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed(ratio=0.4) +
  labs(x="Score on Q5", y="Score on Q7",color="Experimental Condition") +
  theme(legend.position="bottom",
        legend.background=element_rect("#E0E0E0"))