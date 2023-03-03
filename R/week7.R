# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)



# Data Import and Cleaning
week7_tbl <-read_csv("../data/week3.csv",col_types = cols(timeStart=col_datetime(format="%Y-%m-%d %H:%M:%S"), timeEnd=col_datetime(format = "%Y-%m-%d %H:%M:%S"),condition="f",gender="f",q1="i",q2="i",q3="i",q4="i",q5="i",q6="i",q7="i",q8="i",q9="i",q10="i")) %>%
  rename(Condition=condition, Gender=gender) %>%
  mutate(Condition= factor(Condition, levels = c("A","B","C"),labels=c("Block A","Block B","Control")))%>%
  mutate(Gender= factor(Gender, levels = c("F","M"),labels=c("Female","Male"))) %>%
  filter(q6==1) %>%
  select(-q6) %>%
  mutate(timeSpent=timeEnd-timeStart) %>%
 



 
# Visualization
  week7_tbl %>% ggplot(aes(timeStart,q1)) +
  geom_point() +
  labs(x="Date of Experiment", y="Q1 Score") 
  
                     