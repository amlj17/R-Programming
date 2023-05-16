#1
library(tidyverse)
library(nycflights13)
#a) mayo octubre o diciembre
flights %>% 
  filter(month %in% c(5, 10, 12))
# hay 85,820

#b) 2 o mas horas de retraso
flights %>%
  filter(dep_delay >= 120)

#c) volaron a Houston
flights %>% 
  filter(dest == 'HOU' | dest == 'IAH')

#d)  llegaron dos o mas horas tarde pero salieron a tiempo
flights %>% 
  filter(arr_delay >=120, dep_delay == 0)

#e) retrasos de 1 hora + pero ganaron 30 mins en el vuelo
flights %>% 
  filter(dep_delay >=60, arr_delay <= 30)

#2)  filter between media noche y 6 am
flights %>% 
  filter(between(sched_dep_time, 0, 600))
