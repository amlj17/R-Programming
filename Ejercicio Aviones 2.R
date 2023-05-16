library(tidyverse)
library(nycflights13)
#1
#a)valores ausente de col deptime aparezcan primero
flights %>%
  arrange(!is.na(dep_delay))
#is.na nos dice si es na o no

#b) mayores retrasos primero
flights %>% 
  arrange(desc(arr_delay)) %>% 
  relocate(arr_delay, .before = dep_time)

#c)mayor retraso y que salieron más temprano
flights %>% 
  relocate(hour, .before = sched_dep_time) %>% 
  arrange(desc(dep_delay), hour) %>% 
  head(10)

#d) vuelos mas veloces
flights %>% 
  mutate(velocidad = distance/air_time) %>% 
  relocate(velocidad, .before = dep_time) %>% 
  arrange(desc(velocidad))

#2
#a) vuelos de mayor distancia
flights %>% 
  relocate(distance, .before = dep_time) %>% 
  arrange(desc(distance)) %>% 
  head(10)

summarise(flights, max(distance))

#a) vuelos de menor distancia
flights %>% 
  relocate(distance, .before = dep_time) %>% 
  arrange((distance))
  head(10)

summarise(flights, min(distance))
