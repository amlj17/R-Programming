#Tidyr
library(tidyr)
relig_income

relig_income %>% 
  pivot_longer(-religion,
               names_to ='income',
               values_to = 'frequency')

billboard

billboard %>% 
  pivot_longer(-c(artist, track, date.entered),
               names_to = 'week',
               values_to = 'rank',
               values_drop_na =TRUE,
               names_prefix = 'wk',
               names_transform= list(week = as.integer))


who

names(who)<- gsub('new_?', '', names(who))

who %>% 
  pivot_longer(-c(1:4),
               names_to = 'codigo',
               values_to = 'casos') %>% 

  separate(codigo,into = c('diagnostico', 'sex_age'), sep= '_') %>% 
  separate(sex_age,into = c('sexo', 'edad'), sep =1)
  
