library(tidyverse)

#1. Filter----
#funcion filter: seleccion de condiciones

starwars %>% 
  filter(hair_color == 'grey' | hair_color=='brown')

#"2. SLices seleccionar  observaciones de fila tal a tal
starwars %>% 
  slice(3:9)

starwars %>% 
  slice_head(n = 4)

starwars %>% 
  slice_min(order_by = mass, n = 3)

starwars %>% 
  slice_sample(prop = 0.2)

#3. arrange ----
starwars %>% 
  arrange(mass) %>% 
  slice(1:3)

#4. Select ----
starwars %>% 
  select(hair_color:sex)

#5. Mutate -----
starwars %>% 
  mutate(height_m = height/100, bmi = mass/height_m^2) %>% 
  select(name, height_m, bmi)

#6. group_ by %>%  summarise -----
starwars %>% 
  na.omit() %>% 
  group_by(gender, sex) %>% 
  summarise(mean_height = mean(height),
            median = median(height),
            var_height = var(height),
            std_height = sd(height),
            iqr_height = IQR(height),
            n = n())

starwars %>% 
  group_by(gender, sex) %>% 
  summarise(mean_height = mean(height, na.rm = FALSE ))

starwars %>% 
  na.omit() %>% 
  group_by(mass>68) %>% 
  summarise(median_height = median(height))
  
