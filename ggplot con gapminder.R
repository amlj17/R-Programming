library(dslabs)
gapminder %>% 
  as_tibble()

gapminder %>% 
  filter(year == 1962) %>% 
  ggplot(aes(x= fertility, y=life_expectancy,
             color = continent))+
  geom_point(alpha = 0.7) +
  ggtitle("Fertility vs Life Expectancy Worldwide 1962")


gapminder %>% 
  filter(year == 2012) %>% 
  ggplot(aes(x= fertility, y=life_expectancy,
             color = continent))+
  geom_point(alpha = 0.7) +
  ggtitle("Fertility vs Life Expectancy Worldwide 2012")


#pongamos dos graficas juntas por año distinto
#hagamoslo presentable
gapminder %>% 
  filter(year %in% c(1962, 1980, 1990, 2000, 2005, 2012)) %>% 
  ggplot(aes(x= fertility, y=life_expectancy,
             color = continent))+
  geom_point(alpha = 0.7) +
  facet_wrap(~year, ncol = 2, scales = "free") +
  ylab('Esperanza de vida') +
  xlab('Tasa de fertilidad') +
  ggtitle('Primer mundo y paises en desarrollo') +
  theme_minimal()

#salvar el ultimo plot  
ggsave('FirstWorld.pdf')


 # theme_<algo> () cambia el estilo de la grafica

mis_paises <- c('Kenya', 'Spain')

gapminder %>%
  filter(country %in% mis_paises) %>% 
  ggplot(aes(x = year, y = life_expectancy, color = country)) +
  geom_line()


#freedman diaconis
# --- 2*IQR over cube root of sample size
fd <- function(x){
  n <- length(x) #elementos de un vector (sample size)
  2*IQR(x)/(n)^(1/3)
}

gapminder %>% 
  filter(year == 2010) %>%
  ggplot(aes(x =gdp/10^6)) +
  geom_histogram(binwidth = fd,
                 color ='white',
                 fill = 'steelblue')+
  scale_x_continuous(trans = 'log2') +
  xlab('Gdp in millions USD (log2)') +
  ylab('')+
  theme_minimal()
