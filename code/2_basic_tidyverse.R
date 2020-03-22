# Koostatud TÜ kursuse jaoks Suurte tekstiandmete töötlemine ja analüüs humanitaarteadustes, Peeter Tinits, 2019
#
# Allikad:
# https://github.com/rstudio/webinars/blob/master/46-tidyverse-visualisation-and-manipulation-basics/ (RStudio webinar by garretgman, CC)
# http://rpubs.com/aelhabr/tidyverse-basics
#
# Selle tüki eesmärk on tutvustuda R tidyverse lihtsamaid käske ja tööloogikat ühe andmestiku lihtsal muutmisel
#
# Tükk kasutab Gapminder andmestikku
#
# Lihtmuutujate analüüsiks vaata siia: 
# https://www.gapminder.org/answers/how-does-income-relate-to-life-expectancy/
# Andmestikus on veel muutujaid, mida tükis ei käsitleta.



### ARVUTIKLASSIS KASUTAMISEKS (kui pole admin õigusi) ###
# 1) Mine kohta: "C:/Users/Public/Documents/" (kopeeri see asukohareale) & tee uus kataloog nimega "Rstudio_packages" (kui seda pole)
# 2) Jooksuta järgmist käsku.
.libPaths("C:/Users/Public/Documents/Rstudio_packages")
#    See määrab, et uued paketid installitakse sinna.


# See käsk installib vajalikud paketid juhul kui need arvutis puuduvad
lapply(c("gapminder","tidyverse"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

# Paketid tuleb aktiveerida iga kord kui avad R-i. Need käsud avavad R-i praeguses keskkonnas.
library(gapminder)
library(tidyverse)


#gapminder <- read.csv("gapminder.csv")

# Graafik, mis sarnaneb Hans Roslingu esitatule. See võib tunduda esialgu keeruline, aga saab tüki lõpuks mõistetavamaks.
gapminder %>%
  group_by(country) %>%
  filter(year==max(year)) %>%
  ggplot(aes(y=lifeExp,x=gdpPercap,size=pop,color=continent))+
  geom_point()+
  scale_x_log10()


# Lihtsad tidyverse käsud
#
# Töömudel on järgmine
# andmed %>%
#   protsess1() %>%
#   protsess2()
#
#  %>% - vii andmed järgmisesse protsessi
# select() vali muutujad
# filter() vii andmestik läbi filtri/sõela
# arrange() järjesta andmed
# group_by() grupeeri andmestik
# summarise() võtad andmed kokku mõne funktsiooniga
# join() ühenda kaks andmestikku
# mutate() loo uus muutuja


#Ja võime vaadata seda vajutades sellele ülal-paremal või kasutades view() funktsiooni
View(gapminderr)

#Viime andmed läbi filtri, kus riigiks peab olema Soome
finland <- gapminder %>%
  filter(country=="Finland")

#Võtame ainult Soome aasta ja rahvaarvu
gapminder %>%
  filter(country=="Finland") %>%
  select(year,pop)
  

#Rahvaarvud suurimast väiksemani aastal 1952
gapminder %>% 
  filter(year == 1952) %>% 
  arrange(desc(pop))

#Madalaim oodatav eluiga aastal 2007
gapminder %>% 
  filter(year == 2007) %>% 
  arrange(lifeExp) %>% 
  select(country, lifeExp)

#Tee uus muutuja - gdp
gapminder %>% 
  mutate(gdp = pop * gdpPercap)


# Visualiseerimiseks saab kasutada ggplot-i
#
#
# Selle põhikuju tidyverse-is
#
# andmed %>%
# ggplot(mapping = aes(<MAPPINGS>)) +
#    <GEOM_FUNCTION>()
#
# Vaata ?ggplot2 üksikasjalikumaks infoks. Google ggplot2 aitab ka.


# Graafik, mis sarnaneb esimesele, aga nüüd ühe riigi kohta läbi aastate
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=lifeExp,y=gdpPercap))+
  geom_point()+
  theme_minimal()

gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=pop))+
  geom_line()+
  theme_minimal()




gapminder

# summarise()
# Andmestiku suurim gdp
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  summarise(max_gdp = max(gdp))

# group_by() + summarise()
# Suurim gdp maailmajao kohta
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(continent) %>% 
  summarise(max_gdp = max(gdp))

# Suurim gdp riigi kohta antud perioodil
gapminder %>% 
  group_by(country) %>% 
  summarise(max_gdpPercap = max(gdpPercap)) %>% 
  arrange(desc(max_gdpPercap))

# Maailmajagude keskmine gdp inimese kohta
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap)) %>% 
  arrange(desc(mean_gdpPercap))

# Kontinentide võrdlus ajas, gdp
gapminder %>% 
  group_by(continent,year) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap)) %>% 
  arrange(desc(continent,year)) %>% 
  ggplot(aes(x=year,y=mean_gdpPercap,color=continent))+
  geom_line()+
  theme_minimal()




# Trendid ajas
# Esimene ja viimane
# first()
# last()
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  summarise(first_gdp = first(gdp), last_gdp = last(gdp))
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(first_gdp = first(gdp), last_gdp = last(gdp))


# Esimese ja viimase võrdlus

gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(gdp1952 = first(gdp), gdp2007 = last(gdp))

# Kasv protsentides aastas
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(gdp1952 = first(gdp), gdp2007 = last(gdp)) %>% 
  mutate(cagr = ((gdp2007 / gdp1952) ^ (1/55) - 1) * 100) %>%  # kokku 55 aastat
  arrange(desc(cagr)) %>% 
  select(country, cagr)


# gdp riigi kohta 1952
gapminder
gapminder %>% 
  filter(year == 1952) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  arrange(desc(gdp)) %>% 
  select(country, gdp)



#Top 10 riiki gdp järgi 1952 nimekirjana
#seda salvestatud muutujat kasutame hiljem
top_10 <-
  gapminder %>% 
  filter(year == 1952) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  arrange(desc(gdp)) %>% 
  top_n(10, gdp) %>% 
  pull(country) #võtab tulba andmestikust nimekirjaks


#Top 10 riikide muutused ajas 
# %in% - riigid mis on nimekirjas
gapminder
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = gdp, color = country))



#Top 10 gdp-d
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap)

#Gdpd võrreldes 1952 aastaga, nt mitu korda suurem on Hiina gdp aastal 2007
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  mutate(scaled_gdp = gdp / first(gdp)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = scaled_gdp, color = country)) +
  labs(title = "GDP Per Capita (Scaled)")



#Top 10 riikide kasvukiirus
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(start = first(gdp), end = last(gdp)) %>% 
  mutate(cagr = ((end/start) ^ (1 / 55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr)


# Top 10 riikide kasvukiirus võrdluses
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country,continent) %>% 
  summarise(start = first(gdp), end = last(gdp)) %>% 
  mutate(cagr = ((end/start) ^ (1 / 55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr,continent) %>% 
  ggplot() +
  geom_col(mapping = aes(x = country, y = cagr))

# Top 10 riikide kasvukiirus võrdluses, maailmajao kaupa
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country,continent) %>% 
  summarise(start = first(gdp), end = last(gdp)) %>% 
  mutate(cagr = ((end/start) ^ (1 / 55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr,continent) %>% 
  ggplot() +
  geom_col(mapping = aes(x = country, y = cagr))+
  facet_wrap(~continent,scales="free_x")
