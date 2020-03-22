# See käsk installib vajalikud paketid juhul kui need arvutis puuduvad
lapply(c("gapminder","tidyverse"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

# Paketid tuleb aktiveerida iga kord kui avad R-i. Need käsud avavad R-i praeguses keskkonnas.
library(gapminder)
library(tidyverse)


# ggplot2 on peamine viis teha ilusaid graafikuid R-is
# Lisainfot vaata siit https://ggplot2.tidyverse.org/


# Alustuseks näidis, proovime tüki haaval lahti võtta.
gapminder %>%
  group_by(country) %>%
  filter(year==max(year)) %>%
  ggplot(aes(y=lifeExp,x=gdpPercap,size=pop,color=continent))+
  geom_point()+
  scale_x_log10()
ggsave("plots/näidis.png")



# Teeme andmestiku
y2007 <- gapminder %>%
  group_by(country) %>%
  filter(year==max(year))


# Alustame kuvamisega
# Toruga saadame andmestiku kuvamisfunktsiooni
# ggplot() teeb tühja graafiku
y2007%>%
  ggplot()


# Graafiku sisu määratakse aes() funktsiooni sees.
# Siin määrame, et x teljel oleks oodatav eluiga.
y2007%>%
  ggplot(aes(x=lifeExp))


# Üks võimalus on lihtsalt loendada, kui palju riike teatud eluigadega on.
# Selleks on olemas histogram funktsioon.
# Märkus - ggplot-i sees käib ridade lisamine + märgiga, mitte %>%  toruga
y2007%>%
  ggplot(aes(x=lifeExp))+
  geom_histogram()



# Enamasti tahaks me aga teada midagi täpsemat, et näiteks millest see sõltub.
# Defineerides nii x-i kui y-i saame kahemõõtmelise graafiku.
y2007%>%
  ggplot(aes(x=lifeExp,y=gdpPercap))



# Kahemõõtmelisele graafikule saame lisada kihi geom_point(), mis kuvab punkte.
y2007%>%
  ggplot(aes(x=lifeExp,y=gdpPercap))+
  geom_point()


# Parameetreid on võimalik lisada rohkem. Näiteks värvime nad maailmajao kaupa.
y2007%>%
  ggplot(aes(x=lifeExp,y=gdpPercap,color=continent))+
  geom_point()

# Graafikule saab lisada kihte rea haaval
y2007%>%
  ggplot(aes(x=lifeExp,y=gdpPercap,color=continent))+
  geom_point()+
  geom_rug()

# Iga kiht muudab midagi
# facet_wrap on kasulik funktsioon, kui tahta mitut võimalust korraga vaadata
y2007%>%
  ggplot(aes(x=lifeExp,y=gdpPercap,color=continent))+
  geom_point()+
  geom_rug()+
  facet_wrap(~continent)


# Antud juhul tasub meil muuta ka y skaala logaritm-skaalaks (mis toob suuremad numbrid väiksematele lähemale) 
y2007%>%
  ggplot(aes(x=lifeExp,y=gdpPercap,color=continent))+
  geom_point()+
  geom_rug()+
  facet_wrap(~continent)+
  scale_y_log10()


# Võime muuta ka punkti suurust
y2007%>%
  ggplot(aes(x=lifeExp,y=gdpPercap,color=continent,size=pop))+
  geom_point()


# Enne vaatasime ühte aastat
# Võime aga vaadata näiteks üht riiki erinevatel hetkedel
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=lifeExp,y=gdpPercap))+
  geom_point()

# Sisu sõltub parameetritest, mida me sätime
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=pop))+
  geom_point()

# Punkti asemel võime teha ka joone
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=pop))+
  geom_line()

# Kuna kihte võib lisada lõputult, võime kasutada mõlemat korraga.
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=pop))+
  geom_line()+
  geom_point()


# Geom_bar funktsiooniga saab näidata tüüpide esinemishulka
y2007%>%
  ggplot(aes(x=continent))+
  geom_bar()



# geom_text funktsiooniga saab lisada teksti (label määrab kuvatava teksti sisu)
# Tekst paigutatakse x ja y asukohale, mis on enne defineeritud, vjust ja hjust nihutavad seda veidi
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=lifeExp,y=gdpPercap))+
  geom_point()+
  geom_text(aes(label = gdpPercap), vjust = -0.1, hjust = -0.1)


# geom_col kuvab väärtused punktide asemel tulpadena
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=gdpPercap))+
  geom_col()

# sisu sõltub defineeritud tunnustest
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=lifeExp))+
  geom_col()



#labs kaudu saab muuta x ja y telgede pealkirju, title kaudu saab lisada graafikule pealkirja
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=lifeExp,y=gdpPercap))+
  geom_point()+
  geom_text(aes(label = gdpPercap), vjust = -0.1, hjust = -0.1)+
  labs(title="SKT vs oodatav eluiga",x="Keskmine oodatav eluiga",y="Sisemajanduse kogutoodang inimesekohta")

# Eelmise trükitud pildi saab salvestada ggsave funktsiooniga
# piisab ainult salvestatava faili nimest.
ggsave("plots/soome.png")


# geom_point vajab määratletud x ja y telge
y2007 %>%
  ggplot()+
  geom_point()



# telgedele saab panna nii arve kui gruppe
# kas kuvatud pilt on kasulik, sõltub küsimusest
# Antud graafik näiteks näitab riikide asukohta tähestiku järjekorras
y2007 %>%
  ggplot(aes(x=continent,y=country))+
  geom_point()


# Või teistpidi
y2007 %>%
  ggplot()+
  geom_point(aes(x=country,y=continent))


