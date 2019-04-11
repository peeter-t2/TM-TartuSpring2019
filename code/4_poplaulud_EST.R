# Koostatud TÜ kursuse jaoks Suurte tekstiandmete töötlemine ja analüüs humanitaarteadustes, Peeter Tinits, 2019

# Teadaolevad probleemid andmetega
# Mõnedel lugudel on refraan/chorus täissõnadega välja kirjutatud, mõnel mitte.
# Mitmel lool on laulusõnad puudu.
# Tekstid on lemmatiseerimata.


# See käsk installib vajalikud paketid juhul kui need arvutis puuduvad
lapply(c("tidytext","tidyverse","gridExtra","scales"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

# Paketid tuleb aktiveerida iga kord kui avad R-i. Need käsud avavad R-i praeguses keskkonnas.
library(tidyverse)
library(tidytext)
library(gridExtra)
library(scales)



# Loeme andmestiku
edetabel <- read_tsv("data/eesti_top40/eesti_skyplus_top40_1994-2018.tsv")




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

# Käsud tekstitöötluseks
# count(muutuja) - loeb esinemiste arvu
# top_n(number, muutuja) - võta esimesed 10 muutuja kohta
# left_join(andmestik) - ühenda üks andmestik teisega
# unnest_tokens(ühik, muutuja) - segmenteeri tekst sõnedeks, 2-grammideks, 3-grammideks
# str_detect(muutuja, "sõne") - osaline ühestamine


#Võtame 10 juhuslikku lugu viimasest aastast
#Mitu lugu ära tunned
now <- edetabel %>%
  filter(year==max(year)) %>%
  sample_n(10)


#Võtame 10 juhuslikku lugu aastast 1997
year1997 <- edetabel %>%
  filter(year==1997) %>%
  sample_n(10)




#filter smilers
#count songs,
#sometimes they repeat

#or count years,
#can eventually plot them...



#Esinemiskorrad top 40s
edetabel %>%
  count(artist)

#Esinemisekorrad sageduse järgi top 40s
edetabel %>%
  count(artist) %>%
  arrange(desc(n))



# Top 10 esinemiste arvu järgi top 40s
edetabel %>%
  count(artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


# Top 20 esinemiste arvu järgi top 40s
edetabel %>%
  count(artist) %>%
  arrange(desc(n)) %>%
  head(20)

# Mitmel artistil on kui palju laule
edetabel %>%
  count(artist) %>%
  count(n) %>%
  arrange(desc(n))


# Kõige üksluisem aasta muusikas
edetabel %>%
  group_by(year) %>%
  count(artist) %>%
  arrange(desc(n))



# Salvestame top 10 muutujana
top10 <- edetabel %>%
  count(artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


# Lisa esinemiste arv andmestikule
edetabel %>%
  count(artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(edetabel)

#Kõik laulud mõnelt artistilt
smilers <-  edetabel %>%
  filter(artist=="Smilers")

#Smilersi lood aasta ja koha järgi
smilers %>%
  ggplot(aes(x=year,y=-rank,color=song))+
  geom_point()+
  geom_line()

#Terminaatori lood aasta ja koha järgi (sama kui smilers-iga, aga me ei salvesta muutujat)
edetabel %>%
  filter(artist=="Terminaator") %>%
  ggplot(aes(x=year,y=-rank,color=song))+
  geom_point()+
  geom_line()


# Mitme lauluga oli parim artist top 40s
edetabel %>%
  group_by(year) %>%
  count(artist) %>%
  top_n(1) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=year,y=n))+
  geom_point()+
  geom_line()


# Mis artist see oli
edetabel %>%
  group_by(year) %>%
  count(artist) %>%
  top_n(1) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=year,y=n, color=artist))+
  geom_point()+
  geom_text(aes(x=jitter(year),y=jitter(n,3),label=artist))+
  geom_line()




# Parim koht igale smailersi laulule
edetabel %>%
  filter(artist=="Smilers") %>%
  group_by(song) %>%
  summarise(minrank=min(rank)) %>%
  arrange(minrank)


# Lisame ka aastaarvu - esimene kord kui laul esines
edetabel %>%
  filter(artist=="Smilers") %>%
  group_by(song) %>%
  summarise(minyear=min(year),minrank=min(rank)) %>%
  arrange(minrank)


# Kõik smilers-i laulud
edetabel %>%
  filter(artist=="Smilers") %>%
  ggplot(aes(x=year,y=-rank,group=song,color=song))+
  geom_line()+
  geom_point()+
  theme_minimal()


# Bändi eluiga esimesest aastast viimaseni edetabelis top 10-le artistile
# Vanilla Ninja ja Caater olid paljude lauludega aga lühikest aega, Terminaator ja Smilers tunduvad igavesed
edetabel %>%
  count(artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(edetabel) %>%
  group_by(artist) %>%
  summarise(min_year=min(year),max_year=max(year)) %>%
  gather(type,year,c(min_year,max_year)) %>% # a new function that changes the dataframe a bit
  ggplot(aes(y=artist))+
  geom_line(aes(x=year),size=3)+
  theme_minimal()


####################################################################
## Laulusõnade analüüs
####################################################################

# unnest_tokens(ühik, muutuja) - segmenteeri tekst sõnedeks, 2-grammideks, 3-grammideks
# str_detect(muutuja, "sõne") - osaline ühestamine

### Lood ei olnud ainult eesti keeles, vaatame, mis keeles neid oli
edetabel %>% 
  count(language)

### Vaatame aasta lõikes
edetabel %>% 
  count(language,year)

### Teeme visuaalse ülevaate
edetabel %>% 
  count(language,year) %>% 
  ggplot(aes(x=year,y=n,fill=language))+
  geom_bar(stat="identity")


# Sõnad laulutekstides
# Teeme sõna-rea-kohta andmestiku
edetabel_tokens <- edetabel %>%
  unnest_tokens(word, lyrics)  %>% #teeb sõnadest eraldi read
  filter(word!="")

## kokku 157454 rida


## Sagedasemad sõnad
edetabel_tokens %>%
  count(word) %>%
  arrange(desc(n))

#K. Uiboaed stopsõnad
stopwords <- read_lines("data/uiboaed_stopwords/estonian-stopwords.txt")


# Sagedasemad eestikeelsed mitte-stopsõnad
edetabel_tokens %>%
  filter(language=="et") %>%
  count(word) %>%
  filter(!word %in% c(stopwords)) %>%
  arrange(desc(n))


# 2-grammide tabel
edetabel_bigrams <- edetabel %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2, n_min = 2) %>%
  filter(bigram!="")

# 2-grammide sagedused
bigrams_totalfreqs <- edetabel_bigrams %>%
  count(bigram) %>%
  arrange(desc(n))

# sagedused laulude sees
edetabel_bigrams %>%
  count(bigram,song) %>%
  arrange(desc(n))

# ühe fraasi esinemine laulude sees
edetabel_bigrams %>%
  count(bigram,song) %>%
  filter(bigram=="ei saa") %>%
  arrange(desc(n))


## mitmes laulus artist kasutas
edetabel_bigrams %>%
  count(bigram,song,artist) %>%
  filter(bigram=="ei saa") %>%
  count(artist) %>%
  arrange(desc(n))

# Mitmes laulus on
bigrams_by_song <- edetabel_bigrams %>%
  count(bigram,song,artist) %>%
  count(bigram) %>%
  arrange(desc(n))

# str_detect otsib osasõnet, mitte tervikut
edetabel_bigrams %>%
  count(bigram) %>%
  filter(str_detect(bigram,"saa")) %>%
  arrange(desc(n))

# võib otsida ka tühikuga
edetabel_bigrams %>%
  count(bigram,song) %>%
  filter(str_detect(bigram,"ei saa")) %>%
  arrange(desc(n))

# võib kasutada ka regulaaravaldisi
edetabel_bigrams %>%
  count(bigram,song) %>%
  filter(str_detect(bigram,"^ei ")) %>%
  arrange(desc(n))



# 3-grammidega
edetabel_trigrams <- edetabel %>%
  unnest_tokens(trigram, lyrics, token = "ngrams", n = 3, n_min = 3) %>% 
  filter(trigram!="")

# edetabel
edetabel_trigrams %>%
  count(trigram) %>%
  arrange(desc(n))


#mitmes laulus on
edetabel_trigrams %>%
  count(trigram,song,artist) %>%
  count(trigram) %>%
  arrange(desc(n))




# Üksikud sõnad 
# proportsioonina kogukorpusest

edetabel_tokens %>%
  count(word) %>%
  arrange(desc(n))


##### Loenda sõnu
edetabel_tokens %>%
  filter(!word %in% c(stopwords,"ref")) %>%
  filter(language=="et") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(20)

## Salvestame sõna mida otsida
word_to_search <- "täna"
word_to_search <- "mees"

# Salvestame graafiku pealkirja
my_title <- paste("'",word_to_search, "' ajas", sep="")
# Aasta kohta kogusõnede hulk
total_words <- count(edetabel_tokens, year)


## Loome andmestiku ja teeme graafiku
edetabel %>%
  unnest_tokens(word, lyrics) %>%
  count(word, year) %>%
  filter(word==word_to_search) %>%
  complete(year=1994:2018, fill=list(n=0)) %>%
  rename(count=n) %>%
  left_join(total_words) %>%
  ggplot(aes(x=year, y=count/n)) +
  geom_point(shape=19, alpha=.5, colour="red" ) +
  geom_smooth(method=loess, colour="red", fill="grey") +
  labs(y="Proportion", title=my_title)+
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) # +


# Sama aga salvestatud vaheandmestikust (kiirem)
edetabel_tokens %>%
  count(word, year) %>%
  filter(word==word_to_search) %>%
  complete(year=1994:2018, fill=list(n=0)) %>%
  rename(count=n) %>%
  left_join(count(edetabel_tokens, year)) %>%
  ggplot(aes(x=year, y=count/n)) +
  geom_point(shape=19, alpha=.5, colour="red" ) +
  geom_smooth(method=loess, colour="red", fill="grey") +
  labs(y="Proportion", title=my_title)+
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) # +






################################################
###  Sõnavara üldomadused
################################################



#Kas laulud muutuvad lühemaks või pikemaks
token_count <- edetabel_tokens %>%
  group_by(song,artist,year) %>%
  summarise(tokens=n())

# Keskmine aasta kohta
token_count %>%
  group_by(year) %>%
  summarise(tokens=mean(tokens)) %>% 
  ggplot(aes(x=year,y=tokens))+
  geom_point()+
  geom_smooth()

# Iga laul eraldi
token_count %>%
  ggplot(aes(x=year,y=tokens))+
  geom_point(alpha=0.2)+
  geom_smooth()


# Sõnavara ja sõnad
type_token_count <- edetabel_tokens %>%
  group_by(song,artist,year) %>%
  summarise(types=n_distinct(word),tokens=n())

# Kas sõnavara kasvab
type_token_count  %>%
  ggplot(aes(x=year,y=types))+
  geom_point(alpha=0.2)+
  geom_smooth()

# Kas sõnad muutuvad üksluisemaks
# vt "type-token ratio" lisainformatsiooniks
type_token_count  %>%
  ggplot(aes(x=year,y=types/tokens))+
  geom_point(alpha=0.2)+
  geom_smooth()




############################################################
###### Võrdle tekste
############################################################


# Esimene võrdlusgrupp
comparison1 <- edetabel %>%
  filter(artist=="Smilers") %>%
  unnest_tokens(word, lyrics) %>%
  count(word)
  
# Teine võrdlusgrupp
comparison2 <- edetabel %>%
  filter(artist=="Terminaator") %>%
  unnest_tokens(word, lyrics) %>%
  count(word)

# Võrdlused koos
comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

# Teeb graafiku võrdlusest
comparison %>%
  ggplot(aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


# Ilma stopsõnadeta
comparison %>%
  filter(!word %in% stopwords) %>%
  ggplot(aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")+
  labs(x="Smilers",y="Terminaator")



