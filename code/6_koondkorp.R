# Koostatud TÜ kursuse jaoks Suurte tekstiandmete töötlemine ja analüüs humanitaarteadustes, Peeter Tinits, 2019


#Installime raamatukogud
lapply(c("tidyverse","scales","tidytext","png","grid"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Avame raamatukogud
library(tidyverse)
library(scales)
library(tidytext)
library(png)
library(grid)


# Seekord on andmeteks Koondkorpuse ilukirjanduse tekstid, mis on tehtud tidyverse kujul kergesti töödeldavaks
# Rohkem infot korpuste kohta siin: https://www.cl.ut.ee/korpused/segakorpus/eesti_ilukirjandus_1990/index.php?lang=et


# 0) Esiteks vaatame andmeid

text1 <- read_tsv("data/corpus/tidy_ilukirj/texts/ilu_ahasveerus.tsv")
text1

# Andmed on tidy kujul
grid.newpage(); grid.raster(png::readPNG("slides/tidy_data.png")) #Pilt raamatust R for Data Science (Hadley Wickham & Garrett Grolemund)


# Teeme nimekirja failidest
filelist <- tibble(file_w_path=list.files("data/corpus/tidy_ilukirj/texts/",full.names=T)) %>% #failide nimekiri kataloogist
  mutate(filename= basename(file_w_path)) #salvestame ka failinime eraldi

# Ja loeme failid sisse
# Vt abi https://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R
data <- filelist %>% # failide nimed
  mutate(file_contents = map(file_w_path,  ~ read_tsv(.x)))  # võta failid ja loe nad uude tulpa

data


data %>%
  unnest(file_contents) %>%
  group_by(filename) %>% 
  summarise(length=n()) %>% 
  ggplot(aes(x=filename,y=length))+
  geom_point()+
  coord_flip()




# Meil on ka metaandmed
meta <- read_tsv("data/corpus/tidy_ilukirj/koond_iluk_tidy_meta.tsv")

# Ühendame need kaks andmestikku käsuga left_join
data_m <- data %>%
  left_join(meta)

# Kontrollime kuidas õnnestus
# is.na(muutuja) - proovib leida ridu, kus info puudub (info == NA ehk Not Available)
data_m %>% 
  filter(is.na(pealkiri))
# Pealkirjata on 0 rida, meil õnnestus lisada pealkiri kõigile


#Andmestike ühendamine (tavaline metaandmete puhul)
#left_join - jätta vasakpoolsele kõik read, parempoolsele ainult klappivad
#right_join - jätta parempoolsele kõik read, vasakpoolsele ainult klappivad.
#inner_join - jäta ainult klappivad read kummastki liidetavast.
#full_join - võta maksimum mõlemast liidetavast.


grid.newpage(); grid.raster(readPNG("slides/joins.png"))  #Pilt lehelt https://www.btskinner.me/rworkshop/modules/wrangle_two.html


#Veel
#semi_join - jätta ainult klappivad read, aga ära lisa midagi.
#anti_join - eemalda klappivad read, ära lisa midagi.


#K. Uiboaed stopsõnad
stopwords <- tibble(word=read_lines("data/uiboaed_stopwords/estonian-stopwords.txt"))
#anti_join(stopwords) !


#Vaatame, mis andmestikus on

data_m

data_m %>% 
  count(autor,sort=T) #mitu teost on


# Kuvame tulemused
data_m %>% 
  count(autor,sort=T) %>%
  mutate(autor=factor(autor,levels=unique(autor))) %>% # vajalik kuvamiseks
  ggplot(aes(x=n,y=autor))+
  geom_point(stat="identity")


# Vaatame ühte autorit
data_m %>% 
  filter(autor=="Indrek Hargla")


# Loendame ühe autori sõnu
sonad <- data_m %>% 
  group_by(autor) %>% 
  filter(autor=="Indrek Hargla") %>%
  unnest(file_contents) %>% # uus käsk, nüüd on tekstid eraldi ridadel
  unnest_tokens(word, sentence) %>%
  anti_join(stopwords) %>%
  count(word,sort=T)

sonad


# 1) Kõigepealt otsime sõnu
# Mis on peamised sõnad
# Regex otsingud


# Olulised käsud
# mutate - loo uus tulp
# str_detect(koht, otsing) - otsi poolsõna
# str_extract(koht, otsing) - võta leitav tekst välja
# count(muutuja, sort=T) - muutuja variantide esinemiskordade arv
# unnest - võta andmestik pesast välja
# unnest_tokens - jaota tekstiväli mitmele reale

str_detect("öölased","lased$")
str_detect("öölased","[^i]d$")
str_detect("öölane","[^i]d$")


str_extract("öölased","lased$")
str_extract("öölased","[^i]d$")
str_extract("öölane","[^i]d$")



#Ulmekirjanikud
ulmekirjanikud <- c("Veiko Belials","Indrek Hargla","Juhan Habicht","Karen Orlau","Maniakkide Tänav","Siim Veskimees")
 
 
#Võrdleme autorite üldsõnavara

autor1 <- ulmekirjanikud[1]
autor2 <- ulmekirjanikud[2]


comparison1 <- data_m %>% 
  group_by(autor) %>% 
  filter(autor==autor1) %>%
  unnest(file_contents) %>% 
  unnest_tokens(word, sentence) %>% 
  group_by(autor) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stopwords)

comparison2 <- data_m %>% 
  group_by(autor) %>% 
  filter(autor==autor2) %>%
  unnest(file_contents) %>% 
  unnest_tokens(word, sentence) %>%
  group_by(autor) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stopwords)


comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1), # mida see rida teeb???
         comparison2 = comparison2 / sum(comparison2))


#Võrdluse tegemiseks
comparison %>% 
  ggplot(aes(jitter(comparison1), jitter(comparison2))) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")+
  labs(x=autor1,y=autor2)



# Saame vaadata ühe autoriga
sonad <- data_m %>% 
  group_by(autor) %>% 
  filter(autor=="Indrek Hargla") %>%
  unnest(file_contents) %>% # uus käsk, nüüd on tekstid eraldi ridadel
  unnest_tokens(word, sentence) %>%
  anti_join(stopwords) %>%
  count(word,sort=T)

sonad


# Aga saame võtta ka mitu autorit
# Me tegime jada ulmekirjanikud
ulmekirjanikud
# filter(autor %in% ulmekirjanikud) %>%

sonad <- data_m %>% 
  group_by(autor) %>% 
  filter(autor %in% ulmekirjanikud) %>%
  unnest(file_contents) %>% 
  unnest_tokens(word, sentence) %>% 
  group_by(autor) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stopwords)


sonad <- sonad %>%
  group_by(autor) %>% 
  mutate(perc=n/sum(n))

vaata <- sonad %>% 
  filter(str_detect(word,"[^i]d$"))

vaata <- sonad %>% 
  filter(str_detect(word,"[^i]d$")) %>% 
  arrange(desc(perc))


vaata <- sonad %>% 
  filter(str_detect(word,"lased$"))
vaata <- sonad %>% 
  filter(str_detect(word,"vampiir"))




## Proovime leida tegelasi ulmejuttudest
# str_detect(koht, otsing)
# str_extract(koht, otsing)

# Kuidas otsida mitut võimalust?



str_detect(c("päkapikud", "mäetrollid", "haldjad"),"otsing")

str_detect(c("päkapikud", "mäetrollid", "trollibussid" , "haldjad"),"otsing")








sonad %>% 
  filter(str_detect(word,"^tulnuka|draakon|peninuk|koerakoon|härjapõlv|päkapik|haldj|zombi|luukere|deemon|^troll|ahjual")) %>% 
  mutate(muutuja=str_extract(word,"^tulnuka|draakon|peninuk|koerakoon|härjapõlv|päkapik|haldj|zombi|luukere|deemon|^troll|ahjual")) %>% 
  group_by(autor,muutuja) %>% 
  filter(autor %in% ulmekirjanikud) %>% 
  summarise(n=sum(n)) %>% 
  
  ggplot(aes(x=muutuja,y=n,color=autor))+
  geom_point()+
  geom_line(aes(group=autor))+
  scale_y_log10()

sonad %>% 
  filter(str_detect(word,"mõõ[gk]|kirve|vibu|^amb|haamer|^relv|^püss|^kahur")) %>% 
  mutate(muutuja=str_extract(word,"mõõ[gk]|kirve|vibu|^amb|haamer|^relv|^püss|^kahur")) %>% 
  group_by(autor,muutuja) %>% 
  filter(autor %in% ulmekirjanikud) %>% 
  summarise(n=sum(n)) %>% 
  
  ggplot(aes(x=muutuja,y=n,color=autor))+
  geom_point()+
  geom_line(aes(group=autor))+
  scale_y_log10()


sonad %>% 
  filter(str_detect(word,"^kosmos||hobu[ns]e|rong|arvuti|tank|^laev|^paadi")) %>% 
  mutate(muutuja=str_extract(word,"^kosmos|hobu[ns]e|rong|arvuti|tank|^laev|^paadi")) %>% 
  group_by(autor,muutuja) %>% 
  filter(autor %in% ulmekirjanikud) %>% 
  filter(!is.na(muutuja)) %>% 
  summarise(n=sum(n)) %>% 
  
  ggplot(aes(x=muutuja,y=n,color=autor))+
  geom_point()+
  geom_line(aes(group=autor))+
  scale_y_log10()


# Pilte saab ka salvestada
# Variant 1:
# Vasakul-all Plots: Export -> Save as image (vt Directory ja filename) -> Save
# Variant 2:
ggsave("plots/ulmetehnoloogia.png") #Salvestab viimati trükitud joonise. On võimalik salvestada ka varem loodud jooniseid (sellest teinekord).


sonad %>% 
  filter(str_detect(word,"koletis|peletis")) %>% 
  mutate(muutuja=str_extract(word,"koletis|peletis"))


# Autorite võrdlemine konkreetsete sõnade puhul
sonad %>% 
  filter(str_detect(word,"koletis|peletis")) %>% 
  mutate(muutuja=str_extract(word,"koletis|peletis")) %>% 
  group_by(autor,muutuja) %>% 
  summarise(n=sum(n)) %>% 
  spread(muutuja,n,fill=0) %>%
  
  ggplot(aes(koletis, peletis)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = autor), check_overlap = TRUE,
            vjust = -0.1, hjust = -0.1) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(color = "red")+
  labs(x="koletis",y="peletis")


# Ülesanded
# 1. Otsi üles kõik kaasaütlevad sõnad (nt minuga, paadiga)
# 2. Vaata, mitu korda kokku on autorid kasutanud kaasaütlevat vormi. Kes on kasutanud kõige rohkem? (vihje summarise)
# [Raskem] 3. Kes on kasutanud kõige rohkem osakaaluna kõikidest sõnadest? (vihje mutate)
# 4. Tee uus võrdlus mõnedest sõnadest, mida me varem ei otsinud.
# 5. Salvesta see pildina
  













#2) Otsime lemmadega

# Lemmade kataloog tuleb enne lahti pakkida
# Loeme sisse lemmade failid
filelist <- tibble(file_w_path=list.files("data/corpus/tidy_ilukirj/lemmas/",full.names=T)) %>% #failide nimekiri kataloogist
  mutate(filename= basename(file_w_path)) #salvestame ka failinime eraldi

data <- filelist %>% # failide nimed
  mutate(file_contents = map(file_w_path,  ~ read_tsv(.x)))  # võta failid ja loe nad uude tulpa

data_m <- data %>%
  left_join(meta)


lemmad <- data_m %>% 
  group_by(autor) %>% 
  filter(autor=="Veiko Belials") %>%
  unnest(file_contents) %>% 
  unnest_tokens(word, lemmas) %>% # nüüd on siin lemmad, mitte sentence!
  group_by(autor) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stopwords)



autor1 <- ulmekirjanikud[1]
autor2 <- ulmekirjanikud[2]


comparison1 <- data_m %>% 
  group_by(autor) %>% 
  filter(autor==autor1) %>%
  unnest(file_contents) %>% 
  unnest_tokens(word, lemmas) %>%  #seekord lemmad!
  group_by(autor) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stopwords) # eemaldame ikka stoppsõnad

comparison2 <- data_m %>% 
  group_by(autor) %>% 
  filter(autor==autor2) %>%
  unnest(file_contents) %>% 
  unnest_tokens(word, lemmas) %>% #seekord lemmad!
  group_by(autor) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stopwords) # eemaldame ikka stoppsõnad


comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))


#Võrdluse tegemiseks
comparison %>% 
  ggplot(aes(jitter(comparison1), jitter(comparison2))) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")+
  labs(x=autor1,y=autor2)










# -----------
# Märgendatud tekst
# Enne tuleb lahti pakkida kataloog 'postagged'

# Vaatame faili

# Seekord on failid päris suured
text1 <- read_tsv("data/corpus/tidy_ilukirj/postagged/ilu_ahasveerus.tsv")

text1 %>% 
  filter(postags=="S") %>% 
  count(lemmas,sort=T)

# Failide nimistu
filelist <- tibble(file_w_path=list.files("data/corpus/tidy_ilukirj/postagged/",full.names=T)) %>% #failide nimekiri kataloogist
  mutate(filename= basename(file_w_path)) #salvestame ka failinime eraldi

# Tasub arvutit võimalikult vähe koormata
# Valimie välja ainult ulmekirjanikud enne kui me loeme failid sisse
ulmekirjanikefailid <- filelist %>%
  left_join(meta) %>% 
  filter(autor %in% ulmekirjanikud)

# Nüüd loeme ainult ulmekirjanikud
data <- ulmekirjanikefailid %>% # failide nimed
  mutate(file_contents = map(file_w_path,  ~ read_tsv(.x)))  # võta failid ja loe nad uude tulpa
# 74 faili

#Juba natuke üle miljoni sõne
ulmesonad_tagged <- data %>%
  unnest(file_contents)
  

# Siis saab loendada näiteks omadussõnu või nimisõnu
# Märgenduse tähendused on nähtavad siin: https://estnltk.github.io/estnltk/1.4/tutorials/morf_tables.html
# A 	omadussõna - algvõrre (adjektiiv - positiiv), nii käänduvad kui käändumatud 	kallis või eht
# C 	omadussõna - keskvõrre (adjektiiv - komparatiiv) 	laiem
# D 	määrsõna (adverb) 	kõrvuti
# H 	pärisnimi 	Edgar
# I 	hüüdsõna (interjektsioon) 	tere
# J 	sidesõna (konjunktsioon) 	ja
# P 	asesõna (pronoomen) 	see
# S 	nimisõna (substantiiv) 	asi
# U 	omadussõna - ülivõrre (adjektiiv - superlatiiv) 	pikim
# V 	tegusõna (verb) 	lugema


ulmesonad_tagged %>% 
  filter(postags=="P") %>% 
  count(lemmas,sort=T)





# Ülesanded
# 1. Proovi otsida varem-otsitud sõnu lemmade kaupa. Kas tulemused on samad?
# 2. Otsi sagedasemaid omadussõnu autorite kohta. Mis erinevusi või sarnasusi näed?
# 3. Otsi sagedasemaid nimisõnu autorite kohta. Mis erinevusi või sarnasusi näed?
# 4. Võrdle mõnd konkreetset sõna autorite vahel.








# 3) Sõnade paiknemine teksti sees

#  mutate(rownumber=row_number()) %>% 
#  mutate(posintext=rownumber/(max(rownumber)+1)) %>% 


data_m %>% 
  group_by(autor,filename) %>% 
  filter(autor=="Juhan Habicht") %>%
  unnest(file_contents) %>% 
  mutate(rownumber=row_number())


data_m %>% 
  group_by(autor,filename) %>% 
  filter(autor=="Juhan Habicht") %>%
  unnest(file_contents) %>% 
  mutate(rownumber=row_number()) %>% 
  mutate(posintext=rownumber/(max(rownumber)+1))



data_m %>% 
  group_by(autor,filename) %>% 
  filter(autor=="Juhan Habicht") %>%
  unnest(file_contents) %>% 
  mutate(rownumber=row_number()) %>% 
  mutate(posintext=rownumber/(max(rownumber)+1)) %>% 
  unnest_tokens(word, lemmas)




data_m %>% 
  group_by(autor,filename) %>% 
  filter(autor=="Juhan Habicht") %>%
  unnest(file_contents) %>% 
  mutate(rownumber=row_number()) %>% 
  mutate(posintext=rownumber/(max(rownumber)+1)) %>% 
  unnest_tokens(word, lemmas) %>% 
  group_by(autor) %>%
  mutate(pealkiri=as.factor(pealkiri)) %>% 
  filter(str_detect(word,"elu")) %>% 
  
  ggplot(aes(x=posintext,y=pealkiri))+
  geom_point(shape=124,size=2)+
  scale_x_continuous(limits=c(0,1))+
  scale_y_discrete(drop=FALSE)




data_m %>% 
  group_by(autor,filename) %>% 
  filter(autor=="Maniakkide Tänav") %>%
  unnest(file_contents) %>% 
  mutate(rownumber=row_number()) %>% 
  mutate(posintext=rownumber/(max(rownumber)+1)) %>% 
  unnest_tokens(word, lemmas) %>% 
  group_by(autor) %>%
  mutate(pealkiri=as.factor(pealkiri)) %>% 
  filter(str_detect(word,"elu")) %>% 
  
  ggplot(aes(x=posintext,y=pealkiri))+
  geom_point(shape=124,size=2)+
  scale_x_continuous(limits=c(0,1))+
  scale_y_discrete(drop=FALSE)



## Ülesanded.
# 1. Uuri välja, millised sõned, me eelmise otsinguga kätte saime. Kas oleks mõistlik midagi parandada?
# 2. Proovi varem otsitud sõnade paiknemist teksti sees. Mida huvitavat näed?
# [Raskem] 3. Me praegu otsime asukohta lause kaupa. Kuidas saaks otsida asukohta teksti sees sõna kaupa?
# 4. Salvesta oma tehtud otsingutest üks pilt.
