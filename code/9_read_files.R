library(tidyverse)

####
# Failide ühekaupa lugemiseks
####

text1 <- data_frame(txt=read_lines("data/corpus/presidentide_koned/24.veebr/texts/ilves_2007.txt")) %>%
  mutate(filename="ilves_2007.txt")
text2 <- data_frame(txt=read_lines("data/corpus/presidentide_koned/24.veebr/texts/ilves_2008.txt")) %>%
  mutate(filename="ilves_2008.txt")
text3 <- data_frame(txt=read_lines("data/corpus/presidentide_koned/24.veebr/texts/ilves_2009.txt")) %>%
  mutate(filename="ilves_2009.txt")
text4 <- data_frame(txt=read_lines("data/corpus/presidentide_koned/24.veebr/texts/ilves_2010.txt")) %>%
  mutate(filename="ilves_2010.txt")

#rbind == bind rows, ehk seo read kokku
texts_together <- rbind(text1,text2,text3,text4)


#####
## Lemmatiseerimise näidis
#####

# lemmatiseerimata tekst
text_plain <- read_lines("data/lemmatiseerimine/ilves_100_orig.txt")

# https://tekstianalyys.utlib.ut.ee/ lemmatiseeritud tekst
text_lemmas <- read_lines("data/lemmatiseerimine/ilves_100_lemmas.txt")

# https://tekstianalyys.utlib.ut.ee/ morfoloogiliselt analüüsitud tekst
# tulbad on eristatud komadega (csv ehk comma-separated values formaat)

text_tagged <- read_csv("data/lemmatiseerimine/ilves_100_tagged.txt")


######
# Loe palju faile korraga
#######

#Teeb etteantud kataloogi failidest nimekirja
filelist <- list.files("data/corpus/presidentide_koned/24.veebr/texts/",full.names=T,recursive = T)
#Loeb sisse failid 'filelist' põhjal
texts <- map_df(filelist, ~ data_frame(txt = read_lines(.x)) %>%
                  mutate(filename = .x)) %>%
  mutate(filename= basename(filename))

#Sama lemmade jaoks
filelist <- list.files("data/corpus/presidentide_koned/24.veebr/lemmas/",full.names=T,recursive = T)
lemmas <- map_df(filelist, ~ data_frame(txt = read_lines(.x)) %>%
                  mutate(filename = .x)) %>%
  mutate(filename= basename(filename))




# Meil on failinimedes info nii aasta kui ka presidendi kohta.
# Teeme sellest infost uued muutujad, et saaks teksti kuvada nendega

# Lisame ka rea numbri asukohaks teksti sees.

# Kuvame graafiku, mis näitab seda kõike.












texts <- texts %>%
  mutate(year= str_extract(filename,"[0-9]{4}"))

texts <- texts %>%
  mutate(set=str_extract(filename,"ilves|meri|rüütel|kaljulaid"))
  
texts <- texts %>%
  filter(txt!="")

texts <- texts %>%
  group_by(filename) %>% 
  mutate(rownumber=row_number())


texts %>% 
  group_by(year,set,filename) %>% 
  summarise(rows=max(rownumber)) %>% 
  ggplot(aes(x=year,y=rows,color=set))+
  geom_point()
