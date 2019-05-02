library(tidyverse)

#tavaline
text1 <- read_lines("data/lemmatiseerimine/ilves_100_orig.txt")

# https://tekstianalyys.utlib.ut.ee/ lemmatiseeritud tekst
text2 <- read_lines("data/lemmatiseerimine/ilves_100_lemmas.txt")

# https://tekstianalyys.utlib.ut.ee/ morfoloogiliselt analüüsitud tekst
# tulbad on eristatud komadega (csv ehk comma-separated values formaat)

text3 <- read_csv("data/lemmatiseerimine/ilves_100_tagged.txt")


#loe palju faile korraga

filelist <- list.files("data/corpus/presidentide_koned/24.veebr/",full.names=T,recursive = T)
texts <- map_df(filelist, ~ data_frame(txt = read_lines(.x)) %>%
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
