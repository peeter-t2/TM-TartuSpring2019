# Koostatud TÜ kursuse jaoks Suurte tekstiandmete töötlemine ja analüüs humanitaarteadustes, Peeter Tinits, 2019






#Installime paketid
lapply(c("tidyverse", "tidytext", "gutenbergr", "scales","zoo"),
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

# Käivitame paketid
library(tidyverse)
library(tidytext)
library(scales)
library(zoo)

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
# ungroup() võta grupid lahku
# summarise() võtad andmed kokku mõne funktsiooniga
# join() ühenda kaks andmestikku
# mutate() loo uus muutuja

# Käsud tekstitöötluseks
# count(muutuja) - loeb esinemiste arvu
# top_n(number, muutuja) - võta esimesed 10 muutuja kohta
# left_join(andmestik) - ühenda üks andmestik teisega
# unnest_tokens(ühik, muutuja) - segmenteeri tekst sõnedeks, 2-grammideks, 3-grammideks
# str_detect(muutuja, "sõne") - osaline ühestamine
# str_extract(muutuja, "sõne") - vaste väljatoomine

#anti_join - eemalda klappivad elemendid
#inner_join - jäta alles ainult klappivad elemendid
#rename - muuda tulba nime
#spread - tee ühest tulbast mitu

#stop_words - stopsõnad inglise keeles
#get_sentiments - meelsussõnavara inglise keeles





library(gutenbergr)
#gutenbergr annab meile mõned varjatud andmestikud (st nad on sisse loetud, aga pole ülal paremal näha), üks neist on gutenberg_metadata
View(gutenberg_metadata)


## Meeldetuletuseks
# Vaatame, milliseid autoreid ja teoseid seal on
# (count, filter, arrange)
















gutenberg_metadata %>%
  filter(str_detect(author,"Wells, H. G."))

gutenberg_metadata %>%
  filter(str_detect(author,"Austen"))

gutenberg_metadata %>%
  filter(str_detect(title,"Time Machine"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(title,"Time Machine"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"en"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"de"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"de")) %>%
  filter(str_detect(author,"Shakespeare"))



gutenberg_metadata %>% 
  count(language) %>%
  filter(n>2) %>% 
  ggplot(aes(x=language,y=n))+
  geom_point()+
  coord_flip()



#Gutenbergr-ga allalaadimiseks
#1) ehita indeks
gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(author,"Wells, H. G.")) %>%
  filter(str_detect(language,"en")) -> hgwells_index

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(author,"Verne, Jules")) %>%
  filter(str_detect(language,"en")) -> jverne_index

#2) lae alla tekstid
#hgwells_texts <- gutenberg_download(hgwells_index$gutenberg_id[1:15], meta_fields = "title")
#jverne_texts <- gutenberg_download(jverne_index$gutenberg_id[1:15], meta_fields = "title")

#kui pole internetis
load("data/corpus/wells_verne.RData")

## Vaatame tekste













#loenda - mida see loendab
hgwells_texts %>%
  count(title)

jverne_texts %>%
  count(title)

#unnest_tokens - tee tekstist sõnad
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  count(title)

#group_by - grupeeri muutuja kaupa
#count - grupi kaupa, loenda sõnad
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  group_by(title) %>%
  count(word, sort = TRUE) #can also be done with count(title, word, sort=TRUE)



# stopsõnad
data("stop_words")
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE)

# kaks võrdlusgruppi
comparison1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="The Time Machine") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

comparison2 <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="A Journey into the Interior of the Earth") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)


comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

#this is just to plot the comparison
ggplot(comparison, aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


# sama suurema korpusega
hgwells_texts %>%
  count(title)
comparison1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

jverne_texts %>%
  count(title)
comparison2 <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

# võrdluse kuvamiseks
ggplot(comparison, aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")











## üksiksõnade otsing
jverne_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  count(title)


hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  count(title)







# asukoht raamatute sees

jverne_texts <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()
jverne_texts

hgwells_texts <- hgwells_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

jverne_texts%>%
  group_by(title)%>%
  ggplot(aes(x=title, y=max(linenumber)))+
  geom_bar(stat="identity")


hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  ggplot(aes(x=linenumber,y=title))+
  geom_point(shape=124)


jverne_texts%>%
  group_by(title)%>%
  ggplot(aes(x=title, y=max(linenumber)))+
  geom_bar(stat="identity")















### Meelsusanalüüs asukoha järgi
##
## Meelsusanalüüsiks on lihtsalt sõnastik

hgwellssentiment <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(hgwellssentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

jvernesentiment <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jvernesentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, scales = "free_x")



# Asukoht protsentides teksti sees











all_texts_comp1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(decile) %>%
  summarize(score = sum(score * n) / sum(n))

all_texts_comp2 <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(decile) %>%
  summarize(score = sum(score * n) / sum(n))

ggplot(data=all_texts_comp1,aes(decile, score)) +
  geom_line(colour="red") +
  geom_line(data=all_texts_comp2,colour="blue") +
  scale_x_continuous(labels = percent_format()) +
  expand_limits(y = 0) +
  labs(x = "Position within a story",
       y = "Average AFINN sentiment score")







# Asukoht peatüki kaupa










jverne_texts %>% 
  mutate(chapter=str_extract(text,"Chapter .*|CHAPTER .*")) %>% 
  count(chapter)


jverne_texts %>% 
  mutate(chapter=str_extract(text,"Chapter .*|CHAPTER .*")) %>% 
  count(chapter)

see<- jverne_texts %>% 
  filter(str_detect(text,"Chapter|CHAPTER"))

see<- jverne_texts %>% 
  filter(str_detect(text,"Chapter|CHAPTER")) %>% 
  count(title)

hgwells_texts %>% 
  filter(str_detect(text,"Chapter|CHAPTER")) %>% 
  count(title)


all_texts_comp1 <- jverne_texts %>% 
  mutate(chapter=str_extract(text,"Chapter .*|CHAPTER .*")) %>% 
  mutate(chapter=na.locf(chapter,na.rm=F))%>%
  group_by(title,chapter)%>%
  summarise(n=n()) #number of lines in chapter



# Meelsus peatüki kaupa














# Sõnad peatüki kaupa - millises peatükis on













### Ühe teksti sees


hamlet <-gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(title,"Hamlet"))

hamlet_text <- gutenberg_download(hamlet$gutenberg_id[1], meta_fields = "title")

hamlet_text %>%
  mutate(row_nr = row_number()) %>%
  filter(str_detect(text,"THE TRAGEDY OF HAMLET, PRINCE OF DENMARK"))






## Tegelased, stseenid
# koosesinevusteks on tarvis grupeerida








hamlet_conv <- hamlet_text %>%
  mutate(row_nr = row_number()) %>%
  filter(row_nr>211) %>% 
  mutate(character=str_extract(text,"[A-Z][a-z][a-z]\\. ")) %>% 
  mutate(character=na.locf(character,na.rm=F))

hamlet_conv <- hamlet_conv %>%
  mutate(act=str_extract(text,"Act [A-Z]\\.|ACT [A-Z]\\.")) %>% 
  mutate(act=na.locf(act,na.rm=F)) %>% 
  mutate(scene=str_extract(text,"Scene .*")) %>% 
  mutate(scene=na.locf(scene,na.rm=F))



# Ridu tegelase kohta
hamlet_conv %>% 
  count(character)


# Tegelased stseenides
hamlet_summary <- hamlet_conv %>% 
  group_by(act,scene) %>% 
  summarise(characters=list(unique(character)))

hamlet_summary




members_of_scenes <- hamlet_conv %>%
  group_by(act,scene) %>%
  distinct(character)

pairs_in_scenes <- members_of_scenes %>%
  left_join(members_of_scenes, by=c("act","scene")) %>%
  filter(character.x!=character.y) %>%
  group_by(character.x,character.y) %>%
  count() %>%
  arrange(desc(n))



lapply(c("igraph","ggraph"),
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

library(igraph)
library(ggraph)


## Võrgustikuülevaade hamletist
pairs_in_scenes %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()















