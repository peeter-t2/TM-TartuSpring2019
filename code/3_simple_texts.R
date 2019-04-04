# Kiire näidis tekstitöötlusest. Sellest järgmistes tundides juba pikemalt.

library(tidyverse)
library(tidytext)

#Loeme failid kataloogist
filelist <- list.files("data/corpus/",full.names=T,recursive = T)
texts <- map_df(filelist, ~ data_frame(txt = read_lines(.x)) %>%
                  mutate(filename = .x)) %>%
  mutate(filename= gsub("data/corpus/","",filename))

#K. Uiboaed stopsõnad
stopwords <- read_lines("data/uiboaed_stopwords/estonian-stopwords.txt")

# Mitu sõna on igas failis
texts %>%
  mutate(set=str_extract(filename,"ilves|meri")) %>%
  unnest_tokens(word,txt) %>%
  group_by(filename,set) %>%
  summarise(n=n()) %>%
  ggplot(aes(filename,n))+
  geom_bar(stat="identity")+
  facet_wrap(~set,scales="free")+
  coord_flip()

# Sama info teistmoodi esitatult
texts %>%
  mutate(set=str_extract(filename,"ilves|meri")) %>%
  unnest_tokens(word,txt) %>%
  group_by(filename,set) %>%
  summarise(n=n()) %>%
  ggplot(aes(set,n,color=set))+
  geom_jitter(alpha=0.3,width = 0.1)+
  theme_minimal()



## Tekstide sõnasageduste võrdlus
# 
texts %>%
  mutate(set=str_extract(filename,"ilves|meri")) %>% 
  unnest_tokens(word,txt) %>%
  count(word,set) %>%
  spread(set, n, fill = 0) %>%
  ggplot(aes(ilves, meri)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  geom_abline(color = "red")


# Tekstide sõnasageduste võrdlus stopsõnadeta
texts %>%
  mutate(set=str_extract(filename,"ilves|meri")) %>% 
  unnest_tokens(word,txt) %>%
  count(word,set) %>%
  group_by(set) %>%
  mutate(perc=n/sum(n)) %>% 
  filter(!word %in% c(stopwords,"eesti","euroopa","eestis")) %>%
  select(set,perc,word) %>%
  spread(set, perc, fill = 0) %>%
  ggplot(aes(ilves, meri)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  geom_abline(color = "red")


