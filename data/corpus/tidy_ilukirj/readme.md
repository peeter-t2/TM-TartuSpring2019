## Koondkorpuse Eesti ilukirjandus 1990 _tidy_ vormingus

See kogu esitab [Koondkorpuse Eesti ilukirjandus 1990-](https://www.cl.ut.ee/korpused/segakorpus/eesti_ilukirjandus_1990/) tekstid tabelkujul nö [_tidy_](https://en.wikipedia.org/wiki/Tidy_data) vormingus.  Andmekogu aadress hetkel on [https://github.com/peeter-t2/tidy_ilukirj](https://github.com/peeter-t2/tidy_ilukirj).

Tekstid on esitatud üks-rida-vaatluse-kohta vormingus kolmes kataloogis. 

```
tidy_ilukirj
├── texts
│   ├── ilu_ahasveerus.tsv
│   ├── ilu_ajalooilu.tsv
│   └── ...
├── lemmas
│   ├── ilu_ahasveerus.tsv
│   ├── ilu_ajalooilu.tsv
│   └── ...
├── postagged
│   ├── ilu_ahasveerus.tsv
│   ├── ilu_ajalooilu.tsv
│   └── ...
├── koond_iluk_tidy_meta.tsv
├── readme.md
├── (texts.zip)
├── (lemmas.zip)
└── (postagged.zip)
```

- "Texts" sisaldab tekste toorkujul, kus on lause iga rea kohta. _sentence_ sisaldab lauset ennast, _parnr_ sisaldab lõigu järjekorranumbrit ja _head_ üksuse pealkirja - enamasti peatüki pealkiri. 
- "Lemmas" sisaldab tekste toorkujul ja nende lemmatiseeritud vorme tulbas nimega _lemmas_. 
- "Postagged" sisaldab segmenteeritud faile, kus on üks sõne rea kohta koos selle märgendusega - _word_texts_ on sõned toorkujul, 'lemmas' on selle lemma, 'roots' on selle sõna juur,'root_tokens' on juure erivormid,'forms' on vormitunnus,'endings' on sõnalõpp, 'postags' on sõnaliigi tunnus, 'postag_descriptions' on sõnaliigi tunnus lahti kirjutatult. Märgenduste tähendused on nähtaval [siin](https://estnltk.github.io/estnltk/1.4/tutorials/morf_tables.html).

Kollektsiooni peamine eesmärk on aidata õppida tekstikaevemeetodeid. Tekstide lemmatiseerimiseks ja märgendamiseks on kasutatud [EstNLTK](https://github.com/estnltk/estnltk) Pythoni paketti.

Koondkorpuse tekstid on on vabad kasutamiseks mitte-ärilistel eesmärkidel ja siinsel kollektsioonil on sama litsents [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/en/), Peeter Tinits 2019


-----------

This collection presents Mixed Corpus Fiction texts collection from Tartu University [see here](https://www.cl.ut.ee/korpused/segakorpus/eesti_ilukirjandus_1990/) (_Eesti ilukirjandus 1990- allkorpus_) in a tidy text format. The main purpose of this collection is education, to learn basic text mining methods.

The texts here are free to use for non-commercial purposes. The tidy dataset shares the same licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/en/), Peeter Tinits 2019