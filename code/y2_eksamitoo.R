# Eksamitöö aines HVEE.01.006	Suurte tekstiandmete töötlemine ja analüüs humanitaarteadustes (3 EAP)

# Nimi:
# Kuupäev:


# Alustame puhtalt lehelt
rm(list=ls()) #eemaldab need muutujad


# Eksamitöö koosneb kahest osast:

# 1. osas tuleb lahendada tüüpülesandeid etteantud tekstide kohal. (20 punkti)
# 2. osas tuleb tüüpvahenditega koostada lühike analüüs oma või etteantud tekstide põhjal, kasutades õpitud tekstitöötlusvahendeid (20 punkti)


# Faili alguses loe sisse paketid/raamatukogud, mis on analüüsi jaoks vajalikud
library(tidyverse)
library(tidytext)



# Küsimustele vastake sellele järgneva koodiga. Näiteks:

# 1. Loe sisse laulusõnade andmestik.

Kood

# 2. Küsimus

Kood

# 3. Küsimus jne


# Soovitus praegu ja edaspidi: konsulteeri kataloogis help files toodud abimaterjalidega, eriti data-transformation-cheatsheet.pdf, ggplot2-cheatsheet-2.1.pdf, strings.pdf


########################
## 1. osa: praktilised ülesanded
#######################

# 1. Loe sisse laulusõnade andmestik (eesti või inglise keeles). Vihje: vaata varasemaid faile

# 2. Milline artist oli andmete järgi edetabelis esinemistelt kolmandal kohal vahemikus 2000 kuni 2010?

# 3. Millised olid 10 levinumat sõna eestikeelsetes tekstides aastal 1999?

# 4. Millised olid 10 kõige sagedamat kolmgrammi aastal 2000? (vihje: poplaulude failis on vahendid kolmgrammide loomiseks)

# 5. Millised 10 kolmgrammi esinesid kõige rohkemates lugudes kõigi aastate peale kokku? (vihje: group_by; ungroup)



# 6. Loe sisse presidentide 24. veebruari kõnede toortekstid. (vihje: kõnede sisselugemiseks on kood read_files.R juures)

# 7. Tee tekstidest sagedusloend ja eemalda stopsõnad

# 8. Millised olid president Meri ja president Ilvese 10 kõige sagedasemat sõna (kui stopsõnad on eemaldatud)

# 9. Millistes sõnavormides esineb sõna euroopa nendes tekstides? nt euroopa, euroopasse, euroopalik

# 10. Mis on nende sõnavormidega kõige sagedasemalt kaasnev sõna (vihje: bigrammid, str_replace või str_remove)



# 11. Loe sisse presidentide 24. veebr kõnede lemmatiseeritud tekstid.

# 12. Millised olid president Meri ja president Ilvese 5 sagedasemat lemmat?

# 13. Millised olid 5 sagedasemat lemmat kummalgi kui eemaldada stopsõnad?

# 14. Tee pilt, mis võrdleb küsimuses 13. leitud lemmade sagedust Meri ja Ilvese vahel.


# 15. Loe sisse mõned koondkorpuse või gutenbergi tekstikollektsiooni tekstid.

# 16. Vali välja kolm autorit ja kuva 10 sagedasemat sõna igalt autorilt.

# 17. Mis erinevused nende vahel on?

# 18. Vali välja kolm teost ja märgi eraldi tulbana sõna asukoht iga teose sees. (vihje: mutate)

# 19. Väljasta kõik sõnad, mis esinevad vähemalt 50 korda kolme raamatu peale kokku.

# 20. Vali punktis (19) leitud sõnadest välja 2 ja kuva nende asukohad valitud kolme teose sees.


###########################
## 2. osa: uurimisraport
###########################

# 2. osas tuleb tüüpvahenditega koostada lühike analüüs oma või etteantud tekstide põhjal, kasutades õpitud tekstitöötlusvahendeid. Mõtle välja uurimisküsimus ja püüa sellele vastata. Analüüsi pikkuseks on 3-5 lehekülge. (20 punkti)

# Analüüsis kirjelda:
# 1) Tekste, millele tugined.
# 2) Küsimust, mida esitad.
# 3) Vahendeid, mida kasutad.
# 4) Järeldusi, mida saad teha.

# Alusta analüüsi üldisest ülevaatest tekstidest, mida uurid. Kui pikad nad on jne.
# Analüüsi käigus tuleb võrrelda vähemalt kahte teksti või tekstiosa (näiteks peatükki, kõnelejat, teksti algust või lõppu jne)
# Analüüsis tuleb otsida vähemalt üht konkreetset sõna.
# Analüüsis peab olema vähemalt 2 pilti.
# Manuses lisa analüüsi skript, mis võimaldab analüüsi korrata.
# Kui kasutasid enda tekstifaile, tuleb lisada ka need

# Hindamisele tuleb 1) püstitatud küsimus ja analüüsi vastavus sellele, 2) tekstidega tehtud operatsiooni ja skript, mis võimaldab neid korrata, 3) analüüsi raport ise.
