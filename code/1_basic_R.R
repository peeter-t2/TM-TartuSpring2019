#Esmaversioon David Lorenz 2017, see R_Glossary v1-4.pdf
#Koondatud TÜ aine Suurte tekstiandmete töötlemine ja analüüs humanitaarteadustes jaoks, Peeter Tinits 2019

# Jooksuta* neid käske järgemööda (iga rida eraldi). Üldiselt peaks olema toimuv mõistetav, isegi kui keelt veel ei tunne

## *Jooksutamiseks võid sa 1) kopeerida rea R-i konsooli ja vajutada ENTER või liigutades tekstikursori skriptis õigele reale ja vajutades CNTRL+ENTER (Windows) või CMD+Enter (Mac). See käivitab selle rea, millel kursor on. 3) Võib ka valida mitu rida, siis käivitatakse nad komplektina.

#Soovitatav on soft-wrap sisse lülitada
#Tools -> Global options -> Code -> Editing -> Soft-wrap R source files (turn it on)


# Lihtsad käsud
1+1
4-2
4*2
27*17
459/17
sqrt(25)

# Muutujate tegemine
x <- 4
y <- 2
x_2 = 4
y_2 = 2
x
y

# Käsud muutujatega
x+y
x*y

# Kontrolli samasust
1==1
1==2
x==y
x==y*2

# Tee kompleksmuutujaid
z <- x+y*2
z <- (x+y)*2
z

# Tee ja vaata esimesed vektorid
1:10
myFirstVector <- c(1:10)
myFirstVector
mySecondVector <- c("a", "b", "c", "d","e","f","g","h","i","j")
mySecondVector
class(myFirstVector)
class(mySecondVector)
myThirdVector <- c(a,b,c,d)  # ei tööta - miks?
myThirdVector <- c("a","b","c","d")


myFirstVector[6]
mySecondVector[c(2,3,5)]  # mida teeb c() nurksulgude vahel?
myFirstVector[6,8,9] # ei tööta, vajab c()


myFirstVector %in%  mySecondVector #ükski ei klapi
mySecondVector %in%  myThirdVector #esimesed neli klapivad
mySecondVector[mySecondVector %in%  myThirdVector] # vali need esimesed neli, mis klapivad

length(myFirstVector)

# tee lihtsaid tabeleid
anotherVector <- c(11:20)
myFirstMatrix <- cbind(myFirstVector, anotherVector) 
myFirstMatrix 
mySecondMatrix <- rbind(myFirstVector, anotherVector)
mySecondMatrix  # mis on erinevus cbind ja rbind vahel?
myFirstMatrix[3,2]
myFirstMatrix[3,]
myFirstMatrix[,2] # mida teevad nurksulud nüüd?

# lihtne ülevaade tabelist
myFirstTibble <- tibble(myFirstMatrix)
nrow(myFirstTibble)
ncol(myFirstTibble)
dim(myFirstTibble)
str(myFirstTibble)   


# tabeli tegemine
QuickTibble <- data.frame(letters=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), numbers=1:10)
QuickTibble

ls()   # 'ls' näitab kõiki salvestatud andmeid

getwd()   ## Näita töökataloogi
