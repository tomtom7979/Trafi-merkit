# ladataan ja asennetaan kirjastot
# install.packages('tm')
# install.packages('SnowballC')
# install.packages('wordcloud')
# install.packages('RColorBrewer')
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(readr)
# asetetaan tyokansio, vaihda oma ty�kansio
setwd("C:/Users/tuotevayla/OneDrive - KamIT 365/Data-analytiikka/EsimerkkejaR")
# data haettu http://www.trafi.fi/tietopalvelut/avoin_data palvelusta (Tieliikenne_AvoinData_4_8) ja uudelleen nimetty
# Haetaan data csv-tiedostosta, pistetty omiin sarakkeisiin
AvoinData48 <- read_delim("AvoinData48.csv", ";", escape_double = FALSE, trim_ws = FALSE)
# tutkitaan dataa
summary(AvoinData48)
head(AvoinData48)
# Testataan sanapilve�
merkkiselvak <- AvoinData48[,c('merkkiSelvakielinen')]

# datassa hurjasti riveja, voi olla tietokoneelle tyollasta ladata koko taulukko, joten tutkitaan osissa 
# Ladataan 50000 rivia dataa trafikantask muuttujaan ja siivotaan/tutkitaan dataa
merkit <- Corpus(VectorSource(merkkiselvak[1:50000,]))
merkit <- tm_map(merkit, PlainTextDocument)
merkit <- tm_map(merkit, removePunctuation)
# stemmataan monikko sanat yksikkosanoiksi, ei vaikutusta
merkit <- tm_map(merkit, stemDocument)

# tehdaan sanapilvi otannasta
wordcloud(merkit, max.words = 30, random.order = FALSE, colors= brewer.pal(6, "Dark2"))

# Mik�li k�yt�ss� laskenta tehoa --> Otetaan mukaan laajempi m��r� dataa ja tutkitaan taulukon loppup��t�

merkitkk <- Corpus(VectorSource(merkkiselvak[4700000:4950030,]))
merkitkk <- tm_map(merkitkk, PlainTextDocument)
merkitkk <- tm_map(merkitkk, removePunctuation)
# stemmataan monikko sanat yksikkosanoiksi, ei vaikutusta
merkitkk <- tm_map(merkitkk, stemDocument)

# tehdaan sanapilvi kokodatasta, rajattu max. 60
wordcloud(merkitkk, max.words = 60, random.order = FALSE, colors= brewer.pal(6, "Dark2"))

# tutkitaan eri kohdasta dataa
merkitkk2 <- Corpus(VectorSource(merkkiselvak[3500000:4000000,]))
merkitkk2 <- tm_map(merkitkk2, PlainTextDocument)
merkitkk2 <- tm_map(merkitkk2, removePunctuation)
merkitkk2 <- tm_map(merkitkk2, stemDocument)
wordcloud(merkitkk2, max.words = 60, random.order = FALSE, colors= brewer.pal(6, "Dark2"))

# Tutkitaan m��ri�, miten tilastossa esiintyy eri automerkkej�
library(plyr)
library(ggplot2)
theme_set(theme_bw()) # parempi ggplot teema

x <- as.matrix(merkkiselvak) #asettaa datan matrix:iin
counts <- table(x) #establish frequencies
par(ps = 8, cex = 1, cex.main = 2) #text size
luokittelu <- counts[which(counts>120000)] 
bp <- barplot(luokittelu, las = 2, main = "Suomessa rekister�innit merkeitt�in, kaikki", xlab = "Merkit") #piirt�� plotin, joissa kulkuneuvojen m��r� yli 120000
text(bp, 0, luokittelu, cex=0, pos=3)

# Filtteroidaan viel� AvoinData48 DataFramea:

View(AvoinData48)
# co2 filtteroitu 0-100 eli tutkitaan matalap��st�isi� rekisteriss�
# Testataan sanapilve�
install.packages('dplyr')
library(dplyr) 
co2matala <- filter(AvoinData48, Co2 <= 100)
co2matalam <- co2matala[,c('merkkiSelvakielinen')]
co2matalawc <- Corpus(VectorSource(co2matalam))
co2matalawc <- tm_map(co2matalawc, PlainTextDocument)
co2matalawc <- tm_map(co2matalawc, removePunctuation)
# stemmataan monikko sanat yksikkosanoiksi, ei vaikutusta
co2matalawc <- tm_map(co2matalawc, stemDocument)

# tehdaan sanapilvi kokodatasta, rajattu max. 60
wordcloud(co2matalawc, max.words = 30, random.order = FALSE, colors= brewer.pal(6, "Dark2"))

# Tehd��n viel� histogrammi, matalap��st�iset

xc <- as.matrix(co2matalam) #asettaa datan matrix:iin
counts <- table(xc) #establish frequencies
par(ps = 8, cex = 1, cex.main = 2) #text size
luokittelu <- counts[which(counts>2000)] 
bp <- barplot(luokittelu, las = 2, main = "Suomessa co2 (0-100) rekister�innit", xlab = "Merkit") #piirt�� plotin, joissa kulkuneuvojen m��r� yli 2000
text(bp, 0, luokittelu, cex=0, pos=3)

# Analyysi: Eri otantojen perusteella yleisia kulkuneuvojen merkkeja ovat esimerkiksi: toyota, volkswagen, ford, volvo, mercedesbenz. 
# Omavalmisteisia kulkuneuvoja nayttaisi olevan suomalaisilla runsaasti k�yt�ssa. Matalap��st�isiss� henkil�autoissa Ford 
# n�ytt�isi ylt�v�n jo toiselle tilalle, tosin Toyota on my�s m��r�llisesti ylivoimainen matalap��st�isiss� autoissa.






