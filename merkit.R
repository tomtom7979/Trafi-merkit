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

# asetetaan tyokansio, vaihda oma työkansio
setwd("C:/Users/tuotevayla/OneDrive - KamIT 365/Data-analytiikka/EsimerkkejaR")

# heataan data ja asetaan muuttujaan, data parsittu trafin kannasta, taulukossa pelkat merkit sarake
trafikanta <- read.csv('mselvk.csv', stringsAsFactors = FALSE)
# tutkitaan dataa
# print(trafikanta[1:100,])
# dim(trafikanta)
# datassa hurjasti riveja, voi olla tietokoneelle tyollasta ladata koko taulukko 
# Ladataan 50000 rivia dataa merkit muuttujaan ja siivotaan/tutkitaan dataa, ei vaikutusta
merkit <- Corpus(VectorSource(trafikanta[1:50000,]))
merkit <- tm_map(merkit, PlainTextDocument)
merkit <- tm_map(merkit, removePunctuation)
# stemmataan monikko sanat yksikkosanoiksi, ei vaikutusta
merkit <- tm_map(merkit, stemDocument)

# tehdaan sanapilvi otannasta
wordcloud(merkit, max.words = 30, random.order = FALSE, colors= brewer.pal(6, "Dark2"))

# Mikäli käytössä laskenta tehoa --> Koko data merkitkk muuttujaan ja siivotaan sitä:

merkitkk <- Corpus(VectorSource(trafikanta))
merkitkk <- tm_map(merkitkk, PlainTextDocument)
merkitkk <- tm_map(merkitkk, removePunctuation)
merkitkk <- tm_map(merkitkk, stemDocument)

# tehdaan sanapilvi kokodatasta, rajattu max. 60
wordcloud(merkitkk, max.words = 60, random.order = FALSE, colors= brewer.pal(6, "Dark2"))

# Analyysi: Otannan ja koko datan perusteella yleisia kulkuneuvojen merkkeja ovat esimerkiksi: toyota, volkswagen, ford, volvo. 
# Omavalmisteisia kulkuneuvoja nayttaisi olevan suomalaisilla runsaasti käytössa.






