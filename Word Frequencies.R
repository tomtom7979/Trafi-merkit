# install.packages('plyr')
# install.packages('ggplot2')

library(plyr)
library(ggplot2)
theme_set(theme_bw()) # parempi ggplot teema

# asetetaan tyokansio, vaihda oma tyokansio
setwd("C:/Users/tuotevayla/OneDrive - KamIT 365/Data-analytiikka/EsimerkkejaR")

# heataan data ja asetaan muuttujaan, data parsittu trafin kannasta, taulukossa pelkat merkit
trafidf <- read.csv('mselvk.csv', stringsAsFactors = FALSE)

x <- as.matrix(trafidf) #asettaa datan matrix:iin
counts <- table(x) #establish frequencies
par(ps = 8, cex = 1, cex.main = 2) #text size
luokittelu <- counts[which(counts>120000)]
bp <- barplot(luokittelu, las = 2, main = "Suomessa rekisteröityjen kulkuneuvojen määrä merkeittäin", xlab = "Merkit") #piirtää plotin, joissa kulkuneuvojen määrä yli 120000
text(bp, 0, luokittelu, cex=0, pos=3)

# Analyysi: Histogrammi plotti tukee sanapilveä eli toyota, vw ovat yleisimmät automerkit trafin rekisterissä.
# Suhteellisen paljon Omavalmisteisia ajoneuvoja löytyy trafin datasta.