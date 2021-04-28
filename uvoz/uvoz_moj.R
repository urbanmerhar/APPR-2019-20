# 2.faza: UVOZ PODATKOV
#======================
# R me je opozoril, da je to priporočeno odpiranje knjižnjic
library(plyr)
library(dplyr)

# Uvoz podatkov državljanstva oseb, ločenih po spolu
uvozi.prebivalstvo <- function(){
  data <- read_delim("podatki/prebivalstvo_po_drzavi_drzavljanstva.csv", 
                     ";", escape_double = FALSE, col_names=TRUE, col_types = cols(.default = col_character()), locale = locale(encoding = "WINDOWS-1250"), 
                     na = c("-", "..."),
                     trim_ws = TRUE, skip = 1)
  # "-" in "..." je potrebno zamenjati z vrednostjo "NA"
  # "col_types = cols(.default = col_character())" je tukaj zato, da mi vsa imena stolpcev spremeni v character
  return(data)
}

# Samo uvoženi podatki
prebivalstvo.surovi.podatki <- uvozi.prebivalstvo()

# ČIŠČENJE PODATKOV
#==================

# potrebno je odstraniti vrstice, ki jih lahko izračunam iz drugih (primer: spol SKUPAJ, države v skupinah "SEVERNA AMERIKA")

prebivalstvo.ciscenje.vrstice <- prebivalstvo.surovi.podatki %>% slice((-c(1:175, 220, 264, 293, 296, 342, 347, 348, 393, 437, 466, 469, 515)))
                          # izbrisane so bile vse vrstice s spolom "SKUPAJ"

# odstranimo stolpce, ki jih lahko izračunamo(primer: 2011 Starost SKUPAJ)

prebivalstvo.ciscenje.stolpci <- subset(prebivalstvo.ciscenje.vrstice, select = -c(`2011 Starost - SKUPAJ`, `2012 Starost - SKUPAJ`,
                                                                                   `2013 Starost - SKUPAJ`, `2014 Starost - SKUPAJ`,
                                                                                   `2015 Starost - SKUPAJ`, `2016 Starost - SKUPAJ`,
                                                                                   `2017 Starost - SKUPAJ`, `2018 Starost - SKUPAJ`,
                                                                                   `2019 Starost - SKUPAJ`))

# uporabimo funkcijo "distinct", da zagotovo ni podvojenih vrstic v tabeli

prebivalstvo.ciscenje <- distinct(prebivalstvo.ciscenje.stolpci)

# še lepšamo tabelo tako da bomo dodali stolpce z leti
# dajmo razbit trenutno tabelo na več tabel in dodamo vsaki stolpec z letom in preimenujmo stolpce oblike "`2011 Starost - SKUPAJ`"
# te manjše tabele pa združimo nazaj skupaj

prebivalstvo.ciscenje.2011 <- prebivalstvo.ciscenje[ , 1:21]
# zdaj imam tabelo z vsemi vrsticami kot prej, vsebuje pa samo podatke za leto 2011, dodajmo to leto v nov stolpec in preimenujmo stolpce

# vektor za dodajo leta
leto.2011 <- rep.int(2011, 332)

# dodamo stolpec leta 2011
library(tibble)
add_column(prebivalstvo.ciscenje.2011, leto.2011, .after = 2)

library(plyr) #za preimenovanje stolpcev
rename(prebivalstvo.ciscenje.2011, c(`2011 0-4 let` = "0-4 let",
                                     `2011 5-9 let` = "5-9 let",
                                     `2011 10-14 let` = "10-14 let",
                                     `2011 15-19 let` = "15-19 let",
                                     `2011 20-24 let` = "20-24 let",
                                     `2011 25-29 let` = "25-29 let",
                                     `2011 30-34 let` = "30-34 let",
                                     `2011 35-39 let` = "35-39 let",
                                     `2011 40-44 let` = "40-44 let",
                                     `2011 45-49 let` = "45-49 let",
                                     `2011 50-54 let` = "50-54 let",
                                     `2011 55-59 let` = "55-59 let",
                                     `2011 60-64 let` = "60-64 let",
                                     `2011 65-69 let` = "65-69 let",
                                     `2011 70-74 let` = "70-74 let",
                                     `2011 75-79 let` = "75-79 let",
                                     `2011 80-84 let` = "80-84 let",
                                     `2011 85-89 let` = "85-89 let",
                                     `2011 90 + let` = "90 + let"))

# Zdruzimo zdaj zgornje stvari in dejansko ustvarimo tabelo
prebivalstvo.2011 <- prebivalstvo.ciscenje[ , 1:21] %>% 
                    add_column(leto.2011, .after = 2) %>% 
                    setNames(c("SPOL",
                               "DRŽAVA DRŽAVLJANSTVA",
                               "LETO",
                               "0-4 let",
                               "5-9 let",
                               "10-14 let",
                               "15-19 let",
                               "20-24 let",
                               "25-29 let",
                               "30-34 let",
                               "35-39 let",
                               "40-44 let",
                               "45-49 let",
                               "50-54 let",
                               "55-59 let",
                               "60-64 let",
                               "65-69 let",
                               "70-74 let",
                               "75-79 let",
                               "80-84 let",
                               "85-89 let",
                               "90 + let"))

# Podobno naredimo za ostala leta
prebivalstvo.2012 <- prebivalstvo.ciscenje[ ,c(1,2, 22:40)] %>%
  add_column(rep.int(2012, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

prebivalstvo.2013 <- prebivalstvo.ciscenje[ ,c(1,2, 41:59)] %>%
  add_column(rep.int(2013, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

prebivalstvo.2014 <- prebivalstvo.ciscenje[ ,c(1,2, 60:78)] %>%
  add_column(rep.int(2014, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

prebivalstvo.2015 <- prebivalstvo.ciscenje[ ,c(1,2, 79:97)] %>%
  add_column(rep.int(2015, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

prebivalstvo.2016 <- prebivalstvo.ciscenje[ ,c(1,2, 98:116)] %>%
  add_column(rep.int(2016, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

prebivalstvo.2017 <- prebivalstvo.ciscenje[ ,c(1,2, 117:135)] %>%
  add_column(rep.int(2017, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

prebivalstvo.2018 <- prebivalstvo.ciscenje[ ,c(1,2, 136:154)] %>%
  add_column(rep.int(2018, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

prebivalstvo.2019 <- prebivalstvo.ciscenje[ ,c(1,2, 155:173)] %>%
  add_column(rep.int(2019, 332), .after = 2) %>%
  setNames(c("SPOL",
             "DRŽAVA DRŽAVLJANSTVA",
             "LETO",
             "0-4 let",
             "5-9 let",
             "10-14 let",
             "15-19 let",
             "20-24 let",
             "25-29 let",
             "30-34 let",
             "35-39 let",
             "40-44 let",
             "45-49 let",
             "50-54 let",
             "55-59 let",
             "60-64 let",
             "65-69 let",
             "70-74 let",
             "75-79 let",
             "80-84 let",
             "85-89 let",
             "90 + let"))

# po zabavi copy-paste in menjavi števil združimo vse tabele v eno
prebivalstvo.leta <- rbind(prebivalstvo.2011,
                          prebivalstvo.2012,
                          prebivalstvo.2013,
                          prebivalstvo.2014,
                          prebivalstvo.2015,
                          prebivalstvo.2016,
                          prebivalstvo.2017,
                          prebivalstvo.2018,
                          prebivalstvo.2019)

# zdaj bi bilo lepo še narediti, to da starost prenesem v vrstice, da imam potem (spol, državljanstvo, leto, starost)

razbitje.starosti <- c("0-4 let",
                       "5-9 let",
                       "10-14 let",
                       "15-19 let",
                       "20-24 let",
                       "25-29 let",
                       "30-34 let",
                       "35-39 let",
                       "40-44 let",
                       "45-49 let",
                       "50-54 let",
                       "55-59 let",
                       "60-64 let",
                       "65-69 let",
                       "70-74 let",
                       "75-79 let",
                       "80-84 let",
                       "85-89 let",
                       "90 + let")

# for row in "prebivalstvo.leta" ustvari dovolj vrstic za vektor "razbitje.starosti" in vzemi pravo vrednost iz "prebivalstvo leta"

# USTVARIMO NOVO TABELO; KI JO BOMO NATO SHRANILI V CSV, DA BOMO PRIHRANILI ČAS OB PONOVNIH ZAGONIH
# prazna tabela prebivalstva
#prebivalstvo <- data.frame(spol=character(),
#                           drzavljanstvo=character(),
#                           leto=integer(),
#                           starost=character(),
#                           stevilo=integer())
# TEST
# tako vstavimo podatke v prazno tabelo
#prebivalstvo %>% add_row(spol = "m", drzavljanstvo="A", leto=2012, starost="d", stevilo=5)



# zazeneš dvojno for zanko in dobiš tabelo o populaciji v "pomoje" tidy data obliki
# iz časovnih razlogov je ta koda zdaj zakomentirana, da tega ustvarjanja tabele ne storimo vsakič
#for (i in 1:nrow(prebivalstvo.leta)) {
#  for (j in 1:length(razbitje.starosti)) {
#    prebivalstvo <- prebivalstvo %>% add_row(spol = as.character(prebivalstvo.leta[i,1]),
#                             drzavljanstvo = as.character(prebivalstvo.leta[i,2]),
#                             leto = as.integer(prebivalstvo.leta[i,3]),
#                             starost = as.character(razbitje.starosti[j]),
#                             stevilo = as.integer(prebivalstvo.leta[i,j+3]))
#  }
#}

# Za konec še zamenjam vrednosti 'NA' z 0, ker mi je to drugače nagajalo pri analizi
#prebivalstvo[is.na(prebivalstvo)] <- 0

# za preglednost odstranimo nepotrebne tabele iz environmenta
rm(prebivalstvo.2011, prebivalstvo.2012, prebivalstvo.2013, prebivalstvo.2014, prebivalstvo.2015, prebivalstvo.2016,
   prebivalstvo.2017, prebivalstvo.2018, prebivalstvo.2019, prebivalstvo.ciscenje, prebivalstvo.ciscenje.2011,
   prebivalstvo.ciscenje.stolpci, prebivalstvo.ciscenje.vrstice, prebivalstvo.leta)

# Tabelo, ki smo jo dobili iz dvojne zanke smo shranili, da jo lahko zdaj beremo
#write.csv(prebivalstvo, "C:\\Users\\Urban\\Documents\\Finančna matematika\\Projekt R\\APPR-2019-20-master\\podatki\\prebivalstvo.csv")

prebivalstvo <- read_delim("podatki/prebivalstvo.csv", 
                   ",", escape_double = FALSE,
                   col_names=TRUE,
                   col_types = NULL,
                   locale = locale(encoding = "WINDOWS-1250"), 
                   trim_ws = TRUE,)

prebivalstvo$X1 <- NULL
