# 4. faza: Analiza podatkov
#==========================

#! je moja oznaka, da hitro najdem kode za grafe

# VEČ ALI MANJ VEDNO DELAM S FILTROM ZA LETO, zato za graf za poljubno leto zamenjaj vrednost leta pri vseh filtrih
# tukaj določim leto analize [2011 - 2019]
izbrano.leto <- 2011

#Primerjave Slovenija, Evropa, EU, Ostalo
#========================================

# V tej tabeli zdrzimo grupiramo po letih in drzavljanstvih, da dobimo tabelo z vrsticami o skupnih številkah drzavljanov
tabela.leto.drzavljanstvo.stevilo <- prebivalstvo %>%
            group_by(leto, drzavljanstvo) %>%
            summarise_at(vars(stevilo), list(skupaj = sum))

#Prejšnjo tabelo ločim na države iz Evrope, Slovenijo in ostalo

# drzave evrope brez slo
drzave.evropa <- c('Albanija', 'Andora', 'Avstrija', 'Belgija', 'Belorusija', 'Bolgarija',
                    'Bosna in Hercegovina', 'Ciper', 'Češka republika', 'Črna gora', 'Danska', 'Estonija', 'Finska',
                   'Francija', 'Grčija', 'Hrvaška', 'Irska', 'Islandija', 'Italija', 'Kosovo', 'Latvija', 'Lihtenštajn',
                   'Litva', 'Luksemburg', 'Madžarska', 'Malta', 'Moldavija, Republika', 'Nemčija', 'Nizozemska',
                   'Norveška', 'Poljska', 'Portugalska', 'Romunija', 'San Marino', 'Severna Makedonija',
                   'Slovaška', 'Slovenija', 'Srbija', 'Španija', 'Švedska', 'Švica', 'Ukrajina', 'Združeno kraljestvo')

# Tabela Slovensko, Evropsko, Ostalo
tabela.evropa.slovenija.ostalo <- tabela.leto.drzavljanstvo.stevilo %>%
                                  group_by(leto, drzavljanstvo %in% drzave.evropa, drzavljanstvo == 'Slovenija') %>%
                                  summarise_at(vars(skupaj), list(skupaj = sum)) %>% 
                                  add_column(rep(c('Ostalo', 'Evropsko', 'Slovensko'), times=9), .after = 'leto') %>%
                                  ungroup() %>%
                                  select(1, 2 ,5) %>%
                                  setNames(c('leto', 'drzavljanstvo', 'stevilo'))

#drzave evropske unije
drzave.eu <- c("Avstrija", "Belgija", "Bolgarija", "Hrvaška", "Ciper", 'Češka republika', "Danska", "Estonija", "Finska", "Francija",
               "Nemčija", "Grčija", "Madžarska", "Irska", "Italija", "Latvija", "Litva", "Luksemburg", "Malta", "Nizozemska",
               "Poljska", "Portugalska", "Romunija", "Slovaška", 'Slovenija', "Španija", "Švedska")

# Tabela Slovensko, EU, Evropsko brez EU, Ostalo
tabela.eu.slovenija.preostalaevropa.ostalo <- tabela.leto.drzavljanstvo.stevilo %>%
                                              group_by(leto, drzavljanstvo %in% drzave.eu,
                                                       drzavljanstvo == 'Slovenija',
                                                       drzavljanstvo %in% drzave.evropa) %>%
                                              summarise_at(vars(skupaj), list(skupaj = sum)) %>%
                                              add_column(rep(c('Ostalo', 'Evropa brez EU', 'Evropsko', 'Slovensko'), times=9), .after = 'leto') %>%
                                              ungroup() %>%
                                              select(1, 2, 6) %>%
                                              setNames(c('leto', 'drzavljanstvo', 'stevilo'))

                                  
#Tabeli dodamo vsote vseh drzavljanov po letih

tabela.vsot.po.letih <- tabela.eu.slovenija.preostalaevropa.ostalo %>%
                        group_by(leto) %>%
                        summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                        add_column(c('SKUPAJ'), .after = 'leto') %>%
                        setNames(c('leto', 'drzavljanstvo', 'stevilo'))

tabela.eu.slovenija.preostalaevropa.ostalo.SKUPAJ <- full_join(tabela.eu.slovenija.preostalaevropa.ostalo, tabela.vsot.po.letih) %>%
                                              arrange(leto)

# Tabela v %
#dodaj stolpec s skupnimi vrednostimi in nato deli
#sepravi nek join po letih,...
tabela.eu.slovenija.preostalaevropa.ostalo.z.odstotki <- left_join(tabela.eu.slovenija.preostalaevropa.ostalo, tabela.vsot.po.letih, by = 'leto') %>%
                                                          select(1, 2, 3, 5) %>%
                                                          mutate(odstotek = stevilo.x / stevilo.y) %>%
                                                          select(1, 2, 3, 5)

#Primerjave znotraj Evrope
#=========================

#Tabela števila tujih državljanov (vsa leta)
tabela.evropa <- tabela.leto.drzavljanstvo.stevilo %>%
                  filter(drzavljanstvo %in% drzave.evropa)


#Graf spremembe prebivalcev (Ostalo, EU, evropa brez EU)
#=======================================================

test1 <- tabela.eu.slovenija.preostalaevropa.ostalo %>%
          filter(drzavljanstvo == 'Ostalo')
test2 <- tabela.eu.slovenija.preostalaevropa.ostalo %>%
          filter(drzavljanstvo == 'Evropsko')
test3 <- tabela.eu.slovenija.preostalaevropa.ostalo %>%
          filter(drzavljanstvo == 'Evropa brez EU')

test4 <- left_join(test1, test2, by = 'leto')
test4 <- left_join(test4, test3, by = 'leto')

#! GRAF SPREMEMBE PREBIVALCEV GLEDE NA PREJŠNJE LETO
# plot(x = test4$leto, y = test4$stevilo.x,
#      ylim=range(0, 120000),
#      type = "b",
#      lwd = "2",
#      pch = 16,
#      xlab = 'Leto',
#      ylab = 'Število',
#      main = "Spremembe prebivalcev")
# 
# lines(x = test4$leto, y = test4$stevilo.y,
#       type = 'b',
#       lwd = "2",
#       pch = 16,
#       col = 'blue')
# 
# lines(x = test4$leto, y = test4$stevilo,
#       type ='b',
#       lwd = "2",
#       pch = 16,
#       col = 'green')
# 
# legend("topleft", 
#        legend=c("Ostalo", "Evropsko", 'Evropsko brez EU'),
#        col=c("black", "blue", 'green'),
#        lwd = "2",
#        cex=0.8)

# Graf spremembe prebivalcev
#===========================

slo.ostali <- tabela.leto.drzavljanstvo.stevilo %>% 
              group_by(leto, drzavljanstvo == "Slovenija") %>%
              summarise_at(vars(skupaj), list(skupaj = sum)) %>%
              add_column(rep(c("Ostalo", "Slovensko"), times = 9), .after = "leto") %>%
              ungroup() %>%
              select(1, 2, 4) %>%
              setNames(c("leto", "drzavljanstvo", "stevilo"))

#! GRAF DRŽAVLJANSTVA SLO, OSTALO
# # Tortni diagram (slo, ostalo)
# pie((slo.ostali %>% filter(leto == izbrano.leto))$stevilo,
#     labels = slo.ostali$drzavljanstvo,
#     col = c("darkred", "darkgreen"),
#     main ="Delež državljanstva")

#! GRAF PIE DRŽAVLJANSTVA SLO, EU, OSTALO
# # Tortni diagram (Slo, EU, ostalo)
# pie((tabela.eu.slovenija.preostalaevropa.ostalo %>% filter(leto == izbrano.leto))$stevilo,
#     labels = tabela.eu.slovenija.preostalaevropa.ostalo$drzavljanstvo,
#     col = c("black", "darkred", "darkblue", "darkgreen"),
#     main = "Državljanstva")

# Razbijmo "ostalo" na ene top 10 delov

ostali <- tabela.leto.drzavljanstvo.stevilo %>%
          filter(drzavljanstvo != "Slovenija") %>%
          group_by(leto, drzavljanstvo) %>%
          summarise_at(vars(skupaj), list(skupaj = sum))
          
razbitje.ostali <- top_n(ostali, 10)

ostanek2011 <- sum((ostali %>% filter(leto == 2011))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2011))$skupaj)
ostanek2012 <- sum((ostali %>% filter(leto == 2012))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2012))$skupaj)
ostanek2013 <- sum((ostali %>% filter(leto == 2013))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2013))$skupaj)
ostanek2014 <- sum((ostali %>% filter(leto == 2014))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2014))$skupaj)
ostanek2015 <- sum((ostali %>% filter(leto == 2015))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2015))$skupaj)
ostanek2016 <- sum((ostali %>% filter(leto == 2016))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2016))$skupaj)
ostanek2017 <- sum((ostali %>% filter(leto == 2017))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2017))$skupaj)
ostanek2018 <- sum((ostali %>% filter(leto == 2018))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2018))$skupaj)
ostanek2019 <- sum((ostali %>% filter(leto == 2019))$skupaj) - sum((razbitje.ostali %>% filter(leto == 2019))$skupaj)

razbitje.ostali <- razbitje.ostali %>%
                   ungroup() %>%
                   add_row(leto = 2011, drzavljanstvo = "Ostalo", skupaj = ostanek2011) %>%
                   add_row(leto = 2012, drzavljanstvo = "Ostalo", skupaj = ostanek2012) %>%
                   add_row(leto = 2013, drzavljanstvo = "Ostalo", skupaj = ostanek2013) %>%
                   add_row(leto = 2014, drzavljanstvo = "Ostalo", skupaj = ostanek2014) %>%
                   add_row(leto = 2015, drzavljanstvo = "Ostalo", skupaj = ostanek2015) %>%
                   add_row(leto = 2016, drzavljanstvo = "Ostalo", skupaj = ostanek2016) %>%
                   add_row(leto = 2017, drzavljanstvo = "Ostalo", skupaj = ostanek2017) %>%
                   add_row(leto = 2018, drzavljanstvo = "Ostalo", skupaj = ostanek2018) %>%
                   add_row(leto = 2019, drzavljanstvo = "Ostalo", skupaj = ostanek2019) %>%
                   arrange(leto, skupaj)

#! GRAF PIE PORAZDELITVE PREOSTALIH DRŽAVLJANSTEV
# # tortni diagram(brez slo, top_10 + ostalo)
# pie((razbitje.ostali %>% filter(leto == izbrano.leto))$skupaj,
#     labels = (razbitje.ostali %>% filter(leto == izbrano.leto))$drzavljanstvo,
#     topo.colors(11),
#     main = " Razporeditev prebivalcev brez Slovenskega državljanstva")

# Graf prirastkov drzavljanov glede na prejšnje leto
#===================================================

tabela.sprememba.leto <- left_join(tabela.evropa %>% filter(leto == 2011), tabela.evropa %>% filter(leto == 2012), by = "drzavljanstvo") %>%
                         left_join(tabela.evropa %>% filter(leto == 2013), by ="drzavljanstvo") %>%
                         left_join(tabela.evropa %>% filter(leto == 2014), by ="drzavljanstvo") %>%
                         left_join(tabela.evropa %>% filter(leto == 2015), by ="drzavljanstvo") %>%
                         left_join(tabela.evropa %>% filter(leto == 2016), by ="drzavljanstvo") %>%
                         left_join(tabela.evropa %>% filter(leto == 2017), by ="drzavljanstvo") %>%
                         left_join(tabela.evropa %>% filter(leto == 2018), by ="drzavljanstvo") %>%
                         left_join(tabela.evropa %>% filter(leto == 2019), by ="drzavljanstvo") %>%
                         select(2, 3, 5, 7, 9, 11, 13, 15, 17, 19) %>%
                         setNames(c('drzavljanstvo', 'stevilo2011', 'stevilo2012', 'stevilo2013', 'stevilo2014', 'stevilo2015',
                                  'stevilo2016', 'stevilo2017', 'stevilo2018', 'stevilo2019')) %>%
                         mutate(sprememba1112 = stevilo2012 - stevilo2011, 
                                sprememba1213 = stevilo2013 - stevilo2012,
                                sprememba1314 = stevilo2014 - stevilo2013,
                                sprememba1415 = stevilo2015 - stevilo2014,
                                sprememba1516 = stevilo2016 - stevilo2015,
                                sprememba1617 = stevilo2017 - stevilo2016,
                                sprememba1718 = stevilo2018 - stevilo2017,
                                sprememba1819 = stevilo2019 - stevilo2018) %>%
                        select(1, c(11:18)) %>%
                        filter(drzavljanstvo %in% c("Slovenija", "Hrvaška", "Srbija", "Kosovo", "Bosna in Hercegovina", "Severna Makedonija"))


# Zdaj imam tabelo, ki pove +- glede na spremembo državljanov neke države glede na prejšnje leto
# Naredimo graf iz največjih vrednosti torej:Slovenija, Bih, Hrvaška, Srbija, Makedonija, KOsovo

#! GRAF PRIRAST/UPAD ŠTEVILA DRŽAVLJANOV GLEDE NA PREJŠNJE LETO
# #Bosna in Hercegovina
# plot(x = c(2012:2019), y = tabela.sprememba.leto[1,][c(2:9)],
#      ylim=range(-10000, 10000),
#      type="b",
#      lwd = "2",
#      pch = 16,
#      col ='darkblue',
#      xlab="Leto",
#      ylab="Prirast/Upad državljanov",
#      main= "Prirast/Upad števila državljanov glede na prejšnje leto")
# 
# # Hrvaška
# lines(x = c(2012:2019), y = tabela.sprememba.leto[2,][c(2:9)],
#       type = 'b',
#       lwd = "2",
#       pch = 16,
#       col = 'red')
# 
# #Kosovo
# lines(x = c(2012:2019), y = tabela.sprememba.leto[3,][c(2:9)],
#       type = 'b',
#       lwd = "2",
#       pch = 16,
#       col = 'blue')
# 
# #Severna Makedonija
# lines(x = c(2012:2019), y = tabela.sprememba.leto[4,][c(2:9)],
#       type = 'b',
#       lwd = "2",
#       pch = 16,
#       col = 'green')
# 
# #Slovenija
# lines(x = c(2012:2019), y = tabela.sprememba.leto[5,][c(2:9)],
#       type = 'b',
#       lwd = "2",
#       pch = 16,
#       col = 'forestgreen')
# 
# #Srbija
# lines(x = c(2012:2019), y = tabela.sprememba.leto[6,][c(2:9)],
#       type = 'b',
#       lwd = "2",
#       pch = 16,
#       col = 'purple')
# 
# #legend
# legend("topleft",
#        title="Državljanstvo",
#        legend = c("Bosna in Hercegovina", "Hrvaška", "Kosovo", "Severna Makedonija", "Slovenija", "Srbija"),
#        col = c("darkblue", "red", "blue", "green", "forestgreen", "purple"),
#        lwd = "2",
#        cex=0.8)


#BARplot porazdelitve starosti za moške in ženske
#================================================

#SKUPAJ
# do poljubnega leta pridem vedno z 'izbrano.leto', določeno na vrhu

# summarise tukaj spremeni vrstni red vrstic, kar popravim ročno, zato dodam slice(match(x, stolpec))
prebivalstvo.skupaj <- prebivalstvo %>% 
                       group_by(spol, leto, starost) %>%
                       summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                       slice(match(c("0-4 let", "5-9 let", "10-14 let", "15-19 let", "20-24 let", "25-29 let", "30-34 let", 
                                     "35-39 let", "40-44 let", "45-49 let", "50-54 let", "55-59 let", "60-64 let", "65-69 let",
                                     "70-74 let", "75-79 let", "80-84 let", "85-89 let", "90 + let"), starost)) %>%
                       filter(leto == izbrano.leto)

prebivalstvoBAR <- rbind((prebivalstvo.skupaj %>% filter(spol == "Moški"))$stevilo,
                         (prebivalstvo.skupaj %>% filter(spol == "Ženske"))$stevilo)

# STAR! GRAF PORAZDELITEV PREBIVALCEV PO STAROSTI (VSI) še nepravilni
# barplot(prebivalstvoBAR, beside =TRUE,
#         col = c("blue", "red"),
#         main = "Porazdelitev prebivalcev po starosti (VSI)",
#         xlab = "Starost",
#         ylab = "Število prebivalcev",
#         names.arg = prebivalstvo.skupaj$starost[1:19])


# #! GRAF PORAZDELITEV PREBIVALCEV PO STAROSTI (VSI)
# #moški
# barplot(100 * prebivalstvoBAR[1,]/sum(prebivalstvoBAR),
#         horiz=TRUE,
#         xlim = c(-5, 5),
#         col = "lightblue",
#         main = "Porazdelitev prebivalcev po starosti (VSI)",
#         xlab = "Število prebivalcev",
#         las=1,
#         xaxt="n",
#         names.arg = prebivalstvo.skupaj$starost[1:19])
# #ženske
# barplot(100 * (-prebivalstvoBAR[2,]/sum(prebivalstvoBAR)),
#         add=TRUE,
#         horiz=TRUE,
#         col = "lightcoral",
#         las=1,
#         xaxt="n",
#         names.arg = prebivalstvo.skupaj$starost[1:19])
# 
# # ker je xlim nastavljena na [-5,5] se umetno nastavljena axis ujema
# axis(side = 1, at=(-5):5, labels=FALSE, tck=.02)
# mtext(paste0(c(5:1, 0:5), "%"), side = 1, line = 1, at=(-5):5, las=1)


# SLO
#====
slo.moski <- prebivalstvo %>% filter(drzavljanstvo == "Slovenija") %>% filter(spol == "Moški") %>% filter(leto == izbrano.leto)
slo.zenske <- prebivalstvo %>% filter(drzavljanstvo == "Slovenija") %>% filter(spol == "Ženske") %>% filter(leto == izbrano.leto)

# barplot(slo.moski$stevilo,
#         main = "Porazdelitev prebivalcev po starosti (SLO, moški)",
#         xlab = "Starost",
#         ylab = "Število prebivalcev",
#         names.arg = slo.moski$starost)
# 
# barplot(slo.zenske$stevilo,
#         main = "Porazdelitev prebivalcev po starosti (SLO, ženske)",
#         xlab = "Starost",
#         ylab = "Število prebivalcev",
#         names.arg = slo.zenske$starost)

# STAR! GRAF PORAZDELITEV PREBIVALCEV PO STAROSTI (SLOVENCI)
sloBAR <- rbind(slo.moski$stevilo, slo.zenske$stevilo)
# barplot(sloBAR, beside=TRUE,
#         col = c("blue", "red"),
#         main = "Porazdelitev prebivalcev po starosti (SLO)",
#         xlab = "Starost",
#         ylab = "Število prebivalcev",
#         names.arg = slo.moski$starost)

# #GRAF PORAZDELITEV PREBIVALCEV PO STAROSTI (SLOVENCI)
# #moški_slo
# barplot(100 * sloBAR[1,]/sum(prebivalstvoBAR),
#         horiz=TRUE,
#         xlim = c(-5, 5),
#         col = "lightblue",
#         main = "Porazdelitev prebivalcev po starosti (SLO)",
#         xlab = "Število prebivalcev",
#         las=1,
#         xaxt="n",
#         names.arg = prebivalstvo.skupaj$starost[1:19])
# #ženske_slo
# barplot(100 * (-sloBAR[2,]/sum(prebivalstvoBAR)),
#         add=TRUE,
#         horiz=TRUE,
#         col = "lightcoral",
#         las=1,
#         xaxt="n",
#         names.arg = prebivalstvo.skupaj$starost[1:19])
# # ker je xlim nastavljena na [-5,5] se umetno nastavljena axis ujema
# axis(side = 1, at=(-5):5, labels=FALSE, tck=.02)
# mtext(paste0(c(5:1, 0:5), "%"), side = 1, 1, at=(-5):5, las=1)

# NE SLO
#=======
# ponovno summarise sortira vrstni red po svoje
tujci.skupaj <- prebivalstvo %>% filter(drzavljanstvo != "Slovenija") %>%
                group_by(spol, leto , starost) %>%
                summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                slice(match(c("0-4 let", "5-9 let", "10-14 let", "15-19 let", "20-24 let", "25-29 let", "30-34 let", 
                              "35-39 let", "40-44 let", "45-49 let", "50-54 let", "55-59 let", "60-64 let", "65-69 let",
                              "70-74 let", "75-79 let", "80-84 let", "85-89 let", "90 + let"), starost)) %>%
                filter(leto == izbrano.leto)

tujciBAR <- rbind((tujci.skupaj %>% filter(spol == "Moški"))$stevilo,
                  (tujci.skupaj %>% filter(spol == "Ženske"))$stevilo)
# STAR! GRAF PORAZDELITEV PREBIVALCEV PO STAROST (SAMO TUJCI)
# barplot(tujciBAR, beside =TRUE,
#         col = c("blue", "red"),
#         main = "Porazdelitev prebivalcev po starosti (TUJCI)",
#         xlab = "Starost",
#         ylab = "Število prebivalcev",
#         names.arg = prebivalstvo.skupaj$starost[1:19])

# #! GRAF PORAZDELITEV PREBIVALCEV PO STAROST (SAMO TUJCI)
# #moški_tujci
# barplot(100 * tujciBAR[1,]/sum(prebivalstvoBAR),
#         horiz=TRUE,
#         xlim = c(-0.5, 0.5),
#         col = "lightblue",
#         main = "Porazdelitev prebivalcev po starosti (TUJCI)",
#         xlab = "Število prebivalcev",
#         las=1,
#         xaxt="n",
#         names.arg = prebivalstvo.skupaj$starost[1:19])
# #ženske_tujci
# barplot(100 * (-tujciBAR[2,]/sum(prebivalstvoBAR)),
#         add=TRUE,
#         horiz=TRUE,
#         col = "lightcoral",
#         las=1,
#         xaxt="n",
#         names.arg = prebivalstvo.skupaj$starost[1:19])
# # ker je xlim nastavljena na [-5,5] se umetno nastavljena axis ujema
# axis(side = 1, at=((-5):5)/10, labels=FALSE, tck=.02)
# mtext(paste0(c(5:1, 0:5)/10, "%"), side = 1, line = 1, at=((-5):5)/10, las=1)

# Nominalne spremembe prebivalcev
#================================

prebivalstvo.nom <- prebivalstvo %>% 
                    group_by(spol, leto, drzavljanstvo) %>%
                    summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                    spread(leto, stevilo)

# Slovenci in slovenke
# Graf, ki prikazuje nominalne vrednosti spremembe moškega in ženskega prebivalstva s slovenskim državljanstvom
#plot(x = 2011:2019,
#     y = (prebivalstvo.nom %>% filter(spol == "Moški", drzavljanstvo == "Slovenija"))[1,][c(3:11)],
#     ylim=range(940000, 1020000),
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "Število državljanov",
#     main= "Število prebivalcev in prebivalk s slovenskim državljanstvom"
#     )
#
#lines(x = 2011:2019,
#      y = (prebivalstvo.nom %>% filter(spol == "Ženske", drzavljanstvo == "Slovenija"))[1,][c(3:11)],
#      type= "b",
#      lwd = "2",
#      pch = 16,
#      col = 'lightcoral'
#      )
#
#
#legend("left",
#       title="Spol",
#       legend = c("Moški", "Ženske"),
#       col = c("lightblue", "lightcoral"),
#       lwd = "2",
#       cex=0.8)


#Spola skupaj (SLO)
prebivalstvo.nom.spola.skupaj <- prebivalstvo %>% 
                                 group_by(leto, drzavljanstvo) %>%
                                 summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                                 spread(leto, stevilo)

#!GRAF SAMO SLO DRŽAVLJANSTVO
#Graf, ki prikazuje nominalne vrednosti prebivalcev slovenije s slovenskim državljanstvom skozi leta 2011-2019
#plot(x = 2011:2019,
#     y = (prebivalstvo.nom.spola.skupaj %>% filter(drzavljanstvo == "Slovenija"))[1,][c(2:10)],
#     ylim=range(1940000, 2100000),
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "Število državljanov",
#     main= "Število državljanov s slovenskim državljanstvom"
#)

#Spola skupaj (VSI)
prebivalstvo.nom.spola.skupaj.vsi <- prebivalstvo %>% 
                                     group_by(leto) %>%
                                     summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                                     spread(leto, stevilo)

#! GRAF VSI PREBIVALCI SKUPAJ
#Graf, ki prikazuje nominalne vrednosti prebivalcev slovenije skozi leta 2011-2019
#plot(x = 2011:2019,
#     y = prebivalstvo.nom.spola.skupaj.vsi,
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "Število državljanov",
#     main= "Število prebivalcev v Sloveniji"
#)

### DODATEK k GRAFU SAMO SLO
#lines(x = 2011:2019,
#      y = prebivalstvo.nom.spola.skupaj.vsi,
#      type= "b",
#      lwd = "2",
#      pch = 16,
#      col = 'lightblue',
#      xlab= "Leto",)

prebivalstvo.nom.tujci.spola <- prebivalstvo %>%
                                filter(drzavljanstvo != "Slovenija") %>%
                                group_by(spol, leto) %>%
                                summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                                spread(leto, stevilo)

#! GRAF tujcev, ločeno moški in ženske
#plot(x = 2011:2019,
#     y = (prebivalstvo.nom.tujci.spola %>% filter(spol == "Moški"))[2:10],
#     ylim=range(24000, 91000),
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "Število",
#     main= "Število prebivalcev in prebivalk s tujim državljanstvom "
#)
#
#lines(x = 2011:2019,
#      y = (prebivalstvo.nom.tujci.spola %>% filter(spol == "Ženske"))[2:10],
#      type= "b",
#      lwd = "2",
#      pch = 16,
#      col = 'lightcoral',
#      xlab= "Leto",)
#
#legend("topleft",
#       title="Spol",
#       legend = c("Moški", "Ženske"),
#       col = c("lightblue", "lightcoral"),
#       lwd = "2",
#       cex=0.8)

# Tujci

prebivalstvo.nom.tujci <- prebivalstvo.nom %>%
                          filter(drzavljanstvo != "Slovenija") %>%
                          top_n(5)
# top_n sortiram kar po letu 2019, zato mi tega ni treba posebno dopisati

# GRAF prebivalk in prebivalcev v slov, samo državljani BIH (ni v poročilu)
# Posebi Bih
#plot(x = 2011:2019,
#     y = (prebivalstvo.nom.tujci %>% filter(spol == "Moški", drzavljanstvo == "Bosna in Hercegovina"))[1,][c(3:11)],
#     ylim=range(0, 50000),
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "Število državljanov",
#     main= "Število prebivalcev in prebivalk z državljanstvom Bih"
#)
#
#lines(x = 2011:2019,
#      y = (prebivalstvo.nom.tujci %>% filter(spol == "Ženske", drzavljanstvo == "Bosna in Hercegovina"))[1,][c(3:11)],
#      type= "b",
#      lwd = "2",
#      pch = 16,
#      col = 'lightcoral'
#)

# Preostalih top 5 držav (ni v poročilu)
#plot(x = 2011:2019,
#     y = (prebivalstvo.nom.tujci %>% filter(spol == "Moški", drzavljanstvo == "Hrvaška"))[1,][c(3:11)],
#     ylim=range(0, 12000),
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "Število državljanov",
#     main= "Število prebivalcev s tujim državljanstvom"
#)
#
#lines(x = 2011:2019,
#      y = (prebivalstvo.nom.tujci %>% filter(spol == "Moški", drzavljanstvo == "Kosovo"))[1,][c(3:11)],
#      type= "b",
#      lwd = "2",
#      pch = 16,
#      col = 'red'
#)
#
#lines(x = 2011:2019,
#      y = (prebivalstvo.nom.tujci %>% filter(spol == "Moški", drzavljanstvo == "Severna Makedonija"))[1,][c(3:11)],
#      type= "b",
#      lwd = "2",
#      pch = 16,
#     col = 'blue'
#)
#
#lines(x = 2011:2019,
#      y = (prebivalstvo.nom.tujci %>% filter(spol == "Moški", drzavljanstvo == "Srbija"))[1,][c(3:11)],
#      type= "b",
#      lwd = "2",
#     pch = 16,
#      col = 'orange'
#)

# Realne spremembe prebivalcev normirane na leto 2011
#====================================================

# dejmo naredit še realne grafe, ki jih lahko hitro ločimo na moške in ženske, vedno pa primerjam z letom 2011

prebivalstvo.real <- prebivalstvo %>% 
                     group_by(spol, leto, drzavljanstvo) %>%
                     summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                     ungroup() %>%
                     group_by(spol, leto, drzavljanstvo == "Slovenija") %>%
                     summarise_at(vars(stevilo), list(stevilo = sum)) %>%
                     ungroup() %>%
                     transmute(spol, leto, `drzavljanstvo == "Slovenija"`,
                               "delez" = stevilo / as.numeric(prebivalstvo.nom.spola.skupaj.vsi[1])
                               )

# y = vsota vseh prebivalcev
#plot(x = 2011:2019,
#     y = (prebivalstvo.real %>%
#         group_by(leto) %>%
#         summarise_at(vars(delez), list(stevilo = sum)))$stevilo,
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "",
#     las = 1,
#     main = "Delež prebivalstva glede na leto 2011"
#     )

# y = vsota vseh prebivalcev, ločeni po spolu
#plot(x = 2011:2019,
#     y = (prebivalstvo.real %>%
#            group_by(spol, leto) %>%
#            summarise_at(vars(delez), list(stevilo = sum)) %>%
#            filter(spol == "Moški"))$stevilo,
#     ylim=range(0.49, 0.51),
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "",
#     las = 1,
#     main = "Delež prebivalstva ločen po spolu"
#    )
#
#lines(x = 2011:2019,
#      y = (prebivalstvo.real %>%
#             group_by(spol, leto) %>%
#             summarise_at(vars(delez), list(stevilo = sum)) %>%
#             filter(spol == "Ženske"))$stevilo,
#      type= "b",
#      lwd = "2",
#      pch = 16,
#      col = 'lightcoral'
#     )
#
#legend("bottomright",
#       title="Spol",
#       legend = c("Moški", "Ženske"),
#       col = c("lightblue", "lightcoral"),
#       lwd = "2",
#       cex=0.8)

# y = vsota prebivalcev z slovenskim državljanstvom
#plot(x = 2011:2019,
#     y = (prebivalstvo.real %>%
#       group_by(leto, `drzavljanstvo == "Slovenija"`) %>%
#       summarise_at(vars(delez), list(stevilo = sum)) %>%
#         filter(`drzavljanstvo == "Slovenija"` == TRUE))$stevilo,
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "",
#     las = 1,
#     main = "Delež prebivalstva s slovenskim državljanstvom, glede na leto 2011"
#    )
#abline(h = 1, col="red", lty=2)

# y = vsota tujcev
#plot(x = 2011:2019,
#     y = (prebivalstvo.real %>%
#            group_by(leto, `drzavljanstvo == "Slovenija"`) %>%
#            summarise_at(vars(delez), list(stevilo = sum)) %>%
#            filter(`drzavljanstvo == "Slovenija"` == FALSE))$stevilo,
#     type= "b",
#     lwd = "2",
#     pch = 16,
#     col = 'lightblue',
#     xlab= "Leto",
#     ylab= "",
#     las = 1,
#     main = "Delež prebivalstva s tujim državljanstvom, glede na leto 2011"
#)