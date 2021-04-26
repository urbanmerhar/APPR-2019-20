# 3. faza: Vizualizacija podatkov
#================================

# z zemljevidom predstavimo samo podatke za evropo, ker preostali svet nima toliko vrednosti v naših podatkih

library(ggplot2)
library(maps)
library(viridis)

# izbrano leto analizr
izbrano.leto.zem <- 2011


# Države v Evropi (dodana Turčija in brez evropskega dela Rusije)
evropske.drzave <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
                     "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                     "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands",
                     "Macedonia", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia",
                     "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", "Vatican City", "Kosovo")

# Želimo zemljevid evropskih držav
evropske.drzave.zemljevid <- map_data("world", region = evropske.drzave)
 
# # Center države za imenovanje
# # Koordinate centra države uporabimo za poimenovanje
# region.lab.data <- evropske.drzave.zemljevid %>%
#   group_by(region) %>%
#   summarise(long = mean(long), lat = mean(lat))
# 
# ggplot(evropske.drzave.zemljevid, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region)) +
#   #geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5) +
#   scale_fill_viridis_d() +
#   theme_void() +
#   theme(legend.position = "none")

# Zemljevid držav iz evropske unije
# vektor držav v EU
drzave.v.evropski.uniji <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                             "Greece", "Croatia", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta",
                             "Germany", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

drzave.v.evropski.uniji.zemljevid <- map_data("world", region = drzave.v.evropski.uniji)

# region.lab.data2 <- drzave.v.evropski.uniji.zemljevid %>%
#   group_by(region) %>%
#   summarise(long = mean(long), lat = mean(lat))
# 
# #pri data=imena držav
# ggplot(drzave.v.evropski.uniji.zemljevid, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region)) +
#   #geom_text(aes(label = region), data = region.lab.data2,  size = 3, hjust = 0.5) +
#   scale_fill_viridis_d() +
#   theme_void() +
#   theme(legend.position = "none")


### IGRAM SE
# vektor test so testne vrednosti za graf
# za podatke uporabim 'tabela.evropa'
# zemljevid zamuštra z norveško, ker nagaja, če izvzamem Svalbard...
evropske.drzave.ang <- c("Albania", "Andorra", "Austria", "Belgium", "Belarus", "Bulgaria", "Bosnia and Herzegovina", "Cyprus",
                         "Czech Republic", "Montenegro", "Denmark", "Estonia", "Finland", "France", "Greece", "Croatia", "Ireland", "Iceland", 
                         "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Hungary", "Malta", "Moldova", "Germany", "Netherlands",
                         "Norway", "Poland", "Portugal", "Romania", "San Marino", "Macedonia", "Slovakia", "Slovenia","Serbia",
                         "Spain", "Sweden", "Switzerland", "Ukraine", "UK")

evropske.drzave.zemljevid.ang <- map_data("world", region = evropske.drzave.ang)

# graf je filtriran za leto 2011
testZ <- tabela.evropa %>% 
          add_column(rep(evropske.drzave.ang, times=9), .after = 'skupaj') %>% filter(leto == izbrano.leto.zem)
test <- testZ$skupaj
# v grafih za poročilo, se zamenja vektor pri "vrednost"
test.tabela <- data.frame(region = as.character(evropske.drzave.ang),
                          vrednost = as.numeric(test))

test.tabela.zemljevid <- inner_join(evropske.drzave.zemljevid.ang, test.tabela, by = "region")

zemljevid.test <- ggplot(data = evropske.drzave.zemljevid.ang %>% filter(region != "Slovenia"), mapping = aes(x = long, y = lat, group = group)) +
                  scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
                  geom_polygon(data = test.tabela.zemljevid %>% filter(region != "Slovenia"), aes(fill= vrednost), color = "white") +
                  theme(
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank()
                  )


### Graf, ki ima samo države iz balkana (po WIKI https://en.wikipedia.org/wiki/Balkans)
# graf je filtriran za leto 2011
# PROBLEM GRAFA JE NJEGOVA SKALA; KER NASTANE NEPREGLEDNA

balkan <- c("Albanija", "Bolgarija", "Bosna in Hercegovina", "Črna gora", "Grčija", "Hrvaška", "Kosovo", "Severna Makedonija",
            "Srbija")

balkan.ang <- c("Albania", "Bulgaria", "Bosnia and Herzegovina", "Montenegro","Greece", "Croatia", "Kosovo", "Macedonia", "Serbia")

balkan.tabela <- tabela.evropa %>% filter(drzavljanstvo %in% balkan) %>%
                add_column(rep(balkan.ang, times=9), .after = 'skupaj') %>% filter(leto == izbrano.leto.zem)

balkanske.drzave.zemljevid <- map_data("world", region = balkan.ang)

balkan.tabela.zemljevid <- inner_join(balkanske.drzave.zemljevid, 
                                      data.frame(region = as.character(balkan.ang), vrednost = as.numeric(balkan.tabela$skupaj)), by= "region")

zemljevid.balkan <- ggplot(data = balkanske.drzave.zemljevid, mapping = aes(x = long, y = lat, group = group)) +
                    scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
                    geom_polygon(data = balkan.tabela.zemljevid, aes(fill= vrednost), color = "white") +
                    theme(
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title = element_blank()
                    ) + 
                    theme(legend.position = "right")

### GRAF, ki ima države balkana, brez BIH

balkan.brez.bih <- balkan[-3]
balkan.brez.bih.ang <- balkan.ang[-3]

balkan.brez.bih.tabela <- balkan.tabela %>% filter(drzavljanstvo %in% balkan.brez.bih)

balkan.brez.bih.zemljevid <- map_data("world", region = balkan.brez.bih.ang)

balkan.brez.bih.tabela.zemljevid <- inner_join(balkan.brez.bih.zemljevid,
                                              data.frame(region = as.character(balkan.brez.bih.ang), vrednost = as.numeric(balkan.brez.bih.tabela$skupaj)),
                                              by = "region")

zemljevid.balkan.brez.bih <- ggplot(data = balkan.brez.bih.zemljevid, mapping = aes(x = long, y = lat, group = group))+
                              scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
                              geom_polygon(data = balkan.brez.bih.tabela.zemljevid, aes(fill= vrednost), color = "white") +
                              theme(
                                axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                axis.title = element_blank()
                              ) +
                              theme(legend.position = "right")

### GRAF, ki ima samo države iz Evropske unije

drzave.eu <- c("Avstrija", "Belgija", "Bolgarija", "Hrvaška", "Ciper", 'Češka republika', "Danska", "Estonija", "Finska", "Francija",
               "Nemčija", "Grčija", "Madžarska", "Irska", "Italija", "Latvija", "Litva", "Luksemburg", "Malta", "Nizozemska",
               "Poljska", "Portugalska", "Romunija", "Slovaška", 'Slovenija', "Španija", "Švedska")

drzave.v.eu <- tabela.evropa %>% filter(drzavljanstvo %in% drzave.eu) %>%
                add_column(rep(drzave.v.evropski.uniji, times=9), .after = 'skupaj') %>% filter(leto == izbrano.leto.zem)

drzave.v.eu.zemljevid <- map_data("world", region = drzave.v.evropski.uniji)

drzave.v.eu.tabela.zemljevid <- inner_join(drzave.v.eu.zemljevid,
                                           data.frame(region = as.character(drzave.v.evropski.uniji), vrednost = as.numeric(drzave.v.eu$skupaj)),
                                           by = "region")

zemljevid.eu <- ggplot(data = drzave.v.eu.zemljevid %>% filter(region != "Slovenia"), mapping = aes(x = long, y = lat, group = group))+
                geom_polygon(color = "black", fill = NA) +
                geom_polygon(data = drzave.v.eu.tabela.zemljevid %>% filter(region != "Slovenia"), aes(fill= vrednost), color = "white") +
                theme(
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank()
                ) +
                scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
                theme(legend.position = "right")

### Še zemljevid brez slo in hrv

zemljevid.eu.brez.hrvaske <- ggplot(data = drzave.v.eu.zemljevid %>% filter(region != "Slovenia") %>% filter(region != "Croatia"), mapping = aes(x = long, y = lat, group = group))+
  geom_polygon(color = "black", fill = NA) +
  geom_polygon(data = drzave.v.eu.tabela.zemljevid %>% filter(region != "Slovenia") %>% filter(region != "Croatia"), aes(fill= vrednost), color = "white") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  scale_fill_gradient(name = "Vrednost", low = "lightblue", high = "darkblue", na.value = "grey50") +
  theme(legend.position = "right")