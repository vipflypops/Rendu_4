## Installation des library necessaires ##

library("knitr")
library("rmarkdown")
library("markdown")
library("lubridate")
library("forecast")
library("tidyverse")

## Creation d'une serie temporelle de CO2 ##

hawai <- read_csv("data/hawai.csv")
CO2_ts <- ts(c(hawai$CO2), start = c(1958, 3), frequency = 12)
CO2_ts

## Separation en partie d'entrainement et de test ## (pour avoir 70% des donnees en entrainement il faut finir en 1989)

hawai_ts_train <- window(CO2_ts, end = c(1989, 12))
hawai_ts_test <- window(CO2_ts, start = c(1990, 1)) 

## Creation d'un modele ETS pour les donnees d'entrainement ##

hawai_model <- ets(hawai_ts_train)
hawai_model
autoplot(hawai_model)

# On observe une hausse du niveau de CO2 de 1958 à 1990 et on voit une variation saisonniere du niveau de CO2.
# (M,Ad,M) signifie que l'erreur est multiplicative, la tendance est presente, et la saison est multiplicative.


## Projection de la prevision de CO2 atmosphérique ##

hawai_ets <- hawai_ts_train %>% ets()
hawai_fc <- hawai_ets %>% forecast(h=12*10)
hawai_fc %>% autoplot() 
summary(hawai_fc)

autoplot(CO2_ts)
autoplot(hawai_ts_test)

# Par comparaison, on peut voir que la prediction de l'ETS semble bonne pour la variation saisonniere.
# En revanche, on remarque globalement que les predictions sont en dessous de la realite.

# On peut qussi vérifier la précision de la prevision :

accuracy(hawai_fc, CO2_ts)


## Analyse des residus ##

checkresiduals(hawai_ets)

# On observe dans le graphique "Lag" que certaines données dépassent le seuil de confiance d'autocorrélation.
# Il existe donc une corrélation signidicative dans les données.
# Enfin, le graphique "residuals" semble graphiquement suivre une loi normale.

# Test de Shapiro # (pour vérification)

hawai_residu <- residuals(hawai_ets)
shapiro.test(hawai_residu)

# On obtient une p-value proche de zero, donc significative. L'echantillon ne suis donc pas une loi normale.

## Amelioration du modele ##

BoxCox.lambda(hawai_ts_train)

hawai_ets <- hawai_ts_train %>% ets(lambda = 0.1127)
hawai_fc <- hawai_ets %>% forecast(h=12*10)
hawai_fc %>% autoplot() 
summary(hawai_fc)

# Le resultat est bien mieux. On peut observer une forte diminution de la variabilite du modele (partie bleue)

