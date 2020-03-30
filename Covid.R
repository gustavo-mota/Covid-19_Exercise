#install.packages("tstools","plyr")

library(tstools)
require(stats)
library(plyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(base)

covid <- read_excel("RStudio/Covid-19_Task/COVID-19-geographic-disbtribution-worldwide-2020-03-17.xlsx")
View(covid)

# All the dates
dates <- unique(covid$DateRep[order(covid$DateRep)])
number_days <- nrow(dates)

# Numero de casos por dia no mundo
casos.dia <- tapply(covid$Cases,covid[,"DateRep"],sum)

countries_list <- unique(covid$`Countries and territories`)

# Infections per country
countr_cases <- tapply(covid$Cases,covid[,7],sum)

# Countries with infections only
countries_infected <- countr_cases[countr_cases != 0]
n_countries_infected <- nrow(countries_infected)

# Countries without infections
countries_winfections <- countr_cases[countr_cases == 0]

# Ordered index of cases per country
ind_OrdCounCas <- order(countr_cases, decreasing = TRUE)

#The 10 countries with most cases
top_10Most <- countr_cases[ind_OrdCounCas[1:10]]

#Number of infected countries per day
infected_countries_day <- tapply(covid$Cases > 0,covid$DateRep, sum)

# Countries with infections
infected_countries <- tapply(covid$`Countries and territories`, covid$Cases > 0, sum)

df.final <- data.frame(date=c(), country=c())
for(date in dates){
    ppp <-  covid %>%
            filter(Cases > 0 & DateRep == date) %>%
            select(`Countries and territories`)
    df.final <- rbind(df.final, date,nrow(ppp))
}

#df.infcday <- data.frame(date=c(),number=c())
#Cases per months
case_month <- tapply(covid$Cases, covid$Month, sum)

# Amostra
# Day of the first infection on countries
df.final2 <- data.frame(datee=character(0), country=character(0))
for(country in unique(covid$`Countries and territories`)){
  ppp <-  covid %>%
          filter(`Countries and territories` == country)
  indde <- order(ppp$DateRep, decreasing = FALSE)
  ind2 <- match(1, ppp$Cases[indde])
  Dattes <- ppp$DateRep[indde]
  #class(Dattes[ind2])
  newDF <- data.frame(Dattes[ind2],country)
  names(newDF) <- names(df.final2)
  df.final2 <- rbind(df.final2, newDF)
}
# Its wrong
first_infection <- df.final2

# Day of the first one death on countries
# Only the countries where the deaths starts with one victim
df.final3 <- data.frame(datee=character(0), country=character(0))
for(country in unique(covid$`Countries and territories`)){
  ppp <-  covid %>%
          filter(`Countries and territories` == country)
  indde <- order(ppp$DateRep, decreasing = FALSE)
  ind2 <- match(1, ppp$Deaths[indde])
  Dattes <- ppp$DateRep[indde]
  #class(Dattes[ind2])
  newDF <- data.frame(Dattes[ind2],country)
  names(newDF) <- names(df.final3)
  df.final3 <- rbind(df.final3, newDF)
}

#df.final3$country[!is.na(df.final3$datee)]
# Its wrong
first_deaths <- df.final3 %>%
                filter(!is.na(datee)) %>%
                select(datee, country)

# Number of deaths per country
mortes.pais <- tapply(covid$Deaths,covid[,7],sum)

# Only countries with deaths and their numbers
pais_mortes <- mortes.pais[which(mortes.pais > 0)]

# Number of countries with deaths
n_pais_mortes <- nrow(pais_mortes)

# Deaths per day
deaths.day <- tapply(covid$Deaths > 0,covid$DateRep, sum)

wd_countries <- tapply(covid$Deaths == 0,covid$DateRep, sum) #Acho que ta errado

# Paises sem mortes
pais_sem_mortes <- mortes.pais[which(mortes.pais == 0)]

# Numero de paises sem mortes
n_pais_sem_mortes <- nrow(pais_sem_mortes)

# Quantos pa?ses em m?dia possuem menos de 10 infectados
# M?dia de pa?ses com menos de 10 infectados


# Quantos foram infectados em m?dia por dia
# Average infections recorded per day
avg_infections_day <- mean(casos.dia)

# Quantos morreram em m?dia por dia
# Averages deaths recorded per day
avg_deaths_day <- mean(deaths.day)

# Quantos pa?ses em m?dia tiveram mortes
# M?dia de pa?ses com mortes
avg_countries_deaths <- n_pais_mortes/145

# How many countries on average had no deaths
# Average countries without deaths
avg_countries_wdeaths <- n_pais_sem_mortes/145

# Average countries with infections
avg_countries_infected <- n_countries_infected/145

# Average countries without infections
avg_countries_winfections <- 1 - avg_countries_infected

# Average deaths per country
avg_deaths_countries <- mean(mortes.pais)

# Average infections per country
avg_infections_country <- mean(countr_cases)

# Qual varia mais: mortes ou infec??es por pa?s?
# Coeficiente de varia??o infec??es e mortes por pa?s
cv_cases_country <- sd(countr_cases)/mean(countr_cases)
cv_deaths_country <- sd(mortes.pais)/mean(mortes.pais) # This

# Do deaths or infections vary more per day ?
cv_deaths_day <- sd(deaths.day)/avg_deaths_day # This
cv_cases_day <- sd(casos.dia)/avg_infections_day

# Quantos dias em m?dia da primeira infe??o at? a primeira morte
# M?dia do intervalo de dias desde infec??o at? a morte


# Plots
#Grafico feio
plot(1:5388,covid$Cases)

barplot(top_10Most)

plot(casos.dia,lty=1,type='l')

summary(casos.dia) # sumarios estatisticos

hist(casos.dia)

boxplot(casos.dia)