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

# Basics plots
plot(1:5388,covid$Cases)

# All the dates
dates <- unique(covid$DateRep[order(covid$DateRep)])
number_days <- nrow(dates)

# Number of cases per day
cases.day <- tapply(covid$Cases,covid[,"DateRep"],sum)
plot(cases.day,lty=1,type='l')

summary(cases.day) # sumarios estatisticos
hist(cases.day)
boxplot(cases.day)

countries_list <- unique(covid$`Countries and territories`)

# Infections per country
countr_cases <- tapply(covid$Cases,covid[,8],sum)
pie(countr_cases[countr_cases > 1000])
pie(countr_cases[countr_cases < 1000 & countr_cases > 200])

# Countries with infections only
countries_infected <- countr_cases[countr_cases != 0]
n_countries_infected <- nrow(countries_infected)

# Countries without infections
countries_winfections <- countr_cases[countr_cases == 0]

ind.countrcases <- order(countr_cases, decreasing = TRUE) # Ordered index of cases per country

#The 10 countries with most cases
top_10Most <- countr_cases[ind.countrcases[1:10]]
barplot(top_10Most)

#Number of infected countries per day
infected_countries_day <- tapply(covid$Cases > 0,covid$DateRep, sum)
plot(infected_countries_day)

# Countries with infections
infected_countries <- tapply(covid$`Countries and territories`, covid$Cases > 0, sum)

#Cases per months
case_month <- tapply(covid$Cases, covid$Month, sum)

# Amostra
# Day of the first infection on countries
first_infection <- data.frame(datee=character(0), country=character(0))
for(country in unique(covid$`Countries and territories`)){
  ppp <-  covid %>%
          filter(`Countries and territories` == country)
  indde <- order(ppp$DateRep, decreasing = FALSE)
  ind2 <- match(1, ppp$Cases[indde])
  Dattes <- ppp$DateRep[indde]
  #class(Dattes[ind2])
  newDF <- data.frame(Dattes[ind2],country)
  names(newDF) <- names(first_infection)
  first_infection <- rbind(first_infection, newDF)
}
# Its wrong

# Day of the first one death on countries
# Only the countries where the deaths starts with one victim
first.deaths <- data.frame(datee=character(0), country=character(0))
for(country in unique(covid$`Countries and territories`)){
  ppp <-  covid %>%
          filter(`Countries and territories` == country)
  indde <- order(ppp$DateRep, decreasing = FALSE)
  ind2 <- match(1, ppp$Deaths[indde])
  Dattes <- ppp$DateRep[indde]
  
  newDF <- data.frame(Dattes[ind2],country)
  names(newDF) <- names(first.deaths)
  first.deaths <- rbind(first.deaths, newDF)
}
# Its wrong

#first.deaths$country[!is.na(first.deaths$datee)]
first.deaths <- first.deaths %>%
                filter(!is.na(datee)) %>%
                select(datee, country)

# Number of deaths per country
deaths.country <- tapply(covid$Deaths,covid[,8],sum)
pie(deaths.country[deaths.country > 50])

# Only countries with deaths and their numbers
pais_mortes <- deaths.country[which(deaths.country > 0)]

# Number of countries with deaths
n_pais_mortes <- nrow(pais_mortes)

# Deaths per day
deaths.day <- tapply(covid$Deaths,covid$DateRep, sum)

# Number of countries with deaths per day
deaths.country <- tapply(covid$Deaths > 0,covid$DateRep, sum)
plot(deaths.country)

wd_countries <- tapply(covid$Deaths == 0,covid$DateRep, sum) #Acho que ta errado

# Paises sem mortes
pais_sem_mortes <- deaths.country[which(deaths.country == 0)]
n_pais_sem_mortes <- nrow(pais_sem_mortes)

# Quantos pa?ses em m?dia possuem menos de 10 infectados
# M?dia de pa?ses com menos de 10 infectados

# Mortos por infectado e por dia
tax_mort <-  data.frame(PerInfect=(deaths.country*100)/countr_cases, perday=deaths.country/78)

# General mortality
tax_genDeath <- sum(covid$Deaths)*100/sum(covid$Cases)

# Quantos foram infectados em m?dia por dia
# Average infections recorded per day
avg_infections_day <- mean(cases.day)

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
avg_deaths_countries <- mean(deaths.country)

# Average infections per country
avg_infections_country <- mean(countr_cases)

# Qual varia mais: mortes ou infec??es por pa?s?
# Coeficiente de varia??o infec??es e mortes por pa?s
cv_cases_country <- sd(countr_cases)/mean(countr_cases)
cv_deaths_country <- sd(deaths.country)/mean(deaths.country) # This

# Do deaths or infections vary more per day ?
cv_deaths_day <- sd(deaths.day)/avg_deaths_day # This
cv_cases_day <- sd(cases.day)/avg_infections_day

# Quantos dias em m?dia da primeira infe??o at? a primeira morte
# M?dia do intervalo de dias desde infec??o at? a morte

