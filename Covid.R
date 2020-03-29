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

#Grafico feio
plot(1:5388,covid$Cases)

# Numero de casos por dia no mundo
casos.dia <- tapply(covid$Cases,covid[,"DateRep"],sum)

# Infectados por pa?s
countr_cases <- tapply(covid$Cases,covid[,7],sum)

# Ordered cases per country
ind_OrdCounCas <- order(countr_cases, decreasing = TRUE)

#The 10 countries with most cases
top_10Most <- countr_cases[ind_OrdCounCas[1:10]]

barplot(top_10Most)

#Number of infected countries per day

dates <- unique(covid$DateRep[order(covid$DateRep)])
number_days <- nrow(dates)

#Lista de paises
countries_list <- unique(covid$`Countries and territories`)

infected_countries_day <- tapply(covid$Cases > 0,covid$DateRep, sum)

# Countries with infections
infected_countries <- tapply(covid$`Countries and territories`, covid$Cases > 0, sum)

#chek
unfected_countries <- tapply(covid$Cases == 0,covid$DateRep, sum)
#check end

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

# Days interval until last date
last_date <- covid$DateRep[1]
first_date <- covid$DateRep[nrow(covid)]

#Plot interval of days since spread begins in the country  
#Days of infection in th most infected countries

plot(casos.dia,lty=1,type='l')

summary(casos.dia) # sumarios estatisticos

hist(casos.dia)

boxplot(casos.dia)

# Day of the first death on countries
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

first_deaths <- df.final3 %>%
                filter(!is.na(datee)) %>%
                select(datee, country)

# Numero de mortes por pais
mortes.pais <- tapply(covid$Deaths,covid[,7],sum)

# Paises com mortes
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

#Medidas de posi??o

# M?dia de pa?ses infectados
md_infected <- mean()

# Quantos pa?ses em m?dia possuem menos de 10 infectados
# M?dia de pa?ses com menos de 10 infectados


# Quantos morreram em m?dia por pa?s
# M?dia de mortes


# Quantos foram infectados em m?dia por pa?s
# M?dia de infectados


# Quantos foram infectados em m?dia por dia
# Average infections recorded per day
avg_infections_day <- mean(casos.dia)

# Quantos morreram em m?dia por dia
# Averages deaths recorded per day
avg_deaths_day <- mean(deaths.day)

# Quantos pa?ses em m?dia foram infectados
# M?dia de pa?ses infectados


# Quantos pa?ses em m?dia tiveram mortes
# M?dia de pa?ses com mortes

# Qual a moda de mortes
# Moda em Deaths
# Descobre quantas mortes se tem maior frequ?ncia e se compara com a m?dia para descobrir que isso ocorre apenas no come?o


# Qual varia mais: mortes ou infec??es por pa?s?
# Coeficiente de varia??o infec??es e mortes por pa?s


# Qual varia mais: mortes ou infec??es por dia?
# Coeficiente de varia??o infec??es e mortes por dia


# Quantos dias em m?dia da primeira infe??o at? a primeira morte
# M?dia do intervalo de dias desde infec??o at? a morte

