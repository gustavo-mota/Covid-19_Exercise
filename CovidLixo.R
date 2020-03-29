#Day who the spread begins in the most infected countries
day_countries <-  covid %>%
  filter(covid$`Countries and territories` %in% names(countr_cases[ind_OrdCounCas[1:10]]) & Cases > 0) %>%
  select(DateRep,Cases, `Countries and territories`)

for(country in names(countr_cases[ind_OrdCounCas[1:10]])){
  inde <- day_countries %>%
    filter(day_countries$`Countries and territories` == country) %>%
    select(Cases, DateRep)
  ind <- which.min(inde$Cases)
  new_row <- c(inde$DateRep[ind],
               inde$Cases[ind],
               country)
  df_day_initial <- rbind(df_day_initial, new_row)
  #day_countries[order(inde)]
  
}