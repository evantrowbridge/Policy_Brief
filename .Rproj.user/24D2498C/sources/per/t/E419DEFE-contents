library(tidyverse)
library(WDI) # A package that uses the World Bank's data API
library(plm)
library(stargazer)

# Electric Power Consumption (kWh per capita)
df_elec <- WDI(
  indicator = "EG.USE.ELEC.KH.PC", 
  country = c('HN','GT','SV','NI','CR'), 
  start = 1980, end=2014) %>% 
  select(-iso2c) %>% 
  rename(kwh_per_cap = EG.USE.ELEC.KH.PC)

# Primary School Completion Rate
df_edu <- WDI(
  indicator = "SE.PRM.CMPT.ZS", 
  country = c('HN','GT','SV','NI','CR'), 
  start = 1980, end=2014) %>% 
  select(-iso2c) %>% 
  rename(edu_comp = SE.PRM.CMPT.ZS)

df_all <- full_join(df_elec, df_edu, by = c("country", "year"))

df_all %>%
  ggplot(aes(x = kwh_per_cap, y = edu_comp, col = country)) +
  geom_point() +
  facet_wrap(vars(country)) +
  stat_smooth(method = "lm", se = FALSE) +  
  labs(
    x = "Electric Power Consumption (kWh per capita)",
    y = "Primary School Completion Rate",
    col = ""
  ) +
  theme(legend.position = "none")

p_all <- pdata.frame(df_all, index = c("country", "year"))

model_all <- plm(edu_comp ~ kwh_per_cap, data = p_all, model = "within")

summary(model_all)

stargazer(model_all, 
          title = "Electricity and Education in Central America", 
          type = "html",
          covariate.labels = "Electric Power Consumption (kWh per capita)",
          dep.var.labels = "Primary School Completion Rate",
          out="edu_elec.html")
#####


