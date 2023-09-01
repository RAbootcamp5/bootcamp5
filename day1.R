install.packages("tidyverse")

library(tidyverse)
NTL_1993 <- read_csv("Data/Extensionzonalstat_101993.csv")
GDP_raw <- readxl::read_xlsx("Data/WDI_2014_11.xlsx", sheet = 6)
GDP_NY <- GDP_raw %>% 
  filter(`Indicator Code`== "NY.GDP.MKTP.KN")
Fiw <- read_csv("Data/FIW.csv")

Estimations_data <- read_dta("Data/Estimations.dta")
library(haven)
??view_df
install.packages("sjPlot")
library(sjPlot)
view_df(Estimations_data)

#使用変数名
#lndn13,fiw, lngdp14,autocracyFH



install.packages("plm")
library(plm)
?plm
est_3 <- plm(formula = lngdp14 ~ lndn13 + fiw + lndn13*fiw, effect = "twoways",
                         data = Estimations_data,model = "within", index = c("countryname","year"))
summary(est_3, cluster = "countryname")
est_3 <- plm(formula = lngdp14 ~ lndn13 + fiw + lndn13*fiw, effect = "twoways",
             data = Estimations_data,model = "within", index = c("countryname","year"))
coeftest(est_3, vcov = vcovHC)
est_4 <- plm(formula = lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_4)
coeftest(est_4, vcov = vcovHC)

est_1 <- plm(formula = lngdp14 ~ lndn13, effect = "twoways",
             data = Estimations_data,model = "within", index = c("countryname","year"))
summary(est_1)
est_2 <- plm(formula = lngdp14 ~ lndn13 + fiw, effect = "twoways",
             data = Estimations_data,model = "within", index = c("countryname","year"))
summary(est_2)
coeftest(est_2, vcov = vcovHC)
##################################################################
es_1184 <- Estimations_data %>% 
  select(countryname,year,lngdp14,lndn13,fiw)  %>% 
  mutate(sample=ifelse(is.na(fiw),0,1)) %>% 
  group_by(countryname) %>% 
  mutate(insubset=ifelse(sum(sample)>7,T,F)) %>% 
  filter(insubset) %>% 
  filter(year > 1991, year < 2014) 

# Table1_(1) --------------------------------------------------------------


ungroup() 


library(naniar)
n_complete(Estimations_data$fiw)
View(miss_var_summary(es_1184))

es_1184 <- Estimations_data %>% 
  select(countryname,year,lngdp14,lndn13,fiw)  %>% 
  drop_na(fiw)
est_1 <- plm(formula = lngdp14 ~ lndn13, effect = "twoways",
             data = es_1184,model = "within", index = c("countryname","year"))
summary(est_1)

?coeftest
library(lmtest)
coeftest(est_1, vcov = vcovHC)

est_5 <- plm(formula = lngdp14 ~ lndn13 + Dpfree+Dnfree + lndn13*Dpfree+lndn13*Dnfree, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_5)
coeftest(est_5, vcov = vcovHC)

est_6 <- plm(formula = lngdp14 ~ lndn13 + autocracyFH+lndn13*autocracyFH, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_6)
coeftest(est_6, vcov = vcovHC)
names(Estimations_data)

est_7 <- plm(formula = lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_7)
coeftest(est_7, vcov = vcovHC)

est_7 <- plm(formula = lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, effect = "twoways",
               data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_7)

fiw7 <- Estimations_data %>%
  select(year, countryname, fiw) %>% 
  pivot_wider(names_from = year, values_from = fiw) %>% 
  rename(fiw_1992="1992", fiw_1993="1993", fiw_2012="2012", fiw_2013="2013")

fiw7 <- fiw7 %>% 
  mutate("9293"=rowSums(fiw7[, c("fiw_1992", "fiw_1993")])/2,
         "1213"=rowSums(fiw7[, c("fiw_2012", "fiw_2013")])/2) %>% 
  select(countryname, "9293", "1213") #%>% 
  #drop_na("9293") %>% 
  #drop_na("1213")

fiw7 <- fiw7 %>% 
  pivot_longer(cols = c("9293", "1213"), names_to = "year", values_to = "fiw")

ntl7 <- Estimations_data %>%
  select(year, countryname, lndn13) %>% 
  pivot_wider(names_from = year, values_from = lndn13) %>% 
  rename(lndn_1992="1992", lndn_1993="1993" , lndn_2012="2012", lndn_2013="2013")

ntl7 <- ntl7 %>% 
  mutate("9293"=rowSums(ntl7[, c("lndn_1992", "lndn_1993")])/2,
         "1213"=rowSums(ntl7[, c("lndn_2012", "lndn_2013")])/2) %>% 
  select(countryname, "9293", "1213") %>% 
  drop_na("9293") %>% 
  drop_na("1213")

ntl7 <- ntl7 %>% 
  pivot_longer(cols = c("9293", "1213"), names_to = "year", values_to = "ntl")

GDP7 <- Estimations_data %>%
  select(year, countryname, lngdp14) %>% 
  pivot_wider(names_from = year, values_from = lngdp14) %>% 
  rename(lngdp14_1992="1992", lngdp14_1993="1993" , lngdp14_2012="2012", lngdp14_2013="2013")

GDP7 <- GDP7 %>% 
  mutate("9293"=rowSums(GDP7[, c("lngdp14_1992", "lngdp14_1993")])/2,
         "1213"=rowSums(GDP7[, c("lngdp14_2012", "lngdp14_2013")])/2) %>% 
  select(countryname, "9293", "1213") %>% 
  drop_na("9293") %>% 
  drop_na("1213")

GDP7 <- GDP7 %>% 
  pivot_longer(cols = c("9293", "1213"), names_to = "year", values_to = "gdp")

Table1_7 <- GDP7 %>% 
  left_join(fiw7, by = c("countryname", "year")) %>% 
  left_join(ntl7, by = c("countryname", "year")) #%>% 
  #drop_na(fiw) %>% 
  #drop_na(ntl)

est_7 <- plm(formula = gdp ~ ntl + fiw + I(fiw^2) + ntl*fiw, effect = "twoways",
             data = Table1_7, model = "within", index = c("countryname","year"))
summary(est_7)
coeftest(est_7, vcov = vcovHC)

install.packages("quantreg")
library(quantreg)

quantile(es_1184$fiw, probs = c(0.25, 0.75))
quantile(Estimations_data$fiw, probs = c(0.25, 0.75),na.rm = T)

df_1 <- Estimations_data %>% 
  select("countryname","year","lngdp14", "lndn13", "fiw") %>% 
  drop_na("lngdp14") %>% 
  drop_na("lndn13") %>% 
  drop_na("fiw")

qt_fiw_0.25 <- quantile(df_1$fiw, probs = c(0.25),na.rm = T)
qt_fiw_0.75 <- quantile(df_1$fiw, probs = c(0.75),na.rm = T)
Sigma_4 <- (est_4$coefficients[4]/est_4$coefficients[1])*(qt_fiw_0.75 - qt_fiw_0.25)
Sigma_3 <- (est_3$coefficients[3]/est_3$coefficients[1])*(qt_fiw_0.75 - qt_fiw_0.25)

