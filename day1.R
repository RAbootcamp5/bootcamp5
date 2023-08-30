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
summary(est_3)


est_4 <- plm(formula = lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_4)
