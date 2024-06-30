#R-Analysis Panel Data Project

library(wbstats)
library(readxl)
library(dplyr)
library(openxlsx)
library(magrittr)
library(tidyr)
library(plm)
library(pco)
library(cointReg)
library(urca)

rm(list = ls())
wd <- "C:/Users/philj/OneDrive/Dokumente/GitHub/Environmental_Kuznets_Curve"
setwd(wd)

df <- read.csv(paste0(wd,"/output_data/","df_log.csv"))
#Setup
df<-pdata.frame(df, index = c("country", "date"))
df%<>%select(-c("iso2c","iso3c"))


fm_KOF = CO2 ~ Y + Y2 + E_kwh + KOF + MVA + HDI

#Testing for Cross-sectional dependence Pesarans CD test:

pcdtest(fm_KOF,data=df,test='cd') #No rejection, we can use first generation Unit Root tests

#First Generation Unit Root Testing: 

#IPS Lag=1

results_IPS_L1 <- data.frame(variable = character(), test_statistic = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

for (variable in c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF")) {
  result <- purtest(df[[variable]], exo = 'trend', test = 'ips', lags = 1)
  test_statistic <- result$statistic$statistic
  p_value <- result$statistic$p.value
  results_IPS_L1 <- rbind(results_IPS_L1, data.frame(variable = variable, test_statistic = test_statistic, p_value = p_value))
}
rownames(results_IPS_L1) <- NULL

#IPS Lag=3

results_IPS_L3 <- data.frame(variable = character(), test_statistic = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

for (variable in c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF")) {
  result <- purtest(df[[variable]], exo = 'trend', test = 'ips', lags = 3)
  test_statistic <- result$statistic$statistic
  p_value <- result$statistic$p.value
  results_IPS_L3 <- rbind(results_IPS_L3, data.frame(variable = variable, test_statistic = test_statistic, p_value = p_value))
}
rownames(results_IPS_L3) <- NULL

#LLC Lags=AIC

results_LLC_AIC <- data.frame(variable = character(), test_statistic = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

for (variable in c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF")) {
  result <- purtest(df[[variable]], exo = 'trend', lags = 'AIC')
  test_statistic <- result$statistic$statistic
  p_value <- result$statistic$p.value
  results_LLC_AIC <- rbind(results_LLC_AIC, data.frame(variable = variable, test_statistic = test_statistic, p_value = p_value))
}
rownames(results_LLC_AIC) <- NULL

#LLC Lags=Hall

results_LLC_Hall <- data.frame(variable = character(), test_statistic = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

for (variable in c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF")) {
  result <- purtest(df[[variable]], exo = 'trend', lags = 'Hall')
  test_statistic <- result$statistic$statistic
  p_value <- result$statistic$p.value
  results_LLC_Hall <- rbind(results_LLC_Hall, data.frame(variable = variable, test_statistic = test_statistic, p_value = p_value))
}
rownames(results_LLC_Hall) <- NULL

#Pedroni Test: 

heho_KOF = array(c(as.double(df$CO2),as.double(df$Y),as.double(df$Y2),as.double(df$E_kwh),as.double(df$KOF),as.double(df$MVA),as.double(df$HDI)),dim=c(30,7,6))

pedroni99m(heho_KOF,type.stat=2) #We found evidence for cointegration as H0 -> no cointegration

#D-OLS: (Not preferred for models with multiple cointegrating variables)

X <- df[, c("Y","Y2", "E_kwh", "KOF", "MVA", "HDI")]%>%
  data.frame()
constant_vector <- rep(1, 210)
Y<-as.numeric(df$CO2)

cointRegD(x=X, y=Y,deter=constant_vector,info.crit='AIC')

#FM-OLS: (Not preferred for models with multiple cointegrating variables)

cointRegFM(x=X, y=Y,deter=constant_vector,info.crit='AIC')

#Breitung VAR: Draft

# Estimate cointegrating vectors for model fm_KOF
johansen_results_KOF <- ca.jo(df[, c("CO2", "Y","Y2", "E_kwh", "KOF", "MVA", "HDI")], ecdet = "const", type = "trace",K=2)
loading_matrix_KOF <- johansen_results_KOF@V
data_matrix_KOF <- as.matrix(df[, c("CO2", "Y","Y2", "E_kwh", "KOF", "MVA", "HDI")])
data_matrix_KOF <- cbind(1, data_matrix_KOF)
transformed_data_KOF <- as.data.frame(data_matrix_KOF %*% loading_matrix_KOF)
colnames(transformed_data_KOF)[1:7] <- c("CO2", "Y","Y2", "E_kwh", "KOF", "MVA", "HDI")
summary(johansen_results_KOF)

#Pool the transformed data and run pooled OLS regression
pooled_model_KOF <- lm(fm_KOF, data = transformed_data_KOF)

# Print cointegrating vectors
summary(pooled_model_KOF)


