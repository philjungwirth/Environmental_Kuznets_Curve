#################################################
#################################################
######           Setup
#################################################
#################################################

#install.packages("wbstats")
#Info wbstats: https://cran.r-project.org/web/packages/wbstats/vignettes/wbstats.html

library(wbstats)
library(readxl)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stargazer)

rm(list = ls())
wd <- "C:/Users/philj/OneDrive/Dokumente/GitHub/Environmental_Kuznets_Curve"
setwd(wd)

#################################################


#################################################
#################################################
######           Load Data
#################################################
#################################################

### Search Data through the World Bank API 
#CO2 Emissions
C_indicator <- wb_search("CO2 emissions")
C_indicator <- C_indicator[11,]

#Trade openness (Trade/GDP)
TO_indicator <- wb_search("Trade")
TO_indicator <- TO_indicator[279,]

#Manufacturing value added
MVA_indicator <- wb_search("value added")
MVA_indicator <- MVA_indicator[49,]

indicators <- c(C_indicator$indicator_id, TO_indicator$indicator_id, 
                MVA_indicator$indicator_id) #Save indicators

#Define countries
geos_iso <- c("EGY", "IRN", "JOR", "MAR", "SAU", "TUR", "TUN")

#Define Date
start <- 1990
end <- 2020

#Download World Bank data
df <- wb_data(indicator = indicators, country = geos_iso, 
              start_date = start, end_date = end)


#Import Energy consumption from Our World In Data 
#https://ourworldindata.org/energy-production-consumption
E <- read.csv(paste0(wd, "/data/per_capita_energy_use.csv")) %>% 
  filter(Code %in% geos_iso) %>% 
  select(2:4) 
colnames(E) <- c("iso3c", "date", "E_kwh")


#Import Human Development Index
#https://hdr.undp.org/data-center/documentation-and-downloads
HDI <- read.csv(paste0(wd, "/data/HDI23_24.csv")) %>% 
  select(c(1,6:38)) %>% 
  filter(iso3 %in% geos_iso) %>% 
  pivot_longer(cols = 2:34, 
               names_to = "date",
               values_to = "HDI") %>% 
  mutate(date = as.numeric(sub(".*_", "", date)))

colnames(HDI) <- c("iso3c", "date", "HDI") 

#Import KOF index (Globalization index)
#https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-globalisation-index.html
KOF <- read_xlsx(paste0(wd, "/data/KOFGI_2023_public.xlsx")) %>% 
  select(c(1,3:4)) %>% 
  filter(code %in% geos_iso)
colnames(KOF) <- c("iso3c", "date", "KOF")

#Import GDP per capita data
#https://www.imf.org/external/datamapper/NGDPDPC@WEO/IRN/BHR/DZA/EGY/JOR/MAR/OMN/SAU/TUN/TUR
Y <- read_excel(paste0(wd, "/data/imf_gdp_per_capita.xls")) 
Y <- Y[2:11,] %>% 
  pivot_longer(cols = 2:51,
               names_to = "date",
               values_to = "Y") %>% 
  mutate(iso3c = "NULL") %>% 
  mutate(date = as.numeric(date))

colnames(Y) <- c("country", "date", "Y", "iso3c")

Y <- Y %>% 
  mutate(iso3c = case_when(
    country == "Morocco" ~ "MAR",
    country == "Saudi Arabia" ~ "SAU" ,
    country == "Egypt" ~ "EGY",
    country == "Iran" ~ "IRN",
    country == "Jordan" ~ "JOR",
    country == "Tunisia" ~ "TUN",
    country == "Türkiye, Republic of" ~ "TUR",
    TRUE ~ iso3c  # Keep existing values for other countries
  )) %>% 
  filter(iso3c %in% geos_iso) %>% 
  select(2:4)

#################################################

#################################################
#################################################
######           Join Data
#################################################
#################################################

df <- df %>% 
  left_join(Y, by = c("iso3c", "date")) %>% 
  left_join(E, by = c("iso3c", "date")) %>% 
  left_join(HDI, by = c("iso3c", "date")) %>% 
  left_join(KOF, by = c("iso3c", "date")) %>% 
  filter(date < 2020) #2020 removed due to COVID outliers 

#Renaming colums to make it easier in regressions
names <- colnames(df)
names <- c(names[1:4], "CO2", "TRD", "MVA", names[8:11])
colnames(df) <- names

attr(df$Y, "label") <- "GDP per capita (PPP $)"
attr(df$E_kwh, "label") <- "Energy consumption per capita (in kWh)"
attr(df$HDI, "label") <- "Human development index"
attr(df$KOF, "label") <- "KOF index"

df_log <- df %>% 
  mutate(Y = log(Y), Y2 = Y^2, CO2 = log(CO2), TRD = log(TRD), 
         MVA = log(MVA/100), E_kwh = log(E_kwh), HDI = log(HDI), 
         KOF = log(KOF))


country_codes <- c("Iran, Islamic Rep." = 1, "Tunisia" = 2, "Egypt, Arab Rep." = 3, 
                   "Morocco" = 4, "Jordan" = 5, "Turkiye" = 6, "Saudi Arabia" = 7)

# Add a new column 'country_code' based on the country mapping
df_log <- df_log %>%
  mutate(country_code = country_codes[country]) %>% 
  select(c(1:4, "CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF"))

write.csv(df_log, paste0(wd,"/output_data/","df_log.csv"))

