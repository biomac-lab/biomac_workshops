## BIOMAC Workshop: Time Series Analysis

# Carlos Bravo-Vega, Juan D. Umaña, Nicolás Rodriguez

# R script for dengue data import and preparation


## 0. packages ####

# Data import and preparation
# install.packages("remotes")
# remotes::install_github("epiverse-trace/sivirep")
library(sivirep)
library(dplyr)
library(lubridate)
library(rlang)


# setwd("~/outbreak_features_dengue/") # Local path settings

## 1. epidata - Mild, severe and mortality due to dengue (2007-2021) ####
# source: http://portalsivigila.ins.gov.co/Paginas/Buscador.aspx

years_to_analyze <- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
                      2015, 2016, 2017, 2018, 2019, 2020, 2021)

events_to_analyze <- c("DENGUE", "DENGUE GRAVE", "MORTALIDAD POR DENGUE")

tags_to_analyze <- c("CONSECUTIVE", "FEC_NOT", "EDAD", "UNI_MED", "SEXO", "OCUPACION", 
                     "COD_PAIS_O", "COD_DPTO_O", "COD_MUN_O",
                     "COD_DPTO_R", "COD_MUN_R",
                     "COD_DPTO_N", "COD_MUN_N")

data_Dengue <- data.frame(matrix(ncol = length(tags_to_analyze), nrow = 0))
colnames(data_Dengue) <- tags_to_analyze

for(y in years_to_analyze)
{
  for(e in events_to_analyze)
  {
    temp_data_Dengue <- sivirep::import_data_disease_by_year(y,e)
    temp_data_Dengue$FEC_NOT <- as.character(temp_data_Dengue$FEC_NOT)
    temp_data_Dengue$FEC_NOT <- format(as.Date(temp_data_Dengue$FEC_NOT, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),"%Y-%m-%d")
    data_Dengue <- rbind(data_Dengue, dplyr::select(temp_data_Dengue,tags_to_analyze))
  }
}

data_Dengue <- data_Dengue %>%
  mutate(
    #Ages cleaning
    
    EDAD_A = ifelse(UNI_MED==1,EDAD,
                    ifelse(UNI_MED==2,EDAD/12,
                           ifelse(UNI_MED==3,EDAD/365,
                                  ifelse(UNI_MED==4,EDAD/8760,
                                         ifelse(UNI_MED==5,EDAD/525600,
                                                NA))))),
    
    #Codes data cleaning
    COD_MUN_R = ifelse((floor(log10(COD_MUN_R)) + 1)==1, as.numeric(paste(COD_DPTO_R,COD_MUN_R, sep="00")),
                       ifelse((floor(log10(COD_MUN_R)) + 1)==2, as.numeric(paste(COD_DPTO_R,COD_MUN_R, sep="0")),
                              ifelse((floor(log10(COD_MUN_R)) + 1)==3, as.numeric(paste(COD_DPTO_R,COD_MUN_R, sep="")),
                                     NA))),
    
    COD_MUN_O = ifelse((floor(log10(COD_MUN_O)) + 1)==1, as.numeric(paste(COD_DPTO_O,COD_MUN_O, sep="00")),
                       ifelse((floor(log10(COD_MUN_O)) + 1)==2, as.numeric(paste(COD_DPTO_O,COD_MUN_O, sep="0")),
                              ifelse((floor(log10(COD_MUN_O)) + 1)==3, as.numeric(paste(COD_DPTO_O,COD_MUN_O, sep="")),
                                     NA))),
    
    COD_DPTO_O = ifelse(COD_DPTO_O!=1, COD_DPTO_O,
                        ifelse(COD_PAIS_O==862,1862,
                               ifelse(COD_PAIS_O==76,176,
                                      1)))
  )

data_Dengue$MES_NOT <- month(data_Dengue$FEC_NOT)
data_Dengue$ANO_NOT <- year(data_Dengue$FEC_NOT)

# Cleaning of cases without specified municipalities or departments of occurrence
data_Dengue <-  subset(data_Dengue, !is.na(data_Dengue$COD_MUN_O))
data_Dengue <-  subset(data_Dengue, !is.na(data_Dengue$COD_DPTO_O))

# National incidence

inc_COL <- incidence::incidence(data_Dengue$FEC_NOT, interval = "1 epiweek")
plot(inc_COL)
