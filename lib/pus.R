library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(rCharts)
cols <- c('RAC1P', 'INDP', 'PERNP', 'PINCP', 'OCCP', 'WKHP', 'WKW', 'PWGTP')
pusa <- fread('ss13pusa.csv', select = cols)
pusb <- fread('ss13pusb.csv', select = cols)
pus <- bind_rows(pusa, pusb)
rm(pusa, pusb)
str(pus)
pus <- filter(pus, RAC1P == 1 | RAC1P == 6)
pus <- filter(pus, OCCP >= 1005 & OCCP <= 1240)

# INDP(Industry recode for 2013 and later based on 2012 IND codes)
pus$INDP <- ifelse(pus$INDP >= 170 & pus$INDP <= 290, 170, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 370 & pus$INDP <= 490, 370, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 570 & pus$INDP <= 770, 570, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 1070 & pus$INDP <= 3990, 1070, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 4070 & pus$INDP <= 6390, 4070, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 6470 & pus$INDP <= 6780, 6470, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 6870 & pus$INDP <= 7190, 6870, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7270 & pus$INDP <= 7790, 7270, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7860 & pus$INDP <= 7890, 7860, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7970 & pus$INDP <= 8290, 7970, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 8370 & pus$INDP <= 8470, 8370, pus$INDP)
pus$INDP <- ifelse(pus$INDP %in% c(8660, 8680, 8690), 8370, pus$INDP) 
pus$INDP <- ifelse(pus$INDP >= 8770 & pus$INDP <= 9290, 8370, pus$INDP)
pus$INDP <- ifelse(pus$INDP %in% c(8560, 8570, 8580, 8590, 8670), 8560, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9370 & pus$INDP <= 9590, 9370, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9670 & pus$INDP <= 9870, 9670, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9920, 9920, pus$INDP)
pus$INDP <- factor(pus$INDP)
levels(pus$INDP) <- c("Agriculture, Forestry, Fishing, Hunting", "Mining", "Utilities, Construction", 
                      "Manufacturing", "Trade, Logistic", "Information, Communications", "Finance",
                      "Professional", "Education", "Health", "Other Services",
                      "Arts, Entertainment", "Public Administration", "Military", "Unemployed"
)

#RAC1P
pus$RAC1P <- as.factor(pus$RAC1P)
levels(pus$RAC1P) <- c('White', 'Asian')

#Remove NA in PERNP
pus <- pus[!is.na(pus$PERNP),]

#Remove NA in INDP
pus <- pus[!is.na(pus$INDP),]

#Split-Apply-Combine
pus_test <- ddply(pus, .(RAC1P, INDP), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))

#multiBarChart
ENR_Comparison <- nPlot(MEAN ~ RAC1P, group = 'INDP', data = pus_test, type = 'multiBarChart')
ENR_Comparison