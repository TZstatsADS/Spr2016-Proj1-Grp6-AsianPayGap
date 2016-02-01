library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(rCharts)
library(plotly)

# Read data
cols <- c('RAC1P', 'INDP', 'PERNP', 'PINCP', 'OCCP', 'WKHP', 'WKW', 'PWGTP', 'SCHL',
          'ST')
pusa <- fread('ss13pusa.csv', select = cols)
pusb <- fread('ss13pusb.csv', select = cols)
pus <- bind_rows(pusa, pusb)
rm(pusa, pusb)
str(pus)

#Remove rows with NA in pus
pus <- pus[complete.cases(pus),]

# Add state names and abbreviations
ST.anno = read.csv('statenames.csv', header = T)
pus <- mutate(pus, STname = ST.anno[ST, 2], STabbr = ST.anno[ST, 3])

# Extract Asians and Whites
pus <- filter(pus, RAC1P == 1 | RAC1P == 6)
pus$RAC1P <- as.factor(pus$RAC1P)
levels(pus$RAC1P) <- c('White', 'Asian')
# Extract high tech occupations
pus$OCCP <- ifelse(pus$OCCP >= 1005 & pus$OCCP <= 1240, 1, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 800 & pus$OCCP <= 950, 2, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 2105 & pus$OCCP <= 2160, 3, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 3000 & pus$OCCP <= 3540, 4, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 10 & pus$OCCP <= 430, 5, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 510 & pus$OCCP <= 740, 6, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 1300 & pus$OCCP <= 1560, 7, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 1600 & pus$OCCP <= 1965, 8, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 2000 & pus$OCCP <= 2060, 9, pus$OCCP)
pus <- filter(pus, OCCP %in% c(1, 2, 3, 4))
pus$OCCP <- as.factor(pus$OCCP)
levels(pus$OCCP) <- c('CMM', 'FIN', 'LGL', 'MED')

#Split-Apply-Combine
pus_race_occp <- ddply(pus, .(RAC1P, OCCP), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))

ENR_Race_Occp <- nPlot(MEAN ~ OCCP, group = 'RAC1P', data = pus_race_occp, type = 'multiBarChart')
ENR_Race_Occp



# Extract BS, MS, Phd
pus <- filter(pus, SCHL %in% c(21, 22, 24))
pus$SCHL <- as.factor(pus$SCHL)
levels(pus$SCHL) <- c('Bachelor', 'Master', 'Doctorate')
str(pus)


#Split-Apply-Combine
pus_race_edu <- ddply(pus, .(RAC1P, SCHL), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))

ENR_Race_Edu <- nPlot(MEAN ~ RAC1P, group = 'SCHL', data = pus_race_edu, type = 'multiBarChart')
ENR_Race_Edu

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





#Split-Apply-Combine
pus_race_indp <- ddply(pus, .(RAC1P, INDP), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))

#multiBarChart comparing PERNP
ENR_Race_Indp <- nPlot(MEAN ~ RAC1P, group = 'INDP', data = pus_race_indp, type = 'multiBarChart')
ENR_Race_Indp

# The different PERNP in diff state for Asians
asian <- filter(pus, RAC1P == 'Asian')
state_race_ENR <- ddply(asian, .(STname), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))
state_race_ENR$hover <- with(state_race_ENR, paste(state, '<br>', "MEAN", MEAN ))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

plot_ly(state_race_ENR, z = MEAN, text = hover, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = MEAN, colors = 'Blues',
        marker = list(line = l), colorbar = list(title = "USD")) %>%
    layout(title = 'Everage Income for Every State', geo = g)

# Working time
pus$WKW <- as.factor(pus$WKW)

