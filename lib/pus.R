library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(rCharts)
library(plotly)
library(gridExtra)

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

# Extract the Asians and Whites data
pus <- filter(pus, RAC1P == 1 | RAC1P == 6)
pus$RAC1P <- as.factor(pus$RAC1P)
levels(pus$RAC1P) <- c('White', 'Asian')

# Recode OCCP and choose the OCCP with high salary
pus$OCCP <- ifelse(pus$OCCP >= 10 & pus$OCCP <= 430, 1, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 1005 & pus$OCCP <= 1240, 2, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 800 & pus$OCCP <= 950, 3, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 2105 & pus$OCCP <= 2160, 4, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 3000 & pus$OCCP <= 3540, 5, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 510 & pus$OCCP <= 740, 6, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 1300 & pus$OCCP <= 1560, 7, pus$OCCP)
pus$OCCP <- ifelse(pus$OCCP >= 1600 & pus$OCCP <= 1965, 8, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 2000 & pus$OCCP <= 2060, 9, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 2200 & pus$OCCP <=2550, 10, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 2600 & pus$OCCP <=2920, 11, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 3600 & pus$OCCP <= 3655, 12, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 3700 & pus$OCCP <=3955, 13, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 4000 & pus$OCCP <=4150, 14, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 4200 & pus$OCCP <=4250, 15, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 4300 & pus$OCCP <=4650, 16, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >=4700 & pus$OCCP <=4965, 17, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >=5000 & pus$OCCP <=5940, 18, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 6005 & pus$OCCP <=6130, 19, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 6200 & pus$OCCP <=6765, 20, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 6800 & pus$OCCP <=6940, 21, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 7000 & pus$OCCP <=7630, 22, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 7700 & pus$OCCP <=8965,23, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >= 9000 & pus$OCCP <=9750, 24, pus$OCCP)
# pus$OCCP <- ifelse(pus$OCCP >=9800 & pus$OCCP <= 9830, 25, pus$OCCP)
# pus <- filter(pus, OCCP %in% c(1:25))
pus <- filter(pus, OCCP %in% c(1:8))
pus$OCCP <- as.factor(pus$OCCP)
# levels(pus$OCCP) <- c('MGR', 'CMM', 'FIN', 'LGL', 'MED' , 'BUS', 'ENG', 'SCI', 'CMS', 'EDU', 'ENT', 'HLS', 'PRT', 'EAT', 'CLN', 'PRS', 'SAL', 'OFF', 'FFF', 'CON', 'EXT', 'RPR', 'PRD', 'TRN', "MIL")
levels(pus$OCCP) <- c('MGR', 'CMM', 'FIN', 'LGL', 'MED' , 'BUS', 'ENG', 'SCI')


# Use Bar chart to compare the Asian's and White's salary in these OCCPs
pus_race_occp <- ddply(pus, .(RAC1P, OCCP), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))
ggplot(pus_race_occp, aes(x=OCCP  , y=MEAN, fill=factor(RAC1P))) +
    geom_bar(stat="identity",position="dodge") + scale_fill_hue(l=60,c=110) +
    ylab("Mean Salary") + 
    xlab("Industries") + ggtitle(paste("Salary Comparison between Asian & White")) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          panel.background = element_rect(fill = 'white' ))+theme_grey(base_size = 20)


# ENR_Race_Occp <- nPlot(MEAN ~ OCCP, group = 'RAC1P', data = pus_race_occp, type = 'multiBarChart')
# ENR_Race_Occp







# Extract BS, MS, Phd data
pus <- filter(pus, SCHL %in% c(21, 22, 24))
pus$SCHL <- as.factor(pus$SCHL)
levels(pus$SCHL) <- c('Bachelor', 'Master', 'Doctorate')
str(pus)


# Specify CMM occupation
CMM <- filter(pus, OCCP == 'CMM')
# Calculate the freq of diff degrees
CMM_Edu <- ddply(CMM, .(RAC1P, SCHL), summarise, Total = length(SCHL))

# Pie Chart of Education in CMM
Asian <- filter(CMM_Edu, RAC1P == 'Asian')
asianDegreePerc <- Asian$Total/sum(Asian$Total)
Asian<- cbind(Asian, asianDegreePerc)
Education <- levels(Asian$SCHL)
White <- filter(CMM_Edu, RAC1P == "White")
whiteDegreePerc <- White$Total/sum(White$Total)
White<- cbind(White, whiteDegreePerc)
asian_plot <- ggplot(Asian, aes(x="", y=asianDegreePerc, fill=Education))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+theme_minimal()
white_plot <- ggplot(White, aes(x="", y=whiteDegreePerc, fill=Education))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+theme_minimal()
grid.arrange(asian_plot, white_plot, ncol=2)

# For CMM, to every state, Asian or White earn more?
CMM_Ansian <- filter(CMM, RAC1P == 'Asian')
CMM_White <- filter(CMM, RAC1P == 'White')
CMM_state_asian <- ddply(CMM_Ansian, .(STname), summarise, Asian = weighted.mean(PERNP, PWGTP))
CMM_state_white <- ddply(CMM_White, .(STname), summarise, White = weighted.mean(PERNP, PWGTP))
CMM_state_asian <- filter(CMM_state_asian, STname != 'NA')
CMM_state_white <- filter(CMM_state_white, STname != 'NA')
state_name <- fread('statenames.csv', select = c('name', 'abbr'))
CMM_state_salary <-merge(state_name, CMM_state_asian, by.x = 'name', by.y = 'STname', all.x = TRUE)
CMM_state_salary <-merge(CMM_state_salary, CMM_state_white, by.x = 'name', by.y = 'STname', all.x = TRUE)

CMM_state_salary$diff <- apply(CMM_state_salary, 1, function(x) {as.numeric(x[3]) - as.numeric(x[4])})
CMM_state_salary[is.na(CMM_state_salary$diff),]$diff <- 0

CMM_state_salary$hover <- with(CMM_state_salary, paste(name, '<br>', "Difference", diff ))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

plot_ly(CMM_state_salary, z = diff, text = hover, locations = abbr, type = 'choropleth',
        locationmode = 'USA-states', color = diff, colors = 'Blues',
        marker = list(line = l), colorbar = list(title = "USD")) %>%
    layout(title = 'The difference salary between Asians and Whites in every State － CMM', geo = g)




# Specify FIN occupation
FIN <- filter(pus, OCCP == 'FIN')
FIN_Edu <- ddply(FIN, .(RAC1P, SCHL), summarise, Total = length(SCHL))

# Pie Chart of Education in FIN
Asian <- filter(FIN_Edu, RAC1P == 'Asian')
asianDegreePerc <- Asian$Total/sum(Asian$Total)
Asian<- cbind(Asian, asianDegreePerc)
Education <- levels(Asian$SCHL)
White <- filter(FIN_Edu, RAC1P == "White")
whiteDegreePerc <- White$Total/sum(White$Total)
White<- cbind(White, whiteDegreePerc)
asian_plot <- ggplot(Asian, aes(x="", y=asianDegreePerc, fill=Education))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+theme_minimal()
white_plot <- ggplot(White, aes(x="", y=whiteDegreePerc, fill=Education))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+theme_minimal()
grid.arrange(asian_plot, white_plot, ncol=2)


# For FIN, to every state, Asian or White earn more?
FIN_Ansian <- filter(FIN, RAC1P == 'Asian')
FIN_White <- filter(FIN, RAC1P == 'White')
FIN_state_asian <- ddply(FIN_Ansian, .(STname), summarise, Asian = weighted.mean(PERNP, PWGTP))
FIN_state_white <- ddply(FIN_White, .(STname), summarise, White = weighted.mean(PERNP, PWGTP))

FIN_state_asian <- filter(FIN_state_asian, STname != 'NA')
FIN_state_white <- filter(FIN_state_white, STname != 'NA')
FIN_state_salary <-merge(state_name, FIN_state_asian, by.x = 'name', by.y = 'STname', all.x = TRUE)
FIN_state_salary <-merge(FIN_state_salary, CMM_state_white, by.x = 'name', by.y = 'STname', all.x = TRUE)

FIN_state_salary$diff <- apply(FIN_state_salary, 1, function(x) {as.numeric(x[3]) - as.numeric(x[4])})
FIN_state_salary[is.na(FIN_state_salary$diff),]$diff <- 0

FIN_state_salary$hover <- with(FIN_state_salary, paste(name, '<br>', "Difference", diff ))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

plot_ly(FIN_state_salary, z = diff, text = hover, locations = abbr, type = 'choropleth',
        locationmode = 'USA-states', color = diff, colors = 'Reds',
        marker = list(line = l), colorbar = list(title = "USD")) %>%
    layout(title = 'The difference salary between Asians and Whites in every State － FIN', geo = g)




# For MED, to every state, Asian or White earn more?
MED <- filter(pus, OCCP == 'MED')
MED_Edu <- ddply(MED, .(RAC1P, SCHL), summarise, Total = length(SCHL))

# Pie Chart of Education in MED
Asian <- filter(MED_Edu, RAC1P == 'Asian')
asianDegreePerc <- Asian$Total/sum(Asian$Total)
Asian<- cbind(Asian, asianDegreePerc)
Education <- levels(Asian$SCHL)
White <- filter(MED_Edu, RAC1P == "White")
whiteDegreePerc <- White$Total/sum(White$Total)
White<- cbind(White, whiteDegreePerc)
asian_plot <- ggplot(Asian, aes(x="", y=asianDegreePerc, fill=Education))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+theme_minimal()
white_plot <- ggplot(White, aes(x="", y=whiteDegreePerc, fill=Education))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+theme_minimal()
grid.arrange(asian_plot, white_plot, ncol=2)


# For MED, to every state, Asian or White earn more?
MED_Ansian <- filter(MED, RAC1P == 'Asian')
MED_White <- filter(MED, RAC1P == 'White')
MED_state_asian <- ddply(MED_Ansian, .(STname), summarise, Asian = weighted.mean(PERNP, PWGTP))
MED_state_white <- ddply(MED_White, .(STname), summarise, White = weighted.mean(PERNP, PWGTP))


MED_state_asian <- filter(MED_state_asian, STname != 'NA')
MED_state_white <- filter(MED_state_white, STname != 'NA')
MED_state_salary <-merge(state_name, MED_state_asian, by.x = 'name', by.y = 'STname', all.x = TRUE)
MED_state_salary <-merge(MED_state_salary, MED_state_white, by.x = 'name', by.y = 'STname', all.x = TRUE)

MED_state_salary$diff <- apply(MED_state_salary, 1, function(x) {as.numeric(x[3]) - as.numeric(x[4])})
MED_state_salary[is.na(MED_state_salary$diff),]$diff <- 0

MED_state_salary$hover <- with(MED_state_salary, paste(name, '<br>', "Difference", diff ))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

plot_ly(MED_state_salary, z = diff, text = hover, locations = abbr, type = 'choropleth',
        locationmode = 'USA-states', color = diff, colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "USD")) %>%
    layout(title = 'The difference salary between Asians and Whites in every State － MED', geo = g)



# #Split-Apply-Combine
# pus_race_edu <- ddply(pus, .(RAC1P, SCHL), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))
# 
# ENR_Race_Edu <- nPlot(MEAN ~ RAC1P, group = 'SCHL', data = pus_race_edu, type = 'multiBarChart')
# ENR_Race_Edu
# 
# # INDP(Industry recode for 2013 and later based on 2012 IND codes)
# pus$INDP <- ifelse(pus$INDP >= 170 & pus$INDP <= 290, 170, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 370 & pus$INDP <= 490, 370, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 570 & pus$INDP <= 770, 570, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 1070 & pus$INDP <= 3990, 1070, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 4070 & pus$INDP <= 6390, 4070, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 6470 & pus$INDP <= 6780, 6470, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 6870 & pus$INDP <= 7190, 6870, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 7270 & pus$INDP <= 7790, 7270, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 7860 & pus$INDP <= 7890, 7860, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 7970 & pus$INDP <= 8290, 7970, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 8370 & pus$INDP <= 8470, 8370, pus$INDP)
# pus$INDP <- ifelse(pus$INDP %in% c(8660, 8680, 8690), 8370, pus$INDP) 
# pus$INDP <- ifelse(pus$INDP >= 8770 & pus$INDP <= 9290, 8370, pus$INDP)
# pus$INDP <- ifelse(pus$INDP %in% c(8560, 8570, 8580, 8590, 8670), 8560, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 9370 & pus$INDP <= 9590, 9370, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 9670 & pus$INDP <= 9870, 9670, pus$INDP)
# pus$INDP <- ifelse(pus$INDP >= 9920, 9920, pus$INDP)
# pus$INDP <- factor(pus$INDP)
# levels(pus$INDP) <- c("Agriculture, Forestry, Fishing, Hunting", "Mining", "Utilities, Construction", 
#                       "Manufacturing", "Trade, Logistic", "Information, Communications", "Finance",
#                       "Professional", "Education", "Health", "Other Services",
#                       "Arts, Entertainment", "Public Administration", "Military", "Unemployed"
# )
# 
# 
# 
# 
# 
# #Split-Apply-Combine
# pus_race_indp <- ddply(pus, .(RAC1P, INDP), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))
# 
# #multiBarChart comparing PERNP
# ENR_Race_Indp <- nPlot(MEAN ~ RAC1P, group = 'INDP', data = pus_race_indp, type = 'multiBarChart')
# ENR_Race_Indp

# The different PERNP in diff state for Asians
# # asian <- filter(pus, RAC1P == 'Asian')
# state_race_ENR <- ddply(asian, .(STname), summarise, MEAN = weighted.mean(PERNP, PWGTP, na.rm = T))
# 
# 
# # Working time
# pus$WKW <- as.factor(pus$WKW)

