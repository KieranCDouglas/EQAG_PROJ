### Project: EQAG Project
### Created on: August 16th
### Created by: kcd
### Last edited: 08/16/2024

################################################################################
### Install packages and import data
################################################################################
install.packages("tidyverse")
install.packages("readxl")
library(tidyverse)
library(readxl)

dir.create(path = "data")

envscreen <- read_excel('/Users/kieran/Documents/GitHub/EQAG_PROJ/data/CalEnviroScreen 40 Data Dictionary 2021.xlsx')
svi <- read_csv("data/California County SVI.csv")
################################################################################
### Clean data and create sumary stats 
################################################################################
# get rid of na values and convert should-be numeric values to numeric
envclean <- na.omit(envscreen)
envclean$Unemployment <- as.numeric(gsub("[^0-9.]", "", envclean$Unemployment))
envclean$Education <- as.numeric(gsub("[^0-9.]", "", envclean$Education))
envclean$`Housing Burden` <- as.numeric(gsub("[^0-9.]", "", envclean$`Housing Burden`))

# create a merged subset
merged <- merge(envclean, svi, 
                   by.x = "California County", by.y = "COUNTY", 
                   all = TRUE)

# create clean set only including complete cases
envclean_partial <- envclean[complete.cases(envclean[, c("Education", "Housing Burden")]), ]

# how many obs from each county
table(envclean$`California County`)
table(envclean$`Unemployment`)
head(envclean$Unemployment)
head(envclean$Unemployment, 20)

# sd and mean of pollution burden score from each county
sdmean <- envclean %>% 
group_by(`California County`) %>%
  summarise(
    mean_value = mean(`Pollution Burden Score`, na.rm = TRUE),
    sd_value = sd(`Pollution Burden Score`, na.rm = TRUE)
  )
print(sdmean, n=58)

# sd and mean of unemployment rate from each county
unem <- envclean %>% 
  group_by(`California County`) %>%
  summarise(
    mean_value = mean(`Unemployment`, na.rm = TRUE),
    sd_value = sd(`Unemployment`, na.rm = TRUE)
  )
print(unem, n=58)

################################################################################
### Regressions, summary stats, and graphical representation
################################################################################
# base lm, unemployment on pollution burden
polunem <- lm(data = envclean, `Unemployment` ~ `Pollution Burden Score` + `California County`)
summary(polunem)

ggplot(data = envclean, mapping = aes(x = `Pollution Burden Score`, y = Unemployment)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()

ggplot(data = envclean, mapping = aes(x = `Pollution Burden Score`, y = Unemployment, color = `California County`)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()
# pollution burden score is highly correlated with unemployment (p<.001) but after controlling for County it is only correlated sometimes
# in particular counties there is a highly significant relationship whereas others have no relationship.

# lets look at LA
lamodel <- envclean %>% 
  filter(`California County` == "Los Angeles") %>% 
  lm(`Unemployment` ~ `Pollution Burden Score` +  `Poverty` + `Housing Burden` + `Pop. Char. Score` + `Education`, data = .)
summary(lamodel)

test <- envclean %>% 
  filter(`California County` == "Los Angeles") %>% 
  lm(`Unemployment` ~ `Pollution Burden Score` +  `Poverty` + `Housing Burden` + `Pop. Char. Score` + `Education`, data = .)
summary(test)

# core factors model
# this presents a shitty model with a low R2. 
corefactors <- lm(data = envclean_partial, `Unemployment` ~ `Ozone`+`PM2.5`+`Diesel PM`+`Traffic`+`Pesticides`+`Groundwater Threats`+`Haz. Waste`+`Education`+`Housing Burden`+`Poverty`)
summary(corefactors)  

pmmodel <- lm(data = envclean_partial, `Unemployment` ~ `PM2.5`)
summary(pmmodel)
#check for multicolinearity among included independent variables
cor(envclean_partial[, c("Ozone", "PM2.5", "Diesel PM", "Traffic", "Pesticides", "Groundwater Threats", "Haz. Waste", "Education", "Housing Burden", "Poverty")])

# there is not a ton of multicolinearity but some, suggesting that there are some variables whose effect on the outcome is mitigated by other variables inclusion
# specifically between pm2.5 and ozone, pm2.5 and education housing burden and poverty, education poverty housing burden
# try removing pm2.5 from model due to covariance
gwmodel <- lm(data = envclean_partial, `Unemployment` ~ `Groundwater Threats`)
summary(gwmodel)
hazmodel <- lm(data = envclean_partial, `Unemployment` ~ `Haz. Waste`)
summary(hazmodel)
pestmodel <- lm(data = envclean_partial, `Unemployment` ~ `Pesticides`)
summary(pestmodel)
# remove groundwater, haz waste, and pesticides, not significant. 
bigfactors <- lm(data = envclean_partial, `Unemployment` ~ `Ozone`+`PM2.5`+`Diesel PM`+`Traffic`+`Education`+`Housing Burden`+`Poverty`)
summary(bigfactors)


