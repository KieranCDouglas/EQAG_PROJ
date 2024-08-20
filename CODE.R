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

envscreen <- read_excel("Downloads/CalEnviroScreen 40 Data Dictionary 2021.xlsx")

################################################################################
### Clean data and create sumary stats 
################################################################################
# get rid of na values and convert should-be numeric values to numeric
envclean <- na.omit(envscreen)
envclean$Unemployment <- as.numeric(gsub("[^0-9.]", "", envclean$Unemployment))
envclean$Education <- as.numeric(gsub("[^0-9.]", "", envclean$Education))
envclean$`Housing Burden` <- as.numeric(gsub("[^0-9.]", "", envclean$`Housing Burden`))

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








