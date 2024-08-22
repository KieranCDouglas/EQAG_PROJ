### Project: EQAG Project
### Created on: August 16th
### Created by: kcd
### Last edited: 08/16/2024

################################################################################
### Install packages and import data
################################################################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("gt")
install.packages("webshot2")
install.packages("broom")
library(tidyverse)
library(readxl)
library(gt)
library(webshot2)
library(broom)


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

# create partial subset for regression variables
envclean_partial <- na.omit(envclean_partial[c("Unemployment", "Ozone", "Diesel PM", "Traffic", "Education", "Housing Burden", "Poverty")])

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
### Regressions and graphical representation
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

################################################################################
### Tables
################################################################################
### first make summery stats table for California as a whole 
summary_stats <- envclean %>%
  select(Unemployment, Ozone, `Diesel PM`, Traffic, Education, `Housing Burden`, Poverty) %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("variable", ".value"), names_sep = "_")
# use data to make a table to print using gt
sumtable <- summary_stats %>%
  gt() %>%
  tab_header(
    title = "California Summary Statistics",
  ) %>%
  fmt_number(
    columns = c(mean, sd, median, min, max),
    decimals = 2
  ) %>%
  fmt_number(
    columns = n,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  cols_label(
    n = "Sample Size",
    mean = "Mean",
    sd = "Std. Dev.",
    median = "Median",
    min = "Minimum",
    max = "Maximum"
  ) %>%
  tab_source_note(
    source_note = "Data source: California Environmental Health Screening Tool"
  ) %>%
  opt_row_striping() %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    data_row.padding = px(10)
  )
# Save the table as PNG
gtsave(sumtable, filename = "sumtable.png", path = ".")
##########
##########
# Create table outline for gt to use for county level pollution burden stats
sumcounty <- envclean %>%
  group_by(`California County`) %>%
  summarise(across(`Pollution Burden Score`, list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ))) %>%
  rename(
    n = `Pollution Burden Score_n`,
    mean = `Pollution Burden Score_mean`,
    sd = `Pollution Burden Score_sd`,
    median = `Pollution Burden Score_median`,
    min = `Pollution Burden Score_min`,
    max = `Pollution Burden Score_max`
  ) %>%
  arrange(desc(n)) %>%  # Sort by sample size in descending order
  slice_head(n = 10)    # Keep only the top 10 rows

# Make nice table and export
gt_table <- sumcounty %>%
  gt() %>%
  tab_header(
    title = "Pollution Burden Score Summary",
    subtitle = "Top 10 California Counties by Sample Size"
  ) %>%
  fmt_number(
    columns = c(mean, sd, median, min, max),
    decimals = 2
  ) %>%
  fmt_number(
    columns = n,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  cols_label(
    `California County` = "County",
    n = "Sample Size",
    mean = "Mean",
    sd = "Std. Dev.",
    median = "Median",
    min = "Minimum",
    max = "Maximum"
  ) %>%
  tab_source_note(
    source_note = "Data source: California Environmental Health Screening Tool"
  ) %>%
  opt_row_striping() %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    data_row.padding = px(10)
  )
# Save the table as PNG
gtsave(gt_table, filename = "sumcounty_table.png", path = ".")

###################
# core factors model
corefactors <- lm(data = envclean, `Unemployment` ~ `Ozone`+`PM2.5`+`Diesel PM`+`Traffic`+`Pesticides`+`Groundwater Threats`+`Haz. Waste`+`Education`+`Housing Burden`+`Poverty`)
summary(corefactors)  
pmmodel <- lm(data = envclean, `Unemployment` ~ `PM2.5`)
summary(pmmodel)

#check for multicolinearity among included independent variables
cor(envclean[, c("Ozone", "PM2.5", "Diesel PM", "Traffic", "Pesticides", "Groundwater Threats", "Haz. Waste", "Education", "Housing Burden", "Poverty")])

# there is not a ton of multicolinearity but some, suggesting that there are some variables whose effect on the outcome is mitigated by other variables inclusion
# specifically between pm2.5 and ozone, pm2.5 and education housing burden and poverty, education poverty housing burden

# check importance of low correlation variables
gwmodel <- lm(data = envclean, `Unemployment` ~ `Groundwater Threats`)
summary(gwmodel)
hazmodel <- lm(data = envclean, `Unemployment` ~ `Haz. Waste`)
summary(hazmodel)
pestmodel <- lm(data = envclean, `Unemployment` ~ `Pesticides`)
summary(pestmodel)

#####
## Core regression
#####
# remove groundwater, haz waste, and pesticides, not significant. Use Ozone for both Ozone and PM2.5 
bigfactors <- lm(data = envclean, `Unemployment` ~ `Ozone`+`Diesel PM`+`Traffic`+`Education`+`Housing Burden`+`Poverty`)
summary(bigfactors)
#make table in gt
format_e <- function(x) {
  formatC(x, format = 'e', digits = 2)
}

# Apply the formatting function to the relevant columns
summary_df <- summary_df %>%
  mutate(
    estimate = format_e(estimate),
    std.error = format_e(std.error),
    statistic = format_e(statistic),
    p.value = format_e(p.value)
  )

# Create a nice-looking table using gt with E notation
gt_table <- summary_df %>%
  gt() %>%
  tab_header(
    title = "Regression Output",
    subtitle = "Unemployment vs Environmental Factors"
  ) %>%
  cols_label(
    term = "Term",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "Statistic",
    p.value = "P-Value",
    significance = "Significance"
  ) %>%
  tab_options(
    table.font.size = "small"
  )

print(gt_table)

# Save the table as PNG
gtsave(gt_table, filename = "regoutput.png", path = ".")

###
## Graphs for presentation
###
graph1 <- ggplot(data = envclean, mapping = aes(x=`Pollution Burden Score`, y = Unemployment)) +
  geom_point(color = "#93A392") +
  geom_smooth(method = lm, color = "#E2EB98") +
  labs(title = "Polliution Burden Score and Unemployment in California", x = "Pollution Burden Score", y = "Unemployment (%)") +
  theme_light()
print(graph1)
ggsave(filename = "graph1.png", plot = graph1, path = ".", dpi = 300)


graph2 <- ggplot(data = envclean, mapping = aes(x=`Pollution Burden Score`, y = Unemployment, color = Poverty)) +
  geom_point() +
  geom_smooth(method = lm, color = "#E2EB98") +
  labs(title = "Polliution Burden Score and Unemployment in California", x = "Pollution Burden Score", y = "Unemployment (%)") +
  scale_color_gradient(low = "#93A392", high = "#C9F4A9") +
  theme_classic()
print(graph2)
ggsave(filename = "graph2.png", plot = graph2, path = ".", dpi = 300)

ggplot(data = envclean, mapping = aes(x=`Pollution Burden Score`, color = `California County`)) +
  geom_boxplot()

library(ggplot2)

# Create the boxplot with a gradient fill
ggplot(data = envclean, mapping = aes(x = `California County`, y = `Pollution Burden Score`, fill = `Pollution Burden Score`)) +
  geom_boxplot(outlier.colour = "#E2EB98", outlier.shape = 16, outlier.size = 2, notch = TRUE) +
  labs(title = "Pollution Burden Score by California County", x = "County", y = "Pollution Burden Score") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    legend.position = "none" # Hide legend if not needed
  )

# Save the plot
ggsave(filename = "graph2.png", plot = graph2, path = ".", dpi = 300)
#Traffic and ozone are highly correlated, traffic emissions create NO2 which breaks down ozone hence the negative association
trafoz <- lm(data = envclean, `Ozone` ~ `Traffic`)
summary(trafoz)
## Test for interchangeability between Ozone and PM2.5 concentrations
# looks like Ozone and PM2.5 are highly correlated and interchangeable in our model. Because of this we will only include Ozone in the big model
bigfactorsnoZ <- lm(data = envclean, `Unemployment` ~ `Diesel PM`+`Traffic`+`Education`+`Housing Burden`+`Poverty`+`PM2.5`)
summary(bigfactorsnoZ)
bigfactorsnoPM <- lm(data = envclean, `Unemployment` ~ `Ozone`+`Diesel PM`+`Traffic`+`Education`+`Housing Burden`+`Poverty`)
summary(bigfactorsnoPM)
#check for nonlinear relationships, looks fine
ggplot(data = envclean, mapping = aes(x=PM2.5,y=Unemployment)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()
ggplot(data = envclean, mapping = aes(x=Ozone,y=Unemployment)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()

#comparing ozine to PM2.5
#the two are highly correlated, thus it makes sense that they could be somewhat interchangible in the regression model
ggplot(data = envclean, mapping = aes(x=Ozone,y=PM2.5)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()
ozpm <- lm(data = envclean_partial, `PM2.5`~`Ozone`)
summary(ozpm)
#the means of these two variables are different **
diff <- t.test(envclean_partial$Ozone, envclean_partial$PM2.5, paired = TRUE)
print(diff)


unempol <- lm(data = envclean, `Unemployment`~`Haz. Waste`+`Lead`+`PM2.5`+`Ozone`)
summary(unempol)
