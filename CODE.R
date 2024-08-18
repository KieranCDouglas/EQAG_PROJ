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
unique(envscreen$`California County`)

################################################################################
### Regressions, summary stats, and graphical representation
################################################################################

'/Users/kieran/Downloads/CalEnviroScreen 40 Data Dictionary 2021.xlsx'
