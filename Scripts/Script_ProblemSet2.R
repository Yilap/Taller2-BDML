################################################################
# Problem Set 2: Predicting Poverty
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Rivera,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
require("pacman")
p_load("tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","readr")

# Importing Dataset -------------------------------------------------------

test_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")