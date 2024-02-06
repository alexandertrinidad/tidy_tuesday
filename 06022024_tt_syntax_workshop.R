################################################################################
# Author: Alex Trinidad
# Affiliation 1: University of Cologne
# Date: 06.02.2024
# Title: NSC-R TidyTuesday Session
################################################################################
library(tidytuesdayR)
library(tidyverse)
# library(here)

# Get the tidytuesday data
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2024-01-30')

groundhog_predi <- tuesdata$predictions

groundhog_counts <-tuesdata$groundhogs 

# Prediction: If groundhog sees its shadow and goes back into burrow, 
#             six more weeks of Winter

# Glimpse data
glimpse(groundhog_counts)
glimpse(groundhog_predi)
View(groundhog_predi)

# Task 1: Replicate table -------------------------------------------------

# What do we need? (https://groundhog-day.com/predictions)
# 1. YEAR
# 2. Prediction
# 3. Spring:winter
# 4. Consensus
# 5. Total predictions


# Create necessary columns as in the example 

repli_tb <- groundhog_predi |> 
  mutate(early_spring = if_else(condition = shadow == FALSE,
                                 true = 1, 
                                 false =0),
         longer_winter = if_else(condition = shadow == TRUE,
                                 true = 1, 
                                 false = 0)) |> 
  group_by(year) |> 
  summarise(spring = sum(early_spring, na.rm = TRUE),
            winter = sum(longer_winter, na.rm = TRUE),
            total_predict = sum(spring, winter)) |> 
  ungroup() |> 
  mutate(consensus = case_when(
    spring > winter ~ round((spring/total_predict) * 100),
    winter > spring ~ round((winter/total_predict) * 100),
    spring == winter ~ NA),
    consensus = case_when(
      is.na(consensus) ~ "-",
      TRUE ~ paste0(consensus, " ", "%")),
    prediction = case_when(
      spring > winter ~ "Early Spring",
      winter > spring ~ "Longer Winter", 
      spring == winter ~ "-"),
    "spring:winter" = paste0(spring, " : ", winter))
  
# Same column order
repli_tb_order <- repli_tb |> 
  dplyr::select(year, prediction, `spring:winter`, consensus, total_predict)



# Task 2: Improve the table -----------------------------------------------

library(formattable)

# Arrange numbers
repli_table_num <- repli_tb_order |> 
  mutate(consensus = stringr::str_remove(consensus, " %"),
         consensus = as.numeric(consensus)) |> 
  arrange(desc(year)) 

# Include logo 
repli_table_num_logo <- repli_table_num |> 
  rename(!!paste0("\U0001F4C5", "Year") := "year",
         "Prediciton" = "prediction",
         !!paste0("\U0001F33B", ":", "\u2603") := "spring:winter",
         "Consensus %" := "consensus",
         "Total Predictions" = "total_predict")

repli_table_num |> 
  rename(!!paste0("top", "Year") := year,
         Prediciton = prediction,
         `Consensus %` = consensus,
         `Total Predictions` = total_predict)


# unicode symobols
# Sunflower <- "U+1F33B" # this is not R version 
# R version: Sunflower <- "\U0001F33B"
# snowman NO R version <- U+2603
# R version snowman <- "\u2603"


# Format table 
formattable(repli_table_num_logo, list(
  `Consensus %` = color_tile("transparent", "lightgreen"),
  `Total Predictions` = color_bar("lightblue"),
  Prediciton = formatter("span", style = x ~ style(color = 
                                                     ifelse(x == "Longer Winter", "blue", 
                                                            ifelse(x == "Early Spring", 
                                                                   "orange", "black"))),
                         x ~ ifelse(x == "Longer Winter", 
                                    paste0("\u2603", x),
                                    ifelse(x == "Early Spring", 
                                           paste0("\U0001F33B", x), 
                                           x))
  )
))


# Task 3: Does the consensus decrease as the number of total predictions increases? --------


# Observer the relationship between Consensus vs Total Predictions

# Explore relationship 

summary(mymodel <- lm(consensus ~ total_predict, data = repli_table_num))

ggplot(data = repli_table_num) +
  geom_point(aes(y = consensus, x = total_predict)) +
  geom_abline(slope = -0.76085 , intercept = 93.33646, color = "red") 

# Remove 100 consensus
repli_table_num_no100 <- repli_table_num |> 
  dplyr::filter(!consensus == 100)


(mymodel_2 <- lm(consensus ~ total_predict, data = repli_table_num_no100))


ggplot(data = repli_table_num_no100, aes(y = consensus, x = total_predict)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# Task 4: Mapping prediction counts ---------------------------------------

library(sf)
library(mapview)

# Transform to sf object
class(groundhog_counts)

groundhog_counts_sf <- st_as_sf(groundhog_counts, coords = c("longitude", "latitude"), crs = st_crs(4326))
class(groundhog_counts_sf)
# Map 

mapview(groundhog_counts_sf, zcol = "predictions_count")

# Select only few columns
groundhog_sf_seleciton <- groundhog_counts_sf |> 
  dplyr::select(-c(description, current_prediction, source, image))

map1 <- mapview(groundhog_sf_seleciton, zcol = "predictions_count")

map2 <- mapview(groundhog_sf_seleciton, zcol = "active", map.types = "Esri.WorldImagery")

library(leafsync)

sync(map1, map2, ncol = 2)



