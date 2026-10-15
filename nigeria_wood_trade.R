
# Load packages ------------------------------------------------------------

library(tidyverse)

library(janitor)

library(readr)

library(skimr)

library(dplyr)

library(car)

# Import dataset ----------------------------------------------------------

nigeria_wood_trade_raw <- read_csv("https://raw.githubusercontent.com/EU-Study-Assist/data-for-r4r/refs/heads/main/flatfiles/nigeria_wood_exp_data_fao_2000_2025.csv") |> 
  janitor::clean_names()

## Column name clean to snake case ----------------------------------------

nigeria_wood_trade_new <- nigeria_wood_trade_raw |>
  clean_names()

clean_names(nigeria_wood_trade_new)

# Data Preview / inspection -----------------------------------------------

head(nigeria_wood_trade_new)

tail(nigeria_wood_trade_new)

## Random sampling of inspected data --------------------------------------

set.seed(3423)
car::some(nigeria_wood_trade_new)

## Data structure and summary ---------------------------------------------

str(nigeria_wood_trade_new)

summary(nigeria_wood_trade_new)

skim(nigeria_wood_trade_new)

skim_without_charts(nigeria_wood_trade_new)          #Alternatively

# Removing unneeded columns -----------------------------------------------

nigeria_wood_trade_clean <- nigeria_wood_trade_new |>
  select(
    -domain_code,
    -domain,
    -area_code_m49,
    -area,
    -element_code,
    -item_code,
    -year_code,
    -note
  )

nigeria_wood_trade_clean

## Confirm and remove duplicated rows --------------------------------------

n_dupes <- sum(duplicated(nigeria_wood_trade_clean[, c("element", "item", "year")]))

n_dupes       # no duplicated row was found, so all rows are valid

# Export and save clean dataset in folder -----------------------------------

dir.create("processed_data", showWarnings = FALSE)  # folder processed data created

write_csv(nigeria_wood_trade_clean, "processed_data/nigeria_wood_trade_clean.csv")

## Import exported file written --------------------------------------------

read_csv("processed_data/nigeria_wood_trade_clean.csv")

# Filter the values in the element column --------------------------------

element_col <- nigeria_wood_trade_clean |>
  filter(
    element %in% c("Import value", "Export value"),
    unit == "1000 USD"
  )

## Reshaping values in the element column ---------------------------------

element_col |>
  select(year, item, element, value) |>
  pivot_wider(
    names_from = element,
    values_from = value
  )

## Alternatively reshaping the values in element columns ------------------

nigeria_wood_trade_clean |>
  filter(
    element %in% c("Import value", "Export value"),
    unit == "1000 USD"
  ) |>
  select(year, item, element, value) |>
  pivot_wider(
    names_from = element,
    values_from = value
  )

# Convert columns to the right data type ----------------------------------

nigeria_wood_trade_clean <- nigeria_wood_trade_clean |>
  mutate(
    year = as.integer(year),
    value = as.double(value)
  )

# Arranging column based on year, flag_desc, item, element & value --------

nigeria_wood_trade_arranged <- nigeria_wood_trade_clean |> 
  select(
    year, flag_description,
    item, element,  value
    )

# Conversion of current values from 1000 USD to million USD ---------------

nigeria_wood_trade_arranged |> 
  mutate(
    value_million = value / 1000)
