
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
  ) |> 
  filter(between(year, 2000, 2005))  # To allow the dataset from 2000 - 2005

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

trade_millions <- nigeria_wood_trade_arranged |> 
  mutate(
    value_million = value / 1000)

trade_millions

# Number of observation by trade direct and units -------------------------

observations_by_direction_unit <- nigeria_wood_trade_clean |>
  filter(element %in% c("Import value", "Export value")) |>
  count(element, unit, name = "number_of_observations") |>
  arrange(element, unit)

observations_by_direction_unit

# Total & average value by trade direction --------------------------------

trade_summary <- nigeria_wood_trade_clean |>
  filter(element %in% c("Import value", "Export value")) |>
  group_by(element, year, unit) |>
  summarise(
    total_value = sum(value, na.rm = TRUE),
    average_value = mean(value, na.rm = TRUE),
    number_of_observations = n(),
    .groups = "drop"
  ) |>
  arrange(year, unit, element)

trade_summary

# 5 products with the largest export value in millions --------------------

all_items <- nigeria_wood_trade_clean |> 
  distinct(item) |>       
  pull(item)   # step1 inspect the full item list to understand hierarchy

all_items

length(all_items)

## Build an explicit aggregate list --------------------------------------

aggregate_items <- c(
  # Top-level totals
  "Wood and paper products (export/import)",
  "Primary wood and paper products (export/import)",
  "Secondary processed wood products (export/import)",
  "Secondary processed paper products (export/import)",
  
  # Roundwood branch
  "Roundwood",
  "Wood fuel",
  "Wood fuel, all species (export/import, 1961-2016)",
  "Industrial roundwood",
  "Industrial roundwood, coniferous (export/import)",
  "Industrial roundwood, non-coniferous tropical (export/import)",
  "Industrial roundwood, non-coniferous non-tropical (export/import)",
  "Sawlogs and veneer logs",
  "Pulpwood, round and split (production)",
  "Other industrial roundwood",
  
  # Chips branch
  "Wood chips, particles and residues",
  
  # Sawnwood branch
  "Sawnwood",
  
  # Panels branch
  "Wood-based panels",
  "Fibreboard",
  
  # Pulp branch
  "Total fibre furnish",
  "Pulp for paper",
  "Wood pulp",
  "Chemical wood pulp",
  
  # Paper branch
  "Paper and paperboard",
  "Graphic papers",
  "Packaging paper and paperboard",
  
  # Secondary processed sawnwood
  "Further processed sawnwood (export/import)"
)

length(aggregate_items)

## Top 5 export items with aggregates excluded ----------------------------

top_export_value <- trade_millions |>
  filter(
    element == "Export value",
    !item %in% aggregate_items
  ) |>
  group_by(item) |>
  summarise(
    total_export_million = sum(value_million, na.rm = TRUE),
    .groups = "drop"
  ) |>
  slice_max(order_by = total_export_million, n = 5, with_ties = FALSE) |>
  arrange(desc(total_export_million))

top_export_value

# Top 5 export products ---------------------------------------------------

top_import_value <- trade_millions |>
  filter(
    element == "Import value",
    !item %in% aggregate_items
  ) |>
  group_by(item) |>
  summarise(
    total_import_million = sum(value_million, na.rm = TRUE),
    .groups = "drop"
  ) |>
  slice_max(order_by = total_import_million, n = 5, with_ties = FALSE) |>
  arrange(desc(total_import_million))

top_import_value
