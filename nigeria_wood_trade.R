
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
  filter(between(year, 2000, 2025))  # To allow the dataset from 2000 - 2025

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

trade_millions <- nigeria_wood_trade_clean |> 
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

## Top 5 export products with aggregates excluded ----------------------------

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

## Top 5 import products with aggregates excluded -----------------------------

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

# Number & proportion of observation marked estimated or official ---------

flag_summary <- nigeria_wood_trade_clean |>
  count(flag, flag_description, name = "n_observations") |>
  mutate(
    proportion = round(n_observations / sum(n_observations), 3),
    percent    = round(proportion * 100, 3)
  ) |>
  arrange(desc(n_observations))

flag_summary

# Total and average value in million by trade direction and year ---------

annual_trade_millions <- trade_millions |>
  filter(
    element %in% c("Import value", "Export value"),
    unit == "1000 USD"
  ) |>
  group_by(year, element) |>
  summarise(
    total_value_million   = sum(value_million, na.rm = TRUE),
    average_value_million = mean(value_million, na.rm = TRUE),
    number_of_observations = n(),
    .groups = "drop"
  ) |>
  arrange(year, element)

annual_trade_millions

# Chart 1 -----------------------------------------------------------------

total_chart <- annual_trade_millions |>
  filter(element %in% c("Import value", "Export value")) |>
  ggplot(mapping = aes(x = year,
                       y = total_value_million,
                       colour = element)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("Import value" = "darkorange", "Export value" = "steelblue")) +
  labs(
    title = "Total Wood & Paper Trade Value by Direction (2000–2025)",
    subtitle = "Total value summed across all products per year",
    x = "Year",
    y = "Total value (million USD)",
    colour = "Trade Direction" 
  ) +
  theme_bw()

total_chart

# Chart 2 -----------------------------------------------------------------

average_chart <- annual_trade_millions |>
  filter(element %in% c("Import value", "Export value")) |>
  ggplot(mapping = aes(x = year,
                       y = average_value_million,
                       colour = element)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("Import value" = "darkgreen", "Export value" = "darkred")) +
  labs(
    title = "Average Wood & Paper Trade Value by Direction (2000–2025)",
    subtitle = "Mean value per product-year record",
    x = "Year",
    y = "Average value (million USD)",
    colour = "Trade Direction"
  ) +
  theme_bw()

average_chart

# Faceted data -----------------------------------------------------------

facet_data <- annual_trade_millions |>
  filter(element %in% c("Import value", "Export value")) |>
  pivot_longer(
    cols = c(total_value_million, average_value_million),
    names_to = "measure",
    values_to = "value_million"
  ) |>
  mutate(
    measure = if_else(
      measure == "total_value_million", 
      "Total value", 
      "Average value"
    )
  )

facet_data

## Chart 3 - facet chart --------------------------------------------------

facet_chart <- ggplot(
  data = facet_data, 
  mapping = aes(
    x = year, 
    y = value_million, 
    colour = element
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  facet_wrap(~ measure, scales = "free_y") +  
  scale_colour_manual(values = c("Import value" = "darkorange", "Export value" = "steelblue")) +
  labs(
    title = "Total and Average Wood & Paper Trade Value by Direction (2000–2025)",
    subtitle = "Comparing total and average trade values (in million USD) by year and direction",
    x = "Year",
    y = "Value (million USD)",
    colour = "Trade Direction"
  ) +
  theme_light()

facet_chart

# Change in the number of products over time ------------------------------

product_counts <- nigeria_wood_trade_clean |>
  filter(element %in% c("Import value", "Export value")) |>
  group_by(year, element) |>       
  summarise(
    unique_products = n_distinct(item),
    .groups = "drop"
  )

product_counts

# Chart 4 - product over time ---------------------------------------------

product_counts_chart <- ggplot(
  data = product_counts,
  mapping = aes(x = year, y = unique_products, colour = element)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("Import value" = "darkred", "Export value" = "navyblue")) +
  labs(
    title = "Number of Unique Products Recorded by Year and Direction (2000–2025)",
    subtitle = "Tracking the expansion of product categories in FAOSTAT",
    x = "Year",
    y = "Number of Unique Products",
    colour = "Trade Direction"
  ) +
  theme_bw()

product_counts_chart

# Chart 5 - top 5 export product by total trade value ---------------------

top_export_product <- top_export_value |>
  mutate(item = fct_reorder(item, total_export_million)) |>
  ggplot(mapping = aes(x = total_export_million, y = item)) +
  geom_col(fill = "tomato") +
  geom_text(
    mapping = aes(label = round(total_export_million, 1)),
    hjust = -0.2,
    size = 4,
    colour = "black"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 5 Export Products by Total Trade Value (2000–2025)",
    subtitle = "Leaf-level products only (aggregates excluded)",
    x = "Total Export Value (Million USD)",
    y = NULL
  ) +
  theme_bw()

top_export_product
