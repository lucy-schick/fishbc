# This script is to wrangle the raw CDC data into the correct format.

library(tidyverse)

##### Download #####

# Search the BC Conservation Data Centre (https://a100.gov.bc.ca/pub/eswp/search.do) for fish data using the following criteria

# The search criteria is `Fish, Freshwater OR Fish, Marine`
# and the sort order is `Scientific Name Ascending`.

# Download the `Summary Data` to `data-raw/cdc` and save as `raw.csv`


##### Import #####

# Read in raw.csv. The last 4 rows are search criteria info so they must be removed.
raw <- readr::read_csv("data-raw/cdc/raw.csv") |>
  # the last 4 rows are NA or search criteria info so lets remove them
  dplyr::slice(1:(n() - 4))

# Rename `cdc.csv` to `cdc_old.csv` so we can use it as a template, as we will burn over `cdc.csv` with the new data.
# We will remove `cdc_old.csv` at the end
old_name <- "data-raw/cdc/cdc.csv"
new_name <- "data-raw/cdc/cdc_old.csv"
file.rename(old_name, new_name)

# read in old file to use as a template
cdc_old <- readr::read_csv("data-raw/cdc/cdc_old.csv")



##### Rename columns #####

# We need to rename columns to match those in `cdc_old.csv`

# See what columns are present in the new dataset but not in the old
present_new <- dplyr::setdiff(names(raw), names(cdc_old)) |>
  print()

# See what columns are present in the old dataset but not in the new dataset
present_old <- dplyr::setdiff(names(cdc_old), names(raw)) |>
  print()

# Most of these are actually present in the updated dataset but they are just renamed (Ex: Municipality = Municipalities)
# The "Forest Dist" column is not found in raw.csv
# The SARA column gets renamed in the next section

# Rename the columns to match those in `cdc_old.csv`
cdc_prep1 <- raw |>
  dplyr::rename(Ecosection = Ecosections,
                MBCA = `Migratory Bird Convention Act`,
                `Species Level` = `Classification Level`,
                `MOE Region` = `ENV Regional Boundaries`,
                `Regional Dist` = `Regional Districts`,
                Municipality = Municipalities,
                `Habitat Subtype` = `Habitats\n(Type / Subtype / Dependence)`,
                `Mapping Status` = `Mapping Comment`)



##### Format Columns #####

# We need to join the dates and classification for the COSEWIC and SARA columns to match those in cdc_old
# We need to reformat the `MOE Region` to exclude the location ("1- Vancouver Island;" should be "1;") to match those in cdc_old

cdc_prep2 <- cdc_prep1 |>
  #First we need to shorten the COSEWIC and SARA classifications to abbreviations
  dplyr::mutate(COSEWIC = dplyr::case_when(COSEWIC == "Special Concern" ~ "SC",
                                    COSEWIC == "Endangered / Threatened" ~ "E/T",
                                    COSEWIC == "Endangered" ~ "E",
                                    COSEWIC == "Threatened" ~ "T",
                                    COSEWIC == "Not at Risk" ~ "NAR",
                                    COSEWIC == "Data Deficient" ~ "DD",
                                    COSEWIC == "Extinct" ~ "X",
                                    COSEWIC == "Extirpated" ~ "XT",
                                    COSEWIC == "Not Available" ~ "NA",
                                    COSEWIC == "Endangered / Threatened / Special Concern / Data Deficient / Not at Risk" ~ "E/T/SC/DD/NAR",
                                    T ~ COSEWIC),
                `SARA Status` = dplyr::case_when(`SARA Status` == "Special Concern" ~ "SC",
                                          `SARA Status` == "Endangered / Threatened" ~ "E/T",
                                          `SARA Status` == "Endangered" ~ "E",
                                          `SARA Status` == "Threatened" ~ "T",
                                          `SARA Status` == "Not at Risk" ~ "NAR",
                                          `SARA Status` == "Data Deficient" ~ "DD",
                                          `SARA Status` == "Extinct" ~ "XX",
                                          `SARA Status` == "Extirpated" ~ "XT",
                                          `SARA Status` == "Not Available" ~ "NA",
                                          `SARA Status` == "Endangered / Threatened / Special Concern / Data Deficient / Not at Risk" ~ "E/T/SC/DD/NAR",
                                          T ~ `SARA Status`)) |>
  # Re-format the dates
  dplyr::mutate(`COSEWIC Date` = format(lubridate::my(`COSEWIC Date`), "%b %Y"),
                `SARA Date` = format(lubridate::my(`SARA Date`), "%b %Y"),
                ## Now join the dates and classification
                COSEWIC = case_when(!is.na(`COSEWIC Date`) ~ paste0(COSEWIC, " (", `COSEWIC Date`, ")"),
                                    T ~ COSEWIC),
                `SARA` = case_when(!is.na(`SARA Schedule`) ~ as.character(`SARA Schedule`),
                                   TRUE ~ NA),
                `SARA` = case_when(!is.na(`SARA Status`) ~ paste0(`SARA`, "-", `SARA Status`),
                                   TRUE ~ `SARA`),
                `SARA` = case_when(!is.na(`SARA Date`) ~ paste0(`SARA`, " (", `SARA Date`, ")"),
                                   TRUE ~ `SARA`)) |>
  relocate(SARA, .after = `SARA Date`) |>
  # Re-format the `MOE Region` to exclude the location
  rowwise() |>
  mutate(`MOE Region` = case_when(!is.na(`MOE Region`) ~
                                         (str_extract_all(`MOE Region`, "\\d+") |>
                                            unlist() |>
                                            paste0(collapse = ";")),
                                       T ~ `MOE Region`)) |>
  ungroup()



##### Remove and relocate columns #####

# Finally, we need to remove all columns that are present in cdc_old.csv and rearrage.

# columns we must remove
cols_to_remove <- dplyr::setdiff(names(cdc_prep2), names(cdc_old)) |>
  print()

# Because the "Forest Dist" column is not found in the new dataset, we must remove it to reorder
reordered <- dplyr::setdiff(names(cdc_old), "Forest Dist") |>
  print()


cdc_prep3 <- cdc_prep2 |>
  ## Remove all columns that are not found in cdc_old.csv
  select(-c(all_of(cols_to_remove))) |>
  # relocate columns to match those in cdc_old.csv
  dplyr::relocate(all_of(reordered)) |>
  # Sort by `Scientific Name`
  arrange(`Scientific Name`)



##### Burn #####

#Burn over `cdc.csv`
readr::write_csv(cdc_prep3, 'data-raw/cdc/cdc.csv', append = F)

# Delete cdc_old.csv
file.remove("data-raw/cdc/cdc_old.csv")

# Now run data-raw/data-raw.R to test.


