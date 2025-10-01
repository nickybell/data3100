library(tidyverse)
library(fst)
library(lubridate)

# Load the hurricanes data. Note that these landfalls include all landfalls, not just the continental U.S.
hurdat <- read_csv(
  "https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2024-040425.txt",
  col_select = c(1, 2, 3, 4, 7, 8),
  skip = 1
)
colnames(hurdat) <- janitor::make_clean_names(c(
  "Date",
  "Time",
  "Record Identifier",
  "System Status",
  "Max Wind (knots)",
  "Min Pressure (mb)"
))

hurdat2 <- hurdat |>
  separate_wider_position(date, c("year" = 4, "month" = 2, "day" = 2)) |>
  mutate(name = ifelse(is.na(system_status), time, NA_character_)) |>
  fill(name, .direction = "down") |>
  filter(
    !is.na(system_status) & record_identifier == "L" & system_status == "HU"
  ) |>
  # Convert to Eastern time and extract date only
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day),
    hour = as.numeric(str_sub(time, 1, 2)),
    minute = as.numeric(str_sub(time, 3, 4)),
    datetime_utc = make_datetime(year, month, day, hour, minute),
    datetime_eastern = with_tz(datetime_utc, "America/New_York"),
    date = as_date(datetime_eastern)
  ) |>
  arrange(year, name, datetime_eastern) |>
  distinct(year, name, .keep_all = TRUE) |>
  select(
    -year,
    -month,
    -day,
    -time,
    -hour,
    -minute,
    -datetime_utc,
    -datetime_eastern
  ) |>
  filter(date >= "1980-01-01") # Two distinct landfalls on Sept. 19, 2024

hurdat3 <- anti_join(
  hurdat2,
  count(hurdat2, date) |> filter(n > 1),
  by = join_by(date)
) |>
  mutate(
    category = case_when(
      max_wind_knots >= 137 ~ "5",
      max_wind_knots >= 113 ~ "4",
      max_wind_knots >= 96 ~ "3",
      max_wind_knots >= 83 ~ "2",
      max_wind_knots >= 64 ~ "1",
      TRUE ~ "TS" # Tropical Storm (below hurricane strength)
    )
  )

# Load the NY Fed's cost of natural disasters data
nyf <- read_fst(here::here(
  "data/week6/ny_fed_losses_from_natural_disasters.fst"
))

nyf2 <- nyf |>
  filter(weight_type == "Equal" & event_type == "Hurricane") |>
  summarize(
    total_property_damage_millions = sum(
      damages_property_adj / 1000000,
      na.rm = TRUE
    ),
    total_injuries = sum(injuries_direct, na.rm = T),
    total_fatalities = sum(fatalities_direct, na.rm = T),
    .by = c(episode_id, begin_date)
  ) |>
  arrange(begin_date, desc(total_property_damage_millions)) |>
  distinct(begin_date, .keep_all = TRUE)

# If we're lucky, we can join these two datasets by date and it will be a one-to-one match.
df <- inner_join(
  hurdat3,
  nyf2,
  by = c("date" = "begin_date"),
  relationship = "one-to-one"
) |>
  filter(total_property_damage_millions > 0) # remove incorrect data

write_csv(df, here::here("data/week6/hurricane_landfalls.csv"))
