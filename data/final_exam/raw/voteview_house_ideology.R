library(dplyr)
library(readr)

# Load the data
df <- read_csv(here::here("data/week1/raw/Hall_members.csv"))

# Data wrangling
df_first_mem <- df |>
  filter(
    congress >= 102 &
      party_code %in% c(100, 200) &
      !state_abbrev %in% c("DC", "PR", "VI", "GU", "AS", "MP", "USA") &
      (last_means == 1 |
        is.na(last_means))
  ) |>
  mutate(
    party = case_when(
      party_code == 100 ~ "Democrat",
      party_code == 200 ~ "Republican"
    )
  )

# Identify at-large members
df_first_mem <- left_join(
  df_first_mem,
  df_first_mem |>
    group_by(congress, state_abbrev) |>
    summarize(n_districts = n_distinct(district_code)),
  by = join_by(congress, state_abbrev)
) |>
  mutate(
    district_code = if_else(
      n_districts == 1,
      0,
      district_code
    )
  ) |>
  select(-n_districts)

df_first_mem |> # Where more than one member for a district remains in the dataset, choose the lower ICPSR ID number.
  semi_join(
    df_first_mem |>
      group_by(congress, state_abbrev, district_code) |>
      summarize(first_member = min(icpsr)),
    by = join_by(
      congress,
      state_abbrev,
      district_code,
      icpsr == first_member
    )
  ) |>
  select(
    congress,
    bioname,
    state_abbrev,
    district_code,
    party,
    nominate_dim1,
    nominate_dim2
  ) |>
  write_csv(here::here("data/final_exam/voteview_house_ideology.csv"))
