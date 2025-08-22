library(dplyr)
library(readr)

# Load the data
df <- read_csv(here::here("data/raw/voteview_house_ideology.csv"))

# Data wrangling
df_last_mem <- df |>
  filter(
    congress >= 102 &
      party_code %in% c(100, 200) &
      !state_abbrev %in% c("DC", "PR", "VI", "GU", "AS", "MP", "USA")
  ) |>
  mutate(
    party = case_when(
      party_code == 100 ~ "Democrat",
      party_code == 200 ~ "Republican"
    )
  ) |>
  # Only keep the last member for each district in each Congress
  semi_join(
    df |>
      group_by(congress, state_abbrev, district_code) |>
      summarize(last_member = max(last_means)),
    by = join_by(
      congress,
      state_abbrev,
      district_code,
      last_means == last_member
    )
  )

df_last_mem |> # Where more than one member for a district remains in the dataset, remove them. Probably we should come up with a gist to handle this better.
  anti_join(
    count(df_last_mem, congress, state_abbrev, district_code) |>
      filter(n > 1),
    by = join_by(
      congress,
      state_abbrev,
      district_code,
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
  write_csv(here::here("data/voteview_house_ideology.csv"))
