# heavily adapted from Elliott Morris's code (https://github.com/markjrieke/2026-war/blob/main/analyses/WARP-ideology/merge_with_ideo.R)
library(tidyverse)
library(arrow)

# read in all pols
pols <- read_parquet(
  "https://github.com/markjrieke/2026-war/raw/refs/heads/main/out/summary/mappings/full_data.parquet"
) |>
  select(
    M,
    cycle,
    state_name,
    district,
    dem_two_party_pct = pct,
    candidate_DEM,
    candidate_REP
  ) |>
  pivot_longer(
    c(candidate_DEM, candidate_REP),
    names_to = "party",
    names_prefix = "candidate_",
    values_to = "candidate"
  ) |>
  mutate(
    uncontested = ifelse(any(candidate == 'Uncontested'), 1, 0),
    .by = M
  ) |>
  filter(candidate != 'Uncontested') |>
  mutate(
    winner = (party == "DEM" & dem_two_party_pct > 0.5) |
      (party == "REP" & dem_two_party_pct < 0.5),
    .by = candidate
  )

# join WAR
WAR <- read_parquet(
  "https://github.com/markjrieke/2026-war/raw/refs/heads/main/out/summary/variables/WAR.parquet"
) |>
  filter(quantile == 0.5) |>
  mutate(party = stringr::str_to_upper(party))

WARP <- read_parquet(
  "https://github.com/markjrieke/2026-war/raw/refs/heads/main/out/summary/variables/WARP.parquet"
) |>
  mutate(party = stringr::str_to_upper(party))

pols |>
  left_join(WAR, by = join_by(M, party)) |>
  left_join(WARP, by = join_by(M, party)) |>
  mutate(chamber = 'House') |>
  select(-quantile) |>
  write_csv(here::here("data/morris_war.csv"))
