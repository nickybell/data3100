# renv::install("NOAA-EDAB/buoydata")
library(buoydata)
library(purrr)
library(furrr)
future::plan("multisession")

dir <- tempdir()
ids <- unique(buoydata::buoyDataWorld$ID)
n_groups <- 4
groups <- dplyr::ntile(ids, n_groups)

out1 <- future_map(
  ids[groups == 1],
  \(id) {
    rlang::try_fetch(
      {
        min_year <- min(buoydata::buoyDataWorld$Y1[
          buoydata::buoyDataWorld$ID == id
        ])
        max_year <- min(buoydata::buoyDataWorld$YN[
          buoydata::buoyDataWorld$ID == id
        ])
        get_buoy_data(buoyid = id, year = min_year:max_year, outDir = dir)
        suppressMessages(
          combine_buoy_data(
            buoyid = id,
            variable = "WTMP",
            inDir = dir
          )
        )
      },
      error = function(e) tibble::tibble()
    )
  },
  .progress = TRUE,
  .options = furrr_options(packages = "buoydata")
)
saveRDS(out1, file = "data/week3/raw/buoydata_part1.rds")
rm(out1)

out2 <- future_map(
  ids[groups == 2],
  \(id) {
    rlang::try_fetch(
      {
        min_year <- min(buoydata::buoyDataWorld$Y1[
          buoydata::buoyDataWorld$ID == id
        ])
        max_year <- min(buoydata::buoyDataWorld$YN[
          buoydata::buoyDataWorld$ID == id
        ])
        get_buoy_data(buoyid = id, year = min_year:max_year, outDir = dir)
        suppressMessages(
          combine_buoy_data(
            buoyid = id,
            variable = "WTMP",
            inDir = dir
          )
        )
      },
      error = function(e) tibble::tibble()
    )
  },
  .progress = TRUE,
  .options = furrr_options(packages = "buoydata")
)
saveRDS(out2, file = "data/week3/raw/buoydata_part2.rds")
rm(out2)

out3 <- future_map(
  ids[groups == 3],
  \(id) {
    rlang::try_fetch(
      {
        min_year <- min(buoydata::buoyDataWorld$Y1[
          buoydata::buoyDataWorld$ID == id
        ])
        max_year <- min(buoydata::buoyDataWorld$YN[
          buoydata::buoyDataWorld$ID == id
        ])
        get_buoy_data(buoyid = id, year = min_year:max_year, outDir = dir)
        suppressMessages(
          combine_buoy_data(
            buoyid = id,
            variable = "WTMP",
            inDir = dir
          )
        )
      },
      error = function(e) tibble::tibble()
    )
  },
  .progress = TRUE,
  .options = furrr_options(packages = "buoydata")
)
saveRDS(out3, file = "data/week3/raw/buoydata_part3.rds")
rm(out3)

out4 <- future_map(
  ids[groups == 4],
  \(id) {
    rlang::try_fetch(
      {
        min_year <- min(buoydata::buoyDataWorld$Y1[
          buoydata::buoyDataWorld$ID == id
        ])
        max_year <- min(buoydata::buoyDataWorld$YN[
          buoydata::buoyDataWorld$ID == id
        ])
        get_buoy_data(buoyid = id, year = min_year:max_year, outDir = dir)
        suppressMessages(
          combine_buoy_data(
            buoyid = id,
            variable = "WTMP",
            inDir = dir
          )
        )
      },
      error = function(e) tibble::tibble()
    )
  },
  .progress = TRUE,
  .options = furrr_options(packages = "buoydata")
)
saveRDS(out4, file = "data/week3/raw/buoydata_part4.rds")
rm(out4)

part2 <- readRDS("data/week3/raw/buoydata_part2.rds")
names(part2) <- ids[groups == 2]
part2 <- rlist::list.clean(part2, rlang::is_empty)
part2_monthly <- imap(part2, \(df, id) {
  summarize(df, WTMP = mean(WTMP, na.rm = TRUE), .by = c(YEAR, MONTH)) |>
    filter(!is.nan(WTMP)) |>
    mutate(ID = id)
}) |>
  bind_rows()
rm(part2)

part3 <- readRDS("data/week3/raw/buoydata_part3.rds")
names(part3) <- ids[groups == 3]
part3 <- rlist::list.clean(part3, rlang::is_empty)
part3_monthly <- imap(part3, \(df, id) {
  summarize(df, WTMP = mean(WTMP, na.rm = TRUE), .by = c(YEAR, MONTH)) |>
    filter(!is.nan(WTMP)) |>
    mutate(ID = id)
}) |>
  bind_rows()
rm(part3)

part4 <- readRDS("data/week3/raw/buoydata_part4.rds")
names(part4) <- ids[groups == 4]
part4 <- rlist::list.clean(part4, rlang::is_empty)
part4_monthly <- imap(part4, \(df, id) {
  summarize(df, WTMP = mean(WTMP, na.rm = TRUE), .by = c(YEAR, MONTH)) |>
    filter(!is.nan(WTMP)) |>
    mutate(ID = id)
}) |>
  bind_rows()
rm(part4)

final <- bind_rows(
  part1_monthly,
  part2_monthly,
  part3_monthly,
  part4_monthly
)

readr::write_csv(final, "data/week3/buoydata.csv")
