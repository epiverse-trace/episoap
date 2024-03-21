library(dplyr)
library(readr)

dat <- read_csv("GB.csv", guess_max = 500) %>%
  select(
    date = events.confirmed.date,
    gender = demographics.gender,
    region = location.administrativeAreaLevel1,
  ) %>%
  filter(
    !is.na(region),
    # As for the original aggregated data, we only look at regions within
    # England
    !region %in% c("England", "Sctoland", "Wales", "Northern Ireland")
  ) %>%
  filter(
    date >= as.Date("2020-07-01")
  )

saveRDS(
  dat,
  "inst/rmarkdown/templates/transmissibility/skeleton/data/covid_linelist_england.rds"
)
