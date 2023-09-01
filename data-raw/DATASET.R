## code to prepare `DATASET` dataset goes here
full_data <-
  readRDS("inst/app/test_data.rds") %>%
  dplyr::mutate(
    birth_date = lubridate::ymd(birth_date),
    age = lubridate::as.period(lubridate::interval(birth_date, lubridate::today()))@year,
    preservation_media = "unspecified"
  )

usethis::use_data(full_data, overwrite = TRUE, internal = TRUE)
