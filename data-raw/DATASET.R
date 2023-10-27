## code to prepare `DATASET` dataset goes here
full_data <-
  readRDS("inst/app/test_data.rds") %>%
  dplyr::mutate(
    birth_date = lubridate::ymd(birth_date),
    age = lubridate::as.period(lubridate::interval(birth_date, lubridate::today()))@year,
    preservation_media = "unspecified"
  )

full_data <-
  1:7 %>%
  purrr::map_dfr( ~ {
    df <- full_data %>%
      dplyr::mutate(
        url_bucket = stringr::str_replace_all(url_bucket, "6|4", as.character(.x)),
        aliquot_id = stringr::str_replace_all(aliquot_id, "4", as.character(.x)),
        project_id = stringr::str_replace_all(project_id, "4", as.character(.x)),
        project_name = stringr::str_replace_all(project_name, "6|4", as.character(.x)),
      )

    if (.x %% 2 == 0) {
      df <-
        df %>%
        dplyr::mutate(
          batch_id = stringr::str_replace_all(batch_id, "WMGS", "16S"),
          experiment_type_id = stringr::str_replace_all(experiment_type_id, "WMGS", "16S"),
          sub_class = "16S"
        )
    }
    df
  })

usethis::use_data(full_data, overwrite = TRUE, internal = TRUE)
