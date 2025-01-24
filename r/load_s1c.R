load_s1c <- function() {
  df <- readRDS(here::here("data_public/Study1c_public.rds"))

  df |>
    dplyr::mutate(grouping_dummy = dplyr::case_when(
      labels(acceptance) == "0" & labels(closeness) == "0" ~ "Rejected by Stranger",
      labels(acceptance) == "1" & labels(closeness) == "0" ~ "Accepted by Stranger",
      labels(acceptance) == "0" & labels(closeness) == "1" ~ "Rejected by Close Other",
      labels(acceptance) == "1" & labels(closeness) == "1" ~ "Accepted by Close Other",
      TRUE ~ NA_character_
    )) |>
    # Factors should be effect-coded
    # Effect coding the factors
    dplyr::mutate(
      acceptanceEC = dplyr::case_when(
        labels(acceptance) == "0" ~ -0.5,
        labels(acceptance) == "1" ~ 0.5
      ),
      closenessEC = dplyr::case_when(
        labels(closeness) == "0" ~ -0.5,
        labels(closeness) == "1" ~ 0.5
      )
    )
}
