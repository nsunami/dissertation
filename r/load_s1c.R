load_s1c <- function() {
  df <- readRDS(here::here("data_public/Study1c_public.rds"))

  df <- df |>
    dplyr::mutate(grouping_dummy = dplyr::case_when(
      as.character(acceptance) == "0" & as.character(closeness) == "0" ~ "Rejected by Stranger",
      as.character(acceptance) == "1" & as.character(closeness) == "0" ~ "Accepted by Stranger",
      as.character(acceptance) == "0" & as.character(closeness) == "1" ~ "Rejected by Close Other",
      as.character(acceptance) == "1" & as.character(closeness) == "1" ~ "Accepted by Close Other",
      TRUE ~ NA_character_
    )) |>
    # Factors should be effect-coded
    # Effect coding the factors
    dplyr::mutate(
      acceptanceEC = dplyr::case_when(
        as.character(acceptance) == "0" ~ -0.5,
        as.character(acceptance) == "1" ~ 0.5
      ),
      closenessEC = dplyr::case_when(
        as.character(closeness) == "0" ~ -0.5,
        as.character(closeness) == "1" ~ 0.5
      )
    )

  # Check if the resulting data frame includes expected values
  if (!("Rejected by Stranger" %in% df$grouping_dummy)) {
    stop("Rejected by Stranger was not found in the dummies")
  }
}
