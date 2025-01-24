load_s1c <- function() {
    rio::import(here("data_public/Study1c_public.rds")) |>
        mutate(grouping_dummy = case_when(
            acceptance == "0" & closeness == "0" ~ "Rejected by Stranger",
            acceptance == "1" & closeness == "0" ~ "Accepted by Stranger",
            acceptance == "0" & closeness == "1" ~ "Rejected by Close Other",
            acceptance == "1" & closeness == "1" ~ "Accepted by Close Other"
        )) |>
        # Factors should be effect-coded
        # Effect coding the factors
        mutate(
            acceptanceEC = case_when(
                acceptance == "0" ~ -0.5,
                acceptance == "1" ~ 0.5
            ),
            closenessEC = case_when(
                closeness == "0" ~ -0.5,
                closeness == "1" ~ 0.5
            )
        )
}
