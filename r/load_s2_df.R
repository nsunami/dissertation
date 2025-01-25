# Load Study 2 Data
load_s2_df <- function() {
    s2_df <- readRDS(
        here::here("data_public", "Study2_public.rds")
    )
    ## Factorize Essay Conditionsw
    s2_df |>
        dplyr::mutate(
            essay_condition = factor(
                essay_condition,
                levels = c("Social Surrogacy", "Non-Social Surrogacy")
            )
        )
}
