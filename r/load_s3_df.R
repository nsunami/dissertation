load_s3_df <- function() {
    s3_df <- readRDS(
        here("data_public", "Study3_public.rds")
    ) %>%
        dplyr::filter(Finished == 1)
}
