# Load Study 2 Data
load_s2_df <- function() {
    s2_df <- read_rds(here("data_public", "Study2_public.rds"))
    ## Factorize Essay Conditions
    s2_df %>%
        mutate(essay_condition = factor(essay_condition,
            levels = c("Social Surrogacy", "Non-Social Surrogacy")
        ))
}
