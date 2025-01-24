get_s1c_sensitivity_APA <- function(df) {
    df %>%
        describe_by_factor(acceptance, vars = heart2) %>%
        mutate(
            msd = to_msd(heart2_mean, heart2_sd),
            acceptance = to_factor(acceptance)
        ) %>%
        select(acceptance, msd) %>%
        deframe() %>%
        as.list() %>%
        append(list(
            t = s1c_sensitivity_mod %>% describe.ttest(),
            d = describe_d_ci(s1c_sensitivity_d)
        ))
}
