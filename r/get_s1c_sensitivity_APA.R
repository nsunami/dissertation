get_s1c_sensitivity_APA <- function(df) {
  s1c_sensitivity_mod <- t.test(formula = heart2 ~ acceptance, data = df)

  ## Get Cohen's d
  s1c_sensitivity_d <- effectsize::cohens_d(
    heart2 ~ acceptance,
    data = df,
    ci = .95
  )

  df |>
    describe_by_factor(acceptance, vars = heart2) |>
    dplyr::mutate(
      msd = to_msd(heart2_mean, heart2_sd),
      acceptance = to_factor(acceptance)
    ) |>
    select(acceptance, msd) |>
    tibble::deframe() |>
    as.list() |>
    append(list(
      t = s1c_sensitivity_mod |> describe.ttest(),
      d = describe_d_ci(s1c_sensitivity_d)
    ))
}
