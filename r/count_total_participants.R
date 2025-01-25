source(here::here("r", "s2_count_participants_by_attention_check.R"))
source(here::here("r", "load_s2_df.R"))
source(here::here("r", "load_s3_df.R"))
source(here::here("r", "load_s1c.R"))
source(here::here("r", "namifunc.R"))

count_total_participants <- function() {
    # s1a
    s1a_df <- rio::import(here::here("data_public/Study1a_public.rds"))
    # s1b
    s1b_df <- rio::import(here::here("data_public/Study1b_public.rds"))
    # s1c
    s1c_df <- load_s1c()
    # s1d
    s1d_df <- rio::import(here::here("data_public/Study1d_public.rds"))
    # s1e
    s1e_df <- rio::import(here::here("data_public/Study1e_public.rds"))

    s1a_n <- s1a_df |>
        complete.cases() |>
        sum()
    s1b_n <- s1b_df |>
        dplyr::distinct(id) |>
        nrow()
    s1c_n <- s1c_df |> length()
    s1d_n <- s1d_df |> length()
    s1e_n <- s1e_df |> nrow()
    # Total participants
    s1_n_total <- s1a_n + s1b_n + s1c_n + s1d_n + s1e_n

    # Study 2
    s2_df <- load_s2_df()
    s2_attention_APA <- s2_df |>
        s2_count_participants_by_attention_check()

    # Study 3
    s3_df <- load_s3_df()
    s3_attention_df <- s3_df |>
        dplyr::group_by(attention_all_correct) |>
        dplyr::summarise(n = n()) |>
        deframe_as_list()

    # Sum up
    s1_n_total + s2_attention_APA$total + s3_attention_df$"TRUE"
}
