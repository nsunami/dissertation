# Total number of participants by attention check
s2_count_participants_by_attention_check <- function(df) {
    df %>%
        summarise(
            across(
                .cols = c(
                    attention_rejection_correct,
                    attention_VG_correct,
                    attention_all_correct
                ),
                .fns = list(
                    correct = sum,
                    failed = ~ sum(!.)
                )
            )
        ) |>
        as.list() %>%
        # append total N
        append(., list(total = nrow(df)))
}
