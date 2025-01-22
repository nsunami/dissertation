create_s1_measures_table <- function() {
  # get data
  s1_measures_path <- here("data_public", "Study1_measures.csv")
  s1_measures_tbl <- read_csv(s1_measures_path)
  # add ronames
  s1_measures_tbl <- s1_measures_tbl %>% mutate(id = row_number())
  # abbreviate the labels
  s1_measures_tbl <- s1_measures_tbl %>%
    mutate(
      Validity = str_replace(Validity, "Convergent", "Con."),
      Validity = str_replace(Validity, "Discriminant", "Dis.")
    )
  # Starting positions for packing
  pack_starts <- s1_measures_tbl %>%
    mutate(rownum = row_number()) %>%
    group_by(Study, `Main Measure`) %>%
    mutate(Submeasure = row_number()) %>%
    mutate(
      start_num = case_when(Submeasure == 1 ~ rownum),
      end_num = case_when(Submeasure == n() ~ rownum)
    ) %>%
    ungroup() %>%
    fill(start_num, .direction = "down") %>%
    fill(end_num, .direction = "up") %>%
    filter(Submeasure == 1) %>%
    mutate(
      need_packing = (start_num != end_num),
      `Subscale (if any)` = case_when(need_packing == FALSE ~ `Main Measure`)
    ) %>%
    mutate(labels = case_when(need_packing == TRUE ~ `Main Measure`, TRUE ~ " ")) %>%
    mutate(intervals = end_num - start_num + 1) %>%
    select(id, Study, labels, intervals)

  # Join Table
  s1_measures_tbl <- s1_measures_tbl %>%
    left_join(pack_starts) %>%
    rename(
      "Measure" = `Subscale (if any)`,
      "Construct" = `Measured Construct`
    )

  # Clean NAs from the labels
  s1_measures_tbl <- s1_measures_tbl %>%
    mutate(labels = case_when(is.na(labels) ~ "", TRUE ~ labels))
  pack_starts_nested <- s1_measures_tbl %>%
    nest_by(Study) %>%
    ungroup()

  # List of df to feed to the function in-text to render table on the spot
  pack_starts_nested %>%
    select(Study, data) %>%
    deframe_as_list()
}
