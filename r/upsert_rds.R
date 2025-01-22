#' Upsert an RDS file
#'
#' @param data
#' @param path
#'
#' @returns
#' @export
#'
#' @examples
upsert_rds <- function(data, path) {
  is_file_exists <- file.exists(path)

  temp_file <- tempfile()
  readr::write_rds(data, temp_file)

  if (!is_file_exists) {
    cat("The file does not exist. Creating a new file.")
    readr::write_rds(
      data,
      path
    )
  } else {
    checksum <- tools::md5sum(files = c(
      temp_file[1],
      path
    ))

    is_file_same <- checksum[1] == checksum[2]

    if (is_file_same) {
      cat("The RDS files are the same. Skipping saving")
    } else {
      cat("The RDS files are different. Overwriting the existing file ")
      readr::write_rds(
        data,
        path
      )
    }
  }
}
