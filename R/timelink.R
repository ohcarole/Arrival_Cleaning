
#' Build ordered relative-day windows around a target date
#'
#' @description
#' Given a vector of terminal day offsets (e.g., `c(-10, 10, -90, 60)`) and a
#' scalar `target` (usually 0), returns a tibble of contiguous windows that
#' partition the range before and after the target, tagging each window as
#' `"prior"` (<= target) or `"after"` (> target) and assigning an increasing
#' `priority` (1, 2, ... in the order of `rng`).
#'
#' @details
#' The algorithm walks `rng` in order. For `"prior"` windows, it expands
#' backward from the target; for `"after"` windows, it expands forward. This
#' lets you interleave narrow/strict windows (e.g., -10:10) with broader
#' catch-alls (e.g., -90:60) at lower priority.
#'
#' @param rng integer/numeric vector of terminal boundaries (days) relative to `target`.
#'   Negative values fall on/before the target; positive values are after.
#'   Example: `c(-10, 10, -90, 60)`.
#' @param target integer/numeric scalar, usually 0 (the anchor day).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{priority} (integer): 1..length(rng) in the order processed
#'   \item \code{type} (chr): "prior" or "after"
#'   \item \code{start}, \code{end} (integer): inclusive relative-day bounds
#' }
#'
#' @examples
#' make_buffer_ranges(c(-10, 10, -90, 60))
#'
#' @export

make_buffer_ranges <- function(rng, target = 0) {
  # Initialize storage vectors
  priority_list <- c()
  type_list     <- c()
  start_list    <- c()
  end_list      <- c()

  prev <- target + 1
  post <- target

  for (index in seq_along(rng)) {
    priority_list <- c(priority_list, index)
    terminal <- rng[index]

    type <- if (terminal <= target) "prior" else "after"
    type_list <- c(type_list, type)

    if (type == "prior") {
      start_list <- c(start_list, terminal)
      end_list   <- c(end_list, prev - 1)
      prev <- terminal
    } else {
      start_list <- c(start_list, post + 1)
      end_list   <- c(end_list, terminal)
      post <- terminal
    }
  }

  # Return tibble
  tibble(
    priority = priority_list,
    type     = type_list,
    start    = start_list,
    end      = end_list
  )
}

#' Derive a target date and its source for each patient
#'
#' @description
#' Adds `targetdate` and `targetsource` to `df` by selecting the first
#' non-missing date among `date_priority`. The field name used is recorded in
#' `targetsource`. Keeps `idlist`, the two new columns, then `everything()`.
#'
#' @param df A data frame containing patient identifiers and date columns.
#' @param date_priority Character vector of column names (ordered by precedence).
#'   The first non-NA among these becomes `targetdate`.
#' @param idlist Character vector of identifier columns to keep at the front.
#'
#' @return The input `df` with two new columns:
#' \itemize{
#'   \item \code{targetdate} (Date)
#'   \item \code{targetsource} (chr; the name of the chosen column)
#' }
#'
#' @section Column requirements:
#' Columns in `date_priority` must exist in `df`. Missing ones are treated as
#' all-NA. `idlist` should exist; absent IDs are silently dropped by `select(any_of())`.
#'
#' @examples
#' set_target_date(
#'   df = tibble::tibble(recordid = 1, dxdate = as.Date("2023-01-01")),
#'   date_priority = c("dxngsdate","treatmentdate","arrivaldate","dxdate")
#' )
#'
#' @export

set_target_date <- function(df, 
                            date_priority = c("dxngsdate", "treatmentdate", "arrivaldate", "dxdate"),
                            idlist = c('recordid','ptmrn','ptlastname')) {
  # if (FALSE) {
  #   df <- tp53pop_
  #   date_priority = c("dxngsdate", "treatmentdate1", "arrivaldate1", "dxdate")
  # }
  
  df <- df |>
      mutate(
        targetdate = coalesce(!!!syms(date_priority)),
        targetsource = case_when(
          !!!purrr::imap(date_priority, function(var, i) {
            expr(!is.na(!!sym(var)) ~ !!var)
          })
        )
      ) |>
    select(any_of(idlist), targetsource, targetdate, everything())
    return(df)
  }

#' Build buffer windows for multiple target sources
#'
#' @description
#' For each `targetsource` (e.g., "dxngsdate", "treatmentdate1"), create the
#' same set of windows returned by `make_buffer_ranges(rng)`, and bind them
#' together. Adds a `targetsource` column.
#'
#' @param targetsources Character vector of target source names.
#' @param rng Numeric/integer vector passed to `make_buffer_ranges()`.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{targetsource} (chr)
#'   \item \code{priority}, \code{type}, \code{start}, \code{end}
#' }
#'
#' @examples
#' make_multi_source_buffers(
#'   targetsources = c("dxngsdate","treatmentdate1","arrivaldate1","dxdate"),
#'   rng = c(-10, 10, -90, 60)
#' )
#'
#' @export

make_multi_source_buffers <- function(targetsources = c("dxngsdate", "treatmentdate1", "arrivaldate1", "dxdate"),
                                      rng = c(-10, 10, -90, 60)) {
  purrr::map_dfr(targetsources, function(src) {
    make_buffer_ranges(rng) |>
      mutate(targetsource = src, .before = 1)
  })
}


#' Link tests to patients using buffer windows and priority
#'
#' @description
#' Cross joins each patient's `targetdate` with their candidate test dates,
#' filters to rows within the source-specific buffer windows, then chooses the
#' best match by \strong{priority} (1 = best), then \strong{abs(days_from_target)},
#' then \strong{earliest testdate} (implicit via arrange). Returns one row per
#' patient (by `idlist`) if a match exists.
#'
#' @param reference_df Data frame containing `idlist`, `targetdate`, and `targetsource`.
#'   Typically the result of `set_target_date()`, then reduced to one row per patient.
#' @param test_df Data frame containing test records, including `test_date_col`
#'   and optionally a unique test key like `LabOrderEpicId`. Must contain `idlist`.
#' @param idlist Character vector of patient identifiers (used for joining and grouping).
#' @param test_date_col Name of the test date column in `test_df` (string).
#' @param buffer_rules Tibble from `make_multi_source_buffers()` with columns
#'   `targetsource`, `priority`, `type`, `start`, `end`.
#' @param date_priority Optional: character vector of date columns to carry through
#'   to the output (e.g., your original `date_priority` list).
#' @param testkey column in test data that uniquely identifies a test set, defaults to 'LabOrderEpicId'
#'
#' @return A tibble with (at least):
#' \itemize{
#'   \item \code{idlist...}: the identifier columns
#'   \item \code{priority}: chosen window priority (integer)
#'   \item \code{days_from_target}: testdate - targetdate (integer)
#'   \item \code{targetsource}, \code{targetdate}
#'   \item \code{test_count}, \code{testdate}, \code{first_testdate}
#'   \item any of \code{date_priority} that exist in \code{reference_df}
#' }
#'
#' @section Selection rule:
#' Within each patient, rows are filtered to those inside `[start, end]` for that
#' patient’s `targetsource`, then sorted by `(priority, abs_diff)`, and the first
#' row is kept.
#'
#' @section Failure modes:
#' \itemize{
#'   \item Patients with no `targetdate` or no tests in-window will be dropped.
#'   \item If \code{reference_df} is not unique by \code{idlist}, the function errors.
#' }
#'
#' @examples
#' # See the Example Code block in your Rmd
#'
#' @export

match_buffered_testdate <- function(reference_df,
                                    test_df,
                                    idlist        = c("recordid", "ptmrn", "ptlastname"),
                                    test_date_col = "testdate",
                                    buffer_rules,
                                    date_priority = character(),
                                    testkey       = "LabOrderEpicId") {
  
  # testing code
  if (FALSE) {  
    reference_df  = pop.target
    test_df       = NGS_Long
    idlist        = c("recordid", "ptmrn", "ptlastname")
    test_date_col = 'CollectionDate'
    buffer_rules  = buffer_rules
    date_priority = date_priority
    testkey       = "LabOrderEpicId"
  }
  
  # keep only IDs + target cols; ensure 1 row per patient
  reference_df <- reference_df |> select(any_of(idlist), starts_with('target'))
  if (nrow(reference_df) != nrow(distinct(reference_df, across(all_of(idlist))))) {
    stop("reference_df contains duplicate rows for one or more patients based on idlist.")
  }

  # Normalize test date
  test_df <- test_df |> mutate(testdate = as.Date(.data[[test_date_col]]))  
  
  # Compute keylist = idlist + testkey (only if present)
  has_testkey <- !is.null(testkey) && length(testkey) == 1L && has_name(test_df, testkey)
  keylist     <- if (has_testkey) unique(c(idlist, testkey)) else idlist

  # Compute test count and first test date per patient
  test_summary <- test_df |>
    filter(!is.na(testdate)) |> 
    group_by(across(all_of(keylist))) |>
    summarise(
      test_count     = n(),
      testdate       = min(testdate),
      first_testdate = min(testdate),
      .groups = "drop"
    )
  
  # Join back to get full row from test_df
  test_detail <- test_df |> right_join(test_summary, by = c(keylist, 'testdate'))  
  
  # Perform left join to get test summary per patient
  # df_ <- left_join(reference_df, test_detail, by = idlist)
  df <- left_join(reference_df, test_detail, by = idlist) |> 
    filter(!is.na(targetdate), !is.na(testdate)) |>
    mutate(
      days_from_target = as.integer(testdate - targetdate),
      abs_diff = abs(days_from_target),
      relative_label = case_when(
        days_from_target == 0 ~ "on target date",
        days_from_target <  0 ~ paste0(abs_diff, " days prior"),
        days_from_target >  0 ~ paste0(days_from_target, " days after"),
        .default = 'missing'
      )
    )
  
  
  # Match to buffer rules and get best test date
  # Cross-join reference and test dates
  df_with_window <- df |>
    inner_join(buffer_rules, by = "targetsource", relationship = "many-to-many") |>
    filter(days_from_target >= start, days_from_target <= end) |>
    group_by(across(all_of(idlist))) |>
    arrange(priority, abs_diff, .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(matched = TRUE) |>
    select(any_of(idlist),
           priority,
           days_from_target,
           relative_label,
           targetsource,
           targetdate,
           test_count,
           testdate,
           first_testdate,
           any_of(date_priority),
           everything(),
           -start, -end, -matched)
  
  return(df_with_window)
}

