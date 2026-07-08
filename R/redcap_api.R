del_rc_records <- function(paramsect, id_field = NULL, silent = TRUE) {
  
  
  if (FALSE) {
    id_field  = NULL
    paramsect = 'karyo_lookup'
    silent    = FALSE
  }
  
  
  if (is.null(paramsect)) {
    stop("You must provide a REDCap parameter section.")
  }
  params <- if (typeof(paramsect)=='character') {
    get_configset(paramsect)
  } else {
    paramsect
  }  

  # Extract and decrypt token
  section   <- params$section
  api_token <- params$encrypted_token
  api_url   <- params$url
  keyfields <- params$keyfields

  # Default to first keyfield if none explicitly provided
  if (is.null(id_field)) {
    id_field <- keyfields[1]
  }

  if (!silent) message("Fetching record IDs from REDCap using field: ", id_field)

  # Step 1: Get record IDs
  export_data <- list(
    token        = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    content      = 'record',
    format       = 'json',
    type         = 'flat',
    fields       = id_field,
    returnFormat = 'json'
  )

  response_export <- httr::POST(api_url, body = export_data, encode = "form")
  if (httr::status_code(response_export) != 200) {
    stop("Failed to fetch records: ", httr::content(response_export, "text", encoding = "UTF-8"))
  }

  all_records <- jsonlite::fromJSON(httr::content(response_export, as = "text", encoding = "UTF-8"))
  record_ids  <- all_records[[id_field]]

  if (length(record_ids) == 0) {
    if (!silent) message("No records found to delete.")
    return(invisible(NULL))
  }

  # Step 2: Delete records
  delete_data <- list(
    token        = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    action       = 'delete',
    content      = 'record',
    returnFormat = 'json'
  )

  for (i in seq_along(record_ids)) {
    delete_data[[paste0("records[", i - 1, "]")]] <- record_ids[i]
  }

  response_delete <- httr::POST(api_url, body = delete_data, encode = "form")

  if (httr::status_code(response_delete) != 200) {
    stop("Failed to delete records: ", httr::content(response_delete, "text", encoding = "UTF-8"))
  }

  result <- httr::content(response_delete, as = "text", encoding = "UTF-8")
  if (!silent) message("Deletion result: ", result)
  
  return(result)
}


get_rc_changes <- function(fld_code_df=NULL, log_df=NULL, weeks=1, fld=NULL, section='esteylist') {

  if(FALSE){
    fld_code_df=NULL
    log_df=NULL
    weeks=52
    fld=field_of_interest
    section='esteylist'
  }
  
  # no fld and no dictionary, oughta here
  if(is.null(fld_code_df) & is.null(fld)) return(fld_code_df=log_df[0,])

  # no fld dictionary sent in
  if(is.null(fld_code_df) & !is.null(fld)) fld_code_df <- get_rc_code(fld, section=section)

  # no fld name sent in
  if(!is.null(fld_code_df) & is.null(fld)) fld <- fld_code_df[[1, 'fld']]

  # no log, go grab one for the most recent n weeks
  if(is.null(log_df)) log_df <- get_rc_log(section=section, weeks=weeks)

  if(fld_code_df[1, 'type']!='checkbox') {
    # Use the dynamically generated codelist expression in mutate when of type checkbox
    rslt_df <- log_df |>
      filter(stringr::str_detect(details, fld)) |>
      mutate(
        field=fld,
        changed_date = lastupdate) |>
      select(names(log_df), starts_with(c('field', 'change'))) |>
      distinct()
  
  } else {
    # Initialize the codelist expression
    fld_expr <- '' # this is the computer readable version
    fld_view <- '' # this is just the human readable version

    for(i in 1:nrow(fld_code_df)) {
      code <- fld_code_df[i, 'code']
      fld_expr <- paste0(fld_expr, glue::glue('if_else(stringr::str_detect(details, "{fld}\\\\({code}\\\\) = "), "{code}", "NA"), '))
      fld_view <- paste0(fld_view, '\t', glue::glue('if_else(stringr::str_detect(details, "{fld}\\\\({code}\\\\) = "), "{code}", "NA"), '), '\n')
      }
    
    # Wrap the expression with paste and str_c correctly
    fld_expr <- paste0('paste(stringr::str_c(', fld_expr, ' sep = " @@@ "))')
    fld_view <- paste0('paste(stringr::str_c(\n', fld_view, '\tsep = " @@@ "))')
    
    cat('\nField expression:\n', fld_view, '\n')

    # Use the dynamically generated codelist expression in mutate when of type checkbox
    rslt_df <- log_df |>
      filter(stringr::str_detect(details, fld)) |>
      mutate(
        field=fld,
        changed_date = lastupdate,
        # Use the codes vector to detect special population codes in the details column
        codelist = eval(parse(text = fld_expr))  # Parsing and evaluating the generated expression
      ) |>
      # Separate the codelist into individual rows based on the separator ' @@@ '
      separate_rows(codelist, sep = " @@@ ") |>
      # Filter out NA values
      filter(codelist != 'NA') |>
      group_by(timestamp, username, action, record) |>
      mutate(changed_codes = paste(codelist, collapse = " | "),
             changed_code_cnt = n()) |>
      ungroup() |>
      select(names(log_df), starts_with(c('field', 'change'))) |>
      distinct()
    }


  return(rslt_df)
}


#' Retrieve Exploded REDCap Field Codes
#'
#' Loads and formats a REDCap dictionary for a specified section and set of fields. Ensures the dictionary is in "exploded" format, with codes and values separated into rows.
#'
#' @param flds Optional character vector of field names to subset. Defaults to all fields in the section.
#' @param section REDCap section name, e.g. 'esteylist'.
#' @param only_codes Logical; if TRUE, returns only coded fields (ignores calculations and uncoded).
#' @return A tibble with one row per code/value combination.
#' @export
get_rc_code <- function(flds = NULL, section = "esteylist", only_codes = FALSE) {
  # Ensure local scope for rcdd assignment
  # rcdd <- if (exists("rcdd_exp", inherits = FALSE) && "code" %in% names(rcdd)) {
  #   rcdd
  # } else {
  #   get_rc_dictionary(section, reload = TRUE, explode = TRUE)
  # }
  
  rcdd <- get0("rcdd_exp",
               envir = get0(".pkg_env", envir = .GlobalEnv, inherits = FALSE, ifnotfound = .GlobalEnv),
               inherits = FALSE,
               ifnotfound = NULL)

  if (is.null(rcdd) || !"code" %in% names(rcdd)) {
    rcdd <- get_rc_dictionary(section,
                              ddname    = "rcdd_exp",
                              reload    = FALSE,   # <-- respect existing cache
                              explode   = TRUE,
                              assign_to = "pkg_env")
  }

  # Field selection: default to all fields if flds is NULL
  flds <- if (!is.null(flds)) flds else rcdd$field_stem

  field_dd <- rcdd |>
    filter(field_stem %in% flds) |>
    mutate(is_coded = (field_type != "calc") & !is.na(code) & code != "") |>
    filter(!only_codes | is_coded) |>
    mutate(field_code_name = ifelse(field_type == "checkbox",
                                    paste(field_name, code, sep = "___"),
                                    field_name)) |>
    relocate(field_code_name, .after = field_name) |>
    select(form_name, field_stem, field_code_name, field_type, code, any_of(c('value', 'label')), is_coded)

  return(field_dd)
}


#' Get code/label map for one REDCap field
#'
#' @param fld   REDCap field_name (stem for checkbox)
#' @param section  Config section, e.g. "esteylist"
#' @return tibble(field_name, code, label, field_type)
get_rc_codemap <- function(fld, section = "esteylist") {

  rcddexp <- get_rc_dictexp(section, reload = FALSE)  # you already export this [file:1]
  out <- rcddexp |>
    dplyr::filter(.data$field_name == !!fld,
                  !is.na(.data$code),
                  .data$field_type %in% c("radio", "dropdown", "checkbox")) |>
    dplyr::select(field_name  = .data$field_name,
                  field_type = .data$field_type,
                  code       = .data$code,
                  label      = .data$label)


  
  if (!nrow(out)) {
    stop("No coded choices found for field ", fld, call. = FALSE)
  }
  
  out
}

# Other versions of the same function
get_rc_map            <- function(fld, section = "esteylist") get_rc_codemap(fld, section)
get_rc_code_label_map <- function(fld, section = "esteylist") get_rc_codemap(fld, section)


#' Build R code snippet to recode REDCap *codes* to *labels*
#'
#' Example use:
#'   cat(rc_snippet_codes_to_labels("labsavailablility"))
#'   df <- df |>
#'     dplyr::mutate(labsavailablility = dplyr::recode(labsavailablility, !!!labsavailablility_map))
#'
#' @param fld      REDCap field_name
#' @param section  Config section for dictionary
#' @param object   Name of object to create in the snippet
#' @return character(1) containing R code
get_rc_snippet <- function(fld,
                           type = c('code_to_label', 'label_to_code'),
                           section = "esteylist",
                           object = paste0(fld, "_map")) {

  type <- match.arg(type)
  dict <- get_rc_codemap(fld, section = section)

  if (!nrow(dict)) {
    stop("No codes found for field ", fld, call. = FALSE)
  }

  # Ensure safe quoting; REDCap codes are often simple but labels may have commas, quotes, etc.
  code_vec  <- vapply(dict$code,  deparse, character(1))
  label_vec <- vapply(dict$label, deparse, character(1))

  # names = labels, values = codes OR the reverse depending on use; for recode(codes â†’ labels)
  # we want named by codes? No â€“ recode(old = "new"); old = code, value = label.
  pairs <- if (type == 'code_to_label') {
    paste0(code_vec, " = ", label_vec)
  } else {
    paste0(label_vec, " = ", code_vec)
  }

  paste0(
    object, " <- c(\n  ",
    paste(pairs, collapse = ",\n  "),
    "\n)\n"
  )
}

get_rc_browser_session <- function(report_url = NULL) {
  # library(httr) should be installed already
  if (is.null(report_url)) {
    report_url <- "https://redcap.iths.org/redcap_v17.1.1/DataExport/index.php?pid=51231"
  }
  parsed <- parse_url(report_url)
  base_url <- paste0(parsed$scheme, "://", parsed$hostname)

  # library(chromote) should be installed already
  b <- chromote::ChromoteSession$new()
  b$go_to(base_url)
  b$view()
  b  
}

#' Match and Assign Standardized Checkbox Codes
#'
#' Matches cleaned values from a dataframe to the best entry in a REDCap dictionary, assigning the standardized code, field name, or label. Matching prioritizes exact set equality, then maximum intersection (unless \code{require_exact = TRUE}); rows with missing source values yield NA.
#'
#' @param x A data frame containing the column to match (labels/codes, etc.).
#' @param dict An exploded REDCap dictionary (must include columns specified by \code{linkby}, \code{code}, and other result fields).
#' @param result_type Which standardized field from the dictionary to assign. One of \code{"field_name"}, \code{"code"}, or \code{"label"}; defaults to \code{"field_name"}.
#' @param linkby Which column to use for matching. One of \code{"label"} or \code{"code"}; defaults to \code{"label"}.
#' @param require_exact Logical, default \code{FALSE}. If \code{TRUE}, only exact matches are assigned; unmatched or partial matches return \code{NA}.
#'
#' @return A data frame identical to \code{x}, with an appended column \code{<result_type>_best_match} containing the best matching dictionary value or \code{NA}.
#'
#' @examples
#' \dontrun{
#' dict <- rcdd[rcdd$field_stem == "racecode", ]
#' zz <- get_rc_chkbox(your_demographics_df, dict, require_exact = TRUE)
#' }
#' @export
get_rc_chkbox <- function(x, dict, result_type = c("field_name", "code", "label"), linkby = c("label", "code"), require_exact = FALSE) {
  # Internal text cleaning function
  text_scrub <- function(vec) {
    txt <- tolower(vec)
    txt <- stringr::str_remove_all(txt, "\\s*\\[.*\\]")
    txt <- stringr::str_remove_all(txt, "\\b(to|or|of|is)\\b")
    txt <- stringr::str_replace_all(txt, "-", " ")
    txt <- stringr::str_replace_all(txt, "non", "not")
    txt <- stringr::str_replace_all(txt, "latino/a", " latino latina")
    txt <- stringr::str_squish(txt)
    txt <- stringr::str_split(txt, pattern = '\\s')
    txt <- lapply(txt, sort)
    txt
  }

  result_type <- match.arg(result_type)
  linkby <- match.arg(linkby)

  if (!(linkby %in% names(x)) || !(linkby %in% names(dict))) {
    stop("The specified linkby column must exist in both the data frame and dictionary.")
  }
  if (!'code' %in% names(dict)) stop("Dictionary parameter must be an exploded REDCap dictionary.")

  x_raw      <- x[[linkby]]
  x_terms    <- text_scrub(x_raw)
  dict_terms <- text_scrub(dict[[linkby]])

  best_matches <- vapply(seq_along(x_terms), function(i) {
    if (is.na(x_raw[i])) return(NA_character_)
    x_term <- x_terms[[i]]
    eq_match <- vapply(dict_terms, function(dict_term) setequal(x_term, dict_term), logical(1))
    if (any(eq_match)) {
      return(dict[[result_type]][which(eq_match)[1]])
    } else if (!require_exact) {
      overlaps <- vapply(dict_terms, function(dict_term) length(intersect(x_term, dict_term)), integer(1))
      return(dict[[result_type]][which.max(overlaps)])
    } else {
      return(NA_character_)
    }
  }, character(1))

  new_col <- paste0(result_type, "_best_match")
  x[[new_col]] <- best_matches
  x
}

get_rc_choices <- function(fld='racecode', label = TRUE, section='esteylist') {
  # Load REDCap dictionary if not already loaded
  get_rc_dictionary(section)


  # Get the field row from rcdd
  row <- rcdd[rcdd$field_stem == fld, ]

  if (nrow(row) == 0) {
    stop(paste("Field", fld, "not found in rcdd"))
  }

  # Get the choices string
  choices_str <- row$select_choices_or_calculations

  # Split into key-value pairs by |
  choices <- unlist(strsplit(choices_str, " \\| "))

  # Split each pair by comma
  split_choices <- strsplit(choices, ",\\s*")

  # Build named vector: names = label, values = code (or reversed if label = FALSE)
  if (label) {
    out <- setNames(sapply(split_choices, `[`, 2), sapply(split_choices, `[`, 1))
  } else {
    out <- setNames(sapply(split_choices, `[`, 1), sapply(split_choices, `[`, 2))
  }

  return(out)
}

#' Build REDCap checkbox variable names for a field
#' e.g., "arrivaleln17___1", "arrivaleln17___2", ...
#'
#' @param field   REDCap field base name (the checkbox field name)
#' @param section Optional config/section passed through to get_rc_choices()
#' @param sanitize Whether to replace non-alnum in codes with '_' (default FALSE)
#' @return Character vector of variable names
get_rc_chkbox_vars <- function(field, section = NULL, sanitize = FALSE) {
  # Grab choices; with label=TRUE the vector is named by codes
  ch <- get_rc_choices(field, section = section, label = TRUE)

  # Codes: prefer names(ch); if NULL, fall back to the vector itself
  codes <- names(ch)
  if (is.null(codes)) codes <- as.character(ch)

  if (sanitize) {
    # Only if you really need to coerce weird codes
    codes <- gsub("[^A-Za-z0-9_]", "_", codes, perl = TRUE)
  }

  vars <- paste0(field, "___", codes)
  return(vars)
}

#' Map of REDCap checkbox variables to codes and labels
#'
#' @param field   REDCap field base name (the checkbox field name)
#' @param section Optional config/section passed through to get_rc_choices()
#' @param sanitize Whether to replace non-alnum in codes with '_' (default FALSE)
#' @return tibble with columns: var, code, label
get_rc_chkbox_map <- function(field, section = NULL, dict = NULL) {
  if(is.null(dict) | !'code' %in% names(dict)) {
    dict <- get_rc_dictionary(section, 'dict', explode=TRUE)
  }
  rcdd |> filter(field_stem=='racecode') |> select(field_stem, field_name, code, label)
}
# get_rc_choices('racecode')
# rcdd |> filter(field_stem=='racecode') |> select(code, label)


#' Retrieve a Cached REDCap Dictionary
#'
#' Returns a cached data dictionary object (typically named `rcdd`) from the
#' specified environment if it exists.
#'
#' @param ddname Character name of the dictionary object to retrieve (default `"rcdd"`).
#' @param env Environment to look for the object in (default is `parent.frame()`).
#'
#' @return The dictionary object if found, or `NULL` if not present.
#' 
#' #' @family get_rc_dictionary
#'
#' @examples
#' \dontrun{
#' # Retrieve cached REDCap dictionary
#' rcdd <- get_rc_dictcache()
#' }
#'
get_rc_dictcache <- function(ddname = "rcdd", env = parent.frame()) {
  get0(ddname, envir = env, inherits = FALSE)
}

#' Retrieve and Automatically Explode a REDCap Dictionary
#'
#' A thin wrapper around [get_rc_dictionary()] that always expands checkbox fields
#' into row-level codes using `.explode_rc_dictionary()`.
#'
#' @inheritParams get_rc_dictionary
#' 
#' @family get_rc_dictionary
#'
#' @return A tibble representing the exploded REDCap dictionary.
#'
#' @examples
#' \dontrun{
#' # Fetch and explode the REDCap dictionary for 'esteylist'
#' rcdd_exp <- get_rc_dictexp("esteylist", reload = TRUE)
#' }
#'
#' @seealso [get_rc_dictionary()], [.explode_rc_dictionary()]
#' @export
get_rc_dictexp <- function(section, ...) {
  get_rc_dictionary(section, explode = TRUE, ...)
}

# for testing
if (FALSE) {
  params_or_section  = 'esteylist'
  ddname  = "rcdd_exp"
  reload  = FALSE
  explode = TRUE
  explode_order = "nat"
  keep_orders   = FALSE
  assign_to     = "pkg_env"
  assign_env    = NULL
  silent        = TRUE  
}
# get_rc_dictionary(section, explode = TRUE, ...)
#' Expand Checkbox Fields into Row-Level REDCap Codes
#'
#' Takes a raw REDCap data dictionary and expands any checkbox fields
#' so that each individual checkbox choice (code/label pair) becomes its
#' own row. Adds ordering columns (`nat_order`, `code_order`, `lab_order`)
#' and preserves original metadata.
#'
#' @param rcdd A REDCap data dictionary (data frame or tibble) as returned by
#'   `REDCapR::redcap_metadata_read()` or `get_rc_dictionary()`.
#' @param ord Character string specifying the sort order to apply. One of:
#'   \code{"nat"} (natural order), \code{"code"}, or \code{"label"}. Defaults to \code{"nat"}.
#' @param keep_orders Logical; if TRUE, retains all order columns (`*_order`)
#'   in the returned tibble.
#'
#' @return A tibble identical to \code{rcdd} except that each checkbox choice
#'   is represented as its own row with added columns:
#'   \itemize{
#'     \item \code{field_stem} â€” original field name before explosion
#'     \item \code{code}, \code{label} â€” extracted choice code and label
#'     \item \code{code_int} â€” numeric version of code (if convertible)
#'     \item \code{sort_key} â€” sort key combining row and rank
#'   }
#'
#' @details
#' The function automatically adds a temporary \code{rowid} if one does not exist.
#' Sorting order columns are computed within each field. If \code{keep_orders = FALSE},
#' these columns are dropped from the result.
#'
#' @examples
#' \dontrun{
#' rcdd <- get_rc_dictionary("esteylist", reload = TRUE)
#' exploded <- .explode_rc_dictionary(rcdd, ord = "code")
#' }
#'
#' @noRd
#' @keywords internal
#' @importFrom dplyr mutate filter select arrange relocate row_number min_rank if_else coalesce any_of
#' @importFrom tidyr unnest
#' @importFrom stringr str_count str_split str_trim str_extract str_remove
#' @export
#' @family get_rc_dictionary

if (FALSE) {
  rcdd = get_rc_dictionary('esteylist')
  ord = "code"
  keep_orders = FALSE
}


.explode_rc_dictionary <- function(rcdd,
                                   ord = c("nat", "code", "label"),
                                   keep_orders = FALSE) {
  
  # figure out how we are ordering
  ord <- match.arg(ord)
  
  # Track whether we had rowid to begin with
  started_with_rowid <- "rowid" %in% names(rcdd)
  
  # Ensure we have rowid to rank/sort within original rows
  exploded <- rcdd
  if (!started_with_rowid) {
    exploded <- exploded |>
      dplyr::mutate(rowid = row_number())
  }
  
  # Which order columns to retain
  order_cols <- if (isTRUE(keep_orders)) {
    c("nat_order", "code_order", "lab_order")
  } else {
    character(0)
  }
  
  # Base dictionary split:
  # - non-choice fields (kept as-is)
  # - choice fields, to be exploded
  non_choice <- exploded |>
    filter(!str_detect(field_type, "check|radio|drop")) |>
    mutate(sort_key = as.numeric(rowid))
  
  choice_src <- exploded |>
    filter(str_detect(field_type, "check|radio|drop")) |>
    mutate(
      choices_raw = coalesce(select_choices_or_calculations, "")
    ) |>
    filter(str_count(choices_raw, ",") >= 1)
  
  # If there are no fields to explode, just return the original dictionary
  if (!nrow(choice_src)) {
    rcddexp <- exploded |>
      arrange(rowid) |>
      select(any_of(names(rcdd)))
    if (!started_with_rowid && "rowid" %in% names(rcddexp)) {
      rcddexp <- dplyr::select(rcddexp, -rowid)
    }
    return(rcddexp)
  }
  
    # Explode choices
  exploded_choices <- choice_src |>
    mutate(
      choice_list = str_split(choices_raw, "\\s*\\|\\s*")
    ) |>
    tidyr::unnest(choice_list) |>
    filter(choice_list != "") |>
    mutate(
      field_stem = field_name,
      code       = str_trim(str_extract(choice_list, "^[^,]+")),
      label      = str_trim(str_remove(choice_list, "^[^,]+,")),
      field_name = if_else(
        field_type == "checkbox",
        paste0(field_name, "___", code),
        field_stem
      ),
      code_int   = suppressWarnings(as.integer(code))
    ) |>
    # Guard: no blank/NA codes
    filter(!is.na(code), code != "") |>
    mutate(
      nat_rank  = row_number(),
      code_rank = min_rank(if_else(is.na(code_int), Inf, code_int)),
      lab_rank  = rank(label, ties.method = "first"),
      .by = rowid
    ) |>
    mutate(
      nat_order  = rowid + nat_rank / 1000,
      code_order = rowid + code_rank / 1000,
      lab_order  = rowid + lab_rank / 1000,
      sort_order = nat_order # default
    ) |>
    select(
      any_of(names(rcdd)),
      field_stem, code, label, 
      rowid, ends_with('_order')
    )

  # set order if needed
  if (ord == 'code') {
    exploded_choices <- exploded_choices |> mutate(sort_order = code_order)
  } else if (ord == 'lab') {
    exploded_choices <- exploded_choices |> mutate(sort_order = lab_order)
  }
  exploded_choices <- exploded_choices |> arrange(sort_order)
  
  # Combine non-choice and exploded choice rows
  rcddexp <- bind_rows(non_choice, exploded_choices) |>
    arrange(sort_key) |>
    select(any_of(names(rcdd)), field_stem, code, label, any_of(order_cols))
  
  # Drop ordering columns and temporary rowid if we created it
  rcddexp <- select(rcddexp, -ends_with('_order'))
  if (!started_with_rowid && "rowid" %in% names(rcddexp)) {
    rcddexp <- select(rcddexp, -rowid)
  }
  
  rcddexp
}



if (FALSE) {
  params_or_section  = 'esteylist'
  ddname  = "rcdd_exp"
  reload  = FALSE
  explode = TRUE
  explode_order = "nat"
  keep_orders   = FALSE
  assign_to     = "pkg_env"
  assign_env    = NULL
  silent        = TRUE  
}
# get_rc_dictionary(section, explode = TRUE, ...)

#' Retrieve and Optionally Explode a REDCap Data Dictionary
#'
#' Downloads a REDCap metadata dictionary from the specified project (or retrieves
#' a cached version if already loaded) and optionally expands checkbox fields into
#' separate rows ("exploded" form). Supports flexible assignment to environments,
#' reload control, and ordering options for exploded fields.
#'
#' @param params_or_section Either a configuration list (containing at least
#'   \code{section}, \code{encrypted_token}, and \code{url}) or the name of a
#'   REDCap configuration section known to \code{get_configset()}.
#' @param ddname Character string; name of the object to assign the dictionary to
#'   (default \code{"rcdd"}).
#' @param reload Logical; if \code{TRUE}, forces re-download even if a cached copy
#'   exists in the specified environment.
#' @param explode Logical; if \code{TRUE}, expands checkbox fields into separate
#'   rows using \code{.explode_rc_dictionary()}. Automatically enabled if
#'   \code{explode_order} is provided.
#' @param explode_order Character string specifying sort order for exploded
#'   fields: one of \code{"nat"}, \code{"code"}, or \code{"label"}. Defaults to
#'   \code{"nat"}.
#' @param keep_orders Logical; if \code{TRUE}, keeps order-ranking columns
#'   (\code{nat_order}, \code{code_order}, \code{lab_order}) from the exploded
#'   dictionary.
#' @param assign_to Character; determines where the resulting dictionary is
#'   assigned. One of \code{"caller"}, \code{"none"}, \code{"global"},
#'   \code{"pkg_env"}, or \code{"env"} (default \code{"caller"}).
#' @param assign_env An explicit environment to assign to when
#'   \code{assign_to = "env"}.
#' @param silent Logical; if \code{TRUE}, suppresses informational messages and
#'   warnings.
#'
#' @details
#' The function uses \code{REDCapR::redcap_metadata_read()} to retrieve metadata,
#' decrypting the stored API token with \code{safer::decrypt_string()} and a
#' locally stored key from \code{get_encryptkey()}.  
#'  
#' When \code{explode = TRUE}, checkbox fields are processed using
#' \code{.explode_rc_dictionary()}, which expands each choice code and label into
#' its own row. The exploded dictionary is re-sorted according to the specified
#' \code{explode_order}.
#'
#' Cached copies are stored in the selected environment according to
#' \code{assign_to}. Subsequent calls will return the cached version unless
#' \code{reload = TRUE}.
#'
#' @return A tibble representing the REDCap dictionary (exploded or not).  
#'   The object is also assigned to the specified environment unless
#'   \code{assign_to = "none"}.
#'
#' @examples
#' \dontrun{
#' # Load and cache the dictionary for 'esteylist'
#' rcdd <- get_rc_dictionary("esteylist", reload = TRUE)
#'
#' # Explode checkbox fields and sort by code
#' rcdd_exp <- get_rc_dictionary("esteylist", explode_order = "code", reload = TRUE)
#'
#' # Assign to .pkg_env for package-wide access
#' get_rc_dictionary("mpal", reload = TRUE, assign_to = "pkg_env")
#' }
#'
#' @seealso [.explode_rc_dictionary()]
#' @importFrom dplyr mutate filter arrange select relocate bind_rows row_number starts_with
#' @importFrom safer decrypt_string
#' @importFrom REDCapR redcap_metadata_read
#' @importFrom rlang "%||%"
#' @export
#' @family get_rc_dictionary
#' 
#' 
#' 
get_rc_dictionary <- function(params_or_section = NULL,
                              ddname        = "rcdd",
                              reload        = FALSE,
                              explode       = FALSE,
                              explode_order = NULL, # c("nat","code","label"),
                              keep_orders   = FALSE,
                              assign_to     = c("caller","none","global","pkg_env","env"),
                              assign_env    = NULL,
                              silent        = TRUE) {
  
  msg  <- function(...) if (!silent) message(...)
  warn <- function(...) if (!silent) warning(..., call. = FALSE)
  
  if (length(assign_to) > 1) assign_to <- match.arg(assign_to)
  
  # auto explode if an explode order is passed in
  explode       <- isTRUE(explode) || !is.null(explode_order)
  explode_order <- rlang::arg_match0(explode_order %||% "nat", c("nat", "code", "label"))
  
  # Pick cache env used for both read and write
  cache_env <- switch(
    assign_to,
    caller  = parent.frame(),
    global  = .GlobalEnv,
    pkg_env = {
      env <- get0(".pkg_env", envir = .GlobalEnv, inherits = FALSE, ifnotfound = NULL)
      if (!is.environment(env)) {
        warn(".pkg_env not found; falling back to caller env for cache")
        parent.frame()
      } else {
        env
      }
    },
    env = {
      if (!is.environment(assign_env)) {
        warn("assign_env invalid; using caller env for cache")
        parent.frame()
      } else {
        assign_env
      }
    },
    none = NULL  # no caching when assign_to = "none"
  )
  
  # 1. Try cache when allowed
  if (!reload && !is.null(cache_env) && is.character(ddname)) {
    existing <- get0(ddname, envir = cache_env, inherits = FALSE, ifnotfound = NULL)
    if (is.data.frame(existing)) {
      msg("Keeping cached ", ddname)
      return(invisible(existing))
    } else {
      msg("No cached ", ddname, " found; creating new dictionary")
    }
  }
  
  # 2. Require params
  if (is.null(params_or_section)) {
    warn("Pass either a parameter list or the name of a configuration section.")
    return(NULL)
  }
  
  # 3. Resolve config if a section name was passed
  if (is.character(params_or_section)) {
    params_or_section <- get_configset(tolower(params_or_section), silent = silent)
  }
  
  # 4. Validate config structure
  needed <- c("section", "encrypted_token", "url")
  if (!all(needed %in% names(params_or_section))) {
    warn(
      "Missing required REDCap configuration fields: ",
      paste(setdiff(needed, names(params_or_section)), collapse = ", ")
    )
    return(NULL)
  }
  
  section   <- params_or_section$section
  api_token <- params_or_section$encrypted_token
  api_url   <- params_or_section$url
  
  # 5. Fetch metadata with guard
  dd <- tryCatch(
    {
      rs <- REDCapR::redcap_metadata_read(
        redcap_uri     = api_url,
        token          = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
        verbose        = FALSE,
        config_options = NULL
      )
      rs$data
    },
    error = function(e) {
      warn("REDCapR::redcap_metadata_read() failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(dd)) {
    warn(sprintf("Failed to load dictionary '%s' from %s", section, api_url))
    return(NULL)
  }
  
  # 6. Basic sanity
  if (!nrow(dd)) {
    warn("Dictionary is empty.")
    return(NULL)
  }
  if (anyNA(dd$field_name) || any(!nzchar(dd$field_name))) {
    warn("Dictionary missing some field_names.")
  }
  if (any(duplicated(dd$field_name))) {
    warn("Duplicate field_name in dictionary.")
  }
  if (isTRUE(explode) && !exists(".explode_rc_dictionary", mode = "function")) {
    warn("Function .explode_rc_dictionary() missing.")
    return(NULL)
  }
  
  # 7. Add rowid, field_stem, and clean labels
  dd <- clean_html_df(dd, cols = tidyselect::starts_with("field_label")) |>
    dplyr::mutate(
      rowid      = dplyr::row_number(),
      field_stem = field_name
    ) |>
    dplyr::relocate(rowid)
  
  # 8. Optional explode
  if (isTRUE(explode)) {
    dd <- tryCatch(
      .explode_rc_dictionary(dd, ord = explode_order %||% "nat", keep_orders = keep_orders),
      error = function(e) {
        warn(".explode_rc_dictionary() failed: ", conditionMessage(e))
        dd  # fall back to un-exploded
      }
    )
  }
    
    # If explode failed or returned NULL, keep un-exploded dd
    # if (is.null(chk_exp)) {
    #   msg("No checkbox fields to explode; returning un-exploded dictionary.")
    # } else {
    #   # Expect standard columns from explode
    #   if (!all(c("field_stem", "field_name") %in% names(chk_exp))) {
    #     warn("Exploded dict missing expected columns; returning un-exploded dictionary.")
    #   } else {
    #     # non-checkbox rows plus exploded rows
    #     non_chk <- dd |>
    #       dplyr::filter(!stringr::str_detect(field_type, "check|radio|drop")) |>
    #       dplyr::mutate(sort_key = as.numeric(rowid))
    #     
    #     dd <- dplyr::bind_rows(non_chk, chk_exp) |>
    #       dplyr::arrange(sort_key) |>
    #       dplyr::select(-sort_key)
    #   }
    # }
  
  
  
  # 9. Drop rowid for final result
  dd <- dplyr::select(dd, -rowid)
  
  # 10. Assign cache (if requested)
  if (!is.null(cache_env) && assign_to != "none" && is.character(ddname)) {
    assign(ddname, dd, envir = cache_env)
    msg("Dictionary ", ddname, " assigned to cache env (", assign_to, ")")
  } else if (assign_to == "none") {
    msg(sprintf("Dictionary %s not assigned to an environment", ddname))
  }
  
  invisible(dd)
}

# get_rc_dictionary_outdated <- function(params_or_section = NULL,
#                               ddname        = "rcdd",
#                               reload        = FALSE,
#                               explode       = FALSE,
#                               explode_order = NULL, # c("nat","code","label"),
#                               keep_orders   = FALSE,
#                               assign_to     = c("caller","none","global","pkg_env","env"),
#                               assign_env    = NULL,
#                               silent        = TRUE) {
#   
#   msg <- function(...) if (!silent) message(...)
#   warn <- function(...) if (!silent) warning(..., call. = FALSE)
# 
#   if (length(assign_to) > 1) assign_to <- match.arg(assign_to)
#   
#   # auto explode if an explode order is passed in
#   explode <- isTRUE(explode) || !is.null(explode_order)
#   explode_order <- match.arg(explode_order %||% "nat", c("nat", "code", "label"))
# 
#   # choose env to read cached copy from (only if not "none")
#   .read_env <- switch(assign_to,
#     caller  = parent.frame(),
#     global  = .GlobalEnv,
#     pkg_env = {
#       env <- get0(".pkg_env", envir = .GlobalEnv, inherits = FALSE, ifnotfound = NULL)
#       if (is.environment(env)) env else {
#         warn(".pkg_env not found; falling back to caller env for read cache")
#         parent.frame()
#       }
#     },
#     env     = { if (is.environment(assign_env)) assign_env else { warn("assign_env invalid; using caller env"); parent.frame() } },
#     none    = NULL
#   )
# 
#   # return cached copy if present
#   if (!reload && is.character(ddname) && !is.null(.read_env)) {
#     existing <- get_rc_dictcache(ddname, .read_env)
#     if (is.data.frame(existing)) {
#       msg("Keeping cached ", ddname)
#       return(invisible(existing))
#     } else {
#       msg("Creating dictionary ", ddname)
#     }
#   }
# 
#   # require params
#   if (is.null(params_or_section)) {
#     warn("Pass either a parameter list or the name of a configuration section.")
#     return(NULL)
#   }
# 
#   # resolve config if section name
#   if (is.character(params_or_section)) {
#     # grab configurations from ini
#     params_or_section <- get_configset(tolower(params_or_section), silent = silent)
#   }
#   
#   # validate config structure using list presence
#   needed <- c("section","encrypted_token","url")
#   if (!all(needed %in% names(params_or_section))) {
#     warn("Missing required REDCap configuration fields: ", paste(setdiff(needed, names(params_or_section)), collapse = ", "))
#     return(NULL)
#   }  
#   
#   # fetch metadata with guard
#   section   <- params_or_section$section
#   api_token <- params_or_section$encrypted_token
#   api_url   <- params_or_section$url  
#   dd <- tryCatch({
#     rs <- REDCapR::redcap_metadata_read(
#       redcap_uri     = api_url,
#       token          = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
#       verbose        = FALSE,
#       config_options = NULL
#     )
#     rs$data
#   }, error = function(e) {
#     warn("REDCapR::redcap_metadata_read() failed: ", conditionMessage(e))
#     return(NULL)
#   })  
#   if (is.null(dd)) {
#     warn(sprintf("Failed to load dictionary '%s' from %s", section, api_url))
#     return(NULL)
#   }
# 
#   # basic sanity
#   if (!nrow(dd))
#     {warn("Dictionary is empty."); return(NULL)}
#   if (anyNA(dd$field_name) || any(!nzchar(dd$field_name)))
#     {warn("Dictionary missing some field_names."); return(NULL)}
#   if (any(duplicated(dd$field_name)))
#     {warn("Duplicate field_name in dictionary.")}
#   if (isTRUE(explode) && !exists(".explode_rc_dictionary", mode = "function"))
#     {warn("Function .explode_rc_dictionary() missing."); return(NULL)}
#   
#   # add row identification, field stem and clean field label stripping out REDCap wrappers.
#   # clean html removes some html that is a remnant of the REDCap interface
#   dd <- clean_html_df(dd, cols = tidyselect::starts_with("field_label")) |>
#     dplyr::mutate(rowid = dplyr::row_number(), field_stem = field_name) |>
#     dplyr::relocate(rowid)
# 
#   # explode checkbox choices (optional)
#   if (isTRUE(explode)) {
#     chk_exp <- tryCatch({
#       .explode_rc_dictionary(dd, ord = explode_order %||% "nat", keep_orders = keep_orders)
#     }, error = function(e) {
#       warn(".explode_rc_dictionary() failed: ", conditionMessage(e))
#       return(NULL)
#     })
#     
#     # No checkboxes were exploded, should we return the dd?  Maybe just exit this if statement?
#     if (is.null(chk_exp)) msg("No checkbox fields to explode; returning un-exploded dictionary.")
#     
#     # expect standard columns from explode field_stem, field_name
#     if (!all(c("field_stem","field_name") %in% names(chk_exp))) {warn("Exploded dict missing columns."); return(NULL)}
#     
#     non_chk <- dd |>
#       dplyr::filter(!str_detect(field_type, "check|radio|drop")) |>
#       dplyr::mutate(sort_key = as.numeric(rowid))
# 
#     
#     dd <- dplyr::bind_rows(non_chk, chk_exp) |>
#       dplyr::arrange(sort_key) |>
#       dplyr::select(-sort_key)
#   }
# 
#   # drop rowid
#   dd <- dplyr::select(dd, -rowid)
# 
#   # assign where requested (single block)
#   if (assign_to == "none") {
#       msg(sprintf("Dictionary %s not assigned to an environment", ddname))
#       return(invisible(dd))
#     } else {
#       target_env <- switch(assign_to,
#         caller  = parent.frame(),
#         global  = .GlobalEnv,
#         pkg_env = {
#           env <- get0(".pkg_env", envir = .GlobalEnv, inherits = FALSE, ifnotfound = NULL)
#           if (!is.environment(env))
#             stop(".pkg_env does not exist; create it with `.pkg_env <- new.env()`")
#           env
#         },
#         env     = {
#           if (!is.environment(assign_env))
#             stop("assign_env must be an environment when assign_to = 'env'")
#           assign_env
#         }
#       )
#       assign(ddname, dd, envir = target_env)
#   }
# 
#   return(invisible(dd))
# }

if (FALSE) {
  rm(rcdd)
  zz <- get_rc_dictionary('mpal', reload = TRUE, explode = FALSE, assign_to='pkg_env') # no explode
  zz <- get_rc_dictionary('mpal', reload = TRUE, explode = TRUE) # default order
  get_rc_dictionary('mpal', explode_order = 'nat', reload = TRUE) # nat order
  get_rc_dictionary('mpal', explode_order = 'code', reload = TRUE) # code order
  get_rc_dictionary('mpal', explode_order = 'label', reload = TRUE) # label order
}



get_rc_log <- function(token=NULL
                       , url        = "https://redcap.iths.org/api/"
                       , logtype    = 'update'
                       , starttime  = NULL
                       , endtime    = Sys.time()
                       , section    = 'esteylist'
                       , excl_regex = '(?i)port\\b|api\\b|calculation\\b|quality\\b'
                       , weeks      = 1
                       , silent     = TRUE) {
  
  if (FALSE) {
      token=NULL
      url        = "https://redcap.iths.org/api/"
      logtype    = 'update'
      starttime  = chunk_start_date
      endtime    = chunk_end_date
      section    = 'esteylist'
      excl_regex = '(?i)port\\b|api\\b|calculation\\b|quality\\b'
      weeks      = 1
      silent     = FALSE    
  }
  
  if(!silent) print('get_rc_log()')

  # if (section!='esteylist') {
  #   params <- get_configset(tolower(section))
  # } else {
  #   params <- get_configset('esteylist')
  # }
  params    <- get_configset(tolower(section))
  section   <- params$section
  api_token <- params$encrypted_token
  api_url   <- params$url

  if (is.null(logtype)) logtype   = 'update'
  if (is.null(endtime)) endtime    = Sys.time()
  if (!is.null(weeks) & is.null(starttime) ) starttime = format(as.Date(endtime) - (weeks * 7), "%Y-%m-%d %H:%M")

  # format start/end as time
  starttime <- format(as.Date(starttime), "%Y-%m-%d %H:%M")
  endtime   <- format(as.Date(endtime),   "%Y-%m-%d %H:%M")
  
  incl_regex = paste0('(?i)',logtype)

  formData  <- list(
                  token        = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
                  content      = 'log',
                  logtype      = 'record_edit',
                  beginTime    = starttime,
                  endTime      = endtime,
                  format       = 'json',
                  returnFormat = 'json')

  # response <- httr::POST(url, body = formData, encode = "form")
  
  response <- httr::POST(
    api_url,
    body = formData,
    encode = "form",
    httr::add_headers("Accept" = "application/json")
  )

  # result   <- httr::content(response)
  result <- httr::content(response, as = "parsed", type = "application/json")

  
  df <- if (length(result) > 0) {
    bind_rows(result)
  } else {
    tibble()  # return empty tibble if nothing found
  }
  
  if(nrow(df)!=0) {
    df <- df |>
      filter(!grepl(excl_regex, action) & grepl(incl_regex, action)) |>
      mutate(firstword = word(action, 1), lastupdate = as.Date(timestamp))
  }

  if (!silent) {
    cat(nrow(df), 'rows'
        , 'from', format(as.Date(starttime), '%m/%d/%Y')
        , 'to', format(endtime, '%m/%d/%Y')
        , '\nmeeting this exclusion criteria:\t', paste0('"', excl_regex, '"')
        , '\nand this inclusion criteria:\t\t', paste0('"', incl_regex, '"'))
  }

  return(df)

}

#' Get REDCap Project
#'
#' Calls the REDCap API with content='project' and Returns a df of the project details for
#' the given section. The value can also be stored in the config section as
#' \code{pid} to avoid an extra API call.
#'
#' @param paramsect Character name of a configset or a params list with elements
#'   encryptedtoken, url, and optionally pid.
#' @param silent Logical; suppress messages.
#' @return Integer project ID, or NULL on failure.
get_rc_proj <- function(paramsect = NULL, silent = TRUE) {
  
  # if FALSE
  # paramsect <- "esteylist"
  # silent <- FALSE
  
  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect
  if (is.null(params)) {
    warning("Pass either a parameter list or the name of a configuration section.")
    return(NULL)
  }
  
  # Return cached pid from config if available
  if (!is.null(params$pid) && nzchar(as.character(params$pid))) {
    if (!silent) message("Using pid from config: ", params$pid)
    return(as.integer(params$pid))
  }
  
  api_token <- params$encrypted_token
  api_url   <- params$url
  
  formData <- list(
    token        = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    content      = "project",
    format       = "json",
    returnFormat = "json"
  )

  response <- httr::POST(api_url, body = formData, encode = "form")
  
  if (httr::status_code(response) != 200) {
    warning("Failed to fetch project info: ", httr::content(response, "text", encoding = "UTF-8"))
    return(NULL)
  }
  
  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  return(result)
}

#' Get REDCap Project ID
#'
#' Calls the REDCap API with content='project' and returns the project_id for
#' the given section. The value can also be stored in the config section as
#' \code{pid} to avoid an extra API call.
#'
#' @param paramsect Character name of a configset or a params list with elements
#'   encryptedtoken, url, and optionally pid.
#' @param silent Logical; suppress messages.
#' @return Integer project ID, or NULL on failure.
get_rc_pid <- function(paramsect = NULL, silent = TRUE) {
  
  result <- get_rc_proj("esteylist")
  return(result$project_id)
}

#' Get REDCap Version
#'
#' Calls the REDCap API with content='version' and returns the version string
#' (e.g., "17.1.1") for the instance hosting the given section.
#'
#' @param paramsect Character name of a configset or a params list with elements
#'   encryptedtoken and url.
#' @param silent Logical; suppress messages.
#' @return Character version string, or NULL on failure.
#' @export
get_rc_version <- function(paramsect = NULL, silent = TRUE) {
  
  # if FALSE
  # paramsect <- "esteylist"
  # silent <- FALSE
  
  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect
  if (is.null(params)) {
    warning("Pass either a parameter list or the name of a configuration section.")
    return(NULL)
  }
  
  api_token <- params$encrypted_token
  api_url   <- params$url
  
  formData <- list(
    token   = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    content = "version"
  )
  
  response <- httr::POST(api_url, body = formData, encode = "form")
  
  if (httr::status_code(response) != 200) {
    warning("Failed to fetch REDCap version: ", httr::content(response, "text", encoding = "UTF-8"))
    return(NULL)
  }
  
  rc_version <- trimws(httr::content(response, "text", encoding = "UTF-8"))
  if (!silent) message("REDCap version: ", rc_version)
  return(rc_version)
}

#' Get REDCap Base URL
#'
#' Derives the base URL (scheme + hostname only) from the API URL stored in
#' the config section. No API call is made.
#'
#' @param paramsect Character name of a configset or a params list with a url element.
#' @param silent Logical; suppress messages.
#' @return Character base URL (e.g., "https://redcap.iths.org"), or NULL on failure.
#' @export
get_rc_base_url <- function(paramsect = NULL, silent = TRUE) {
  
  # if FALSE
  # paramsect <- "esteylist"
  # silent <- FALSE
  
  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect
  if (is.null(params)) {
    warning("Pass either a parameter list or the name of a configuration section.")
    return(NULL)
  }
  
  parsed   <- httr::parse_url(params$url)
  base_url <- paste0(parsed$scheme, "://", parsed$hostname)
  
  if (!silent) message("REDCap base URL: ", base_url)
  return(base_url)
}


#' Get REDCap Data Export Report URL
#'
#' Builds the full URL to the REDCap Data Exports / Reports page for a project,
#' using \code{get_rc_base_url}, \code{get_rc_version}, and \code{get_rc_pid}.
#' Useful for browser sessions and link generation.
#'
#' @param paramsect Character name of a configset or a params list.
#' @param silent Logical; suppress messages.
#' @return Character URL string, or NULL if any component cannot be resolved.
#' @export
get_rc_allreport_url <- function(paramsect = NULL, silent = TRUE) {
  
  # if FALSE
  # paramsect <- "esteylist"
  # silent <- FALSE
  
  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect
  if (is.null(params)) {
    warning("Pass either a parameter list or the name of a configuration section.")
    return(NULL)
  }
  
  base_url   <- get_rc_base_url(params$section, silent = silent)
  rc_version <- get_rc_version(params$section,  silent = silent)
  pid        <- get_rc_pid(params$section,      silent = silent)
  
  if (is.null(base_url) || is.null(rc_version) || is.null(pid)) {
    warning("Could not resolve one or more components (base_url, version, pid).")
    return(NULL)
  }
  
  report_url <- paste0(base_url, "/redcap_v", rc_version,
                       "/DataExport/index.php?pid=", pid)
  
  if (!silent) message("REDCap report URL: ", report_url)
  return(report_url)
}

get_rc_report <- function(paramsect=NULL
                       , reportid=NULL
                       , label=TRUE
                       , silent = TRUE
                       , keep_newlines = TRUE
                       , newline_patterns = NULL
                       , rcdd=NULL
                       , allblank=FALSE) {             

  if (FALSE) {
    paramsect <- 'esteylist'
    reportid  <- 178695
    label     <- TRUE
    silent    <- TRUE
    keep_newlines = TRUE
    newline_patterns = NULL
    rcdd = NULL
    allblank = TRUE
    
  }

  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect

  # default return value
  df=data.frame()
  if (!silent) message("Loading df with ", reportid, " data")

  # require table name
  if (is.null(reportid)) {return(df)}
  # require parameter object
  if (is.null(params)) {return(df)}

  # set defaults
  api_token <- params$encrypted_token
  api_url   <- params$url
  section   <- if (is.list(params)) params[["section"]] else params

  # set the form data request parameters
  formData <- list(
    token                  = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    content                = 'report',
    format                 = 'json',
    report_id              = reportid,
    csvDelimiter           = '',
    rawOrLabel             = 'raw',
    rawOrLabelHeaders      = 'raw',
    exportCheckboxLabel    = 'false',
    returnFormat           = 'json'
  )

  # Set 'label' for labeled export
  if (label) {formData$rawOrLabel <- 'label'}
  
  # request data from REDCap api url
  response <- httr::POST(api_url, body = formData, encode = "form")

  # Check for successful response
  if (httr::status_code(response) != 200) {
    # Failed return early
    cat('\nError: Failed to fetch',tbl,'data from REDCap\n')
    return (df)
  }

  result <- httr::content(response, as = "text", encoding = "UTF-8")
  if (allblank) {
    df <- jsonlite::fromJSON(result, simplifyDataFrame = TRUE)[1,, drop=FALSE]
  } else {
    df <- jsonlite::fromJSON(result, simplifyDataFrame = TRUE)
  }  

  

  # --- Manage line breaks ----------------------------------------------------
  if (keep_newlines) {
  
    if (is.null(rcdd)) {
      get_rc_dictionary(params)
    }
    notes <- (rcdd |> filter(field_type=='notes'))$field_name
    notes <- intersect(notes, names(df))
    
    default_patterns <- c(
      # "\\s{3,}(?=[\\[(]?\\d{2}[/-]\\d{2}[/-]\\d{4}[\\])]?)",  # mm/dd/yyyy
      # "\\s{3,}(?=[\\[(]?\\d{4}[/-]\\d{2}[/-]\\d{2}[\\])]?)",  # yyyy/mm/dd
      "\\s{3,}(?=NOTE\\b)",         # before NOTE
      # "\\s{3,}(?=\\[NOTE\\])",      # before [NOTE]
      "\\s{3,}(?=[\\[(])"           # before any ( or [
    )

    patterns <- if (!is.null(newline_patterns)) newline_patterns else default_patterns
  
    df <- df |>
      mutate(across(all_of(notes), ~ {
        x <- .x
        x <- stringr::str_replace_all(x, "\\\\r?\\\\n", "\n")
        for (pat in patterns) {
          x <- stringr::str_replace_all(x, regex(pat), "\n")
        }
        # Replace 3+ non-alphanumeric symbol runs with newline
        x <- stringr::str_replace_all(x, "[^A-Za-z0-9\\s]{3,}", "\n\n")
        # cap at 2 newlines
        x <- stringr::str_replace_all(x, "\n{3,}", "\n\n")
        # convert triple spaces to tabs
        x <- stringr::str_replace_all(x, "[ ]{3,}", "\t")
        # Remove trailing/leading spaces around newlines
        x <- stringr::str_replace_all(x, "[ \t]*\n[ \t]*", "\n")
        x
      }))

  }

  return(df)
}


#' Update REDCap report metadata and cumulative field lists
#'
#' This function selects a set of REDCap reports that need metadata updates,
#' extracts their field lists via \code{get_rc_report_fields()}, and updates
#' the cumulative \code{Reports} and \code{Report_Fields} Meta files on disk.
#' Reports can be selected either by status (e.g., "new" or "old") with a
#' row limit, or by an explicit vector of report IDs.
#'
#' @param section Character scalar naming the REDCap project/section to use
#'   when locating Meta files and calling \code{get_rc_report_metadata()}.
#' @param max_reports Integer. Maximum number of reports to detail in this
#'   run when selecting by status. Ignored if \code{report_ids} is supplied.
#' @param max_age_days Integer. Reports older than this (based on their
#'   \code{updated} date) are considered "old" and eligible for update.
#' @param include_types Character vector of report types to include when
#'   selecting by status, typically some combination of \code{"new"} and
#'   \code{"old"}.
#' @param report_ids Optional integer vector of specific report IDs to update.
#'   When supplied, these reports are used instead of status-based selection.
#' @param meta_path Character scalar giving the directory where cumulative
#'   \code{Reports} and \code{Report_Fields} CSV files are written.
#' @param verbose Logical. If \code{TRUE}, prints progress messages while
#'   fetching report fields and writing output files.
#'
#' @return
#' Invisibly returns a list with components \code{Reports}, \code{Report_Fields},
#' \code{cum_reports}, and \code{cum_report_fields} for the current run. The
#' primary side effect is writing updated Meta CSV files to \code{meta_path}.
#'
#' @seealso
#' \code{\link{get_rc_report_metadata}}, \code{\link{get_rc_report_fields}},
#' and any downstream code that consumes the \code{Reports} or
#' \code{Report_Fields} files.
#'
#' @examples
#' \dontrun{
#' # Update up to 10 new/old reports for esteylist
#' update_rc_report_metadata(
#'   section       = "esteylist",
#'   max_reports   = 10,
#'   max_age_days  = 90,
#'   include_types = c("new", "old")
#' )
#'
#' # Force an update for a specific set of reports
#' update_rc_report_metadata(
#'   section    = "esteylist",
#'   report_ids = c(381026, 381027)
#' )
#' }
update_rc_report_metadata <- function(
  section         = "esteylist",
  max_reports     = 10,
  max_age_days    = 90,
  include_types   = c("new", "old"),  # or "new" only
  report_ids      = NULL,
  meta_dir       = .pkg_env$libdir,
  meta_folder    = "Meta",
  min_fields      = 1,
  silent          = TRUE,
  skip_report_pattern = "^\\s*[#+=*]") {

  if (FALSE) {
    section         = "esteylist"
    max_reports     = 10
    max_age_days    = 90
    include_types   = c("new", "old")
    # report_ids      = c(342788,373688)
    report_ids      = NULL
    meta_dir        = .pkg_env$libdir
    meta_folder     = "Meta"
    silent          = TRUE
    min_fields      = 1
    skip_report_pattern = "^\\s*[#+=*]"
    
    meta_dir = .pkg_env$libdir
    report_ids = report_ids
    max_reports = 100
    max_age_days = -1
  }

 meta_path = file.path(meta_dir, meta_folder)  
  
  # Accumulator for all (reportid, field_name) pairs discovered this run
  allflds <- data.frame(
    reportid   = integer(),
    field_name = character(),
    stringsAsFactors = FALSE)
  
  # guard for non-existent report_fields.csv
  if (!file.exists(file.path(meta_path, 'Report_Fields.csv'))) {
    cat("Making report fields")
    make_sheet( allflds, meta_path,
                filename = "Report_Fields",
                date     = FALSE)    
  }
  
  # Load prior cumulative Report_Fields
  prior_report_fields <- tryCatch({
    get_last_file(path     = meta_path,
                  ext      = "csv",
                  pattern  = "(?i)Report_Fields\\.csv",
                  startRow = 0L,
                  silent   = TRUE) |>
      mutate(updated = as.Date(updated)) |>
      filter(field_count > min_fields)
  }, error = function(e) {
    reports[0, ]   # empty structure with same columns
  })
  
    
  # guard for max_reports is NULL
  if (is.null(max_reports) || length(max_reports) == 0L || max_reports == 0L) {
    cat("'max_reports' did not contain a non-zero integer\n\n")
    return(allflds[0,])
    
  }

  # Select up to max_report_cnt reports that need metadata updates
  max_report = 100000
  reports <- get_rc_report_metadata(
    section    = section,
    report_ids = report_ids,
    limit      = max_reports,
    max_age_days = -1,
    skip_report_pattern = skip_report_pattern) |>
    filter(field_count > min_fields | is.na(field_count)) |>
    convert_date_cols()  

  # guard for empty reports df
  if (nrow(reports) == 0L) {
    cat("No report metadata found\n\n")
    return(reports[0,])
  }
    
  # Initial status message
  if (!silent) {
    cat(glue("Starting with 0 report fields, cumulative total 0, ",
             "updating or adding {nrow(reports)} reports\n\n"))
  }
  
  i = 0
  for (i in seq_len(nrow(reports))) {
    # i = i + 1
    reportid    <- reports$reportid[i]
    report_name <- reports$report_name[i]
    cat(report_name, '\n\n')
    
    # Skip reports whose names begin with '=' or '*', etc.
    if (isTRUE(!is.na(report_name) && str_detect(report_name, skip_report_pattern))) {
      next
    }
    
    # fields on report
    flds <- get_rc_report_fields(reportid, report_name, silent=TRUE) |>
      select(reportid, field_name, report_field_order)
    
    # if only 1 field skip the report, it is probably just a report utilizing the report header to link to other reports
    if (nrow(flds) <= 1) next
    
    # all fields on current reports
    allflds <- dplyr::bind_rows(allflds, flds)
    
    # Progress message for each report
    if (!silent | TRUE) {cat("\nReport: ", report_name, paste0("[reportid=", reportid, "]"),
                      "\nAdded", nrow(flds), "report fields, cumulative total",
                      nrow(allflds), "fields over", i, "reports\n")}
  }
  
  # Add per-report field_count, keep one row per report for newly updated reports
  report_fields <- left_join(reports, allflds, by = "reportid") |>
    group_by(reportid) |>
    mutate(field_count = n()) |>
    ungroup()
  
  reports <- report_fields |>
    select(-field_name) |>
    distinct() |>
    mutate( sequence = dplyr::row_number(),
            type     = "current",
            updated  = Sys.Date(),
            report_folderid = NA_integer_,
            dict_updated = NA_Date_ ) |>
    filter(field_count > min_fields)

  
  # Load prior cumulative reports (if present)
  # prior_reports <- tryCatch({
  #   get_last_file(path     = meta_path,
  #                 ext      = "csv",
  #                 pattern  = "Reports\\.csv",
  #                 startRow = 0L) |>
  #     convert_date_cols()
  # }, error = function(e) {
  #   reports[0, ]
  # })
  
  path_to_reports_csv <- file.path(meta_path, 'Reports.csv')
  
  prior_reports <- if (!file.exists(path_to_reports_csv)) {
    cat("No previous file Reports.csv\n")
    reports[0, ]
  } else tryCatch({
    get_last_file(
      path     = meta_path,
      ext      = 'csv',
      pattern  = 'Reports\\.csv',
      startRow = 0) |>
      convert_date_cols() |>
      filter(field_count > min_fields) 
  }, error = function(e) reports[0, ]) # empty structure if does not exist
  
  # if missing a column add it here
  if (!"dict_updated" %in% names(prior_reports)) {
    prior_reports$dict_updated <- NA_Date_
  }
  if (!'link_html' %in% names(prior_reports)) {
    prior_reports$link_html = NA_character_
  }
  if (!'report_field_order' %in% names(prior_reports)) {
    prior_reports$report_field_order = NA_integer_
  }
  
  

  # All reports that are current as of this run, plus prior runs
  # I want to prioritize old report_folderid's during this join that is 
  # why I did NOT use bind_rows on the old + new below
  old_reports <- anti_join(prior_reports, reports, by='reportid')
  
  cum_reports <- left_join(reports, 
                           prior_reports |> select(-keep), 
                           by=join_by(reportid, 
                                      reportid_auto)) |>
    mutate(sequence = dplyr::row_number(),
           
           # prefer prior values that are maintained outside this run
           type  = coalesce(type.x, type.y),
           dict_updated       = coalesce(dict_updated.y, dict_updated.x),
           link_html          = coalesce(link_html.y, link_html.x),
           
           # prefer current run values, fall back to prior
           report_name        = coalesce(report_name.x, report_name.y),
           section            = coalesce(section.x, section.y),
           report_folderid    = as.integer(coalesce(report_folderid.x, report_folderid.y)),
           field_count        = coalesce(field_count.x, field_count.y),
           updated            = coalesce(updated.x, updated.y),
           report_field_order = coalesce(report_field_order.x, report_field_order.y),
           
           ) |>
    arrange(reportid, desc(updated), desc(report_folderid)) |>
    group_by(reportid) |>
      slice_head(n = 1) |>
    ungroup() |>
    select(-ends_with(".x"), -ends_with(".y")) |>
    filter(field_count > min_fields)
  
    
  .pkg_env$cum_reports <- cum_reports
  .pkg_env$old_reports <- old_reports
  
  # cum_reports <- bind_rows(cum_reports, old_reports)  |>
  #   mutate(sequence = dplyr::row_number())
  if (nrow(old_reports) > 0L & nrow(cum_reports) > 0L){
    cat("\nCombining old and new\n\n")
    cum_reports <- bind_rows(cum_reports, old_reports) |>
          mutate(sequence = dplyr::row_number())  
  }
  
  # Check for dups
  dup_reports <- cum_reports |>
    count(reportid, name = "n") |>
    filter(n > 1)
    
  # these were not just updated, keep older version
  Old_report_fields <- anti_join(prior_report_fields, report_fields)
  
  # put the old and current together
  zz <- bind_rows(report_fields, Old_report_fields) |>
    arrange(reportid, report_field_order, desc(updated))  
  cum_report_fields <- bind_rows(report_fields, Old_report_fields) |>
    arrange(reportid, report_field_order, desc(updated)) |>
    group_by(reportid, field_name) |>
      slice_head(n = 1) |>
    ungroup() |>
    arrange(reportid, report_field_order) |>
    mutate(sequence = dplyr::row_number())  

  # Check for dups
  dup_report_fields <- cum_report_fields |>
    count(reportid, field_name, name = "n") |>
    filter(n > 1)
  
  # Write cumulative reports with timestamped filenames
  make_sheet( cum_reports, meta_path,
              filename = "Reports",
              time     = TRUE)
  make_sheet( cum_report_fields, meta_path,
              filename = "Report_Fields",
              time     = TRUE)
  
  # And also without dates (stable filenames used by downstream code)
  make_sheet( cum_reports, meta_path,
              filename = "Reports",
              date     = FALSE)
  make_sheet( cum_report_fields, meta_path,
              filename = "Report_Fields",
              date     = FALSE)

  .pkg_env$reports <- cum_reports
  .pkg_env$report_fields <- cum_report_fields
  
}

#' Get prioritized REDCap reports for metadata detailing
#'
#' This function identifies REDCap reports that should be (re)processed
#' for metadata detailing, based on the latest REDCap report screen grab
#' and the most recent prior metadata run stored in the Meta directory.
#' It flags reports that are new (no prior metadata) or old (last updated
#' more than `max_age_days` days ago), and optionally limits the number of
#' reports selected in a single run.
#'
#' The function assumes that:
#' \itemize{
#'   \item The Meta directory contains a CSV exported from the REDCap
#'         report creation screen (e.g., \code{Report_Screen_Grab_*.csv}).
#'   \item A separate process produces a \code{Reports.csv} file
#'         with detailed report metadata (including \code{updated},
#'         \code{keep}, \code{field_count}, and \code{type}).
#' }
#' The newest screen grab is joined to the latest metadata file to determine
#' which reports exist, which have been detailed, and when they were last
#' updated. Reports are then classified as \code{"new"}, \code{"old"}, or
#' \code{"current"} based on their \code{updated} date, and only
#' \code{"new"} and \code{"old"} reports are returned. When \code{limit}
#' is not \code{NULL}, at most \code{limit} reports are marked to keep.
#'
#' @param section Character scalar indicating the REDCap project or logical
#'   section name used to locate and annotate metadata files. Defaults to
#'   \code{"esteylist"}.
#' @param meta_path Character scalar giving the directory where Meta CSV files
#'   (screen grabs and \code{Reports.csv}) are stored. Defaults to
#'   \code{"Meta"}.
#' @param ext Character scalar specifying the file extension to search for,
#'   typically \code{"csv"}. Defaults to \code{"csv"}.
#' @param limit Optional integer. If \code{NULL} (default), all eligible
#'   \code{"new"} and \code{"old"} reports are marked for processing.
#'   If a positive integer, only the first \code{limit} reports with
#'   \code{keep == FALSE} are promoted to \code{keep == TRUE} and returned.
#' @param max_age_days Integer. Reports with updated dates older than this
#'   (in days) are considered "old" and eligible for update. Defaults to 90.   
#'
#' @details
#' ReportsPotential_Reports_to_Update are prioritized as follows:
#' \enumerate{
#'   \item Reports with no prior metadata (\code{updated} is \code{NA})
#'         are classified as \code{"new"}.
#'   \item Reports with \code{updated} more than \code{max_age_days} days in
#'         the past are classified as \code{"old"}.
#'   \item All remaining reports are classified as \code{"current"} and
#'         are not returned.
#' }
#' The function returns only rows where \code{keep} is \code{TRUE} and
#' \code{type} is either \code{"new"} or \code{"old"}. If no reports meet
#' these criteria, an empty tibble with the appropriate columns is returned.
#'
#' @return
#' A tibble with one row per selected report and columns including (at
#' least) \code{sequence}, \code{section}, \code{report_name}, \code{keep},
#' \code{updated}, \code{reportid}, \code{reportid_auto}, \code{field_count},
#' and \code{type}. The tibble may have zero rows if there are no reports
#' needing metadata updates.
#'
#' @seealso
#' Functions such as \code{\link{get_last_file}} for locating the most recent
#' Meta files, and downstream utilities that consume the returned tibble to
#' build or update report-specific data dictionaries.
#'
#' @examples
#' \dontrun{
#' # Get all reports needing metadata updates for the esteylist section
#' reports_all <- get_rc_report_metadata(section = "esteylist")
#'
#' # Get at most 10 reports to detail in this run
#' reports_batch <- get_rc_report_metadata(
#'   section = "esteylist",
#'   path    = "Meta",
#'   limit   = 10
#' )
#' }
#' 
if (FALSE){
    section   = section
    meta_path = meta_path
    limit     = max_reports
    max_age_days = max_age_days
}

get_rc_report_metadata <- function(section = 'esteylist', 
                                   meta_folder = 'Meta',
                                   meta_dir = .pkg_env$libdir,
                                   ext = 'csv', 
                                   limit = NULL, 
                                   max_age_days = 90,
                                   min_fields = 1,
                                   report_ids = NULL,                  # optional filter on IDs
                                   include_types = c("new", "old"),    # optional filter on types
                                   skip_report_pattern = "^\\s*[#+=*]") { 
                                   
  if (FALSE) {
    # defaults
    section = 'esteylist' 
    meta_folder = 'Meta'
    meta_dir = .pkg_env$libdir
    ext = 'csv'
    limit = NULL 
    max_age_days = 90
    min_fields = 1
    report_ids = NULL
    include_types = c("new", "old")
    skip_report_pattern = "^\\s*[#+=*]"
    
    # this run
    report_ids = c(385004,385002)
 
  }

  # meta_path <- file.path(projdir, meta_path)
  report_file <- file.path(meta_dir, meta_folder, 'Reports.csv')
  
  .classify_report_type <- function(updated, max_age_days) {
    case_when(
      is.na(updated)                      ~ "new",
      Sys.Date() - updated > max_age_days ~ "old",
      .default                            = "current"
    )
  }

  # target structure
  df_structure <- tibble::tibble(
    sequence        = integer(),
    section         = character(),
    report_name     = character(),
    keep            = logical(),
    updated         = as.Date(character()),
    dict_updated    = as.Date(character()),
    reportid        = integer(),
    reportid_auto   = character(),
    report_folderid = integer(),
    field_count     = integer(),
    type            = character(),
    link_html       = character(),
  )
  
  # section
  # str(section)
  # 
  # zz <- get_last_file(
  #   path     = file.path(meta_dir, meta_folder),
  #   ext      = 'csv',
  #   pattern  = 'Reports\\.csv',
  #   startRow = 0,
  #   silent = FALSE) |>
  #   convert_date_cols() |>
  #   select(-section) |>
  #   mutate(section = section)
  
  
  # this file exists if we have previously created a Report_Fields_yyyymmdd.csv file
  df_last_run <- if (!file.exists(report_file)) {
    df_structure
  } else tryCatch({ get_last_file(path     = file.path(meta_dir, meta_folder),
                                  ext      = 'csv',
                                  pattern  = 'Reports\\.csv',
                                  startRow = 0,
                                  silent = FALSE) |>
                                  convert_date_cols() |>
                                  select(-section) |>
                                  mutate(section = section)
  }, error = function(e) df_structure) # empty structure if does not exist
  
  # import the most recent screen grab in two steps
  df_grab.raw.1 <- get_last_file(
    path     = file.path(meta_dir, meta_folder),
    ext      = 'cSv',
    pattern  = "(?i)Report_Screen_Grab.*\\.csv",
    startRow = 0)
  
  df_grab.raw.2 <- if (ncol(df_grab.raw.1)==6) rename(df_grab.raw.1,
                                                  sequence = 1L, 
                                                  reportid = 5L, 
                                                  reportid_auto = 6L) |>
                                           mutate(section = section) |>
                                           select(sequence, section, report_name, reportid, reportid_auto) else df_grab.raw.1
  
  # assure that it has the correct structure, excluding reports starting with * or = or containing ...
  df_grab.1 <- df_grab.raw.2 |>
    left_join(df_structure, by = c("sequence", "section", "report_name", "reportid", "reportid_auto")) |>
    select(-contains("..."), -keep) |>
    filter(!str_detect(report_name, skip_report_pattern)) |>
    filter(!report_name == toupper(report_name))

  # classify the reports as current, or not
  df_grab.2 <- df_grab.1 |>
    select(-updated, -field_count) |>
    left_join(
      df_last_run |>
        select(section, reportid, report_folderid, keep, updated, 
               # dict_updated, 
               field_count, type),
      by = c("reportid", "section")
    ) |>
    mutate( keep = TRUE,
            type = coalesce(type.y, type.x),
            type = .classify_report_type(updated, max_age_days), # used to filter by type later
            type = factor(type),
            report_folderid = coalesce(report_folderid.y, report_folderid.x),
          ) |>
    select(-ends_with(c(".x", ".y")))

  # Optional reportid constraint if we just want to update certain reports
  if (!is.null(report_ids)) {
    df_grab.2 <- df_grab.2 |> filter(reportid %in% report_ids)
    # if using a recordid contraint, type does not matter
    include_types <- NULL
    # no limit
    limit = NULL
  }
  
  # Optional type constraint (new/old/current/etc.)
  if (!is.null(include_types)) {
    df_grab.2 <- df_grab.2 |> filter(type %in% include_types)
  }
  
  .pkg_env$potential_reports_to_update <- df_grab.2
  
  
  
  # Limit constraint: choose which rows to keep, then mark keep = TRUE
  if (!is.null(limit)) {
    df_grab.2 <- df_grab.2 |> 
      mutate(keep = FALSE)
    idx <- which(!df_grab.2$keep)
    if (length(idx) > 0L) {
      promote <- head(idx, limit)
      df_grab.2$keep[promote] <- TRUE
    }
  }
  
  # Final selection: only "keep" == TRUE, filtered earlier to type and limits.
  df <- df_grab.2 |> filter(keep)
  .pkg_env$reports_to_update <- df

  cat(glue("Selected {nrow(df)} out of {nrow(.pkg_env$potential_reports_to_update)} to update\n\n"))
  
  
  return(df)
  
}

get_rc_report_metadata__ <- function(section='esteylist', 
                                   meta_path='Meta', 
                                   ext='csv', 
                                   limit=NULL, 
                                   max_age_days = 90,
                                   min_fields = 1,
                                   report_ids    = NULL,              # optional filter on IDs
                                   include_types = c("new", "old"),    # optional filter on types
                                   skip_report_pattern = "^\\s*[#+=*]") { 
                                   
  if (FALSE) {
    # defaults
    section='esteylist'
    meta_path=meta_path
    ext='csv'
    limit=NULL
    max_age_days = 90
    report_ids    = NULL
    include_types = c("new", "old")
    
    # this run
    meta_path  = file.path(projdir, 'Meta')
    limit      = 1000
    max_age_days = max_age_days
    skip_report_pattern = skip_report_pattern
  }

  # meta_path <- file.path(projdir, meta_path)
  reports_path <- file.path(meta_path, 'Reports.csv')
  
  .classify_report_type <- function(updated, max_age_days) {
    case_when(
      is.na(updated)                      ~ "new",
      Sys.Date() - updated > max_age_days ~ "old",
      .default                            = "current"
    )
  }

  # target structure
  df_structure <- tibble::tibble(
    sequence        = integer(),
    section         = character(),
    report_name     = character(),
    keep            = logical(),
    updated         = as.Date(character()),
    dict_updated    = as.Date(character()),
    reportid        = integer(),
    reportid_auto   = character(),
    report_folderid = integer(),
    field_count     = integer(),
    type            = character(),
    link_html       = character(),
  )
  
  meta_path 

  # this file exists if we have previously created a Report_Fields_yyyymmdd.csv file
  df_last_run <- if (!file.exists(reports_path)) {
    df_structure
  } else tryCatch({
    get_last_file(
      path     = meta_path,
      ext      = ext,
      pattern  = 'Reports\\.csv',
      startRow = 0) |>
      convert_date_cols()
  }, error = function(e) df_structure) # empty structure if does not exist

  # import the most recent screen grab
  df_grab.raw <- get_last_file(
    path     = meta_path,
    ext      = ext,
    pattern  = "(?i)Report_Screen_Grab.*\\.csv",
    startRow = 0) |>
      rename(sequence = 1L, 
             reportid = 5L, 
             reportid_auto = 6L)
  
  # assure that it has the correct structure, excluding reports starting with * or = or containing ...
  df_grab.1 <- df_grab.raw |>
    left_join(df_structure, by = c("sequence", "report_name", "reportid", "reportid_auto")) |>
    select(-contains("..."), -keep) |>
    filter(!str_detect(report_name, skip_report_pattern))

  # classify the reports as current, or not
  df_grab.2 <- df_grab.1 |>
    select(-updated, -field_count) |>
    left_join(
      df_last_run |>
        select(section, reportid, report_folderid, keep, updated, 
               # dict_updated, 
               field_count, type),
      by = c("reportid", "section")
    ) |>
    mutate( type = coalesce(type.y, type.x),
            type = .classify_report_type(updated, max_age_days), # used to filter by type later
            type = factor(type),
            report_folderid = coalesce(report_folderid.y, report_folderid.x),
          ) |>
    select(-ends_with(c(".x", ".y")))

  # Optional reportid constraint if we just want to update certain reports
  if (!is.null(report_ids)) {
    df_grab.2 <- df_grab.2 |> filter(reportid %in% report_ids)
    # if using a recordid contraint, type does not matter
    include_types <- NULL
    # no limit
    limit = NULL
  }
  
  # Optional type constraint (new/old/current/etc.)
  if (!is.null(include_types)) {
    df_grab.2 <- df_grab.2 |> filter(type %in% include_types)
  }
  
  .pkg_env$potential_reports_to_update <- df_grab.2
  
  
  
  # Limit constraint: choose which rows to keep, then mark keep = TRUE
  if (!is.null(limit)) {
    df_grab.2 <- df_grab.2 |> 
      mutate(keep = FALSE)
    idx <- which(!df_grab.2$keep)
    if (length(idx) > 0L) {
      promote <- head(idx, limit)
      df_grab.2$keep[promote] <- TRUE
    }
  }
  
  # Final selection: only "keep" == TRUE, filtered earlier to type and limits.
  df <- df_grab.2 |> filter(keep)
  .pkg_env$reports_to_update <- df

  cat(glue("Selected {nrow(df)} out of {nrow(.pkg_env$potential_reports_to_update)} to update\n\n"))
  
  
  return(df)
  
}

#' Screen scrape REDCap report list from Data Exports / Reports page
#'
#' Opens a visible Chromote browser session, navigates to the REDCap
#' Data Export page for a project, waits for the report table to appear,
#' then parses the report list into a tibble.
#'
#' @param paramsect Character config section name or params list.
#' @param pid Optional REDCap project id. If NULL, attempts to use params$pid.
#' @param skip_report_pattern Regex for report names to exclude.
#' @param login_wait_secs Max seconds to wait for the report table to appear.
#' @param silent Logical; suppress messages.
#'
#' @return Tibble with columns sequence, reportid, report_name, reportid_auto.
#' @export
get_rc_report_screen_grab <- function(
    paramsect,
    meta_path = 'Meta', 
    pid = NULL,
    skip_report_pattern = "^\\s*[_#+=*x]",
    login_wait_secs = 120,
    silent = TRUE
) {

  if (FALSE) {
    paramsect <- "esteylist"
    pid <- 51231
    skip_report_pattern <- "^\\s*[_#+=*x]"
    login_wait_secs <- 120
    silent <- FALSE
  }

  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect

  if (is.null(params)) {
    stop("A valid paramsect or params list is required.", call. = FALSE)
  }

  section <- params$section

  if (is.null(pid)) {
    pid <- params$pid
  }

  if (is.null(pid) || length(pid) == 0L || !nzchar(as.character(pid))) {
    stop("A project pid is required, either directly or in params$pid.", call. = FALSE)
  }

  url <- paste0(
    "https://redcap.iths.org/redcap_v17.1.1/DataExport/index.php?pid=",
    pid
  )

  if (!silent) {
    message("Opening REDCap report page for pid ", pid)
    message("Manual login may be required in the browser window.")
    message("Waiting up to ", login_wait_secs, " seconds for login and page load...")
  }

  b <- NULL

  tryCatch({

    b <- chromote::ChromoteSession$new()
    b$view()
    b$go_to(url)

    utils::flush.console()

    if (interactive() && !silent) {
      message(
        "\n*** ACTION REQUIRED ***\n",
        "Please log in to REDCap in the opened Chrome window.\n",
        "The function will continue automatically once the report table appears.\n"
      )
    }

    ok <- b$Runtime$evaluate(
      expression = sprintf(
        "
        new Promise(resolve => {
          const start = Date.now();
          function check() {
            const el = document.querySelector('table#table-report_list');
            if (el) resolve(true);
            else if ((Date.now() - start) > %s * 1000) resolve(false);
            else setTimeout(check, 250);
          }
          check();
        })
        ",
        as.integer(login_wait_secs)
      ),
      awaitPromise = TRUE,
      returnByValue = TRUE
    )$result$value

    if (!isTRUE(ok)) {
      stop(
        paste0(
          "Timed out waiting for REDCap report table after ",
          login_wait_secs,
          " seconds. Log in manually in the Chrome window and re-run."
        ),
        call. = FALSE
      )
    }

    html_text <- b$Runtime$evaluate(
      "document.documentElement.outerHTML",
      returnByValue = TRUE
    )$result$value

    page <- rvest::read_html(html_text)
    table_node <- page |> rvest::html_element("table#table-report_list")

    if (length(table_node) == 0 || is.na(table_node)) {
      stop("Could not find table#table-report_list in page HTML.", call. = FALSE)
    }

    report_grab <- table_node |>
      rvest::html_table() |>
      dplyr::filter(suppressWarnings(!is.na(as.integer(X2)))) |>
      dplyr::mutate(
        sequence      = as.integer(X2),
        reportid      = X1,
        report_name   = X3,
        reportid_auto = X7,
        section       = section
      ) |>
      dplyr::select(sequence, section, report_name, reportid, reportid_auto) |>
      dplyr::filter(!stringr::str_detect(report_name, skip_report_pattern))

    if (!nrow(report_grab) && !silent) {
      message("Report table found, but no report rows remained after filtering.")
    }

    report_grab_path <- normalizePath(file.path(.pkg_env$libdir, meta_path))
    make_sheet(report_grab, report_grab_path, filename = "Report_Screen_Grab")
    make_sheet(report_grab, report_grab_path, filename = "Report_Screen_Grab", date = FALSE)

    report_grab

  }, finally = {
    if (!is.null(b)) {
      try(b$close(), silent = TRUE)
      # try(b$parent$close(), silent = TRUE)
    }
  })
}

get_rc_report_screen_grab_old <- function(
    paramsect,
    meta_path = 'Meta', 
    pid = NULL,
    skip_report_pattern = "^\\s*[_#+=*x]",
    login_wait_secs = 120,
    silent = TRUE
) {

  if (FALSE) {
    paramsect <- "esteylist"
    pid <- 51231
    skip_report_pattern <- "^\\s*[_#+=*x]"
    login_wait_secs <- 120
    silent <- FALSE
  }

  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect

  if (is.null(params)) {
    stop("A valid paramsect or params list is required.", call. = FALSE)
  }

  section <- params$section

  if (is.null(pid)) {
    pid <- params$pid
  }

  if (is.null(pid) || length(pid) == 0L || !nzchar(as.character(pid))) {
    stop("A project pid is required, either directly or in params$pid.", call. = FALSE)
  }

  url <- paste0(
    "https://redcap.iths.org/redcap_v17.1.1/DataExport/index.php?pid=",
    pid
  )

  if (!silent) message("Opening REDCap report page for pid ", pid)

  b <- chromote::ChromoteSession$new()
  b$view()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  # b$view()
  b$go_to(url)

  ok <- b$Runtime$evaluate(
    expression = sprintf(
      "
      new Promise(resolve => {
        const start = Date.now();
        function check() {
          const el = document.querySelector('table#table-report_list');
          if (el) resolve(true);
          else if ((Date.now() - start) > %s * 1000) resolve(false);
          else setTimeout(check, 250);
        }
        check();
      })
      ",
      as.integer(login_wait_secs)
    ),
    awaitPromise = TRUE,
    returnByValue = TRUE
  )$result$value

  if (!isTRUE(ok)) {
    stop(
      "Timed out waiting for REDCap report table. Log in manually in the Chrome window and try again.",
      call. = FALSE
    )
  }

  html_text <- b$Runtime$evaluate(
    "document.documentElement.outerHTML",
    returnByValue = TRUE
  )$result$value

  page <- rvest::read_html(html_text)
  table_node <- page |> rvest::html_element("table#table-report_list")

  if (length(table_node) == 0 || is.na(table_node)) {
    stop("Could not find table#table-report_list in page HTML.", call. = FALSE)
  }

  report_grab <- table_node |>
    rvest::html_table() |>
    dplyr::filter(suppressWarnings(!is.na(as.integer(X2)))) |>
    dplyr::mutate(
      sequence      = as.integer(X2),
      reportid      = X1,
      report_name   = X3,
      reportid_auto = X7,
      section       = section
    ) |>
    dplyr::select(sequence, section, report_name, reportid, reportid_auto) |>
    dplyr::filter(!stringr::str_detect(report_name, skip_report_pattern))

  if (!nrow(report_grab) && !silent) {
    message("Report table found, but no report rows remained after filtering.")
  }
  
  report_grab_path <- normalizePath(file.path(.pkg_env$libdir, meta_path))
  make_sheet(report_grab, report_grab_path, filename = "Report_Screen_Grab")
  make_sheet(report_grab, report_grab_path, filename = "Report_Screen_Grab", date=FALSE)

  return(report_grab)
}


get_rc_report_fields <- function(reportid=123, 
                                 name = 'Report Title',
                                 section = "esteylist", 
                                 n_max = 5,
                                 silent = TRUE) {
  
  if (!silent)  cat(reportid, name)
  
  empty_rslt <-tibble::tibble(
      reportid   = as.integer(reportid),
      report     = as.character(''),
      field_name = character(0),
      report_field_order = as.integer())
      
  df <- tryCatch( # reportid=182957; section='esteylist' 
    
    get_rc_report(
      paramsect = section,
      reportid  = reportid,
      label     = FALSE,
      silent    = TRUE,
      allblank  = TRUE,
    ),
    error = function(e) {
      # optionally comment this out if you want truly silent failure
      # message("Error for reportid = ", reportid, ": ", conditionMessage(e))
      NULL
    }
  )

  rslt <- if (is.null(df) || nrow(df) == 0L) {
    empty_rslt
  } else {
    tibble::tibble(
      reportid   = as.integer(reportid),
      report     = as.character(name),
      field_name = names(df),
      report_field_order = seq_along(field_name)
    )
  }
}

#' Export REDCap user roles (aka role-level rights) via API
#'
#' @param paramsect Character name of a configset or a params list with
#'   elements `encrypted_token`, `url`, and optionally `section`.
#' @param silent Logical; suppress messages.
#' @return A tibble with one row per role, plus list-cols for per-form rights.
#'         Use `tidy_role_rights()` below to expand to per-form long format.
get_rc_roles <- function(paramsect = NULL, silent = TRUE) {
  if (FALSE) {
    paramsect <- "mpal"  # or a params list
    silent <- TRUE
  }

  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect

  # default return value
  df <- tibble::tibble()
  if (!silent) message("Loading role definitions")

  # require parameter object
  if (is.null(params)) return(df)

  # set defaults
  api_token <- params$encrypted_token
  api_url   <- params$url
  section   <- if (is.list(params)) params[["section"]] else params

  # form data for role export
  formData <- list(
    token         = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    content       = "userRole",
    format        = "json",
    returnFormat  = "json"
  )

  # request data from REDCap api url
  response <- httr::POST(api_url, body = formData, encode = "form")

  # Check for successful response
  if (httr::status_code(response) != 200) {
    cat("\nError: Failed to fetch userRole from REDCap\n")
    return(df)
  }

  result <- httr::content(response, as = "text", encoding = "UTF-8")

  # Parse: keep nested 'forms' and 'forms_export' as list-cols
  roles_list <- jsonlite::fromJSON(result, simplifyVector = FALSE)

  # Normalize to a tibble with list-cols
  out <- purrr::map_dfr(roles_list, function(x) {
    tibble::tibble(
      unique_role_name = x$unique_role_name,
      role_label       = x$role_label,
      # top-level rights (optional; keep as character/numeric)
      design                 = x$design,
      alerts                 = x$alerts,
      user_rights            = x$user_rights,
      data_access_groups     = x$data_access_groups,
      reports                = x$reports,
      stats_and_charts       = x$stats_and_charts,
      manage_survey_participants = x$manage_survey_participants,
      calendar               = x$calendar,
      data_import_tool       = x$data_import_tool,
      data_comparison_tool   = x$data_comparison_tool,
      logging                = x$logging,
      email_logging          = x$email_logging,
      file_repository        = x$file_repository,
      data_quality_create    = x$data_quality_create,
      data_quality_execute   = x$data_quality_execute,
      api_export             = x$api_export,
      api_import             = x$api_import,
      api_modules            = x$api_modules,
      mobile_app             = x$mobile_app,
      mobile_app_download_data = x$mobile_app_download_data,
      record_create          = x$record_create,
      record_rename          = x$record_rename,
      record_delete          = x$record_delete,
      lock_records_customization = x$lock_records_customization,
      lock_records           = x$lock_records,
      lock_records_all_forms = x$lock_records_all_forms,
      # nested as list-cols:
      forms        = list(x$forms),
      forms_export = list(x$forms_export)
    )
  })

  return(out)
}


#' Get per-form access for one or more REDCap roles
#'
#' @param section REDCap configset name or params used by get_rc_roles()
#' @param role Optional role label(s) or unique_role_name(s) to filter
#' @return list(forms = tibble with columns: unique_role_name, role_label,
#'              form_name, access (0-3), access_text, can_export [TRUE/FALSE]),
#'         by_role = named list of form vectors per category
get_rc_access <- function(section = "mpal", role = NULL) {
  roles <- get_rc_roles(section)

  # allow role label OR unique_role_name (vector-friendly)
  if (!is.null(role)) {
    roles <- roles |> filter(role_label %in% role | unique_role_name %in% role)
  }
  if (nrow(roles) == 0L) return(list(forms = tibble::tibble(), by_role = list()))

  access_label <- c(`0` = "no_access",
                    `1` = "view_edit",
                    `2` = "read_only",
                    `3` = "edit_survey")

  # Unnest with indices_to = "form_name"
  access_tbl <- roles |>
    dplyr::select(unique_role_name, role_label, forms) |>
    unnest_longer(forms, values_to = "access", indices_to = "form_name")

  export_tbl <- roles |>
    dplyr::select(unique_role_name, role_label, forms_export) |>
    unnest_longer(forms_export, values_to = "can_export", indices_to = "form_name")

  # Join by role keys + form_name
  forms_tbl <- access_tbl |>
    left_join(export_tbl, by = c("unique_role_name", "role_label", "form_name")) |>
    mutate(
      access      = as.integer(access),
      can_export  = as.integer(replace_na(can_export, 0L)),
      access_text = access_label[as.character(access)],
      can_export  = can_export == 1L
    ) |>
    arrange(role_label, form_name)

  # Buckets per role
  by_role <- forms_tbl |>
    group_split(role_label, .keep = TRUE) |>
    set_names(map_chr(., ~ unique(.x$role_label))) |>
    map(~ list(
      accessible = sort(unique(.x$form_name[.x$access != 0L])),
      read_only  = sort(unique(.x$form_name[.x$access == 2L])),
      editable   = sort(unique(.x$form_name[.x$access %in% c(1L, 3L)])),
      exportable = sort(unique(.x$form_name[.x$can_export]))
    ))

  return(forms_tbl)
}

get_rc_fields <- function(paramsect = 'esteylist',
                          flds,
                          label     = FALSE,     # FALSE = raw codes; TRUE = labels
                          keyfields = FALSE,     # include keyfields (recordid, ptmrn, ...)
                          as_vector = TRUE,      # if exactly one non-key column, return vector
                          col_class = NULL,      # "numeric","integer","Date","POSIXct"
                          silent    = TRUE) {
  
  if (FALSE) {
    paramsect = 'mpal'
    flds      = c('recordid', 'ptmrn')
    label     = FALSE
    keyfields = TRUE
    as_vector = TRUE
    col_class = NULL
    silent    = TRUE
  }
  
  # ---- guards ----
  # if (missing(flds) || is.null(flds) || length(flds) == 0)
  #   return(invisible(if (as_vector) NULL else data.frame()))
  
  include_keys = keyfields

  # normalize flds to character vector
  stopifnot(is.character(flds))
  flds      <- unique(flds)

  # ---- config & metadata ----
  paramsect <- if (is.character(paramsect)) get_configset(paramsect) else paramsect
  section   <- paramsect$section
  api_token <- paramsect$encrypted_token
  api_url   <- paramsect$url
  keyfields <- paramsect$keyfields
  if (length(keyfields) == 0) keyfields <- list('recordid')

  # ensure rcdd matches this project
  get_rc_dictionary(paramsect, reload = TRUE, silent = TRUE)

  # ---- validate requested fields against data dictionary ----
  orig_flds     <- unlist(flds, use.names = FALSE)
  missing_flds  <- setdiff(orig_flds,   rcdd$field_name)
  keys          <- if (include_keys) intersect(unlist(keyfields, use.names = FALSE), rcdd$field_name) else character(0)
    
  # add the requested fields
  keepflds <- unique(c(keys, orig_flds))

  # if at least one checkbox requested
  has_checkbox  <- nrow(rcdd |> filter(field_name %in% keepflds & field_type=='checkbox')) > 0L
  if (has_checkbox) {
    chkflds        <- intersect(orig_flds, rcdd$field_name[rcdd$field_type == "checkbox"])
    chkflds_expand <- sapply(chkflds, 
                             function(f) get_rc_code(f)$field_code_name,
                             simplify = FALSE, USE.NAMES = FALSE)
    chkflds_expand <- unique(unlist(chkflds_expand))
  }

  if (length(missing_flds) > 0) {
    warning("The following fields are not in the REDCap data dictionary: ", paste(missing_flds, collapse = ", "))
  }

  # ---- build POST body ----
  formData <- list(
    token                  = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    content                = 'record',
    action                 = 'export',
    format                 = 'json',
    type                   = 'flat',
    csvDelimiter           = '',
    rawOrLabel             = if (label) 'label' else 'raw',
    exportCheckboxLabel    = if (label) 'true' else 'false',
    exportSurveyFields     = 'false',
    exportDataAccessGroups = 'false',
    returnFormat           = 'json'
  )

  formData[paste0("fields[", seq_along(keepflds) - 1L, "]")] <- as.list(keepflds)

  # ---- call API ----
  resp <- httr::POST(api_url, body = formData, encode = "form")
  
  if (httr::status_code(resp) != 200) {
    if (!silent) message("REDCap error: ", httr::content(resp, as = "text", encoding = "UTF-8"))
    return(invisible(if (as_vector) NULL else data.frame()))
  }

  rslt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (identical(rslt, "[]")) {
    # construct empty frame with the exact columns we might expect:
    cols <- character(0)
    if (include_keys && keys_first) cols <- c(cols, unlist(keyfields, use.names = FALSE))
    cols <- c(cols, flds)
    if (include_keys && !keys_first) cols <- c(cols, unlist(keyfields, use.names = FALSE))
    return(as.data.frame(setNames(replicate(length(cols), logical(0), simplify = FALSE), cols)))
  }


  # ---- keep exactly what we asked for (plus keys if added) ----
  df   <- jsonlite::fromJSON(rslt, simplifyDataFrame = TRUE)
  # keep <- c(orig_flds, names(df))
  # df   <- select(df, all_of(keep))

  # ---- return shape ----
  if (as_vector && ncol(df) == 1L) return(df[[1]])
  return(df)
}



# section<-'diagnosis_for_aml_arrivals'
# params<-get_configset(section)
# x <- get_rc_table(params,'trm')
# karyolkup <- get_rc_table('karyo_lookup','karyolkup') |> filter(karyo > '') |> distinct(karyocode, karyo)

get_rc_table <- function(paramsect='esteylist'
                       , tbl=NULL
                       , label=FALSE
                       , silent=TRUE)
{
 
  if (FALSE) {
    paramsect <- 'esteylist'
    tbl       <- 'patient_list'
    label     <- TRUE
    silent    <- FALSE
  }
  
  
  
    
  # default return value
  df=data.frame()
  if (!silent) message("Loading df with ", tbl, " data")

  # require table name
  if (is.null(tbl)) {return(df)}
  # require parameter object
  if (is.null(paramsect)) {return(df)}  
  
  # load params from configurations
  paramsect <- if (is.character(paramsect)) get_configset(paramsect) else paramsect
  
  # always reload rcdd in case last time was a different redcap, it's pretty quick
  get_rc_dictionary(paramsect, reload=TRUE, silent=TRUE)

  # set defaults
  section   <- paramsect$section
  api_token <- paramsect$encrypted_token
  api_url   <- paramsect$url
  keyfields <- paramsect$keyfields
  
  
  # let's assume that if keyfields is empty that the first field in the first redcap form is a keyfield
  if (length(keyfields)==0) keyfields = list('recordid')
  
  # is the project set up with a rectime for each table?
  # NOTES:  rectime is a field for knowing if any data has been entered interactively
  # into the form.  Can be misleading for projects where the data are generated
  # such as the karyo_lookup or a project created via LEAF.  On those type of
  # project it would be better to NOT include a <form>_rectime field.
  rectime_column <- glue::glue("{tbl}_rectime")
  has_rectime    <- rectime_column %in% rcdd$field_name

  formData <- list(
    token                  = safer::decrypt_string(api_token,decrypt_encryptkey(get_encryptkey())),
    content                = 'record',
    action                 = 'export',
    format                 = 'json',
    type                   = 'flat',
    csvDelimiter           = '',
    'forms[0]'             = tbl,
    rawOrLabel             = 'raw',  # <-- Default to 'raw'
    exportCheckboxLabel    = 'true',
    exportSurveyFields     = 'false',
    exportDataAccessGroups = 'false',
    returnFormat = 'json'
  )

  # Add the key fields dynamically
  for (i in seq_along(keyfields)) {
    newnode <- paste0('fields[', i-1, ']')
    formData[[newnode]] <- keyfields[i]
    # <- keyfields[i]
  }

  # Optionally set to 'label' for descriptive (label) rather than coded (raw) data in cells
  if (label) {formData$rawOrLabel <- 'label'}  # <-- Set 'label' for labeled export

  # Make the POST request
  response <- httr::POST(api_url, body = formData, encode = "form")
  
  # Check for successful response
  if (httr::status_code(response) != 200) {
    # Failed return early
    cat('\nError: Failed to fetch',tbl,'data from REDCap\n')
    return (df)
  }

  # Parse the JSON response
  result <- httr::content(response, as = "text", encoding = "UTF-8")
  if (result=='[]') { # redcap table does not contain data
    # Need code here to build an empty table
    form_fields <- rcdd |> 
      filter(form_name == tbl | field_name %in% keyfields) |> 
      pull(field_name)
    df <- as.data.frame(setNames(replicate(length(form_fields), logical(0), simplify = FALSE), form_fields))
    df <- df |> select(any_of(keyfields), everything())
  } else if (has_rectime) {
    df <- jsonlite::fromJSON(result) |> 
      # filter(!is.na(.data[[rectime_column]]) & .data[[rectime_column]] != '') |>
      select(all_of(keyfields), , everything())
    # how many of the rectime_column fields are filled in?
  } else {
    df <- jsonlite::fromJSON(result) |> 
      select(all_of(keyfields), everything())
  }

  # adjust via the rectime variable
  if (has_rectime) {
    rectime_df  <- df |> 
      select(ends_with('_rectime')) |> 
      rename(rectime = 1) |>
      filter(rectime > '' & !is.na(rectime))
    if (nrow(rectime_df)>0) {
      df <- df |> 
        filter(!is.na(.data[[rectime_column]]) & .data[[rectime_column]] != '') # |> select(-any_of(rectime_column))
    }
  }
  
  return(df)
  
}
# rslt <- get_rc_row_ids('eln2022', label=FALSE, section='esteylist')

# get_rc_from_list()
get_rc_tables <- function(paramsect=NULL
                           , label=FALSE
                           , silent=TRUE) {
  
  # verify required parameters
  if (eng=='SQLServer' & is.null(paramsect)) {
    # use default parameter section
    paramsect <- 'sqlserver'
  }
  silent <- if (exists("silent")) silent else TRUE
  label  <- if (exists("label")) label else TRUE
  
  # Read in parameters from configurations section
  retain       <- paramsect[['retain']]
  base_schema  <- paramsect[['schema']]
  label_schema <- paste0(paramsect[['schema']],'_label')

  # local vars
  warn_msg <- 'Warnings:'
  # Which column from the summary df should be used to name the table?
  NameCol  <- 2
  
  # output summary
  if (length(retain)>=1) {
    assign("summary_df", data.frame(
      Row     = 1:length(retain),
      Tables  = retain,
      Schemas = paste(base_schema, 'and', label_schema),
      Rows    = -999
    ), envir = .GlobalEnv)
  }

  loop_cnt <- length(retain)
  pb <- get_progress_bar(loop_cnt, "GETTING TABLES FROM REDCAP")
  for (i in 1:loop_cnt) {
    # for loop vars
    error_go_next <- FALSE
    blank_go_next <- FALSE
    rslt          <- NULL
    tbl           <- retain[i]

    pb$tick()

    # abort if no table name
    if (tbl=='') {next}
    
    tryCatch({
        # handle this one table
        rslt <- get_rc_table(paramsect, tbl, label=label, silent=TRUE)
      }, error = function(e) {
        cat("Error getting REDCap Tables:",tbl, e$message, "\n")

        # abort for error, can't next inside a tryCatch
        error_go_next <- TRUE
      }, warning = function(w) {
        warn_msg <- paste(warn_msg,"\n",paste0(toupper(tbl), ": "), w$message)
        blank_go_next <- TRUE
      }
    )

    # loop on no result
    if (blank_go_next) {
      msg <- paste("NULL data frame returned for",tbl,"\n")
      warn_msg <- paste(warn_msg,msg)
      next
    }

    # loop on error
    if (error_go_next) {
      msg <- paste("EMPTY data frame returned for",tbl,"\n")
      warn_msg <- paste(warn_msg,msg)
      next
    }

    # place result into tbl df of the global environment
    assign(tbl, rslt, envir = globalenv())
    summary_df[i, 4] <- nrow(rslt)
    # loaded <- paste0(loaded, tbl, ' (n=', nrow(rslt), ')\n')
    # df_list[[tbl]] <- rslt
  } # end for
  
  rm(pb)
  rm(rslt)
  # need to keep these open because in the next portion we will write structure, adjust and append
  # rm(list = ls(pattern = glue::glue("^arrival\\d*$")), envir = .GlobalEnv)
  

  return(summary_df)
} # end func

# group_rc_chkbox_dt <- function(df, rcdd_exp = NULL, params = NULL, label = TRUE, silent = TRUE) {
# 
#   # Load rcdd_exp as needed
#   if (is.null(local_$rcdd_exp)) {
#     params   <- if (is.null(local_$params)   & !exists('params'))   get_configset() else local_$params
#     rcdd_exp <- if (is.null(local_$rcdd_exp)) get_rc_dictionary('esteylist', ddname = "rcdd_exp", explode = TRUE)
#   }
# 
#   stopifnot(is.data.frame(df), is.data.frame(rcdd_exp))
# 
#   rcdd_chk <- rcdd_exp[
#     rcdd_exp$field_type == "checkbox" & !grepl("show", rcdd_exp$field_stem, fixed = TRUE),
#     c("field_name","field_stem","code","label")
#   ]
# 
#   contrib <- intersect(rcdd_chk$field_name, names(df))
#   if (!length(contrib)) return(df)
# 
#   # Named maps (no joins)
#   stem_of  <- setNames(rcdd_chk$field_stem, rcdd_chk$field_name)
#   code_of  <- setNames(as.character(rcdd_chk$code), rcdd_chk$field_name)
#   label_of <- setNames(rcdd_chk$label, rcdd_chk$field_name)
#   rcdd_chk$ord <- ave(seq_len(nrow(rcdd_chk)), rcdd_chk$field_stem, FUN = seq_along)
#   ord_of   <- setNames(rcdd_chk$ord, rcdd_chk$field_name)
# 
#   # data.table path
#   library(data.table)
# 
#   DT <- as.data.table(df)
#   DT[, .rowid := .I]   # keep original row id
# 
#   m  <- data.table::melt(
#     DT,
#     id.vars      = ".rowid",               # <- carry row ids through melt
#     measure.vars = contrib,
#     variable.name = "field_name",
#     value.name    = "val"
#   )
#   setDT(m)  # ensure it's a data.table
# 
#   # filter checked
#   m <- m[!(is.na(val) | val == 0 | val == "" | val == "Unchecked")]
# 
#   # map metadata via named vectors
#   fn <- as.character(m$field_name)
#   m[, field_stem := stem_of[fn]]
#   if (isTRUE(label)) {
#     m[, display := paste0(code_of[fn], ", ", label_of[fn])]
#   } else {
#     m[, display := code_of[fn]]
#   }
#   m[, ord := ord_of[fn]]
# 
#   # order & collapse per row Ãƒâ€” stem
#   setorder(m, .rowid, field_stem, ord)
#   agg <- m[, .(val = paste(unique(display), collapse = " | ")), by = .(.rowid, field_stem)]
# 
#   # cast wide
#   wide <- data.table::dcast(agg, .rowid ~ field_stem, value.var = "val", fill = "")
#   data.table::setorder(wide, .rowid)
# 
#   # --- ensure 1 row per original record --------------------------------------
#   idx  <- data.table::data.table(.rowid = seq_len(nrow(df)))
#   wide <- merge(idx, wide, by = ".rowid", all.x = TRUE, sort = FALSE)
#   # replace NA with "" in summary columns
#   if (ncol(wide) > 1L) {
#     for (j in names(wide)[names(wide) != ".rowid"]) {
#       data.table::set(wide, i = which(is.na(wide[[j]])), j = j, value = "")
#     }
#   }
#   wide[, .rowid := NULL]
# 
#   # ensure stems exist even if empty
#   stems <- unique(rcdd_chk$field_stem[match(contrib, rcdd_chk$field_name)])
#   for (s in stems) if (!s %in% names(wide)) wide[, (s) := ""]
# 
#   # bind and position stem columns before first contributing ___ column
#   out <- cbind(df, as.data.frame(wide), stringsAsFactors = FALSE)
# 
#   orig  <- names(df)
#   pos   <- setNames(seq_along(orig), orig)
#   first_by_stem <- tapply(contrib, stem_of[contrib], `[`, 1)
#   anchor <- pos[unlist(first_by_stem[names(first_by_stem) %in% stems])]
#   stempos <- setNames(anchor - 0.5, names(anchor))
#   allpos <- pos[intersect(names(out), names(pos))]
#   allpos[names(stempos)] <- stempos
#   out <- out[, names(out)[order(allpos[match(names(out), names(allpos))], na.last = TRUE)], drop = FALSE]
# 
#   # drop raw ___ columns
#   out[contrib] <- list(NULL)
#   out
# 
#   # unloadNamespace("data.table")
# }
# 
# 

# full implementation lives in standardize_decimals()
get_rc_decimals <- function(df,
                   cols     = NULL,
                   digits   = NULL,
                   max_dec  = 6L,
                   blank_na = TRUE) {
  standardize_decimals(df, cols = cols, digits = digits,
                       max_dec = max_dec, blank_na = blank_na)
}

make_rc_sheet <- function(df,
                      path          = "C:/Users/cmshaw/Desktop/",
                      sheet         = "Sheet1",
                      filename      = "Results_",
                      date          = TRUE,
                      time          = FALSE,
                      overwrite     = FALSE) {
  make_sheet( df            = df,
              path          = path,
              sheet         = sheet,
              filename      = filename,
              date          = date,
              time          = time,
              overwrite     = overwrite,
              ext           = "csv",
              null_as_blank = TRUE,
              fix_decimals  = TRUE)
}

# Assumes:
# - redcap_api.Rmd has been sourced (getrcreport, getrcdictexp / getrcdictionary)
# - import_export.Rmd has been sourced (write_to_file)

make_rc_report_workbook <- function(paramsect,
                                    reportid,
                                    dict_section = NULL,
                                    keepnewlines = TRUE,
                                    newlinepatterns = NULL,
                                    directory = NULL,
                                    output_file = 'ReportWorkbook',
                                    date_format = "",
                                    output_type = "xlsx") {
  
  if (FALSE) {
    paramsect = 'esteylist'
    reportid = 376067
    dict_section = NULL
    keepnewlines = TRUE
    newlinepatterns = NULL
    directory = scrapdir
    output_file = NULL
    date_format = ""
    output_type = "xlsx"
  }

  # ---- 1. Pull coded (raw) report ----
  df_raw <- get_rc_report(
    paramsect    = paramsect,
    reportid     = reportid,
    label        = FALSE,
    silent       = TRUE
  )

  # ---- 2. Pull labeled report ----
  df_lab <- get_rc_report(
    paramsect    = paramsect,
    reportid     = reportid,
    label        = TRUE,
    silent       = TRUE
  )

  if (nrow(df_raw) == 0 && nrow(df_lab) == 0) {
    warning("Report has no rows; creating workbook with empty data sheets.")
  }

  # Fields in the report (from raw; labeled should be same cols)
  flds <- colnames(df_raw)

  # ---- 3. Decide which dictionary section to use ----
  if (is.null(dict_section)) {
    if (is.list(paramsect) && "section" %in% names(paramsect)) {
      dict_section <- paramsect$section
    } else {
      dict_section <- paramsect
    }
  }

  # ---- 4. Get a tailored dictionary for just these fields ----
  # Prefer exploded dictionary if available exists('.pkg_env$rcdd_exp')
  if (exists("get_rc_dictexp", mode = "function")) {
    rcdd_exp <- get_rc_dictexp(dict_section, assign_to = "pkg_env", reload = FALSE)
  } else {
    rcdd_exp <- get_rc_dictionary(dict_section, assign_to = "pkg_env", reload = FALSE, explode = TRUE)
  }

  dict_sub <- rcdd_exp |>
    dplyr::filter(field_stem %in% flds | field_name %in% flds) |>
    dplyr::mutate(field_name = field_stem,
                  validation_range = dplyr::case_when(
                    is.na(text_validation_min) & is.na(text_validation_max)   ~ "",
                    is.na(text_validation_min) & !is.na(text_validation_max)  ~ paste0("[<", text_validation_max, "]"),
                    !is.na(text_validation_min) & is.na(text_validation_max)  ~ paste0("[>", text_validation_min, "]"),
                    !is.na(text_validation_min) & !is.na(text_validation_max) ~ paste0("[", text_validation_min, "-", text_validation_max, "]")),
                  validation = dplyr::case_when(
                    is.na(text_validation_type_or_show_slider_number) & validation_range == "" ~ "",
                    is.na(text_validation_type_or_show_slider_number) & validation_range != "" ~ validation_range,
                    !is.na(text_validation_type_or_show_slider_number)                         ~ paste(text_validation_type_or_show_slider_number, validation_range))) |>
    dplyr::select(dplyr::any_of(c("form_name", "field_name", "field_label",
                                  "code", "label", "field_type",
                                  "validation", "validation_range"))) |>
    dplyr::arrange(form_name, field_name, code)
  
  
  # dict_sub <- rcdd_exp %>%
  #   dplyr::filter(field_name %in% flds)
  # 
  # # Make the dictionary more reportâ€‘friendly; tweak as you like
  # dict_sub <- rcdd_exp |> filter(field_stem %in% flds | field_name %in% flds) |>
  #   mutate(field_name = field_stem,
  #          validation_range = case_when(is.na(text_validation_min) 
  #                                     & is.na(text_validation_max) ~ "",
  #                                     is.na(text_validation_min)   ~ paste0("[<", text_validation_max, "]"),
  #                                     is.na(text_validation_max)   ~ paste0("[>", text_validation_min, "]"),
  #                                     .default=paste0("[", text_validation_min, "-", text_validation_max, "]")),
  #         validation       = case_when(is.na(text_validation_type_or_show_slider_number) ~ "",
  #                                     validation_range == ""                            ~ text_validation_type_or_show_slider_number,
  #                                     .default = paste(text_validation_type_or_show_slider_number,validation_range))) |>
  #   select(any_of(c("form_name","field_name", "field_label", 
  #                   "code", "label", "field_type", 
  #                   "validation", "validation_range"))) |>
  #   arrange(form_name, field_name, code)
  
  
  # Identify checkbox fields present in the report
  checkbox_fields <- dict_sub %>%
    dplyr::filter(.data$field_type == "checkbox") %>%
    dplyr::pull(.data$field_name) %>%
    intersect(colnames(df_raw)) %>%
    unique()
  
  # Expand each checkbox field in the coded data
  for (fld in checkbox_fields) {
    df_raw <- expand_checkbox_column(df_raw, fld, dict_sub, sep = ",")
  }  
  

  # ---- 5. Build list of dfs for export ----
  dfs_list <- list(
    "Labeled data"     = df_lab,
    "Coded data"       = df_raw,
    "Data dictionary"  = dict_sub
  )

  # ---- 6. Write with your import_export helpers ----
  # output_names correspond to sheet names (xlsx) or file names (csv)
  output_names <- names(dfs_list)

  file_path <- write_to_file(
    dfs          = dfs_list,
    output_names = output_names,
    directory    = directory,
    date_format  = date_format,
    output_type  = output_type,
    output_file  = output_file
  )

  file_path
}


add_rc_label <- function(df, flds, section = 'esteylist', rcdd_exp = NULL, suffix = "_label") {
  stopifnot(
    is.data.frame(df),
    is.character(flds), 
    length(flds) > 0
  )
  
  flds <- intersect(flds, names(df))
  if (!length(flds)) return(df)
  
  # Resolve exploded dictionary
  if (is.null(rcdd_exp)) {
    rcdd_exp <- get0("rcdd_exp", envir = .pkg_env, inherits = FALSE)
  }
  
  if (is.null(rcdd_exp)) {
    get_rc_dictionary(section, ddname = "rcdd_exp", explode = TRUE)  # your side-effect cache
    rcdd_exp <- get0("rcdd_exp", envir = .pkg_env, inherits = FALSE)
  }
  
  stopifnot(
    is.data.frame(rcdd_exp),
    all(c("field_name", "code", "label") %in% names(rcdd_exp))
  )
  
  # Build one long lookup table for only the requested fields of type radio or dropdown, not for checkbox fields
  lu <- rcdd_exp |>
    dplyr::filter(.data$field_name %in% flds,
                  .data$field_type %in% c("radio", "dropdown")) |>
    dplyr::transmute(
      field = .data$field_name,
      code  = as.character(.data$code),
      label = as.character(.data$label)
    ) |>
    dplyr::distinct(field, code, .keep_all = TRUE)
  
  # Long -> join -> widen (but now guaranteed 1 label per field/code)
  labs <- df |>
    dplyr::mutate(.rowid = dplyr::row_number()) |>
    tidyr::pivot_longer(dplyr::all_of(flds), names_to = "field", values_to = "code") |>
    dplyr::mutate(code = as.character(.data$code)) |>
    dplyr::left_join(lu, by = c("field", "code")) |>
    dplyr::mutate(field = paste0(.data$field, suffix)) |>
    dplyr::select(.rowid, field, label) |>
    tidyr::pivot_wider(names_from = field, values_from = label)
  
  dplyr::bind_cols(df, dplyr::select(labs, - .rowid))
}


expand_checkbox_column <- function(df, field, dict_sub, sep = ",") {
  # dict_sub: tailored dictionary used in your function (includes code per field)
  codes <- dict_sub %>%
    dplyr::filter(.data$field_name == !!field,
                  .data$field_type == "checkbox") %>%
    dplyr::pull(.data$code) %>%
    unique()

  if (length(codes) == 0L || !field %in% names(df)) return(df)

  # Work on a copy of the source column as character
  src <- as.character(df[[field]])

  for (code in codes) {
    new_col <- paste0(field, "___", code)
    df[[new_col]] <- ifelse(
      !is.na(src) & src != "" &
        stringr::str_detect(
          paste0(sep, src, sep),
          paste0(sep, code, sep)
        ),
      1L, 0L
    )
  }

  df[[field]] <- NULL  # remove condensed source; or keep if you prefer
  df
}

if (FALSE) {
  df     <- dx
  ord    <- 'nat'
  label  <- TRUE
  params <- local_$params
}
group_rc_chkbox <- function(
  df,
  params = NULL,
  mode   = c("label", "both", "code", "any_checked"),
  ord    = c("nat", "code", "label"),
  sep    = " | ",
  silent = TRUE
) {
  mode <- match.arg(mode)
  ord  <- match.arg(ord)

  params <- if (is.null(params)) 'esteylist' else params
  
  # Load exploded dictionary for checkboxes
  rcdd_exp <- get_rc_dictionary(
      params, 
      ddname = "rcdd_exp", 
      explode = TRUE, 
      assign_to = "pkg_env")

  dict_chk <- rcdd_exp |>
    dplyr::filter(field_type == "checkbox") |>
    dplyr::distinct(field_name, field_stem, code, label) |>
    dplyr::group_by(field_stem) |>
    dplyr::mutate(
      ord_nat   = dplyr::row_number(),
      ord_code  = rank(code, ties.method = "first"),
      ord_label = rank(tolower(label), ties.method = "first")
    ) |>
    dplyr::ungroup()

  # Identify checkbox columns present in df
  chk_cols <- intersect(dict_chk$field_name, names(df))
  if (!length(chk_cols)) return(df)

  sort_key <- switch(ord, nat = "ord_nat", code = "ord_code", label = "ord_label")

  # Long view of present checkbox columns joined to dictionary; keep only checked
  long_checks <- df |>
    dplyr::mutate(.rowid = dplyr::row_number()) |>
    tidyr::pivot_longer(tidyselect::all_of(chk_cols),
                        names_to = "field_name", values_to = "checked_raw") |>
    dplyr::left_join(
      dict_chk |>
        dplyr::select(field_stem, field_name, code, label, ord_nat, ord_code, ord_label),
      by = "field_name"
    ) |>
    dplyr::filter(!(is.na(checked_raw) | checked_raw == 0 | checked_raw == "" | checked_raw == "Unchecked"))

  # Collapse per row Ãƒâ€” stem
  if (mode == "any_checked") {
    collapsed <- long_checks |>
      dplyr::group_by(.rowid, field_stem) |>
      dplyr::summarise(collapsed_value = 1L, .groups = "drop")
  } else {
    collapsed <- long_checks |>
      dplyr::arrange(.rowid, field_stem, .data[[sort_key]]) |>
      dplyr::mutate(piece = if (mode == "code") {
                      as.character(code)
                    } else if (mode == "label") {
                      label
                    } else {
                      paste0(code, ", ", label)  # mode == "both"
                    }) |>
      dplyr::group_by(.rowid, field_stem) |>
      dplyr::summarise(collapsed_value = paste(unique(piece), collapse = sep), .groups = "drop")
    # collapsed <- long_checks |>
    #   dplyr::arrange(.rowid, field_stem, .data[[sort_key]]) |>
    #   dplyr::mutate(piece = dplyr::case_when(
    #     mode == "code"  ~ as.character(code),
    #     mode == "label" ~ label,
    #     TRUE            ~ paste0(code, ", ", label) # mode == "both"
    #   )) |>
    #   dplyr::group_by(.rowid, field_stem) |>
    #   dplyr::summarise(collapsed_value = paste(unique(piece), collapse = sep), .groups = "drop")
  }

  # Wide view, preserving row order
  row_index <- tibble::tibble(.rowid = seq_len(nrow(df)))

  wide_collapsed <- tidyr::pivot_wider(
    collapsed,
    id_cols     = .rowid,
    names_from  = field_stem,
    values_from = collapsed_value,
    values_fill = if (mode == "any_checked") 0L else ""
  ) |>
    dplyr::right_join(row_index, by = ".rowid") |>
    dplyr::arrange(.rowid) |>
    dplyr::select(-.rowid)

  # Ensure stem columns exist even if entirely unchecked
  present_stems <- unique(dict_chk$field_stem[dict_chk$field_name %in% chk_cols])
  for (s in present_stems) if (!s %in% names(wide_collapsed)) {
    wide_collapsed[[s]] <- if (mode == "any_checked") 0L else ""
  }

  # Compute original column positions and create insertion anchors
  orig_names <- names(df)
  orig_pos   <- setNames(seq_along(orig_names), orig_names)

  # For each stem, choose the earliest raw checkbox column by position in df (anchor)
  stem_anchor <- dict_chk |>
    dplyr::filter(field_name %in% chk_cols) |>
    dplyr::mutate(col_idx = orig_pos[field_name]) |>
    dplyr::group_by(field_stem) |>
    dplyr::slice_min(col_idx, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(field_stem, anchor_col = field_name)

  # Convert to numeric insert positions just before the anchor
  anchor_pos  <- orig_pos[stem_anchor$anchor_col]
  stem_anchor <- setNames(anchor_pos - 0.5, stem_anchor$field_stem)

  # Bind, order columns, and drop raw checkbox columns
  rslt_df <- dplyr::bind_cols(df, wide_collapsed)

  rslt_pos <- orig_pos[intersect(names(rslt_df), names(orig_pos))]
  rslt_pos[names(stem_anchor)] <- stem_anchor[names(stem_anchor)]

  final_order <- names(rslt_df)[order(rslt_pos[match(names(rslt_df), names(rslt_pos))], na.last = TRUE)]
  rslt_df <- rslt_df[, final_order, drop = FALSE]

  rslt_df <- dplyr::select(rslt_df, -tidyselect::any_of(chk_cols))

  return(rslt_df)
}


group_rc_chkbox_old2 <- function(df, 
                            params = NULL, 
                            label = TRUE, 
                            silent = TRUE, 
                            ord=c('nat', 'code', 'label')) {

  # Load rcdd_exp as needed
  get_rc_dictionary(params, ddname = "rcdd_exp", explode = TRUE)
  .pkg_env$rcdd_exp <- rcdd_exp

  rcdd_chk <- rcdd_exp |> 
    filter(field_type == "checkbox", form_name == 'patient_list') |>
    distinct(field_name, field_stem, code, label) |>
    group_by(field_stem) |>
    mutate(ord = row_number()) |>
    ungroup() |>
    distinct(field_stem, field_name)

  # which checkbox columns are actually present
  contrib <- intersect(rcdd_chk$field_name, names(df))
  if (!length(contrib)) return(df) # return if no checkbox fields

  # --- long -> summarise -> wide ---------------------------------------------
  idx_tbl <- tibble::tibble(.rowid = seq_len(nrow(df)))

  long_tbl <- df |>
    mutate(.rowid = row_number()) |>
    pivot_longer(all_of(contrib), 
                 names_to = "field_name", 
                 values_to = "val") |>
    left_join(rcdd_exp |> select(field_stem, 
                                 field_name, 
                                 code, 
                                 label, 
                                 select_choices_or_calculations), 
              by = "field_name") |>
    # keep only "checked"
    filter(!(is.na(val) | val == 0 | val == "" | val == "Unchecked"))

  # collapse per row Ãƒâ€” stem, respecting dictionary order
  if (isTRUE(label)) {
    disp_fmt <- function(code, label) paste0(code, ", ", label)
  } else {
    disp_fmt <- function(code, label) as.character(code)
  }

  summ_tbl <- long_tbl |>
    arrange(.rowid, field_stem, ord) |>
    mutate(display = disp_fmt(code, label)) |>
    group_by(.rowid, field_stem) |>
    summarise(val = paste(unique(display), collapse = " | "), .groups = "drop")

  wide_summ <- pivot_wider(
    summ_tbl,
    id_cols     = .rowid,
    names_from  = field_stem,
    values_from = val,
    values_fill = ""
  ) |>
    right_join(idx_tbl, by = ".rowid") |>
    arrange(.rowid) |>
    select(-.rowid)

  # ensure stems exist even if entirely unchecked
  stems <- unique(rcdd_chk$field_stem[rcdd_chk$field_name %in% contrib])
  for (s in stems) if (!s %in% names(wide_summ)) wide_summ[[s]] <- ""

  # --- place new stem cols before first contributing ___ col ------------------
  # anchor = first contributing field_name per stem that exists in df
  orig_names <- names(df)
  pos_map    <- setNames(seq_along(orig_names), orig_names)

  first_by_stem <- rcdd_chk |>
    filter(field_name %in% contrib) |>
    group_by(field_stem) |>
    summarise(anchor = first(field_name), .groups = "drop")

  anchor_pos <- pos_map[first_by_stem$anchor]
  stem_pos   <- setNames(anchor_pos - 0.5, first_by_stem$field_stem)  # just before anchor

  # bind the new columns to df
  df2 <- bind_cols(df, wide_summ)

  # compute a sortable position for every column in df2
  col_pos <- pos_map[intersect(names(df2), names(pos_map))]          # original cols
  col_pos[names(stem_pos)] <- stem_pos[names(stem_pos)]              # assign stems

  # order columns by position (ties keep original order)
  final_order <- names(df2)[order(col_pos[match(names(df2), names(col_pos))], na.last = TRUE)]
  df2 <- df2[, final_order, drop = FALSE]

  # drop all raw ___ checkbox columns at once
  df2 <- df2 |> select(-any_of(contrib))

  df2
}



group_rc_chkbox_old1 <- function(df, params = NULL, label = TRUE, silent = TRUE) {

  # Load rcdd_exp as needed
  if (is.null(local_$rcdd_exp)) {
    params   <- if (is.null(local_$params)   & !exists('params'))   get_configset() else local_$params
    rcdd_exp <- if (is.null(local_$rcdd_exp)) get_rc_dictionary('esteylist', ddname = "rcdd_exp", explode = TRUE)
    local_$rcdd_exp <- rcdd_exp
  }
  
  rcdd_chk <- rcdd_exp |>
    filter(field_type == "checkbox", !stringr::str_detect(field_stem, "show")) |>
    distinct(field_name, field_stem, code, label) |>
    group_by(field_stem) |>
    mutate(ord = row_number()) |>
    ungroup()

  # which checkbox columns are actually present
  contrib <- intersect(rcdd_chk$field_name, names(df))
  if (!length(contrib)) {
    if (!silent) message("No checkbox columns detected in df.")
    return(df)
  }

  if (!silent) {
    cat(unique(rcdd_chk$field_stem[rcdd_chk$field_name %in% contrib]))
  }

  # --- long -> summarise -> wide ---------------------------------------------
  idx_tbl <- tibble::tibble(.rowid = seq_len(nrow(df)))

  long_tbl <- df |>
    mutate(.rowid = row_number()) |>
    pivot_longer(all_of(contrib), names_to = "field_name", values_to = "val") |>
    left_join(rcdd_chk, by = "field_name") |>
    # keep only "checked"
    filter(!(is.na(val) | val == 0 | val == "" | val == "Unchecked"))

  # collapse per row Ãƒâ€” stem, respecting dictionary order
  if (isTRUE(label)) {
    disp_fmt <- function(code, label) paste0(code, ", ", label)
  } else {
    disp_fmt <- function(code, label) as.character(code)
  }

  summ_tbl <- long_tbl |>
    arrange(.rowid, field_stem, ord) |>
    mutate(display = disp_fmt(code, label)) |>
    group_by(.rowid, field_stem) |>
    summarise(val = paste(unique(display), collapse = " | "), .groups = "drop")

  wide_summ <- pivot_wider(
    summ_tbl,
    id_cols     = .rowid,
    names_from  = field_stem,
    values_from = val,
    values_fill = ""
  ) |>
    right_join(idx_tbl, by = ".rowid") |>
    arrange(.rowid) |>
    select(-.rowid)

  # ensure stems exist even if entirely unchecked
  stems <- unique(rcdd_chk$field_stem[rcdd_chk$field_name %in% contrib])
  for (s in stems) if (!s %in% names(wide_summ)) wide_summ[[s]] <- ""

  # --- place new stem cols before first contributing ___ col ------------------
  # anchor = first contributing field_name per stem that exists in df
  orig_names <- names(df)
  pos_map    <- setNames(seq_along(orig_names), orig_names)

  first_by_stem <- rcdd_chk |>
    filter(field_name %in% contrib) |>
    group_by(field_stem) |>
    summarise(anchor = first(field_name), .groups = "drop")

  anchor_pos <- pos_map[first_by_stem$anchor]
  stem_pos   <- setNames(anchor_pos - 0.5, first_by_stem$field_stem)  # just before anchor

  # bind the new columns to df
  df2 <- bind_cols(df, wide_summ)

  # compute a sortable position for every column in df2
  col_pos <- pos_map[intersect(names(df2), names(pos_map))]          # original cols
  col_pos[names(stem_pos)] <- stem_pos[names(stem_pos)]              # assign stems

  # order columns by position (ties keep original order)
  final_order <- names(df2)[order(col_pos[match(names(df2), names(col_pos))], na.last = TRUE)]
  df2 <- df2[, final_order, drop = FALSE]

  # drop all raw ___ checkbox columns at once
  df2 <- df2 |> select(-any_of(contrib))

  df2
}


# # df <- induction_
# group_rc_chkbox_ <- function(df, params = NULL, label = TRUE, silent = TRUE) {
# 
#    if (FALSE) {
#     df     = induction_
#     params = get_configset()
#     label  = TRUE
#     silent = TRUE
#   }
# 
#   # Load rcdd_exp as needed
#   if (is.null(local_$rcdd_exp)) {
#     params   <- if (is.null(local_$params)   & !exists('params'))   get_configset() else local_$params
#     rcdd_exp <- if (is.null(local_$rcdd_exp)) get_rc_dictionary('esteylist', ddname = "rcdd_exp", explode = TRUE)
#   }
# 
#   rcdd_chk = rcdd_exp |> filter(field_type=='checkbox', !stringr::str_detect(field_stem, 'show')) |>
#     distinct(field_name, field_stem, code, label)
# 
#   # Identify checkbox stems
#   chkbx_cols <- rcdd_chk |> distinct(field_name, field_stem) |>
#     filter(field_name %in% names(df)) |>
#     distinct(field_stem) |>
#     pull(field_stem)
# 
#   if (!silent) {cat(chkbx_cols)}
#   # col <- chkbx_cols[1]
# 
#   for (col in chkbx_cols) {
#     col_df   <- rcdd_chk |> filter(field_stem==col)
#     first <- col_df[1,]$field_name
#     # Create new summarized column
#     df[[col]] <- apply(df[col_df$field_name], 1, function(x) {
#       checked <- which(!is.na(x) & x != 0 & x != "" & x != "Unchecked")
#       if (length(checked) == 0) return("")
#       paste(paste0(col_df$code[checked], ", ", col_df$label[checked]), collapse = " | ")
#     })
#     df <- df |> relocate(any_of(col), .before = first)
#   }
# 
#   df <- df |> select(-any_of(rcdd_chk$field_name))
# 
# }
# 

# params <- get_configset('karyo_lookup')
# put_rc_data(karyo, 'karyo_lookup')
# put_rc_data(clone, 'karyo_lookup')
put_rc_data <- function(df, paramsect = NULL, overwrite = "normal", delete_first = FALSE, chunk_size = 200, max_tries = 3) {

  if (is.null(paramsect)) {
    stop("An instantiated parameter object or section name (from the ini) is required")
  }

  params <- if (is.character(paramsect)) get_configset(paramsect) else paramsect

  section   <- params$section
  api_token <- params$encrypted_token
  api_url   <- params$url
  keyfields <- params$keyfields

  if (delete_first) {
    message("Deleting existing records...")
    del_rc_records(params, record_ids = unique(df[[keyfields]]))
  }

  # Convert all numeric fields to character and replace NAs with ""
  df <- df |>
    mutate(across(where(is.numeric) | where(is.integer), as.character)) |>
    mutate(across(everything(), ~replace_na(.x, "")))

  # Split the data frame into chunks
  chunks <- split(df, ceiling(seq_len(nrow(df)) / chunk_size))
  results <- list()

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    csv_string <- readr::format_csv(chunk)

    formData <- list(
      token             = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
      content           = 'record',
      action            = 'import',
      format            = 'csv',
      type              = 'flat',
      overwriteBehavior = overwrite,
      forceAutoNumber   = 'false',
      data              = csv_string,
      returnContent     = 'count',
      returnFormat      = 'json'
    )

    # Retry logic with exponential backoff
    response <- httr::RETRY(
      "POST",
      url    = api_url,
      body   = formData,
      encode = "form",
      times  = max_tries,
      pause_base = 1,
      pause_cap  = 10,
      pause_min  = 1
    )

    result <- httr::content(response)

    if (httr::status_code(response) != 200 || !is.null(result$error)) {
      warning(glue::glue("Chunk {i} failed: {result$message %||% result$error}"))
    } else {
      message(glue::glue("Ã¢Å“â€œ Uploaded chunk {i} ({nrow(chunk)} records)"))
    }

    results[[i]] <- result
  }

  return(results)
}



put_rc_data.1 <- function(df, paramsect = NULL, overwrite = "normal", delete_first = FALSE) {
  
  if (FALSE) {
    df     <- karyo
    params <- params
    delete_first = TRUE
    overwrite = "normal"
  }
  
  if (is.null(paramsect)) {
    stop('An instantiated parameter object or section name (from the ini) is required')
  } 
  params <- if (typeof(paramsect)=='character') {
    get_configset(paramsect)
  } else {
    paramsect
  }
  
  
  # if (FALSE) df <- clone.1
  # Check if delete_first is TRUE and call del_rc_records() to delete records first
  if (delete_first) {
    # Assume we're deleting by the record ID, which is 'karyocode' in your case
    del_rc_records(params, record_ids = unique(df$karyocode))
  }

  section   <- params$section
  api_token <- params$encrypted_token
  api_url   <- params$url
  keyfields <- params$keyfields 
  # Convert numeric or integer columns to character, then replace NAs with empty string
  df <- df |>
    mutate(across(where(is.numeric) | where(is.integer), as.character)) |>  # Convert to character
    mutate(across(everything(), ~replace(., is.na(.), ""))) 
  csv_string <- readr::format_csv(df)
  
  formData <- list(
    token             = safer::decrypt_string(api_token, decrypt_encryptkey(get_encryptkey())),
    content           = 'record',
    action            = 'import',
    format            = 'csv',
    type              = 'flat',
    overwriteBehavior = 'normal',
    forceAutoNumber   = 'false',
    data              = csv_string,
    returnContent     = 'count',
    returnFormat      = 'json'
  )

  # Make the POST request
  response <- httr::POST(api_url, body = formData, encode = "form")
  
  # Check if the response is successful
  result <- httr::content(response)
  print(result$error)
  if (httr::status_code(response) != 200) {
    cat("Error: ", result$message, "\n")
    return(NULL)
  }
  
  # Return the result of the upload
  return(result)
}

# helper funciton
map_race <- function(race) {
  racetest <- tolower(race)

  mapped_race <- case_when(
    racetest %in% c("white") ~ "White",
    racetest %in% c("asian") ~ "Asian",
    racetest %in% c("unavailable or unknown", "unable to collect") ~ "Unavailable or Unknown",
    racetest %in% c("black or african-american") ~ "Black or African American",
    racetest %in% c("american indian or native alaskan", "american indian", "alaska native") ~ "American Indian or Alaska Native",
    racetest %in% c("native hawaiian or pacific islander", "native hawaiian", "pacific islander") ~ "Native Hawaiian or Other Pacific Islander",
    racetest %in% c("declined to answer") ~ "Declined to Answer",
    racetest %in% c("another race") ~ "Another Race",
    TRUE ~ race
  )

  return(mapped_race)
}

# 
# map_race_ <- function(race) {
#   racetest = tolower(race)
#   mapped_race = case_when(
#     racetest %in% c("white") ~ "White",
#     racetest %in% c("asian") ~ "Asian",
#     racetest %in% c("unavailable or unknown", "unable to collect") ~ "Unavailable or Unknown",
#     racetest %in% c("black or african-american") ~ "Black or African American",
#     racetest %in% c("american indian or native alaskan", "american indian", "alaska native") ~ "American Indian or Alaska Native",
#     racetest %in% c("native hawaiian or pacific islander", "native hawaiian", "pacific islander") ~ "Native Hawaiian or Other Pacific Islander",
#     racetest %in% c("declined to answer") ~ "Declined to Answer",
#     racetest %in% c("another race") ~ "Another Race",
#     TRUE ~ race  # Keep the original if no match
#   )
#   return(mapped_race)
# }

# Create mapping of race to REDCap racecode columns
race_mapping <- c(
  "American Indian or Alaska Native" = "racecode___9",
  "Asian" = "racecode___10",
  "Black or African American" = "racecode___13",
  "White" = "racecode___30",
  "Native Hawaiian or Other Pacific Islander" = "racecode___33",
  "Unavailable or Unknown" = "racecode___26",
  "Declined to Answer" = "racecode___8",
  "Another Race" = "racecode___34"
)


ethnicity_mapping <- c(
  "Hispanic or Latino [28]"          = "ethnicitycode___28",
  "Not Hispanic or Latino [29]"      = "ethnicitycode___29",
  "Unavailable or Unknown [19]"      = "ethnicitycode___19",
  "Declined to Answer [27]"          = "ethnicitycode___27",
  "Unable to Collect [31]"           = "ethnicitycode___31"
)

map_ethnicity <- function(eth) {
  ethtest <- tolower(eth)

  mapped_ethnicity <- case_when(
    ethtest %in% c("hispanic or latino/a or latinx [28]")              ~ "Hispanic or Latino [28]",
    ethtest %in% c("non-hispanic or latino/a or latinx [29]")          ~ "Not Hispanic or Latino [29]",
    ethtest %in% c("unknown to patient [19]")                          ~ "Unavailable or Unknown [19]",
    ethtest %in% c("patient declined to respond [27]")                ~ "Declined to Answer [27]",
    ethtest %in% c("unable to collect [31]")                           ~ "Unable to Collect [31]",
    TRUE ~ eth
  )

  return(mapped_ethnicity)
}

# ethnicity_mapping_ <- c(
#   "Hispanic or Latino [28]" = "ethnicitycode___28",
#   "Not Hispanic or Latino [29]" = "ethnicitycode___29",
#   "Unavailable or Unknown [19]" = "ethnicitycode___19",
#   "Declined to Answer [27]" = "ethnicitycode___27",
#   "Unable to Collect [31]" = "ethnicitycode___31"
# )
# 
# map_ethnicity <- function(eth) {
#   ethtest <- tolower(eth)  # Convert input to lowercase for case-insensitive matching
#   
#   mapped_ethnicity <- case_when(
#     ethtest %in% c("hispanic or latino/a or latinx [28]") ~ "Hispanic or Latino [28]",
#     ethtest %in% c("non-hispanic or latino/a or latinx [29]") ~ "Not Hispanic or Latino [29]",
#     ethtest %in% c("unknown to patient [19]") ~ "Unavailable or Unknown [19]",
#     ethtest %in% c("patient declined to respond [27]") ~ "Declined to Answer [27]",
#     ethtest %in% c("unable to collect [31]") ~ "Unable to Collect [31]",
#     TRUE ~ eth  # Keep the original value if no match
#   )
#   
#   return(mapped_ethnicity)
# }




# Convert a data frame to REDCap-ready strings:
# - Dates -> "MM/DD/YYYY" (configurable)
# - POSIXt -> "MM/DD/YYYY HH:MM" (configurable)
# - Logical -> "1"/"0" (or "Yes"/"No", "TRUE"/"FALSE")
# - Numeric/Integer/Character/Factor -> character; NAs -> ""
# - Everything ends up character so CSV import wonÃ¢â‚¬â„¢t choke on NA

# Helper: coerce blanks -> NA, then to Date (supports "YYYY-MM-DD" and "MM/DD/YYYY")
.norm_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- na_if(stringr::str_squish(as.character(x)), "")
  d1 <- suppressWarnings(as.Date(x))                    # e.g. 2024-07-01
  d2 <- suppressWarnings(as.Date(x, format = "%m/%d/%Y")) # e.g. 07/01/2024
  coalesce(d1, d2)
}

# Helper: TRUE when values differ OR only one is present
.disagree <- function(a, b) {
  (is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
}


prep_rc_df <- function(df,
                         cols = tidyselect::everything(),
                         date_fmt = "%m/%d/%Y",
                         datetime_fmt = "%m/%d/%Y %H:%M",
                         logical_fmt = c("1/0", "Yes/No", "True/False")) {
  logical_fmt <- match.arg(logical_fmt)
  map_logical <- switch(
    logical_fmt,
    "1/0"        = c(`TRUE` = "1",  `FALSE` = "0"),
    "Yes/No"     = c(`TRUE` = "Yes",`FALSE` = "No"),
    "True/False" = c(`TRUE` = "TRUE",`FALSE` = "FALSE")
  )

  coerce_one <- function(x) {
    # Date
    if (inherits(x, "Date"))   return(ifelse(is.na(x), "", format(x, date_fmt)))
    # POSIXt (POSIXct / POSIXlt)
    if (inherits(x, "POSIXt")) return(ifelse(is.na(x), "", format(x, datetime_fmt)))
    # Logical
    if (is.logical(x))         return(ifelse(is.na(x), "", ifelse(x, map_logical[["TRUE"]], map_logical[["FALSE"]])))
    # Factor
    if (is.factor(x))          x <- as.character(x)
    # Numeric / Integer
    if (is.numeric(x))         return(ifelse(is.na(x), "", as.character(x)))
    # Character
    if (is.character(x))       return(replace_na(x, ""))
    # Fallback: make it a string and blank NAs
    as.character(x) |> replace_na("")
  }

  df |>
    mutate(across({{ cols }}, coerce_one))
}
