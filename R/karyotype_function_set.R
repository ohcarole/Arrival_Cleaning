# Standardizes input to have a 'karyo' column; optionally keeps all original columns.
# stru = df
classify_prep <- function(stru = NULL, keep_cols = NULL, karyo_col = NULL) {
  
  if (FALSE) {
    stru=df_orig
    keep_cols=NULL
    karyo_col=NULL
  }
  
  is_df <- is.data.frame(stru)
  if (is.null(keep_cols)) keep_cols <- is_df
  if (!is_df) keep_cols <- FALSE  # vectors never keep extra fields

  # helper: pick source column for karyotype text in a data frame
  pick_karyo_col <- function(d) {
    if (!is.null(karyo_col) && karyo_col %in% names(d)) return(karyo_col)
    if ("karyo" %in% names(d)) return("karyo")
    if (ncol(d) == 1) return(names(d)[1])
    cc <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
    if (length(cc) == 0) stop("No character/factor column found in the data frame.")
    # if (isTRUE(verbose)) message("Using first character/factor column as 'karyo': ", cc[[1]]) 
    cc[[1]]
  }

  # 0-row DF early return
  if (is_df && nrow(stru) == 0) {
    if (keep_cols) {
      tmp_df0 <- stru
      kcol <- pick_karyo_col(tmp_df0)
      if (!"karyo" %in% names(tmp_df0)) tmp_df0[["karyo"]] <- as.character(tmp_df0[[kcol]])
      tmp_df0 <- mutate(tmp_df0,
                           karyo_clean = character(0),
                           karyo_hash  = character(0))
      return(tmp_df0)
    } else {
      return(tibble(karyo = character(0),
                            karyo_clean = character(0),
                            karyo_hash  = character(0)))
    }
  }

  # Normalize input type as tibble's regardless of the original structures format (df/tibble/vector).
  if (is.character(stru)) {
    tmp_df <- tibble(karyo = stru)
  } else if (is_df) {
    kcol <- pick_karyo_col(stru)
    if (keep_cols) {
      tmp_df <- stru
      # add/overwrite 'karyo' but do NOT rename the source column
      if (!"karyo" %in% names(tmp_df)) tmp_df[["karyo"]] <- as.character(tmp_df[[kcol]]) else tmp_df[["karyo"]] <- as.character(tmp_df[["karyo"]])
    } else {
      tmp_df <- tibble(karyo = as.character(stru[[kcol]]))
    }
  } else {
    stop("Input must be a character vector or a data frame with a character/factor column.")
  }

  # Standardize karyotypes
  std <- tmp_df |>
    mutate(
      karyo = karyo |>
        str_replace("^\\((\\d+)/(\\d+)", "(\\1 of \\2") |>
        str_replace("^(\\d+)/(\\d+)", "\\1 of \\2") |>
        str_replace_all("\\[(\\d{1,2})/(\\d{1,2})\\]", "[\\1_SLASH_\\2]") |>
        str_replace_all("\\[(cp\\d{1,2})/(\\d{1,2})\\]", "[\\1_SLASH_\\2]") |>
        str_replace_all("\\\\R\\\\", "-") |>
        str_replace_all("~", "-") |>
        str_replace(",\\s*\\[", "[") |>
        str_replace("^N/A$", "NA") |>
        str_remove(",\\s*$") |>
        str_replace_all("\\[cp?(\\d{1,2})\\[", "[cp\\1]") |>
        str_replace_all("\\[(cp\\d{1,2})[^\\]]*$", "[\\1]") |>
        str_replace_all("\\[(\\d{1,2})[^\\]]*$", "[\\1]") |>
        str_replace_all("\\[\\[", "[") |>
        str_replace_all("\\]\\]", "]") |>
        str_replace_all("\\p{Zs}+", " ") |>                  # normalize Unicode spaces
        str_replace_all("\\s{2,}", " ") |>                   # collapse multiple spaces
        str_replace_all("(?<!\\w)\\s+|\\s+(?!\\w)", "") |>   # strip spaces around punctuation
        str_trim()
    )

  # Safe hasher that yields NA for NA inputs
  hash_or_na <- function(x) ifelse(is.na(x), NA_character_, digest::digest(x, algo = "xxhash64"))

  if (keep_cols) {
    out <- std |>
      mutate(
        karyo_clean = normalize_col(karyo, scheme = "karyo")$norm,
        karyo_hash  = vapply(karyo_clean, hash_or_na, character(1))
      )
  } else {
  # Vector input or keep_cols=FALSE: drop extras and distinct
  out <- std |>
    select(karyo) |>
    distinct() |>
    mutate( karyo_clean = normalize_col(karyo, scheme = "karyo")$norm,
            karyo_hash  = vapply(karyo_clean, hash_or_na, character(1)),
            karyo_hash  = unname(as.character(karyo_hash)))    
  }

  return(out |> mutate(karyo_hash = unname(as.character(karyo_hash))))
}

get_karyo_hist <- function(df=NULL, mode='test') {

  if (!is.data.frame(df)) stop('Dataframe expected')
  
  # always create a typed empty df for safe fallbacks
  empty_df <- data.frame(
    karyo     = NA_character_,
    karyocode = 10000L
  )[0, ]

  # Try to load history; on error fall back to empty_df
  karyo_tmp    <- tryCatch(sql_get_table("LOOKUP.karyo"), error = function(e) empty_df) |>
    mutate(karyocode = as.integer(karyocode))
  clone_tmp    <- tryCatch(sql_get_table("LOOKUP.clone"), error = function(e) empty_df) |>
    mutate(karyocode = as.integer(karyocode), cloneid   = as.integer(cloneid))
  karyo_rc_tmp <- tryCatch(sql_get_table("LOOKUP.karyo_redcap"), error = function(e) empty_df) |>
    mutate(karyocode = as.integer(karyocode))
  clone_rc_tmp <- tryCatch(sql_get_table("LOOKUP.clone_redcap"), error = function(e) empty_df) |>
    mutate(karyocode = as.integer(karyocode), cloneid   = as.integer(cloneid))

  # Apply karyotype history from SQL/RC
  karyo_sql    <- inner_join(df |> select(karyo_hash), karyo_tmp,    by = "karyo_hash")
  karyo_redcap <- inner_join(df |> select(karyo_hash), karyo_rc_tmp, by = "karyo_hash")

  # Clone history (requires karyocode for redcap)
  clone_sql    <- inner_join(karyo_sql |> distinct(karyo_hash), 
                             clone_tmp, 
                             by = "karyo_hash")
  clone_redcap <- inner_join(karyo_sql |> distinct(karyocode), 
                             clone_rc_tmp, 
                             by = "karyocode")

  # remove "name" from hash_code
  clone_sql <- clone_sql |> mutate(karyo_hash = unname(as.character(karyo_hash)))
  karyo_sql <- karyo_sql |> mutate(karyo_hash = unname(as.character(karyo_hash)))
  karyo_redcap <- karyo_sql |> mutate(karyo_hash = unname(as.character(karyo_hash)))

  hist <- list(
    karyo_sql    = karyo_sql,
    karyo_redcap = karyo_redcap,
    # karyo_hist   = karyo_hst,
    clone_sql    = clone_sql,
    clone_redcap = clone_redcap
    # clone_hist   = clone_hst
  )

  return(hist)
}


normalize_keys <- function(x) {
  x %>%
    mutate(
      karyo_hash = unname(as.character(karyo_hash)),  # drop name attribute
      cloneid    = as.integer(cloneid)
    )
}

# classify_all(allkaryo)
#' Title
#'
#' @param df 
#' @param history_mode 
#' @param maxcode 
#' @param test 
#'
#' @returns
#' @export
#'
#' @examples
#' 
#'
if (FALSE) {
  df = allkaryo_df
  history_mode = "rebuild_hist"
  maxcode =  10000
  test=TRUE
}
classify_all <- function(df, history_mode = "use_hist", maxcode =  10000, test=FALSE) {
  
  .pkg_env$parent_env <- environment()
  
  # contained function to print out processing so that you can see progress
  classify_progress <- function(df=NULL, cols=NULL, nm=NULL, env=NULL) {
    if (is.null(df) | is.null(cols) | is.null(nm)) stop('classify_progress parameters were incorrect')
    cols <- unique(c(cols, names(df)))
    df  <- df |> select(any_of(cols))
    env <- if (is.null(env)) .pkg_env$parent_env else env
    assign(nm, df, envir = env)
    pieces <- c(
      if (has_name(df, "karyo"))     sprintf("karyo = %d",     n_distinct(df$karyo))     else NULL,
      if (has_name(df, "karyocode")) sprintf("karyocode = %d", n_distinct(df$karyocode)) else NULL,
      if (has_name(df, "clone"))     sprintf("clone = %d",     n_distinct(df$clone))     else NULL
    )
    msg <- paste(pieces, collapse = " ")
    cat(nm, paste(pieces, collapse = "\t"), '\n')  
    return(df)
  }  

  # mock parameters
  if (test) {
    test=TRUE
    rm(list = ls(pattern = "^clone|^complete|tmp$|rslt$|^df|xx|yy|zz"))
    rm(list = ls(pattern = "^process|^empty|^flags|^patterns|_sql$"))
    rm(list = ls(pattern = "_redcap$|^stru|^out|^std|^tmp|^add_on$|^hist$"))
    rm(list = ls(pattern = "^x$|^y$"))
    
    df =  allkaryo_df # allkaryo
    history_mode = mode # "rebuild_hist" # default
    maxcode = 10000
  }
  
  # --------------------------------------------------------------------------------------
  # SETTING UP RETENSION
  # --------------------------------------------------------------------------------------
  # This first section just loads history from previous run if we are in that mode
  # and a bunch of other set up.  Skip down to the meat of the program classify_karyo

  # make sure a mode is set
  if (!history_mode %in% c("use_hist", "drop_hist", "rebuild_hist")) {
    stop("Invalid history_mode. Must be one of: 'use_hist', 'drop_hist', 'rebuild_hist'")
  }

  # Standardizes input to a tibble with with 3 columns distinct columns for
  # processing: karyo, karyo_clean, karyo_hash and retains original columns for
  # error reporting later
  df_orig <- classify_prep(df, keep_cols=TRUE)
  df <- df_orig |> select(karyo, karyo_clean, karyo_hash) |> distinct() ; nrow(df)

  # --------------------------------------------------------------------------------------
  # Detail already available from SQL Server -- in use_hist mode
  # --------------------------------------------------------------------------------------
  
  # noticed that the clone_sql and clone_redcap have a different number of
  # columns, and it isn't because of repeat instruments
  hist <- if (history_mode == 'use_hist') get_karyo_hist(df, mode='test') else list()
  # redcap_maxcodes <- as.integer(get_rc_fields('karyo_lookup', 'karyocode'))
  # maxcode <- if (length(redcap_maxcodes)==0) {
  #   maxcode 
  # } else if (is.na(redcap_maxcodes)) {
  #   maxcode
  # } else {
  #   max(redcap_maxcodes)
  # }
  # maxcode <- if (is.na(maxcode)) 10000 else maxcode

  # find codes in redcap
  tmp_codes <- get_rc_fields("karyo_lookup", "karyocode")
  
  # force to a plain numeric vector (or length-0)
  redcap_maxcodes <- as.integer(unlist(tmp_codes, use.names = FALSE))
  
  maxcode <- if (history_mode == 'rebuild_hist') {
                10000  
              } else if (length(redcap_maxcodes) == 0) {
                maxcode
              } else if (all(is.na(redcap_maxcodes))) {
                maxcode
              } else {
                max(redcap_maxcodes, na.rm = TRUE)
              }
  
  maxcode <- if (is.na(maxcode)) 10000L else maxcode

  # create process_df, karyotypes that are not in the hist[['karyo_sql']] table
  suppressWarnings(rm(process_df))
  if (history_mode == 'use_hist') {
    process_df  <- left_join(df, 
                             karyo_sql <- hist[['karyo_sql']] |> distinct(karyo_hash, clones), 
                             by=c('karyo_hash')) |>
        filter(is.na(clones)) |>
        mutate(karyocode = seq(from = maxcode + 1, length.out = n())) |>
        select(karyocode, everything(),-clones)
  } else {
    if ('karyocode' %in% names(df)) {
      process_df <- df
    } else {
      process_df <- df |> mutate(karyocode = seq(from = maxcode + 1, length.out = n()))
    }
  }
  tmp <- process_df
      

  # --------------------------------------------------------------------------------------
  # Early Return -- Check to see if all rows were retrieved from history
  # --------------------------------------------------------------------------------------
  if (nrow(process_df)==0) {
    cat("\n\nNo additional karyotypes detailed, using stored clone information")
    return(hist)
  }
  
  # Columns of interest
  # clone.cols <- names(hist[['clone_sql']]) # columns from before
  # karyo.cols <- names(hist[['karoy_sql']]) # columns from before
  cols <- c('karyocode', 'redcap_repeat_instrument', 'redcap_repeat_instance',
            'karyo', 'cloneid', 'clone', 'clone_order',
            names(hist[['clone_sql']]), names(hist[['karoy_sql']]), 
            'karyo_clean', 'karyo_hash')
  
  # Set up for multi processing
  future::plan(multisession) 


  # starting with
  classify_progress(process_df, names(process_df), 'process_df', .pkg_env$parent_env)
  
  # --------------------------------------------------------------------------------------
  # classify_karyo
  # --------------------------------------------------------------------------------------
  # Only relevant if re-running
  rm(list = ls(pattern = "^clone_detail"))
  process_df <- tmp
  
  
  # debugonce(classify_karyo)
  # --------------------------------------------------------------------------------------
  # Part 1 Process karyotype into all of the contained clones and properties 
  # --------------------------------------------------------------------------------------
  # i.e. donor, chromosomes, cells, clonal abormalities
  # --------------------------------------------------------------------------------------
  # process_df <- classify_karyo(process_df, df_orig, chunk_size = 100, chunks = NULL) |>
  #   select(any_of(cols), everything(), -ends_with(c('.x', '.y'))) |>
  #   classify_progress(cols, 'clone_detail.1')
  process_df <- classify_karyo(process_df, df_orig, chunk_size = 100, chunks = NULL) |>
    select(any_of(cols), everything(), -ends_with(c('.x', '.y')))
  cols <- names(process_df)

  # --------------------------------------------------------------------------------------
  # Part 2 Process clones assessing abnormalities and clonality
  # process_df <- tmp.2
  # --------------------------------------------------------------------------------------
  tmp.2 <- process_df
  process_df <- classify_abnormal(process_df) |> 
    select(any_of(cols), everything(), -ends_with(c('.x', '.y'))) |>
    classify_progress(cols, 'clone_detail.2')
  cols <- names(process_df)
  
  # --------------------------------------------------------------------------------------
  # Karyo level detail including monosomial_karyo, complex_karyo
  # --------------------------------------------------------------------------------------
  tmp.3 <- process_df
  process_df <- classify_clones(process_df) |> 
    select(any_of(cols), everything(), -ends_with(c('.x', '.y'))) |>
    classify_progress(cols, 'clone_detail.3') 
  cols <- names(process_df)
  
  # --------------------------------------------------------------------------------------
  # Cleaning up presentation
  # --------------------------------------------------------------------------------------
  tmp.4 <- process_df
  process_df <- process_df |>
    arrange(karyo, cloneid) |>
    mutate(clone_order = paste(karyocode, cloneid, sep="_")) |> 
    select(clone_order, any_of(cols), everything()) |>
    classify_progress(cols, 'clone_detail.4')
  cols <- names(process_df)
  
  # combine the just processed with the historical clone_detail
  clone_detail <- classify_progress(process_df, cols, 'clone_detail')

  # Summarize and copy detail to REDCap
  rc_rslt <- get_summary(clone_detail)
  
  # --------------------------------------------------------------------------------------
  # At this point all of the clone data for records not previously processed are in
  # rc_rslt[['clone']] and rc_rslt[['karyo']]
  # --------------------------------------------------------------------------------------

  # --------------------------------------------------------------------------------------
  clone.rc <- rc_rslt[['clone']]
  karyo.rc <- rc_rslt[['karyo']]
  rc_rslt[['detail']] <- clone_detail

  # get_rc_dictionary('karyo_lookup', ddname='lkup_dict')
  # 
  # # qc'ing columns:  Are there new columns in this run?  Are there columns missing we had before?
  # if(TRUE) {
  #   dict_df <- lkup_dict |> filter(form_name=='karyo' & field_type!='descriptive')
  #   x <- karyo.new
  #   y <- dict_df$field_name
  #   cols_only_in_x <- setdiff(names(x), y)   # only in this data
  #   cols_only_in_y <- setdiff(y, names(x))   # only in redcap
  #   common_cols    <- intersect(names(x), y) # columns shared with redcap
  #   symmetric_diff <- setdiff(union(names(x), y), common_cols) # columns not shared     
  # }
  # 
  # # karyo detail for redcap
  # karyo_redcap <- karyo.new  |>
  #   select(karyocode, any_of(common_cols)) |>
  #   distinct()  
  # 
  # if(TRUE) {
  #   dict_df <- lkup_dict |> filter(form_name=='clone' & field_type!='descriptive')
  #   x <- clone.new
  #   y <- dict_df$field_name
  #   cols_only_in_x <- setdiff(names(x), y)   # only in this data
  #   cols_only_in_y <- setdiff(y, names(x))   # only in redcap
  #   common_cols    <- intersect(names(x), y) # columns shared with redcap
  #   symmetric_diff <- setdiff(union(names(x), y), common_cols) # columns not shared      
  # }
  # 
  # # clone detail for redcap
  # clone_redcap <- clone.new  |>
  #   select(karyocode, starts_with('redcap'), any_of(common_cols)) |>
  #   distinct()  
  # 
  # browser()  
  
  # store if running in test mode
  if (test==TRUE) {
    .pkg_env$rc_rslt <- rc_rslt
    .pkg_env$clone_detail <- clone_detail
  }

 # clone_sql.1    <- rc_rslt$clone  |> 
 #    select(-any_of(c('parse_failed', 'karyo_strip', 'clone_strip', 'karyo_clean'))) |>
 #    select(starts_with(c('karyo','clone')), everything()) |>
 #    distinct()
 #  clone_redcap <- clone_sql
 #  
 #  karyo_sql.1    <- rc_rslt$karyo |> 
 #    select(-any_of(c('parse_failed', 'karyo_strip', 'clone_strip', 'karyo_clean.y'))) |> 
 #    select(starts_with('karyo'), everything()) |>
 #    distinct()
 #  
 #  karyo_redcap <- karyo_sql
 # 
 #  setdiff(names(zz.1), names(karyo_sql)) 
 #  setdiff(names(karyo_sql), names(zz.1)) 

  # before returning bind to values prior to processing
  # karyo_joined <- bind_rows(karyo_hst, karyo_sql |> mutate(karyocode=as.integer(karyocode))) |> distinct()
  
  
  +# Return information
  # rslt <- list( karyo_sql = karyo_sql
  #             , clone_sql = clone_sql
  #             , karyo_redcap = karyo_redcap
  #             , clone_redcap = clone_redcap)
  return(rc_rslt)    
}

classify_karyo <- function(df, df_orig, chunk_size = 100, use_get_sl_df = TRUE,
                           chunks = NULL, verbose = FALSE, parallel = TRUE,
                           write_log = TRUE,
                           log_table = "LOOKUP.karyo_parse_issues") {
  
  # mock parameters
  if (FALSE) {
    df = process_df
    df_orig = df
    chunk_size = 100
    use_get_sl_df = TRUE
    chunks = NULL
    verbose = FALSE
    parallel = TRUE
    write_log = TRUE
    log_table = "LOOKUP.karyo_parse_issues"    
  }
  
  # Preconditions
  stopifnot(is.data.frame(df), "karyo" %in% names(df), "karyo_hash" %in% names(df))
  stopifnot(is.data.frame(df_orig), "karyo_hash" %in% names(df_orig))

  if (nrow(df) == 0L) return(tibble())

  # Unique worklist by karyo_hash
  df <- df |> distinct(karyo_hash, .keep_all = TRUE)

  # Stable run id (one place!)
  run_id <- .pkg_env$run_id

  # Chunking
  n <- nrow(df)
  chunk_size <- max(1L, min(chunk_size, n))
  idx_all  <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  n_chunks <- length(idx_all)
  idx_sel  <- if (is.null(chunks)) seq_len(n_chunks) else sort(unique(pmin(pmax(1L, as.integer(chunks)), n_chunks)))
  idx <- unlist(idx_all[idx_sel], use.names = FALSE)

  worklist <- df[idx, , drop = FALSE] |> mutate(row_index = idx)
  rows_list <- split(worklist, seq_len(nrow(worklist)))

  # Mapper
  mapper <- if (parallel) furrr::future_map else purrr::map
  parts <- mapper(
    rows_list,
    parse_karyo_with_log,
    df_orig = df_orig,
    use_get_sl_df = use_get_sl_df,
    verbose = verbose,
    .options = if (parallel) furrr::furrr_options(seed = TRUE) else NULL
  )

  # Robust binds (expect each part to be list(rows=<df>, log=<df or NULL>))
  safe_rows <- parts |>
    purrr::map("rows") |>
    purrr::discard(is.null) |>
    purrr::discard(~ nrow(.x) == 0)

  results_df <- if (length(safe_rows)) bind_rows(safe_rows) else {
    # Your zero-row schema (as provided)
    structure(list(
      karyoroot = character(0), cloneid = integer(0), parentid = integer(0),
      donor = integer(0), chr_rng = character(0), chr = integer(0),
      sex_chr = character(0), ploidy = character(0),
      cells_observed = integer(0), cells_with_signature = integer(0),
      clonal = integer(0), normal_section = integer(0),
      sideline_text = character(0), clone = character(0),
      clone_inheritance = character(0), abn = character(0),
      abn_inheritance = character(0), clone_abn_list = character(0),
      clone_abn_cnt = numeric(0), parse_failed = logical(0),
      karyo = character(0), karyo_clean = character(0),
      karyo_hash = structure(character(0), names = character(0)),
      karyocode = numeric(0), row_index = integer(0)
    ), row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"))
  }

  safe_logs <- parts |>
    purrr::map("log") |>
    purrr::discard(is.null) |>
    purrr::discard(~ nrow(.x) == 0)

  issues_log <- if (length(safe_logs)) bind_rows(safe_logs) else tibble::tibble()

  # Single, authoritative logging pass
  if (write_log) {
    if (nrow(issues_log)) {
      failed_hashes <- issues_log |> distinct(karyo_hash)

      ids_from_orig <- df_orig |>
        semi_join(failed_hashes, by = "karyo_hash") |>
        select(any_of(c("recordid", "karyodate", "karyo", "karyo_hash", "karyo_clean"))) |>
        distinct()

      issues_with_ids <- ids_from_orig |>
        inner_join(issues_log, by = "karyo_hash") |>
        mutate(run_id = run_id)

      # Optional de-dupe
      dedupe_cols <- intersect(c("recordid", "karyo_hash", "status", "message"), names(issues_with_ids))
      if (length(dedupe_cols)) {
        issues_with_ids <- issues_with_ids |>
          group_by(across(all_of(dedupe_cols))) |>
          slice(1) |>
          ungroup()
      }
    } else {
      # No issues: create a structurally correct, zero-row tibble
      issues_with_ids <- tibble(
        recordid   = character(0),
        karyodate  = as.Date(character(0)),
        karyo      = character(0),
        karyo_hash = character(0),
        karyo_clean= character(0),
        status     = character(0),
        message    = character(0),
        time       = character(0),
        run_id     = character(0)
      )
    }

    # One write per run. Using overwrite=TRUE keeps the table reflecting this run only.
    if (nrow(issues_with_ids)==0) {
      # cmd = "DELETE FROM HEMEDB.LOOKUP.karyo_parse_issues ;"
      cmd <- sprintf("IF OBJECT_ID(N'%s', N'U') IS NOT NULL TRUNCATE TABLE %s;",log_table, log_table)
      sql_run_cmd(section='esteylist', cmd=cmd)
    } else {
      sql_insert_df(issues_with_ids, log_table, overwrite = TRUE)
    }
  }

  results_df
  
}

# classify_abnormal(df=clone_detail.1)

classify_abnormal <- function(df) {
  df <- df |> mutate(abn_temp = coalesce(abn, ""))

  # 1) All active abnormalities → one column per CSV line
  patterns_all <- abn_patterns('all', by = 'field_name')   # every active row
  # 2) Subset for recurrent-only counts
  patterns_recurrent <- abn_patterns('recurrent', by = 'field_name')
  recurrent_names <- names(patterns_recurrent)[!str_detect(names(patterns_recurrent), "^(tri|mon)")]

  # Build ALL abnormality flags
  rslt <- df |>
    bind_cols(
      map_dfc(patterns_all, ~ as.integer(str_detect(df$abn_temp, .x))) |>
      rename_with(~ names(patterns_all))
    )

  # metadata to support chr17 / 17p summaries
  catalog    <- abn_metadata()
  cat_active <- catalog %>% filter(active)
  chr17_fields <- cat_active %>% filter(chr == "17") %>% pull(field_name)
  chr17p_loss_fields <- cat_active %>%
    filter(str_detect(class %||% "", "(^|\\|)17p($|\\|)")) %>%
    pull(field_name)

  chr17_cols       <- intersect(chr17_fields,      names(rslt))
  chr17p_loss_cols <- intersect(chr17p_loss_fields, names(rslt))

  rslt <- rslt |>
    mutate(
      mono_no_sex            = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "mono", FALSE)), collapse = ", ")),
      tri_no_sex             = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "tri",  FALSE)), collapse = ", ")),
      monosomies_with_repeats = map_chr(abn_temp, ~ paste0(extract_somies(.x, "mono"), collapse = ", ")),
      trisomies_with_repeats  = map_chr(abn_temp, ~ paste0(extract_somies(.x, "tri"),  collapse = ", ")),
      monosomies             = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "mono")), collapse = ", ")),
      trisomies              = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "tri")),  collapse = ", ")),
      # Recurrent count uses only recurrent patterns, not every CSV row
      recurrent_cnt = if (length(recurrent_names)) {
        rowSums(across(all_of(recurrent_names)) > 0, na.rm = TRUE)
      } else 0L,
      has_chr17_abn = if (length(chr17_cols)) {
        as.integer(rowSums(across(all_of(chr17_cols)) > 0, na.rm = TRUE) > 0)
      } else 0L,
      has_17p_loss_like = if (length(chr17p_loss_cols)) {
        as.integer(rowSums(across(all_of(chr17p_loss_cols)) > 0, na.rm = TRUE) > 0)
      } else 0L
    ) |>
    select(-abn_temp)

  rslt
}



classify_abnormal_ <- function(df) {
  
  if (FALSE) {
    df <- clone_detail.1 |> mutate(abn_temp = coalesce(abn, ""))
  }

  # Prefer blank to NA
  df <- df |> mutate(abn_temp = coalesce(abn, ""))
  
  # Recurrent AML abnormalities
  # revisit abn_patterns() to make sure that only those that count during code that excludes recurrent
  # patterns <- abn_patterns('recurrent')
  patterns <- abn_patterns('recurrent', by='field_name')
  recurrent_cols  <- abn_names('recurrent', by="clone_col")
  recurrent_names <- names(patterns)[!str_detect(names(patterns), "^(tri|mon)")]


  # Guard: ensure all pattern names are non-empty (fails early if not)
  stopifnot(all(nzchar(names(patterns))))
  
  # NEW 2-12-26
  # Get metadata to identify 17 / 17p abnormalities
    catalog    <- abn_metadata()
    cat_active <- catalog %>% filter(active)
  
    chr17_fields <- cat_active %>%
      filter(chr == "17") %>%
      pull(field_name)
  
    chr17p_loss_fields <- cat_active %>%
      filter(str_detect(class %||% "", "(^|\\|)17p($|\\|)")) %>%
      pull(field_name)
  
   # END NEW 2-12-26

  
  # Build named flag columns in a single pass (no rename_with needed)
  flags_df <- purrr::imap_dfc(
    patterns,
    ~ tibble(!!.y := as.integer(str_detect(df$abn_temp, .x)))
  )
  
  rslt <- df |> 
    bind_cols(
      map_dfc(patterns, ~ as.integer(str_detect(df$abn_temp, .x))) |>
        rename_with(~ names(patterns))
    )

  # ---- new clone-level chr17 detail ----
  chr17_cols <- intersect(chr17_fields, names(rslt))
  chr17p_loss_cols <- intersect(chr17p_loss_fields, names(rslt))
  # ---- end new clone-level chr17 detail ----
  
    
  rslt <- rslt |>
    mutate(
      # Summaries by row (clone) using abn_temp to avoid NA issues
      mono_no_sex = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "mono", FALSE)), collapse = ", ")),
      tri_no_sex  = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "tri",  FALSE)), collapse = ", ")),
      monosomies_with_repeats = map_chr(abn_temp, ~ paste0(extract_somies(.x, "mono"), collapse = ", ")),
      trisomies_with_repeats  = map_chr(abn_temp, ~ paste0(extract_somies(.x, "tri"),  collapse = ", ")),    
      monosomies  = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "mono")), collapse = ", ")),
      trisomies   = map_chr(abn_temp, ~ paste0(unique(extract_somies(.x, "tri")),  collapse = ", ")),      

      # Recurrent abnormality counts all in the names that are non-zero and not NA
      recurrent_cnt = rowSums(across(all_of(recurrent_names)) > 0, na.rm = TRUE),
      
      # ---- new clone-level chr17 detail ----
      has_chr17_abn = if (length(chr17_cols)) {
        as.integer(rowSums(across(all_of(chr17_cols)) > 0, na.rm = TRUE) > 0)
      } else 0L,
  
      has_17p_loss_like = if (length(chr17p_loss_cols)) {
        as.integer(rowSums(across(all_of(chr17p_loss_cols)) > 0, na.rm = TRUE) > 0)
      } else 0L
      # ---- END new clone-level chr17 detail ----

    ) |>
    select(-abn_temp)

    
  # xx <- rslt |> select(clone, starts_with(c('mk', 'mono')), everything())
  # xx <- rslt |> select(ends_with(c('17', '17p', '17q', 'ch17', 'any')), clone, everything())
  
 
  # cols_to_replace <- setdiff(names(rslt), "parentid")
  # for (col in cols_to_replace) {
  #   rslt[[col]][is.na(rslt[[col]])] <- ""
  # }
  
  return(rslt)
}

# df <- classify_abnormal(df)

# df_clones <- clone_detail.2

safe_split <- function(x) {
  if (is.na(x) || x == "") character(0) else str_split(x, ",\\s*")[[1]]
}

classify_clones <- function(df_clones) {
  
  if(FALSE) {
    df_clones = clone_detail.2
  }
   
  
  # Recurrent AML abnormalities
  structural_regex <- get_struct_regex()
  patterns         <- abn_patterns()
  recurrent_names  <- names(patterns)[!str_detect(names(patterns), "^(tri|mon)")]  

  # Capture all clonal clones up front
  df_clonal_all <- df_clones |> filter(clonal == 1)

  # Expand structural abnormalities and build abn_category
  # collapse_values(x, delim_pattern = ",\\s*", dedup = FALSE, sort = c("none", "lexical", "numeric"))  
  
  df_expanded <- df_clones |>
    select(  -any_of(starts_with('stru')),
             -any_of(c('abn_type', 'abn_chromosome')),
             -any_of(ends_with(c('category_list', 'abn_cnt', 'clone_abn')))) |>
    # filter(clonal == 1) |>    
    mutate(
      structural_vec = abn_inheritance |>
        str_extract_all(structural_regex) |>
        map(str_trim) # trimmed values will STILL look padded in the RStudio viewer
    ) |>
    rowwise() |>
    mutate( structural_list = collapse_values(structural_vec, dedup=TRUE)) |>
    ungroup() |>
    unnest_longer(
      structural_vec,
      values_to  = "structural_abn",
      keep_empty = TRUE
    ) |>
    mutate(
      row = row_number(),
      abn_type = structural_abn |>
        str_extract("^[^\\(]+") |>
        str_to_lower() |>
        str_trim(),
    
      # Extract parts inside parentheses
      parens = map(structural_abn, ~ str_extract_all(.x, "(?<=\\()[^\\)]+")[[1]]),
    
      # Clean and normalize the parts
      abn_category = map2_chr(abn_type, parens, ~ {
        type  <- .x
        parts <- .y
    
        if (is.na(type) || type == "" || length(parts) == 0) return(NA_character_)
    
        parts_clean <- parts |>
          str_replace_all("\\s+", "") |>     # remove spaces
          str_replace_all(";", "_")          # normalize semicolon separators
    
        str_c(c(type, parts_clean), collapse = "_")
      })
    ) |>
    rowwise() |>
    mutate(
      monosomies       = collapse_chromosomes(monosomies),
      mono_vec         = list(safe_split(monosomies)),
      mono_cnt         = length(unique(mono_vec)),
  
      mono_no_sex      = collapse_chromosomes(mono_no_sex),
      mono_no_sex_vec  = list(safe_split(mono_no_sex)),
      mono_no_sex_cnt  = length(unique(mono_no_sex_vec)),
  
      trisomies        = collapse_chromosomes(trisomies),
      tri_vec          = list(safe_split(trisomies)),
      tri_cnt          = length(unique(tri_vec)),

      tri_with_repeats     = collapse_chromosomes(tri_no_sex),
      tri_with_repeats_vec = list(safe_split(tri_with_repeats)),
      tri_with_repeats_cnt = length(unique(tri_with_repeats_vec)),
  
      tri_no_sex       = collapse_chromosomes(tri_no_sex),
      tri_no_sex_vec   = list(safe_split(tri_no_sex)),
      tri_no_sex_cnt   = length(unique(tri_no_sex_vec))
    ) |>
    ungroup() |>
    select(everything(), -ends_with('_vec'))

  xx <- df_expanded |> select(clone, starts_with(c('tri', 'mon')), -starts_with(c('monosomy', 'trisomy')))
  xx <- df_expanded |> select(clone, starts_with(c('stru')))
  xx <- df_expanded |> select(clone, starts_with(c('abn_', 'parens', 'stru')))

  # Karyotype-level summary
  # df_karyo_summary <- df_expanded |>
  #   group_by(karyo) |>
  #   summarize(
  #     karyo_abn_list = paste0(unique(abn_category[!is.na(abn_category)]), collapse = ", "),
  #     karyo_abn_cnt  = n_distinct(abn_category[!is.na(abn_category)]),
  #     .groups = "drop"
  #   )
  
  # Karyotype-level summary
  df_karyo_summary <- df_expanded |>
    group_by(karyo) |>
    reframe(
      karyo_abn_list       = paste0(unique(abn_category[!is.na(abn_category)]), collapse = ", "),
      karyo_structural_abn = paste(unique(structural_abn[!is.na(structural_abn)]), collapse=', '),
      karyo_abn_cnt        = n_distinct(abn_category[!is.na(abn_category)])
    )

  # Clone-level summary
  # df_clone_summary <- df_expanded |>
  #   group_by(karyo, clone, clone_inheritance) |>
  #   summarize(
  #     clone_abn_list = paste0(unique(abn_category[!is.na(abn_category)]), collapse = ", "),
  #     clone_abn_cnt  = n_distinct(abn_category[!is.na(abn_category)]),
  #     mk             = as.integer((clone_abn_cnt > 1 & mono_cnt >= 1) | mono_cnt >= 2),
  #     ck             = as.integer(clone_abn_cnt + numeric_cnt - recurrent_cnt >= 3),
  #     .groups = "drop"
  #   )

  # Clone-level summary
  df_clone_summary <- df_expanded |> 
  group_by(karyo, clone, clone_inheritance) |>
  reframe(
    clone_abn_category_list = paste0(unique(abn_category[!is.na(abn_category)]), collapse = ", "),
    clone_abn_cnt  = n_distinct(abn_category[!is.na(abn_category)]),
    structural_cnt = n_distinct(structural_abn[!is.na(structural_abn)]),
    structural_abn = paste0(unique(structural_abn[!is.na(structural_abn)]), collapse = ", "),
    
    normal_section    = first(normal_section),
    numeric_cnt       = first(tri_no_sex_cnt + mono_no_sex_cnt),
    recurrent_cnt     = first(recurrent_cnt),
    monosomies        = first(monosomies),
    mono_cnt          = first(mono_cnt),
    mono_no_sex       = first(mono_no_sex),
    mono_no_sex_cnt   = first(mono_no_sex_cnt),
    trisomies         = first(trisomies),
    tri_cnt           = first(tri_cnt),
    tri_no_sex        = first(tri_no_sex),
    tri_no_sex_cnt    = first(tri_no_sex_cnt),
    clonal            = first(clonal),
    
    mk = case_when(
      first(clonal) != 1 ~ 0L,
      clone_abn_cnt > 1 & mono_no_sex_cnt >= 1 ~ 1L,
      mono_no_sex_cnt >= 2 ~ 1L,
      first(normal_section) == 1 ~ 0L,
      mono_no_sex_cnt <  2 ~ 0L,
      .default = 0L
    ),
    
    ck = if_else(first(clonal) != 1, 0L, as.integer(structural_cnt + numeric_cnt - recurrent_cnt >= 3)),
    sc = if_else(first(clonal) != 1, 0L, as.integer(structural_cnt - recurrent_cnt >= 3))
  ) |>
  distinct()

  
  # df_clone_summary <- df_expanded |> 
  #   group_by(karyo, clone, clone_inheritance) |>
  #   reframe(
  #     clone_abn_category_list = paste0(unique(abn_category[!is.na(abn_category)]), collapse = ", "),
  #     clone_abn_cnt  = n_distinct(abn_category[!is.na(abn_category)]),
  #     structural_cnt = n_distinct(structural_abn[!is.na(structural_abn)]),
  #     structural_abn = paste0(unique(structural_abn[!is.na(structural_abn)]), collapse = ", "),
  #     
  #     normal_section    = first(normal_section),
  #     numeric_cnt       = first(tri_no_sex_cnt + mono_no_sex_cnt),
  #     recurrent_cnt     = first(recurrent_cnt),
  #     monosomies        = first(monosomies),
  #     mono_cnt          = first(mono_cnt),
  #     mono_no_sex       = first(mono_no_sex),
  #     mono_no_sex_cnt   = first(mono_no_sex_cnt),
  #     trisomies         = first(trisomies),
  #     tri_cnt           = first(tri_cnt),
  #     tri_no_sex        = first(tri_no_sex),
  #     tri_no_sex_cnt    = first(tri_no_sex_cnt),
  #     clonal            = first(clonal),
  #   
  #     mk             = case_when(
  #                         clonal != 1 ~ 0L, # must be clonal
  #                         clone_abn_cnt > 1 & mono_no_sex_cnt >= 1 ~ 1L,
  #                         mono_no_sex_cnt >= 2 ~ 1L,
  #                         normal_section  == 1 ~ 0L,
  #                         mono_no_sex_cnt <  2 ~ 0L,
  #                         .default=0L),
  #     # mk             = as.integer(mk),
  #     ck             = if_else(clonal!=1, 0L, as.integer(structural_cnt + numeric_cnt - recurrent_cnt >= 3)),
  #     sc             = if_else(clonal!=1, 0L, as.integer(structural_cnt - recurrent_cnt >= 3))
  #   ) |>
  #   distinct()
  xx <- df_clone_summary |> select(clone, starts_with(c('tri', 'mon', 'nume')))
  xx <- df_clone_summary |> 
    select(clone, starts_with(c('tri', 'mon')), -starts_with(c('monosomy', 'trisomy')))

  # Re-join, flag MK/CK, and finalize columns
  df_final <- df_clone_summary |>
    left_join(df_karyo_summary, by = "karyo") |>
    left_join(df_clones, by = join_by(karyo, clone, clone_inheritance)) |>
    group_by(karyo) |>
    mutate(
      karyocode = as.integer(cur_group_id() + 10000),
      monosomial_karyo = as.integer(sum(mk, na.rm = TRUE) > 0),
      complex_karyo    = as.integer(sum(ck, na.rm = TRUE) > 0)
    ) |>
    ungroup() |>
    arrange(karyo, cloneid) |>
    select(
       starts_with('karyo'),
       complex_karyo, monosomial_karyo,
       starts_with(c('mk', 'mono', 'ck', 'comp')),
       karyo, cloneid, clone, clone_inheritance,
    #   karyo_abn_list, 
    #   karyo_abn_cnt,
       starts_with(c('clone_abn')),
       everything()
     ) |>
    rename_with(~ gsub(".x", "", .x, fixed = TRUE)) |>
    select(-ends_with(fixed(".y"))) |>
    distinct()

  
  # Add back any clonal clones missing from the summary
  # missing_rows <- anti_join(df_clones, df_final, by = c("karyocode", "cloneid"))
  # df_final <- bind_rows(df_final, missing_rows) |>
  #   arrange(karyocode, cloneid) |>
  #   distinct() |>
  #   select(karyo, karyocode, everything())
  
  # Add back any clonal clones missing from the summary
  df_final.2 <- df_clones |> 
    left_join(df_final, by = c("karyocode", "cloneid"), suffix = c("", ".summ")) |>
    mutate(
      complex_karyo = coalesce(complex_karyo, 0L),
      monosomial_karyo = coalesce(monosomial_karyo, 0L)
    ) |>
    select(karyo, karyocode, everything()) |>
    distinct()
  

  return(df_final)
}

#' Parse a single karyotype row and emit results + labeled diagnostics
#'
#' @param row One-row tibble containing `karyo`, `karyo_hash`, `row_index`, plus any metadata to carry through.
#' @param df_orig Record-level data used by the parser for context.
#' @param use_get_sl_df Logical; pass-through to the parser.
#' @param verbose Logical; emit progress messages.
#' @return list(rows = tibble, log = tibble|NULL)
#' @details Calls `parse_karyo_safely()`, ensures `parse_failed` is present,
#'   binds non-overlapping columns from `row`, and tags log entries with
#'   `row_index`, `karyo`, and `karyo_hash` for downstream joins.

parse_karyo_with_log <- function(row, df_orig, use_get_sl_df = TRUE, verbose = FALSE) {
  # `row` is a one-row data.frame/tibble from the worklist
  s      <- row$karyo[[1]]
  s_hash <- row$karyo_hash[[1]]
  i      <- row$row_index[[1]]

  if (isTRUE(verbose)) message(sprintf(" • [%d] %s", i, s))

  res <- parse_karyo_safely(s, df_orig, use_get_sl_df)  # list(result, log)

  parsed <- res$result
  if (is.null(parsed)) {
    parsed <- get_empty_clone()
    parsed$parse_failed <- TRUE
  } else if (!"parse_failed" %in% names(parsed)) {
    parsed$parse_failed <- FALSE
  }

  # Bind original row’s extra columns
  extra_cols <- setdiff(names(row), names(parsed))
  rows <- bind_cols(parsed, row[, extra_cols, drop = FALSE])

  log_tbl <- res$log
  if (!is.null(log_tbl) && nrow(log_tbl)) {
    log_tbl$row_index  <- i
    log_tbl$karyo      <- s           # readable
    log_tbl$karyo_hash <- s_hash      # canonical join key
  }

  list(rows = rows, log = log_tbl)
}


# karyo <- chunk
# karyo <- "46,XX,add(2)(q33)[8]/46,XX,del(5)(q22q33)[5]/45,X,-X,t(9;22)(q34;q11.2)[2]/46,XX[5]"
# rslt <- parse_karyo_safely(karyo)

parse_karyo_safely <- function(karyo, df_orig, use_get_sl_df = TRUE) {
  stopifnot(is.character(karyo), length(karyo) == 1L)

  warn_msgs <- character(0)
  log_tbl <- tibble(karyo = character(), status = character(),
                            message = character(), time = character())

  out <- tryCatch({
    res <- withCallingHandlers({
      tmp <- parse_single_karyo(karyo, use_get_sl_df)
      create_empty_df_code(tmp, .pkg_env$run_id)  # side-effect (optional)
      tmp
    }, warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    if (length(warn_msgs)) {
      log_tbl <- tibble(
        karyo   = rep(karyo, length(warn_msgs)),
        status  = "warning",
        message = warn_msgs,
        time    = .pkg_env$run_id
      )
    }
    res
  }, error = function(e) {
    log_tbl <<- tibble(
      karyo   = karyo,
      status  = "error",
      message = conditionMessage(e),
      time    = .pkg_env$run_id
    )
    NULL
  })

  list(result = out, log = log_tbl)
}

# karyo = process_df$karyo
# karyo <- "46,XX,add(2)(q33)[8]/46,XX,del(5)(q22q33)[5]/45,X,-X,t(9;22)(q34;q11.2)[2]/46,XX[5]"
parse_single_karyo <- function(karyo=character(0), use_get_sl_df=TRUE) {
  
    # testing
  if (FALSE) {
    karyo <- "46,XY,t(8;11)(q24.1;q13)[5]/47,XY,i(16)(p10),+21[5]/46,XY[10]"      
  }
  
  # browser()
  
  # Load an empty clone
  # Load function from the R/ folder
  clone_path <- file.path("R", "get_empty_clone.R")
  
  if (!exists("get_empty_clone") && file.exists(clone_path)) {
    source(clone_path)
  }
  
  rslt_empty <- if (exists("get_empty_clone")) get_empty_clone() else tibble()

    
  # Expect a karyotype with some characters
  if (length(karyo)==0) return(rslt_empty)   

  cleaned <- karyo |>
    str_replace("^//", "_DBLSLASH_") |>
    str_replace("//", "/_DBLSLASH_") |> 
    str_trim()
  
  # Split the karyotype into clones
  clones <- str_split(cleaned, "/", simplify = FALSE)[[1]] |> str_trim()

  # Handle special cases for "normal female" and "normal male" karyotypes
  if (str_detect(karyo, "(?i)normal female")) {
    clones <- "46,XX[20]"
  } else if (str_detect(karyo, "(?i)normal male")) {
    clones <- "46,XY[20]"
  }
  
  # Detect donor clones by leading "//"
  starts <- str_starts(clones, "^_DBLSLASH_")
  first_donor <- which(starts)[1]
  donor <- if (is.na(first_donor)) rep(0L, length(clones)) else as.integer(seq_along(clones) >= first_donor)

  # Restore slashes after separating clones, slashes designate a donor derived
  # clone, and also are found when the cell count is a fraction
  clones <- str_replace_all(clones, "_SLASH_", "/")
  
  # Assign each clone an id
  cloneid    <- seq_along(clones)

  # Replace _DBLSLASH_ in original
  clones      <- str_replace(clones, "_DBLSLASH_", "//")

  # Make a processing copy
  temp_clones <- clones |> str_wipe_ends('l')
  
  # Extract the number of cells as an integer
  cells_observed <- str_extract(temp_clones, "\\[[^\\]]*\\]") |> 
    str_extract("\\d+") |> 
    as.integer()

  # Remove the cells_observed from the right as well as any trailing non-alpha-numerics
  temp_clones <- temp_clones |> str_remove("\\[[^\\]]*\\]$") |> str_wipe_ends('r')

  
  # Get chromosome count or range
  # Handle ranges like 43-45, 43~45, or 43\R\45
  chr_rng <- str_extract(temp_clones, "^\\d+(-\\d+)?")
  # Separate start and end values
  chr_start <- str_extract(chr_rng, "^\\d+") |> as.integer()
  chr_end <- str_extract(chr_rng, "(?<=-)\\d+") |> as.integer()
  # For non-ranges, set chr_rng as the single start value
  chr_rng <- ifelse(is.na(chr_end), as.character(chr_start), chr_rng)

  # Remove chr_rng from the left of the processing copy and leading commas or spaces
  temp_clones <- if_else(is.na(chr_rng), temp_clones, str_remove(temp_clones, paste0("^", fixed(chr_rng))))
  
  # Find any sideline/idem code at the beginning of the clone
  sideline_text <- str_extract(tolower(temp_clones), regex("(sl|sdl\\d*|idem)[^,]*,")) |> str_remove(",\\s*$")

  # Remove sideline from temp_clones if exists
  temp_clones <- ifelse(
    !is.na(sideline_text),
    str_replace(temp_clones, paste0(sideline_text, ',?'), ""),
    temp_clones) |> str_wipe_ends('l')

  # temp_tibble <- tibble(cloneid = cloneid, clone=clones, sideline = case_when(
  #     cloneid == 1            ~ -1L, # stemline
  #     sideline_text == 'sl'   ~ 0L,
  #     sideline_text == 'sdl1' ~ 1L,
  #     sideline_text == 'sdl2' ~ 2L,
  #     sideline_text == 'sdl3' ~ 3L,
  #     sideline_text == 'sdl4' ~ 4L,
  #     .default = NA_integer_)
  #     ) 
  
  
  # temp_tibble <- tibble(
  #   cloneid,
  #   clone   = clones
  # ) |>
  #   mutate(
  #     sideline_text = str_extract(tolower(temp_clones), regex("(sl|sdl\\d*|idem)[^,]*,")) |> str_remove(",\\s*$"),
  #     is_idem = str_detect(clone, regex("(^|,)\\s*idem(,|$)", ignore_case = TRUE)),
  #     sideline = case_when(
  #       cloneid == 1            ~ -1L,  # stemline
  #       sideline_text == 'sl'   ~ 0L,
  #       sideline_text == 'sdl1' ~ 1L,
  #       sideline_text == 'sdl2' ~ 2L,
  #       sideline_text == 'sdl3' ~ 3L,
  #       sideline_text == 'sdl4' ~ 4L,
  #       .default = NA_integer_
  #     ), 
  #     parentid = case_when(
  #       cloneid == 1          ~ -1L,         # does not inherit
  #       is_idem               ~ cloneid - 1, # always inherit from previous clone
  #       sideline_text == 'sl' ~ 1L,          # inherits from stemline, the first clone
  #       TRUE ~ NA_integer_  # need to calculate
  #     )
  #   )
  # head(temp_tibble)

  
  temp_tibble <- tibble(
    cloneid = cloneid,
    clone   = clones
  ) |>
    mutate(
      sideline_text = str_extract(tolower(clone), regex("(sl|sdl\\d*|idem)[^,]*,")) |>
                      str_remove(",\\s*$"),
      
      is_idem = str_detect(clone, regex("(^|,)\\s*idem(,|$)", ignore_case = TRUE)),
      
      sideline = case_when(
        cloneid == 1            ~ -1L,  # stemline
        sideline_text == "sl"   ~ 0L,
        sideline_text == "sdl1" ~ 1L,
        sideline_text == "sdl2" ~ 2L,
        sideline_text == "sdl3" ~ 3L,
        sideline_text == "sdl4" ~ 4L,
        .default = NA_integer_
      ),
      
      parentid = case_when(
        cloneid == 1          ~ NA_integer_,  # stemline: no parent
        is_idem               ~ cloneid - 1L, # idem inherits from previous
        sideline == 0L        ~ 1L,           # sl inherits from stemline
        TRUE                  ~ NA_integer_   # sdl1+, unresolved here (resolved later)
      )      
    )
  
  
  sideline <- get_sl_df(temp_tibble)
  # head(sideline)
  

  # Extract sex chromosome patterns
  sex_chr <- str_extract(temp_clones, "(?i)^([OXY]+)")
  
  # Remove sex_chr from the left of the processing copy
  temp_clones <- str_remove(temp_clones, "^[XOY]{1,3}[,\\s]*") |> str_wipe_ends('l')
  
  # Extract everything between the sex chr and cell count as abnormality
  abnormal <- temp_clones |> str_remove(paste0("\\[",cells_observed,"\\]")) |> str_trim() |> na_if("")  
  
  # Determine ploidy
  ploidy <- case_when(
    str_detect(temp_clones, regex("<3N>", ignore_case = TRUE)) ~ "tri",
    str_detect(temp_clones, regex("<4N>", ignore_case = TRUE)) ~ "tetra",
    str_detect(temp_clones, regex("<2N>", ignore_case = TRUE)) ~ "hypo",
    chr_start > 80                    ~ "tetra",
    chr_start >= 47 & chr_start <= 80 ~ "hyper",
    chr_start == 46                   ~ "normal",
    chr_start < 46                    ~ "hypo",
    TRUE ~ NA_character_)
  
  # Fix cases like //46,XX where no cell count was given, 20 cells is implied.
  cells_observed <- if_else(chr_rng=='46' & is.na(abnormal) & !is.na(sex_chr) & ploidy=='normal' & is.na(cells_observed)
                            , 20L # default normal count explicitly integer
                            , as.integer(cells_observed))
  

  
  
  # Create the tibble
  rslt_tibble <- tibble(
    karyoroot = karyo,
    donor = donor,  # Includes the donor flag for each clone
    cloneid = cloneid,
    chr = chr_start,
    chr_rng = chr_rng,
    chr_start = chr_start,
    chr_end = chr_end,
    ploidy = ploidy,
    sex_chr = sex_chr,
    abn = abnormal,
    abn_inheritance = NA_character_,
    abn_orig = abn,
    clone = clones,
    clone_inheritance = NA_character_,
    clone_orig = clone,
    cells_observed = cells_observed,
    sideline_text = sideline_text) |> 
    left_join(sideline, by=c('cloneid')) |>
    select(karyoroot, cloneid, clone, parentid, everything())

  cells_with_signature_df <- cells_with_signature(rslt_tibble)
  rslt_tibble <- left_join(rslt_tibble, cells_with_signature_df, by='cloneid') |> 
    select(cloneid, parentid, cells_observed, cells_with_signature, everything())

  rslt_tibble <- rslt_tibble |> mutate( clonal = case_when(cells_with_signature > 2 ~ 1,
                                          cells_with_signature < 2 ~ 0,
                                          ploidy %in% c('normal', 'hyper', 'tri', 'tetra') 
                                            & cells_with_signature == 2 ~ 1,
                                          ploidy == 'hypo' & cells_with_signature >= 3 ~ 1,
                                          ploidy == 'hypo' & cells_with_signature <= 2 ~ 0,
                                          .default = 0),
                                        clonal = as.integer(clonal)) |>
    select(cloneid, parentid, cells_observed, cells_with_signature, ploidy, clonal, everything())
  
      
  rslt_tibble$sex_chr <- inherit_sex(rslt_tibble)
  rslt_tibble         <- add_inheritance(rslt_tibble, inherit_col = "clone", separator = "/")
  rslt_tibble         <- add_inheritance(rslt_tibble, inherit_col = "abn",   separator = ",")
  rslt_tibble         <- rslt_tibble |> 
    select(cloneid, parentid, cells_observed, clone, sex_chr, clone_inheritance, abn_inheritance, everything())


  # Look at the abn data in a long form
  abnormal <- rslt_tibble |> 
    filter(!is.na(abn_inheritance)) |>  # optional but safe
    group_by(clone, cloneid) |>
    summarize(
      clone_abn_list = collapse_values(abn_inheritance, dedup = TRUE),
      clone_abn_cnt  = length(unique(trimws(unlist(strsplit(paste(abn_inheritance, collapse = ","), ",\\s*"))))),
      .groups = "drop"
    ) |>
    mutate(clone_abn_list = if_else(clone_abn_list == "", NA_character_, clone_abn_list)) |>
    left_join(rslt_tibble, by = c("clone", "cloneid")) |>
    arrange(cloneid) |>
    select(
      clone, 
      cloneid,
      clone_abn_list,
      clone_abn_cnt,
      any_of(names(rslt_tibble)),
      -abn_orig)

  normal <- rslt_tibble |> 
    filter(is.na(abn_inheritance)) |>
    mutate(clone_abn_list = NA_character_, clone_abn_cnt  = 0) |>
    select(
      clone, 
      cloneid,
      clone_abn_list,
      clone_abn_cnt,
      any_of(names(rslt_tibble)),
      -abn_orig)
  
  rslt_tibble <- bind_rows(normal, abnormal)
  
  rm(normal, abnormal)
    
  # this is NOT a good time to count the abnormalities from abn_inheritance since there may be duplicates
  # rslt_tibble <- rslt_tibble |> mutate(abn_cnt = if_else(is.na(abn_inheritance), 0, str_count(abn_inheritance, ",") + 1))

  
  # Normal clones can be found within an abnormal karyotype
  rslt_tibble <- rslt_tibble |> mutate( normal_section = case_when(
                                                      clone_abn_cnt > 0 ~ 0,          # abnormal
                                                      cells_with_signature >= 10 ~ 1, # enough cells and not abnormal
                                                      .default = -1)                  # not enough cells to call normal
                                        , normal_section = as.integer(normal_section))

  # backup before limiting columns, for testing purposes
  rslt_tibble_backup <- rslt_tibble

  # Column order
  rslt_tibble <- rslt_tibble |> 
    arrange(cloneid) |>
    select(karyoroot, cloneid, parentid
           , donor, chr_rng, chr, sex_chr, ploidy
           , cells_observed, cells_with_signature
           , clonal, normal_section, sideline_text
           , clone, clone_inheritance
           , abn, abn_inheritance, clone_abn_list, clone_abn_cnt)
  
  # make a new empty_tibble in case the structure changes
  # empty_tibble <<- rslt_tibble[0,]
  
  # handle potentially empty result
  # handle potentially empty result
  if (is.null(rslt_tibble) || nrow(rslt_tibble) == 0) {
    rslt_tibble <- rslt_empty
  }
  
  return(rslt_tibble)

}

cells_with_signature <- function(df = clone_cells, outputcol = 'cells_with_signature') {
  rslt <- df$cloneid |>
    set_names() |>
    map_dfr(
      ~ tibble(
        cloneid   = .x,
        descendant = c(.x, get_all_descendants(.x, df |> select(cloneid, parentid)))
      ),
      .id = "cloneid"
    ) |>
    mutate(cloneid = as.integer(cloneid)) |>
    left_join(df |> select(cloneid, cells_observed), by = c("descendant" = "cloneid")) |>
    mutate(cells_observed = replace_na(cells_observed, 0)) |>
    group_by(cloneid) |>
    summarize(!!outputcol := sum(cells_observed), .groups = "drop")

  return(rslt)
}

get_all_descendants <- function(id, data) {
  parent_map <- split(data$cloneid, data$parentid)
  parent_map <- parent_map[!is.na(names(parent_map))]
  
  descendants <- integer()
  queue <- id
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    
    child <- parent_map[[as.character(current)]]
    if (!is.null(child)) {
      descendants <- c(descendants, child)
      queue <- c(queue, child)
    }
  }
  descendants
}


get_sl_df <- function(clone_tibble) {
  stopifnot(all(c("cloneid", "sideline", "is_idem", "parentid") %in% names(clone_tibble)))

  clone_tibble <- clone_tibble |> arrange(cloneid)

  # Fill in parentid (override inherit_from with sideline rules if needed)
  clone_tibble <- clone_tibble |>
    mutate(
      parentid = case_when(
        !is.na(parentid) ~ parentid,  # use pre-set inherit_from when available

        # Handle deeper sideline generations (sdl1, sdl2, etc.)
        !is.na(sideline) & sideline > 0L ~ {
          sideline_level <- sideline
          candidates <- cloneid[sideline == (sideline_level - 1L) & cloneid < cloneid]
          ifelse(length(candidates) > 0, max(candidates), NA_integer_)
        },

        TRUE ~ NA_integer_
      )
    )

  return(clone_tibble |> select(cloneid, parentid))
}


get_sl_df_v4 <- function(clone_tibble, generations = 4) {
  stopifnot(all(c("cloneid", "sideline") %in% names(clone_tibble)))

  clone_tibble <- clone_tibble |> arrange(cloneid)
  clone_tibble$parentid <- NA_integer_

for (i in seq_len(nrow(clone_tibble))) {
  sideline <- clone_tibble$sideline[i]
  this_id  <- clone_tibble$cloneid[i]

  if (!is.na(sideline) && sideline == 0L) {
    clone_tibble$parentid[i] <- 1L
  } else if (!is.na(sideline) && sideline > 0) {
    potential_parents <- clone_tibble$cloneid[
      clone_tibble$cloneid < this_id &
      clone_tibble$sideline == (sideline - 1)
    ]
    if (length(potential_parents)) {
      clone_tibble$parentid[i] <- max(potential_parents)
    }
  }
}

  return(clone_tibble |> select(cloneid, parentid))
}


# Using lag
get_sl_df_v3 <- function(sideline_df, generations = 4) {
  stopifnot(all(c("karyocode", "cloneid", "sideline") %in% names(sideline_df)))

  # Save original row order
  sideline_df <- sideline_df |> mutate(rowid = row_number())

  # Flag karyotypes that have no sideline (only NA or 0L)
  sideline_flags <- sideline_df |>
    group_by(karyocode) |>
    summarize(has_sideline = any(!is.na(sideline) & sideline > 0), .groups = "drop")

  sideline_df <- sideline_df |>
    left_join(sideline_flags, by = "karyocode") |>
    mutate(sideline_flag = !has_sideline)

  # Initialize parentid
  sideline_df <- sideline_df |> mutate(parentid = NA_integer_)

  # Process each karyocode that has sideline
  for (k in unique(sideline_df$karyocode[sideline_df$has_sideline])) {
    rows <- sideline_df |> filter(karyocode == k) |> arrange(cloneid)
    
    for (i in seq_len(nrow(rows))) {
      sideline <- rows$sideline[i]
      this_id <- rows$cloneid[i]

      if (sideline == 0L) {
        rows$parentid[i] <- 1L  # sl gets parent = 1
      } else if (!is.na(sideline) && sideline > 0) {
        parent_ids <- rows$cloneid[rows$cloneid < this_id & rows$sideline == (sideline - 1)]
        if (length(parent_ids)) {
          rows$parentid[i] <- max(parent_ids)
        }
      }
    }

    # Update just the rows for this karyocode
    sideline_df <- sideline_df |> rows_update(rows, by = "rowid")
  }

  # Restore original order
  sideline_df <- sideline_df |> arrange(rowid) |> select(-rowid, -has_sideline)

  return(sideline_df)
}


# Using levels
get_sl_df_v2 <- function(sideline, generations = 4) {
  sideline_levels <- list()
  
  for (lvl in 0:generations) {
    suffix <- paste0("_sdl", lvl)
    sideline_levels[[suffix]] <- filter(sideline, sideline == lvl) |>
      rename_with(~ paste0(.x, suffix))
  }
  
  parent_tables <- list()
  
  for (lvl in generations:1) {
    curr_df <- sideline_levels[[paste0("_sdl", lvl)]]
    prev_df <- sideline_levels[[paste0("_sdl", lvl - 1)]]
    if (nrow(curr_df) == 0 || nrow(prev_df) == 0) next

    parent_df <- crossing(prev_df, curr_df) |>
      filter(.data[[paste0("cloneid_sdl", lvl - 1)]] < .data[[paste0("cloneid_sdl", lvl)]]) |>
      group_by(.data[[paste0("sideline_sdl", lvl)]], .data[[paste0("cloneid_sdl", lvl)]]) |>
      summarize(
        parentid = max(.data[[paste0("cloneid_sdl", lvl - 1)]]),
        .groups = "drop"
      ) |>
      rename(
        cloneid = !!paste0("cloneid_sdl", lvl),
        sideline = !!paste0("sideline_sdl", lvl)
      )

    parent_tables[[lvl]] <- parent_df
  }

  sdl0 <- filter(sideline, is.na(sideline) | sideline == 0) |>
    mutate(parentid = if_else(sideline == 0, 1L, NA_integer_)) |>
    select(-clone)

  result <- sideline |> select(-sideline) |>
    left_join(bind_rows(sdl0, parent_tables |> purrr::compact() |> bind_rows()), by = "cloneid")

  return(result)
}


# Using "generations"
get_sl_df_v1 <- function(sideline, generations = NULL) {

  generations <- if (is.null(generations)) 4 else generations
  
  parent_tables <- list()
  
  # Combined loop for creating sideline generations
  for (lvl in (generations + 1):1) {  # 5:1
    if (lvl > 0) {
      # Always create sideline tables for lvl-1
      suffix <- paste0("_sdl", lvl - 1)
      curr_df <- filter(sideline, sideline == (lvl - 1)) |> 
        rename_with(~ paste0(.x, suffix))
      assign(paste0("sdl", lvl - 1), curr_df)
    }
    
    if (lvl <= generations) {
      prev_lvl <- lvl - 1
      prev_df <- get(paste0("sdl", prev_lvl))
      curr_df <- get(paste0("sdl", lvl))
      curr_cloneid <- paste0("cloneid_sdl", lvl)
      prev_cloneid <- paste0("cloneid_sdl", prev_lvl)
      curr_sideline <- paste0("sideline_sdl", lvl)
      
      parent_df <- crossing(prev_df, curr_df) |>
        filter(.data[[prev_cloneid]] < .data[[curr_cloneid]])
      
      if (nrow(parent_df) > 0) {
        parent_df <- parent_df |>
          group_by(.data[[curr_sideline]], .data[[curr_cloneid]]) |>
          summarize(parentid = max(.data[[prev_cloneid]]), .groups = "drop") |>
          rename(cloneid = !!curr_cloneid, sideline = !!curr_sideline)
      } 
      else {
        parent_df <- tibble(cloneid = integer(), sideline = integer(), parentid = integer())
      }
      
      parent_tables[[lvl]] <- parent_df
    }
  }
  
  # Create sdl0 separately (base cases)
  sdl0 <- filter(sideline, is.na(sideline) | sideline == 0) |>
    mutate(parentid = if_else(sideline == 0, 1L, NA_integer_)) |> select(-clone)
  
  # Assemble all parents
  result <- suppressMessages(
    sideline |> select(-sideline) |>
      left_join(
        bind_rows(
          sdl0, 
          parent_tables[[4]], 
          parent_tables[[3]], 
          parent_tables[[2]], 
          parent_tables[[1]]
        ),
        by = "cloneid"
    ))
  
  return(result)
}


abn_metadata <- function() {
  path <- file.path(projdir, 'inst', 'extdata', 'abnormality_metadata.csv')
  df <- get_sheet(path)
  
}

# returns a named character vector: names = keys, values = regex
abn_patterns <- function(type = c("all","recurrent","abn17p","adverse","favorable"),
                         by   = c("field_name","clone_col","karyo_col"),
                         active_only = TRUE) {
  
  if (FALSE) {
    type = 'abn17p'
    by = 'clone_col'
    active_only=TRUE
  }
  
  catalog <- abn_metadata() |> filter(active==active_only | !active_only) # keep only active rows
  type <- match.arg(type) 
  by   <- match.arg(by)

  cls <- coalesce(catalog$class, "")
  has_tag <- function(tag) str_detect(cls, paste0("(^|\\|)", tag, "(\\||$)"))

  rows <- case_when(
    type == "all"        ~ rep(TRUE, nrow(catalog)),
    type == "recurrent"  ~ has_tag("recurrent"),
    type == "abn17p"     ~ has_tag("17p"),
    type == "adverse"    ~ catalog$eln_tag == "adverse",
    type == "favorable"  ~ catalog$eln_tag == "favorable",
    TRUE                 ~ TRUE
  )

  out <- catalog[rows, c(by, "pattern")]
  setNames(out$pattern, out[[by]])
}


abn_names <- function(type = c("all","recurrent","abn17p","adverse","favorable"),
                      by   = c("field_name","clone_col","karyo_col"),
                      active_only = TRUE) {
  
  catalog <- abn_metadata() |> filter(active==active_only | !active_only) # keep only active rows
  type <- match.arg(type) 
  by   <- match.arg(by)

  cls <- coalesce(catalog$class, "")
  has_tag <- function(tag) str_detect(cls, paste0("(^|\\|)", tag, "(\\||$)"))

  rows <- case_when(
    type == "all"        ~ rep(TRUE, nrow(catalog)),
    type == "recurrent"  ~ has_tag("recurrent"),
    type == "abn17p"     ~ has_tag("17p"),
    type == "adverse"    ~ catalog$eln_tag == "adverse",
    type == "favorable"  ~ catalog$eln_tag == "favorable",
    TRUE                 ~ TRUE
  )

  out <- catalog[rows, c(by, "label")]
  stats::setNames(out$label, out[[by]])
}


# 
# #' AML cytogenetic regex patterns grouped by ELN-2022 categories
# #'
# #' Return named regular-expression patterns for AML cytogenetic abnormalities,
# #' organized into buckets aligned with ELN-2022 usage. Use `type` to select
# #' subsets such as favorable core-binding factor (CBF), APL, adverse named
# #' lesions, abn(17p), and convenience intermediate items (e.g., isolated +8).
# #'
# #' @details
# #' The function exposes pattern groups tailored to ELN-2022 logic:
# #' \itemize{
# #'   \item \code{fav_named}: Core-binding factor (CBF) lesions
# #'         \code{t(8;21)}, \code{inv(16)}, \code{t(16;16)} (favorable).
# #'   \item \code{apl}: \code{t(15;17)} / PML::RARA (handled separately from ELN AML risk).
# #'   \item \code{adv_named}: ELN-2022 adverse named cytogenetic lesions
# #'         (e.g., \code{t(6;9)}, KMT2A-r incl. \code{t(9;11)}, \code{t(9;22)},
# #'         \code{t(8;16)}, MECOM/3q26.2-rearrangements, \code{inv(3)/t(3;3)},
# #'         \code{-5/del(5q)}, \code{-7}).
# #'   \item \code{abn17p}: \code{-17/abn(17p)} cluster (e.g., \code{del(17p)},
# #'         \code{idic(17)}, \code{r(17)}, \code{i(17q)}—typically implying 17p loss).
# #'   \item \code{int_named}: Convenience bucket for intermediate-leaning named items
# #'         (defaults to \code{+8}; extend per your rules).
# #'   \item \code{recurrent}: Union of \code{fav_named}, \code{apl}, \code{adv_named},
# #'         \code{abn17p}, and \code{int_named}.
# #'   \item \code{all}: Return every pattern (no filtering).
# #' }
# #'
# #' Notes:
# #' \enumerate{
# #'   \item Patterns are case-insensitive where appropriate and target ISCN-like strings.
# #'   \item To avoid double-counting, apply ELN hierarchy first (named/recurrent rules),
# #'         then consider any residual "other structural" logic outside this helper.
# #'   \item The function returns a \emph{named list} of regex strings ready for
# #'         \pkg{stringr} detection.
# #' }
# #'
# #' @section Buckets:
# #' Set \code{type} to one or more of:
# #' \code{"fav_named"}, \code{"apl"}, \code{"adv_named"}, \code{"abn17p"},
# #' \code{"int_named"}, \code{"recurrent"}, or \code{"all"}.
# #' You may also use the alias \code{"cbf"} for \code{"fav_named"}.
# #'
# #' @param type Character vector of bucket names to return.
# #'   Defaults to \code{"all"} (no filtering). Unknown values error.
# #'
# #' @return A named \code{list} of regex patterns (character scalars), in the
# #'   original definition order, filtered to the requested bucket(s).
# #'
# #' @examples
# #' # Get everything:
# #' patterns_all <- recurrent_patterns()
# #'
# #' # Only the abn(17p) cluster:
# #' patterns_17p <- recurrent_patterns("abn17p")
# #'
# #' # All recurrent (fav + apl + adverse named + abn17p + intermediate named):
# #' patterns_rec <- recurrent_patterns("recurrent")
# #'
# #' # Combine buckets: adverse named + abn(17p)
# #' patterns_adv_17p <- recurrent_patterns(c("adv_named", "abn17p"))
# #'
# #' # Use with stringr to flag patterns in a column `karyo`:
# #' # (returns a logical data frame for each pattern)
# #' # library(stringr); library(purrr)
# #' # flags <- purrr::map_lgl(patterns_rec, ~ str_detect(karyo, .x))
# #'
# #' @seealso \code{\link[stringr]{str_detect}}, \code{\link[purrr]{imap}},
# #'   and your downstream ELN risk classification helpers.
# #'
# #' @family AML cytogenetics utilities
# #' @export
# recurrent_patterns <- function(type = "all") {
#   # --- full pattern list (your existing definitions) ---
#   pattern_lst <- list(
#     t_8_21   = "(?i)\\bt\\s*\\(\\s*8\\s*;\\s*21\\s*\\)",
#     inv_16   = "(?i)\\binv\\s*\\(\\s*16\\s*\\)",
#     t_16_16  = "(?i)\\bt\\s*\\(\\s*16\\s*;\\s*16\\s*\\)",
#     t_15_17  = "(?i)\\bt\\s*\\(\\s*15\\s*;\\s*17\\s*\\)",
# 
#     t_9_11   = "(?i)\\bt\\s*\\(\\s*9\\s*;\\s*11\\s*\\)",
#     t_9_22   = "(?i)\\bt\\s*\\(\\s*9\\s*;\\s*22\\s*\\)",
# 
#     monosomy_7 = "(?<![0-9A-Za-z])\\-\\s*7(?=($|[,/\\]]))",
#     monosomy_5 = "(?<![0-9A-Za-z])\\-\\s*5(?=($|[,/\\]]))",
#     del_5q     = "(?i)\\bdel\\s*\\(\\s*5\\s*q\\s*\\)",
# 
#     monosomy_17 = "(?<![0-9A-Za-z])\\-\\s*17(?=($|[,/\\]]))",
#     trisomy_17  = "(?<![0-9A-Za-z])\\+\\s*17(?=($|[,/\\]]))",
#     iso_17q     = "(?i)\\bi\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*q\\s*10\\s*\\)|\\bi\\s*\\(\\s*17\\s*q\\s*10?\\s*\\)",
#     r_17        = "(?i)\\br\\s*\\(\\s*17\\s*\\)(?:\\s*\\([^)]*\\))?",
#     r_17p       = "(?i)\\br\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*p\\d",
# 
#     add_17p = paste0(
#       "(?i)\\badd\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*",
#       "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?",
#       "\\s*\\)"
#     ),
#     del_17p = paste0(
#       "(?i)\\bdel\\s*\\(\\s*17\\s*\\)\\s*",
#       "(?:\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?\\s*\\)|",
#       "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?|",
#       "\\(\\s*17\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?\\s*\\))",
#       "(?=$|[,/\\)\\]])"
#     ),
#     dup_17p = paste0(
#       "(?i)\\bdup\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*",
#       "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?",
#       "\\s*\\)"
#     ),
#     der_17p_ch17 = paste0("(?i)\\bder\\s*\\(\\s*17\\s*\\)", "[^)]*\\)\\s*", "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"),
#     der_17p_any  = paste0("(?i)\\bder\\s*\\(", "[^)]*\\b17\\b[^)]*\\)\\s*", "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"),
#     ins_17p      = paste0("(?i)\\bins\\s*\\(", "(?:\\s*17\\s*;[^)]*|[^)]*;\\s*17\\s*)\\)\\s*", "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"),
#     dic_17p      = paste0("(?i)\\bdic\\s*\\(", "[^)]*\\b17\\b[^)]*\\)\\s*", "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"),
# 
#     idic_17  = "(?i)\\bidic\\s*\\(\\s*17\\s*\\)(?:\\s*\\([^)]*\\))?(?=$|[,/\\)\\]])",
#     idic_17p = paste0("(?i)\\bidic\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*", "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#                       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?", "\\s*\\)"),
#     dic_17_17 = "(?i)\\bdic\\s*\\(\\s*17\\s*;\\s*17\\s*\\)(?=$|[,/\\)\\]])",
# 
#     t_8_16     = "(?i)\\bt\\s*\\(\\s*8\\s*;\\s*16\\s*\\)",
#     t_6_9      = "(?i)\\bt\\s*\\(\\s*6\\s*;\\s*9\\s*\\)",
#     t_3q26     = "(?i)\\bt\\s*\\(\\s*3\\s*q\\s*26\\.\\s*2\\s*;[^\\)]*\\)",
#     t_11q23    = "(?i)\\bt\\s*\\([^\\)]*;\\s*11\\s*q\\s*23\\.\\s*3\\s*\\)",
#     trisomy_8  = "(?<![0-9A-Za-z])\\+\\s*8(?=($|[,/\\]]))",
#     inv_3_t_3_3= "(?i)\\binv\\s*\\(\\s*3\\s*\\)|\\bt\\s*\\(\\s*3\\s*;\\s*3\\s*\\)"
#   )
# 
#   # ensure no empty names
#   nm <- names(pattern_lst)
#   if (any(nm == "")) {
#     empties <- which(nm == ""); nm[empties] <- paste0("pattern_", empties); names(pattern_lst) <- nm
#   }
# 
#   # --- groups (new fav_named / int_named added) ---
#   groups <- list(
#     fav_named = c("t_8_21", "inv_16", "t_16_16"),
#     apl       = c("t_15_17"),
#     adv_named = c("t_6_9", "t_9_11", "t_11q23", "t_9_22", "t_8_16",
#                   "t_3q26", "inv_3_t_3_3", "monosomy_5", "del_5q", "monosomy_7"),
#     abn17p    = c("monosomy_17", "del_17p", "der_17p_ch17", "der_17p_any",
#                   "ins_17p", "dic_17p", "idic_17", "idic_17p", "dic_17_17", "r_17", "r_17p", "iso_17q"),
#     int_named = c("trisomy_8")
#   )
#   groups$cbf <- groups$fav_named
#   groups$recurrent <- unique(unlist(groups[c("fav_named","apl","adv_named","abn17p","int_named")], use.names = FALSE))
# 
# 
#   valid_types <- c(names(groups), "all")
#   type <- tolower(type)
#   bad <- setdiff(type, valid_types)
#   if (length(bad)) stop("Unknown type(s): ", paste(bad, collapse = ", "),
#                         ". Valid types: ", paste(valid_types, collapse = ", "))
# 
#   if (length(type) == 1L && type == "all") return(pattern_lst)
# 
#   wanted <- unique(unlist(groups[type], use.names = FALSE))
#   wanted <- intersect(wanted, names(pattern_lst))
#   idx <- match(wanted, names(pattern_lst)); idx <- idx[!is.na(idx)]
#   pattern_lst[idx]
# }
# 

# recurrent_patterns__ <- function() {
#   pattern_lst <- list(
#     # -----------------------------
#     # Favorable
#     # -----------------------------
#     t_8_21   = "(?i)\\bt\\s*\\(\\s*8\\s*;\\s*21\\s*\\)",
#     inv_16   = "(?i)\\binv\\s*\\(\\s*16\\s*\\)",
#     t_16_16  = "(?i)\\bt\\s*\\(\\s*16\\s*;\\s*16\\s*\\)",
#     t_15_17  = "(?i)\\bt\\s*\\(\\s*15\\s*;\\s*17\\s*\\)",
# 
#     # -----------------------------
#     # Intermediate (examples)
#     # -----------------------------
#     t_9_11   = "(?i)\\bt\\s*\\(\\s*9\\s*;\\s*11\\s*\\)",
#     t_9_22   = "(?i)\\bt\\s*\\(\\s*9\\s*;\\s*22\\s*\\)",
# 
#     # -----------------------------
#     # Adverse monosomies / 5q
#     # -----------------------------
#     monosomy_7 = "(?<![0-9A-Za-z])\\-\\s*7(?=($|[,/\\]]))",
#     monosomy_5 = "(?<![0-9A-Za-z])\\-\\s*5(?=($|[,/\\]]))",
#     del_5q     = "(?i)\\bdel\\s*\\(\\s*5\\s*q\\s*\\)",
# 
#     # =======================================================================================
#     # Chromosome 17 (keep each as its own flag; OR them later if you want “abn(17p)”
#     # =======================================================================================
#     monosomy_17 = "(?<![0-9A-Za-z])\\-\\s*17(?=($|[,/\\]]))",
#     trisomy_17  = "(?<![0-9A-Za-z])\\+\\s*17(?=($|[,/\\]]))",
# 
#     # i(17q) — canonical and shorthand
#     iso_17q = "(?i)\\bi\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*q\\s*10\\s*\\)|\\bi\\s*\\(\\s*17\\s*q\\s*10?\\s*\\)",
# 
#     # Rings
#     r_17   = "(?i)\\br\\s*\\(\\s*17\\s*\\)(?:\\s*\\([^)]*\\))?",
#     r_17p  = "(?i)\\br\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*p\\d",
# 
#     # Classic one-chromosome ops (p-arm)
#     add_17p = paste0(
#       "(?i)\\badd\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*",
#       "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?",
#       "\\s*\\)"
#     ),
# 
#     # del(17p): canonical, shorthand after the close paren, and “del(17p...)”
#     del_17p = paste0(
#       "(?i)\\bdel\\s*\\(\\s*17\\s*\\)\\s*",
#       "(?:",
#         # canonical: del(17)(p…[range]…)
#         "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#         "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?\\s*\\)",
#       "|",
#         # shorthand after paren: del(17)p…
#         "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#         "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?",
#       "|",
#         # single-group shorthand: del(17p…)
#         "\\(\\s*17\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#         "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?\\s*\\)",
#       ")",
#       "(?=$|[,/\\)\\]])"
#     ),
# 
#     dup_17p = paste0(
#       "(?i)\\bdup\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*",
#       "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?",
#       "\\s*\\)"
#     ),
# 
#     # Derivatives / insertions / dicentrics implicating 17p
#     der_17p_ch17 = paste0(
#       "(?i)\\bder\\s*\\(\\s*17\\s*\\)",
#       "[^)]*\\)\\s*",
#       "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"
#     ),
#     der_17p_any = paste0(
#       "(?i)\\bder\\s*\\(",
#       "[^)]*\\b17\\b[^)]*\\)\\s*",
#       "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"
#     ),
#     ins_17p = paste0(
#       "(?i)\\bins\\s*\\(",
#       "(?:\\s*17\\s*;[^)]*|[^)]*;\\s*17\\s*)\\)\\s*",
#       "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"
#     ),
#     dic_17p = paste0(
#       "(?i)\\bdic\\s*\\(",
#       "[^)]*\\b17\\b[^)]*\\)\\s*",
#       "\\(\\s*p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)"
#     ),
# 
#     # -----------------------------
#     # NEW: isodicentric 17
#     # -----------------------------
#     # General flag: idic(17) with optional trailing group; use if you want any idic(17)
#     idic_17 = "(?i)\\bidic\\s*\\(\\s*17\\s*\\)(?:\\s*\\([^)]*\\))?(?=$|[,/\\)\\]])",
# 
#     # 17p-specific idic: idic(17)(p…[range]…)
#     idic_17p = paste0(
#       "(?i)\\bidic\\s*\\(\\s*17\\s*\\)\\s*\\(\\s*",
#       "p(?:ter|\\d{1,2}(?:\\.\\d{1,2})?)",
#       "(?:\\s*(?:-|->|~)?\\s*p?(?:ter|\\d{1,2}(?:\\.\\d{1,2})?))?",
#       "\\s*\\)"
#     ),
# 
#     # Common synonym for an isodicentric 17
#     dic_17_17 = "(?i)\\bdic\\s*\\(\\s*17\\s*;\\s*17\\s*\\)(?=$|[,/\\)\\]])",
# 
#     # -----------------------------
#     # Other notable recurrent abnormalities
#     # -----------------------------
#     t_8_16     = "(?i)\\bt\\s*\\(\\s*8\\s*;\\s*16\\s*\\)",
#     t_6_9      = "(?i)\\bt\\s*\\(\\s*6\\s*;\\s*9\\s*\\)",
#     t_3q26     = "(?i)\\bt\\s*\\(\\s*3\\s*q\\s*26\\.\\s*2\\s*;[^\\)]*\\)",
#     t_11q23    = "(?i)\\bt\\s*\\([^\\)]*;\\s*11\\s*q\\s*23\\.\\s*3\\s*\\)",
#     trisomy_8  = "(?<![0-9A-Za-z])\\+\\s*8(?=($|[,/\\]]))",
#     inv_3_t_3_3= "(?i)\\binv\\s*\\(\\s*3\\s*\\)|\\bt\\s*\\(\\s*3\\s*;\\s*3\\s*\\)"
#   )
# 
#   # Ensure no empty names
#   nm <- names(pattern_lst)
#   if (any(nm == "")) {
#     empties <- which(nm == "")
#     nm[empties] <- paste0("pattern_", empties)
#     names(pattern_lst) <- nm
#   }
#   pattern_lst
# }
# 

extract_somies <- function(abn, type = c("mono", "tri"), sex=TRUE) {
  type <- match.arg(type)
  if (is.na(abn) || abn == "") return(character(0))
  
  # Split abnormalities into parts using commas
  parts <- unlist(strsplit(abn, ","))
  
  # Pattern for isolated whole-chromosome monosomy or trisomy
  pat     <- if_else(type == "mono", "-", "\\+")
  sex_pat <- if_else(sex, '|X|Y', '')
  pat     <- glue("(?i)^{pat}(\\d+{sex_pat})$")
  
  # Return only those that match the pattern exactly
  parts[grepl(pat, parts)]
}

clone_ancestory <- function(clone_df) {
  library(dplyr)
  library(stringr)
  
  stopifnot("cloneid"  %in% names(clone_df))
  stopifnot("clonetype" %in% names(clone_df))
  
  n <- nrow(clone_df)
  parentid <- integer(n)

  for (i in seq_len(n)) {
    type_i <- clone_df$clonetype[i]
    
    if (i == 1 || type_i == "root" || type_i == "") {
      # root clone
      parentid[i] <- NA_integer_

    } else if (type_i == "sl") {
      # child of last root
      candidates <- which(clone_df$clonetype[1:(i-1)] == "root")
      parent_idx <- max(candidates)
      parentid[i] <- clone_df$cloneid[parent_idx]

    } else if (str_detect(type_i, "^sdl1$")) {
      # first sideline child of sl
      candidates <- which(clone_df$clonetype[1:(i-1)] == "sl")
      parent_idx <- max(candidates)
      parentid[i] <- clone_df$cloneid[parent_idx]

    } else if (str_detect(type_i, "^sdl\\d+$")) {
      # other sidelines branch to last root
      candidates <- which(clone_df$clonetype[1:(i-1)] == "root")
      parent_idx <- max(candidates)
      parentid[i] <- clone_df$cloneid[parent_idx]

    } else {
      # fallback: previous row
      parent_idx <- i - 1
      parentid[i] <- clone_df$cloneid[parent_idx]
    }
  }
  
  clone_df |> mutate( parentid = parentid )
}

inherit_sex <- function(df) {
  # Create an output vector initialized with existing values
  sex_out <- df$sex_chr
  
  for (i in seq_len(nrow(df))) {
    if (!is.na(sex_out[i]) && sex_out[i] != "") next
    
    parent_id <- df$parentid[i]
    
    # Traverse up the parent chain
    while (!is.na(parent_id)) {
      parent_idx <- which(df$cloneid == parent_id)[1]
      if (is.na(parent_idx)) break
      
      val <- df$sex_chr[parent_idx]
      if (!is.na(val) && val != "") {
        sex_out[i] <- val
        break
      }
      parent_id <- df$parentid[parent_idx]
    }
  }
  
  sex_out
}

get_struct_regex <- function() {
  events <- c(
    "del", "dup", "ins", "trp", "inv",
    "t", "rob", "r", "i", "der",
    "dic", "idic", "psu\\s*dic",
    "add", "mar"
  )
  ev_pat <- str_c(events, collapse = "|")

  #  \\b       = word-boundary
  #  (?:…)     = non-capturing group of event names
  #  \\(…\\)   = one parentheses block
  #  (?:\\(…\\))*  = zero or more *additional* parentheses blocks
  pattern <- str_c(
    "(?i)\\b(?:", ev_pat, ")",
    "\\([^)]*\\)",        # first parentheses
    "(?:\\([^)]*\\))*"    # any extra ones
  )

  pattern
}

collapse_chromosomes <- function(x, delim_pattern = ",\\s*") {
  # Use collapse_values to get a cleaned vector (deduplicated, trimmed, unsorted)
  # We call collapse_values with sort="none" to leverage its splitting & deduplication.
  # To get the intermediate vector, we'll perform steps manually below for clarity.
  
  # Flatten list inputs and handle basic cases
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) x <- unlist(x)
  
  # Split and clean similar to collapse_values()
  if (length(x) > 1) {
    x <- x[!is.na(x)]
    vect <- trimws(as.character(x))
  } else {
    if (is.na(x) || x == "") {
      return(NA_character_)
    }
    vect <- trimws(unlist(strsplit(as.character(x), delim_pattern)))
  }
  vect <- vect[nzchar(vect)]
  if (length(vect) == 0) {
    return(NA_character_)
  }
  vect <- unique(vect)  # always deduplicate for chromosomes
  
  # Custom sorting:
  # Separate sex chromosomes vs numeric chromosomes
  # (Assume entries are like +/-X, +/-Y, or +/-<number>)
  base_val <- gsub("^[+-]", "", vect)        # strip leading sign
  is_sex   <- base_val %in% c("X", "Y")      # identify X or Y entries
  sex_vals <- vect[is_sex]
  num_vals <- vect[!is_sex]
  
  # Sort sex chromosome values in the order: -X, -Y, +X, +Y
  sex_sorted <- character(0)
  if (length(sex_vals) > 0) {
    # Ensure each sex chromosome has an explicit sign for ordering
    sex_vals_mod <- sex_vals
    sex_vals_mod[sex_vals_mod == "X"] <- "+X"
    sex_vals_mod[sex_vals_mod == "Y"] <- "+Y"
    # Create a factor with levels in the desired order
    ord_levels <- c("-X", "-Y", "+X", "+Y")
    sex_factor <- factor(sex_vals_mod, levels = ord_levels)
    sex_sorted <- sex_vals[order(sex_factor)]  # order sex_vals by the factor ranks
  }
  
  # Sort numeric chromosome values by absolute value (ignore sign), then by sign
  num_sorted <- character(0)
  if (length(num_vals) > 0) {
    # Extract numeric part and sign
    numeric_part <- suppressWarnings(as.numeric(gsub("^[+-]", "", num_vals)))
    sign_flag    <- substr(num_vals, 1, 1) != "-"   # TRUE for + or no sign, FALSE for -
    # Order by numeric_part ascending, then sign_flag (FALSE comes before TRUE, so '-' before '+')
    num_order <- order(numeric_part, sign_flag)     # uses multiple keys to break ties:contentReference[oaicite:5]{index=5}
    num_sorted <- num_vals[num_order]
  }
  
  # Combine sex chromosomes first, then numeric
  sorted_vals <- c(sex_sorted, num_sorted)
  # Collapse into a single string using the general function for consistency
  return( collapse_values(sorted_vals, dedup = FALSE, sort = "none") )
}


add_inheritance <- function(df, rootid = 1, inherit_col = NULL, separator = "/") {
  
  # Error checking for valid inherit_col
  if (is.null(inherit_col)) {
    stop("The 'inherit_col' parameter cannot be NULL. Please provide a valid column name.")
  }
  if (!inherit_col %in% names(df)) {
    stop(paste("The column", inherit_col, "does not exist in the dataframe."))
  }
  
  # Output column name based on inherit_col
  output_col <- paste0(inherit_col, "_inheritance")
  
  # Internal lookup tables
  inherit_lookup <- setNames(df[[inherit_col]], df$cloneid)
  parent_lookup  <- setNames(df$parentid, df$cloneid)
  
  build_inheritance <- function(cloneid) {
    parentid <- parent_lookup[as.character(cloneid)]
    if (is.na(parentid)) {
      return(inherit_lookup[as.character(cloneid)])
    } else {
      return(paste0(build_inheritance(parentid), separator, inherit_lookup[as.character(cloneid)]))
    }
  }
  
  df[[output_col]] <- vapply(df$cloneid, build_inheritance, character(1))
  
  return(df)
}

#' Create/refresh get_empty_clone() and self-clean old snapshots
#'
#' @param x            data.frame/tibble whose 0-row prototype you want
#' @param starttime    session key (string) to avoid duplicate writes in one run
#' @param r_dir        directory to write R files into (default "R")
#' @param cache_file   cache file storing last session + schema signature
#' @param silent       suppress messages
#' @param keep_last    keep at most this many timestamped backups
#' @param max_age_days delete backups older than this many days (Inf = keep)
#' @param name         base function/file name (default "get_empty_clone")
create_empty_df_code <- function(
  x, starttime,
  r_dir = "R",
  cache_file = ".last_clone_update",
  silent = TRUE,
  keep_last = 5,
  max_age_days = 30,
  name = "get_empty_clone"
) {
  if (!is.data.frame(x) || nrow(x) == 0) return(invisible(FALSE))
  if (!dir.exists(r_dir)) dir.create(r_dir, recursive = TRUE)

  out_file   <- file.path(r_dir, paste0(name, ".R"))
  cache_path <- file.path(r_dir, cache_file)

  # --- compute a compact schema signature (names + typeof + class) ----
  proto <- x[0, ]
  typev <- vapply(proto, typeof,  character(1))
  clsv  <- vapply(proto, function(col) paste(class(col), collapse = "|"), character(1))
  sig_input <- paste(names(proto), typev, clsv, sep = ":", collapse = ";")
  # lightweight hash without extra deps if digest isn't installed
  if (requireNamespace("digest", quietly = TRUE)) {
    schema_sig <- digest::digest(sig_input)
  } else {
    schema_sig <- sprintf("%x", as.integer(sum(charToRaw(sig_input))) %% 2^31)
  }

  # --- read previous cache (backward-compatible: 1st line = session, 2nd = sig) ---
  last_session <- NA_character_
  last_sig     <- NA_character_
  if (file.exists(cache_path)) {
    ln <- readLines(cache_path, warn = FALSE)
    if (length(ln) >= 1) last_session <- ln[[1]]
    if (length(ln) >= 2) last_sig     <- ln[[2]]
  }

  # Skip if already processed this session or schema unchanged
  if (!is.na(last_session) && identical(starttime, last_session)) {
    if (!silent) message("🔁 Skipping (already processed this session).")
    return(invisible(TRUE))
  }
  if (!is.na(last_sig) && identical(schema_sig, last_sig)) {
    if (!silent) message("✅ Skipping (schema unchanged).")
    # still record the session so we don't re-run this session
    writeLines(c(starttime, schema_sig), cache_path, useBytes = TRUE)
    return(invisible(TRUE))
  }

  # --- write the function file (explicit return per your preference) ---
  con <- file(out_file, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  dump_expr <- capture.output(dput(proto))
  # first line gets an assignment, subsequent lines are continued as-is
  if (length(dump_expr)) {
    dump_expr[1] <- paste0("  out <- ", dump_expr[1])
    dump_expr[-1] <- paste0("  ", dump_expr[-1])
  }
  writeLines(c(
    sprintf("%s <- function() {", name),
    dump_expr,
    "  return(out)",
    "}"
  ), con, useBytes = TRUE)

  # --- make a timestamped backup, then clean old backups ---
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  snap_file <- file.path(r_dir, sprintf("%s_%s.R", name, ts))
  file.copy(out_file, snap_file, overwrite = TRUE)

  # cleanup policy: keep last N and within age
  cleanup_snapshots <- function(dir, base, keep_last, max_age_days) {
    pat <- paste0("^", base, "_\\d{8}_\\d{6}\\.R$")
    files <- list.files(dir, pattern = pat, full.names = TRUE)
    if (!length(files)) return(invisible())

    # newest first
    o <- order(file.mtime(files), decreasing = TRUE)
    files <- files[o]

    keep <- seq_len(min(keep_last, length(files)))
    drop_idx <- setdiff(seq_along(files), keep)

    if (is.finite(max_age_days)) {
      age_days <- as.numeric(difftime(Sys.time(), file.mtime(files), units = "days"))
      drop_idx <- union(drop_idx, which(age_days > max_age_days))
    }
    to_delete <- files[drop_idx]
    if (length(to_delete)) try(unlink(to_delete), silent = TRUE)
  }
  cleanup_snapshots(r_dir, name, keep_last = keep_last, max_age_days = max_age_days)

  # --- update cache (session + schema signature) ---
  writeLines(c(starttime, schema_sig), cache_path, useBytes = TRUE)

  if (!silent) message("📝 ", name, "() saved to: ", out_file)
  invisible(TRUE)
}


# create_empty_df_code__ <- function(x, starttime, r_dir = "R", cache_file = ".last_clone_update", silent=TRUE) {
# 
#   if (!is.data.frame(x) || nrow(x) == 0) return(invisible(FALSE))
# 
#   if (!dir.exists(r_dir)) dir.create(r_dir, recursive = TRUE)
# 
#   # When you’re ready to switch to package structure:
#   # Move get_empty_clone.R to inst/extdata/
#   # Replace file.path("R", ...) with system.file("extdata", ...)
#   out_file <- file.path(r_dir, "get_empty_clone.R")
#   cache_path <- file.path(r_dir, cache_file)
# 
#   last_starttime <- if (file.exists(cache_path)) readLines(cache_path, warn = FALSE) else NA_character_
# 
#   if (!is.na(last_starttime) && starttime == last_starttime) {
#     if (!silent) message("🔁 Skipping get_empty_clone() update (already processed this session).")
#     return(invisible(TRUE))
#   }
# 
#   con <- file(out_file, open = "wt")
#   writeLines("get_empty_clone <- function() {", con)
#   dump_expr <- capture.output(dput(x[0, ]))
#   writeLines(paste0("  ", dump_expr), con)
#   writeLines("}", con)
#   close(con)
# 
#   currtime <- format(Sys.time(), "%Y%m%d_%H%M%S")
#   file.copy(out_file, file.path(r_dir, glue::glue("get_empty_clone_{currtime}.R")), overwrite = TRUE)
# 
#   writeLines(starttime, cache_path)
# 
#   if (!silent) message("📝 get_empty_clone() saved to: ", out_file)
#   invisible(TRUE)
# }
# 

# df <- clone_detail
# rxlt 
get_summary <- function(df) {
  karyo_summary <- get_karyo_summary(df)
  clone_summary <- get_clone_summary(df)
  return(list(
    karyo = karyo_summary,
    clone = clone_summary
  ))
}


get_karyo_summary <- function(df) {
  # Names of abnormalities
  recurrent_abns <- names(abn_patterns(type = "recurrent"))
  allabns        <- names(abn_patterns(type = "all"))

  # Ensure we only use columns that actually exist in df
  abn_cols_present <- intersect(allabns, names(df))

  karyo <- df |>
    filter(!is.na(karyo)) |>
    group_by(karyocode, karyo, karyo_clean, karyo_hash) |>
    summarise(
      # basic karyotype-level structure
      clones             = max(cloneid, na.rm = TRUE),
      clonal_cnt         = sum(clonal == 1, na.rm = TRUE),
      normal_karyo       = as.integer(
        clonal_cnt == 1L &&
        max(cloneid, na.rm = TRUE) == 1L &&
        any(normal_section == "1", na.rm = TRUE)
      ),
      abn_clones         = sum(clonal == 1 & clone_abn_cnt > 0, na.rm = TRUE),
      max_abnormalities  = max(clone_abn_cnt, na.rm = TRUE),
      karyo_abn_cnt      = n_distinct(trimws(unlist(strsplit(na.omit(clone_abn_list), ", ")))),
      karyo_abn_list     = paste(
        unique(trimws(unlist(strsplit(na.omit(clone_abn_list), ",")))),
        collapse = ", "
      ),
      donor_clone_cnt    = sum(donor, na.rm = TRUE),
      all_chr            = paste(unique(na.omit(chr)),     collapse = " | "),
      all_sex_chr        = paste(unique(na.omit(sex_chr)), collapse = " | "),
      all_cells_observed = sum(cells_observed, na.rm = TRUE),
      monosomies         = collapse_chromosomes(paste(monosomies,   collapse = ", ")),
      trisomies          = collapse_chromosomes(paste(trisomies,    collapse = ", ")),
      mono_no_sex        = collapse_chromosomes(paste(mono_no_sex,  collapse = ", ")),
      tri_no_sex         = collapse_chromosomes(paste(tri_no_sex,   collapse = ", ")),
      # counts of clonal clones with each abnormality (what REDCap wants)
      across(
        all_of(abn_cols_present),
        ~ sum(clonal == 1 & .x > 0, na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) |>
    mutate(
      karyo_abn_list = if_else(karyo_abn_list == "", NA_character_, karyo_abn_list)
    )

  # chromosome 17 summaries (already ok)
  karyo_17_summary <- df |>
    group_by(karyocode) |>
    summarise(
      any_chr17_abn     = any(clonal == 1 & has_chr17_abn     == 1, na.rm = TRUE),
      any_17p_loss_like = any(clonal == 1 & has_17p_loss_like == 1, na.rm = TRUE),
      .groups = "drop"
    )

  karyo |>
    left_join(karyo_17_summary, by = "karyocode")
}


get_karyo_summary_ <- function(df) {
  # Get recurrent abnormality variable names from the names of abn_patterns()
  recurrent_abns <- names(abn_patterns(type = "recurrent"))
  allabns <- names(abn_patterns(type='all'))

  overwrite_sum_exprs <- set_names(
    lapply(recurrent_abns, function(x) expr(sum(!!sym(x), na.rm = TRUE))),
    recurrent_abns
  )

  karyo <- df |>
    filter(!is.na(karyo)) |>
    group_by(karyocode, karyo, karyo_clean) |>
    mutate(
      clonal_cnt         = sum(clonal),
      clones             = max(cloneid),
      normal_karyo       = as.integer(sum(clonal) == 1 & normal_section == "1" & max(cloneid) == 1),
      abn_clones         = sum(if_else(clonal == 1 & clone_abn_cnt > 0, 1L, 0L)),
      max_abnormalities  = max(clone_abn_cnt),
      karyo_abn_cnt      = n_distinct(trimws(unlist(strsplit(na.omit(clone_abn_list), ", ")))),
      karyo_abn_list     = paste(
        unique(trimws(unlist(strsplit(na.omit(clone_abn_list), ",")))),
        collapse = ", "
      ),
      donor_clone_cnt    = sum(donor),
      all_chr            = paste(unique(na.omit(chr)), collapse = " | "),
      all_sex_chr        = paste(unique(na.omit(sex_chr)), collapse = " | "),
      all_cells_observed = sum(cells_observed),
      monosomies         = collapse_chromosomes(monosomies),
      trisomies          = collapse_chromosomes(trisomies),
      mono_no_sex        = collapse_chromosomes(mono_no_sex),
      tri_no_sex         = collapse_chromosomes(tri_no_sex),
      !!!overwrite_sum_exprs
    ) |>
    ungroup() |>
    mutate(
      karyo_abn_list = if_else(karyo_abn_list == "", NA_character_, karyo_abn_list)
    ) |>
    select(
      karyocode,
      karyo,
      karyo_hash,
      karyo_clean,
      clones,
      clonal_cnt,
      normal_karyo,
      abn_clones,
      max_abnormalities,
      karyo_abn_list,
      karyo_abn_cnt,
      donor_clone_cnt,
      all_chr,
      all_sex_chr,
      all_cells_observed,
      monosomies,
      mono_no_sex,
      trisomies,
      tri_no_sex,
      any_of(allabns)
    ) |>
    distinct()

  # summarize the chromosome 17 abnormalities at the karyotype level
  karyo_17_summary <- df |>
    group_by(karyocode) |>
    summarise(
      any_chr17_abn     = any(has_chr17_abn == 1, na.rm = TRUE),
      any_17p_loss_like = any(has_17p_loss_like == 1, na.rm = TRUE),
      .groups = "drop"
    )

  # Attach the chromosome 17 abnormalities to the existing karyo summary
  karyo <- karyo |>
    left_join(karyo_17_summary, by = "karyocode")

  return(karyo)
}


get_karyo_summary__ <- function(df) {
  
  # Get recurrent abnormality variable names from the names of abn_patterns()
  recurrent_abns <- names(abn_patterns(type='recurrent'))

  # columns like t_8_21 will summarize the karyotype rather than the clone
  overwrite_sum_exprs <- set_names(
    lapply(recurrent_abns, function(x) expr(sum(!!sym(x), na.rm = TRUE))),
    recurrent_abns
  )

  karyo <- df |>
    filter(!is.na(karyo)) |>
    group_by(karyocode, karyo, karyo_clean) |>
    mutate( clonal_cnt         = sum(clonal)
          , clones             = max(cloneid)
          , normal_karyo       = as.integer(sum(clonal) == 1 & normal_section == '1' & max(cloneid)==1)
          , abn_clones         = sum(if_else(clonal == 1     & clone_abn_cnt > 0, 1, 0))
          , max_abnormalities  = max(clone_abn_cnt)
          , karyo_abn_cnt      = n_distinct(trimws(unlist(strsplit(na.omit(clone_abn_list), ", ")))),
          , karyo_abn_list     = paste( unique(trimws(unlist(strsplit(na.omit(clone_abn_list), ",")))), 
                                        collapse = ", ")
          , donor_clone_cnt    = sum(donor)
          , all_chr            = paste(unique(na.omit(chr)), collapse=' | ')
          , all_sex_chr        = paste(unique(na.omit(sex_chr)), collapse=' | ')
          , all_cells_observed = sum(cells_observed)
          , monosomies         = collapse_chromosomes(monosomies)
          , trisomies          = collapse_chromosomes(trisomies)
          , mono_no_sex        = collapse_chromosomes(mono_no_sex)
          , tri_no_sex         = collapse_chromosomes(tri_no_sex)
          , !!!overwrite_sum_exprs # Overwrite original with sum
          # , any_17p = sum(any_17p)
    ) |>
    ungroup() |>  
    mutate(karyo_abn_list = if_else(karyo_abn_list == '', NA_character_, karyo_abn_list)) |>
    select( karyocode
          , karyo
          , karyo_hash
          , karyo_clean
          # , complex_karyo
          # , monosomial_karyo
          , clones            # cnt clones (includes non-clonal)
          , clonal_cnt        # cnt clonal (excludes non-clonal)
          , normal_karyo      # 1/0 1 = exactly 1 normal-clonal
          , abn_clones        # cnt abnormal-clonal
          , max_abnormalities # abnormalities found in the clone with the most abnormalities
          , karyo_abn_list    # list of abnormalities in the karyotype
          , karyo_abn_cnt     # cnt distinct abnormalities in the karyotype
          , donor_clone_cnt   # cnt donor derived clones
          , all_chr
          , all_sex_chr
          , all_cells_observed
          , monosomies
          , mono_no_sex
          , trisomies
          , tri_no_sex
          , any_of(all_abns)
          # , any_17p
    ) |> distinct()
  
  
  # new
  # summarize the chromosome 17 abnormalities
  karyo_17_summary <- df |>
      group_by(karyocode) |>
      summarise(
        any_chr17_abn     = any(has_chr17_abn == 1, na.rm = TRUE),
        any_17p_loss_like = any(has_17p_loss_like == 1, na.rm = TRUE),
        .groups = "drop")

  # Attach the chromosome 17 abnormalities to the existing karyo summary
  karyo <- karyo |> left_join(karyo_17_flags, by = "karyocode")
  # end new
  
  return(karyo)
}

get_clone_summary <- function(df) {
  if (FALSE) {
    df <- karyo
  }

  exclude <- "mono_no_sex, monosomies, mono_cnt, mono_no_sex_cnt, tri_no_sex, trisomies, tri_cnt, tri_no_sex_cnt, sideline_text"
  exclude <- strsplit(exclude, ",\\s*")[[1]]

  all_abns       <- names(abn_patterns(type = "all"))
  # recurrent_abns <- names(abn_patterns(type = "recurrent"))
  # mono_cols      <- setdiff(grep("^mono", names(df), value = TRUE), recurrent_abns)
  # tri_cols       <- setdiff(grep("^tri",  names(df), value = TRUE), recurrent_abns)
  # cols_to_rename <- unique(c(all_abns, mono_cols, tri_cols))

  clone_summary <- df |>
    rowwise() |>
    mutate(
      redcap_repeat_instrument = "clone",
      redcap_repeat_instance   = cloneid,
      monosomies_vec           = list(str_split(monosomies, ",\\s*")[[1]]),
      monosomies_clone         = paste(sort(unique(monosomies_vec)), collapse = ", "),
      mono_cnt_clone           = length(unique(monosomies_vec)),
      trisomies_vec            = list(str_split(trisomies, ",\\s*")[[1]]),
      trisomies_clone          = paste(sort(unique(trisomies_vec)), collapse = ", "),
      tri_cnt_clone            = length(unique(trisomies_vec)),
      clone_order              = paste(karyocode, cloneid, sep = "_")
    ) |>
    ungroup() |>  # drop rowwise before select/rename
    select(
      karyocode,
      redcap_repeat_instrument,
      redcap_repeat_instance,
      cloneid,
      everything(),
      -any_of(exclude),
      -ends_with(c("karyo", ".summ", "_vec")),
      -starts_with("karyo_")
    ) |>
    rename_with(~ paste0(., "_clone"), .cols = any_of(all_abns))

  return(clone_summary)
}


abnormal_17p <- function(df, ret='detail', require_clonal=0) {
  if (FALSE) {
    df = clone
    require_clonal = 1
    ret='detail'
  }

  require_clonal <- if (is.null(require_clonal)) 0 else require_clonal

  rx_brackets     <- "\\[[^\\]]*\\]"
  rx_year         <- "2017"
  rx_chr17        <- "(^|[^[:alnum:]])\\s*17\\s*([^[:alnum:]]|$)"
  rx_chr17context <- "(^|.{0,6}[^[:alnum:]])\\s*17\\s*([^[:alnum:]].{0,6}|$)"

  # Labels from metadata: names = *_clone columns, values = pretty labels
  reason_labels <- abn_names(type = "abn17p", by = "clone_col", active_only = TRUE)
  # the columns that are abn17p AND in the data
  cols_17 <- intersect(names(df), names(reason_labels))

  browser()

  review_df <- df |>
    mutate(
      karyo   = karyoroot,
      include = case_when(
        require_clonal==0             ~ 1L,
        require_clonal==1 & clonal==1 ~ 1L,
        require_clonal==1 & clonal==0 ~ 0L,
        .default = NA_integer_
      )
    ) |>
    filter(include==1L) |>
    mutate(
      karyo_strip     = stringr::str_remove_all(coalesce(karyo, ""), rx_brackets),
      karyo_strip     = stringr::str_remove_all(karyo_strip, rx_year),
      clone_strip     = stringr::str_remove_all(coalesce(clone, ""), rx_brackets),
      clone_strip     = stringr::str_remove_all(clone_strip, rx_year),
      contains17      = stringr::str_detect(karyo_strip, rx_chr17),
      n_contains_17   = stringr::str_count(karyo_strip,  rx_chr17),
      abn_contains_17_karyo_mch = stringr::str_extract_all(karyo_strip, rx_chr17context),
      abn_contains_17_clone_mch = stringr::str_extract_all(clone_strip, rx_chr17context)
    ) |>
    select(
      contains17, n_contains_17,
      any_of(cols_17),
      any_of(c("clone_order","karyocode","karyo","clone",
                      "clone_inheritance","karyo_clean")),
      everything()
    )

  browser()
  
  # counts + hits
  if (length(cols_17) > 0) {
    # boolean matrix of *_clone flags
    flag_mat <- as.matrix(select(review_df, any_of(cols_17)))
    storage.mode(flag_mat) <- "integer"

    # for each row, collect all *_clone columns that fired (== 1L)
    hits_cols <- apply(flag_mat, 1, function(v) {
      keep <- which(!is.na(v) & v == 1L)
      if (length(keep) == 0) character(0) else colnames(flag_mat)[keep]
    })

    if (!is.list(hits_cols)) hits_cols <- as.list(hits_cols)

    # map to pretty labels via abn_names(); fallback to column name if missing in catalog
    hits_labels <- lapply(hits_cols, function(cols) {
      if (!length(cols)) return(character(0))
      unname(recode(cols, !!!reason_labels, .default = cols))
    })
  
    hits_str <- vapply(hits_labels,
                       function(x) if (length(x)) paste(x, collapse = " | ") else "",
                       character(1))
  
    review_df$abn17_hits <- hits_str
  } else {
    review_df <- review_df |> mutate(abn17_hits = "")
  }

  review_summary <- review_df |>
    select(abn_contains_17_clone_mch) |>
    distinct()

  # keep your original strip-removal (unchanged)
  review_df <- review_df |> select(abn17_hits, everything(), -ends_with(c("_mch", "_strip")))

  if (ret == 'detail')  return(review_df)
  if (ret == 'summary') return(review_summary)
  return(NULL)
}
