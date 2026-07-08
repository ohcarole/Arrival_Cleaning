knitr::opts_chunk$set(message = FALSE, results = 'hide', fig.show = 'asis')
tryCatch({
  # Attempt to set the root using rprojroot
  knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
}, error = function(e) {
  # If an error occurs (e.g., no project root found), fall back to using the current working directory
  warning("No RStudio project found. Falling back to the current working directory.")
  knitr::opts_knit$set(root.dir = getwd())  # Set root to current working directory
})

# User message for adding future directory paths
library(crayon)
msg <- red$bold('NOTE: You can add global dirs in "Knitting and Pathing Setup" of icicle_functions.Rmd')
message('\n', msg, '\n')

onedir      <<- gsub("\\\\", "/", Sys.getenv('OneDrive', unset = NA))
codedir     <<- gsub("\\\\", "/", Sys.getenv('R_CODE', unset = NA))
projdir     <<- rprojroot::find_rstudio_root_file()
compdir     <<- paste0(projdir, '/R') # project complile directcory
metadir     <<- paste0(projdir, '/inst/extdata') # project ini directory
scrapdir    <<- paste0(onedir, '/Scrap')
qcdir       <<- paste0(onedir,  '/Patient Data Entry/Quality Control')
rcimportdir <<- paste0(onedir,  '/Patient Data Entry/REDCap Imports')
datadir     <<- paste0(onedir,  '/data')

set_packages <- function(silent=TRUE) {
  
  #
  # expects the libraries below to be pre-loaded in icicle_setup.R
  # 
  # library(fs)
  # library(crayon)
  # library(dplyr)
  # library(magrittr)
  # library(safer)
  # library(remotes)

  # contained function
  is_installed <- function(pkg) pkg %in% rownames(installed.packages())
  
  # 
  # Variables
  # 
  
  # assures that upgrades always happen when installing a remotes package
  Sys.setenv(R_REMOTES_UPGRADE = "always")
  
  # Initialize a vector to collect names of packages that fail to load
  failed <- character()
  
  # List of required packages
  packages <- c(
    # Core Packages
    "fs", "ini", "R6", "glue", "rstudioapi", "yaml", "conflicted", "devtools",
    # RStudio environment related
    "shrtcts",
    # Database-related
    "DBI", "odbc", "RMariaDB",
    # Stats related
    "table1",
    # Optimization / Matching
    "clue",
    # Text cleaning
    "textutils",
    # Configuration and Encryption
    "ConfigParser", "safer", "keyring",
    # Data Manipulation
    "tidyverse", "dbplyr", "janitor", "lubridate", "data.table", "fuzzyjoin",
    # Parallel Processing
    "furrr", "future",
    # Diagnostic
    "tictoc",
    # REDCap-related
    "REDCapR",
    # Data Import/Export and Utility
    "openxlsx", "readr", "jsonlite", "digest",
    # Display Related
    "knitr", "kableExtra", "progress", "crayon", "consort", 
    "ggplot2", "reshape2", "pheatmap", "gt", "webshot2", "tableone",
    # html related
    "httr", 
    # Send Email
    # depreciated "mailR", 
    "blastula"
  )
  
  # used to test installation of missing packages, removing these so we can reinstall
  if (FALSE) remove.packages(c('ini', 'consort', 'progress'))

  # Skipping install of shrtcts to see if anything crashes
  # Enabling needed for open_file_folder() function
  if (TRUE) {
  # Install shrtcts from GitHub if missing
  if (!is_installed("shrtcts")) {
      message("Installing missing GitHub package: shrtcts")
      remotes::install_github("gadenbuie/shrtcts", upgrade = "never")
    }
  
    # Ensure shrtcts is available in the package list
    packages <- c(packages, "shrtcts")
    
  }
  
  # Install everything else from CRAN (or your repos)
  missing <- packages[!vapply(packages, is_installed, logical(1))]  


  # Install missing packages, kinda like downloading the software
  # only needs to happen once per computer
  for (pkg in missing) {
    if (pkg == "shrtcts") next
    message("Installing missing package: ", pkg)
    tryCatch(
      install.packages(pkg, dependencies = TRUE),
      error = function(e) failed <<- c(failed, pkg)
    )
  }

  # # Install missing packages
  # missing_n = length(missing)
  # for(pkg in missing) {
  #   message("Installing missing package: ", pkg)
  #   if (missing_n>0) {
  #     tryCatch({
  #       install.packages(pkg, dependencies = TRUE)
  #       missing_n = missing_n-1
  #     }, error = function(e) {
  #       failed <<- c(failed, pkg)
  #     })
  #   }
  # }

  
  # Load libraries with error handling
  for(pkg in packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      failed <<- c(failed, pkg)
    })
  }
  
  # remember the packages loaded
  present <- packages[vapply(packages, is_installed, logical(1))]
  .pkg_env$package <- present  
  
  # if (!exists(".pkg_env", inherits = FALSE) || !is.environment(.pkg_env)) {
  # .pkg_env <- new.env(parent = emptyenv())
  # }

  # A tibble of your preferred winners -----------------------------------
  conflict_prefs <- tibble::tribble(
      ~fun,              ~pkg,
      "filter",          "dplyr",
      "first",           "dplyr",
      "lag",             "dplyr",
      "set_names",       "purrr",
      "%||%",            "purrr",
      "wday",            "lubridate",
      "month",           "lubridate",
      "year",            "lubridate",
      "str_replace_all", "stringr",
      "regex",           "stringr",
      "fixed",           "stringr")
  
  # .pkg_env$conflict_prefs <- conflict_prefs
  
  for (i in seq_len(nrow(conflict_prefs))) {
    f <- as.character(conflict_prefs$fun[i])
    p <- as.character(conflict_prefs$pkg[i])
    conflicted::conflict_prefer(f, p, quiet = TRUE)
  }
  
  
  
  # Manage package conflicts
    conflicted::conflict_prefer("filter",          "dplyr",     quiet = TRUE)
    conflicted::conflict_prefer("first",           "dplyr",     quiet = TRUE)
    conflicted::conflict_prefer("lag",             "dplyr",     quiet = TRUE)
    conflicted::conflict_prefer("set_names",       "purrr",     quiet = TRUE)
    conflicted::conflict_prefer("%||%",            "purrr",     quiet = TRUE)
    conflicted::conflict_prefer("wday",            "lubridate", quiet = TRUE)
    conflicted::conflict_prefer("month",           "lubridate", quiet = TRUE)
    conflicted::conflict_prefer("year",            "lubridate", quiet = TRUE)
    conflicted::conflict_prefer("str_replace_all", "stringr",   quiet = TRUE)
    conflicted::conflict_prefer("regex",           "stringr",   quiet = TRUE)
    conflicted::conflict_prefer("fixed",           "stringr",   quiet = TRUE)
  
  # Report packages that failed to load
  if(length(failed) > 0) {
    cat("These are the packages that did not load: ", paste(failed, collapse = ", "), "\n")
  }
}

extract_function_chunks <- function(file_path) {
  # Require namespaces for the qualified calls we use
  if (!requireNamespace("tibble",  quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  if (!requireNamespace("dplyr",   quietly = TRUE)) stop("Package 'dplyr' is required.")
  
  if (!base::file.exists(file_path)) return(tibble::tibble())
  
  lines <- base::readLines(file_path, warn = FALSE)
  chunks <- base::list()
  modified_time <- base::file.info(file_path)$mtime

  is_rmd <- base::grepl("\\.Rmd$", file_path, ignore.case = TRUE)

  # Rmd chunk state
  in_chunk    <- FALSE
  chunk_runs  <- TRUE            # eval != FALSE
  fence_ticks <- ""

  i <- 1L
  pending_description <- base::character()

  starts_chunk <- function(s) {
    m <- regexec("^\\s*(`{3,})(?:\\s*)\\{\\s*[rR][^}]*\\}\\s*$", s)
    regmatches(s, m)[[1]]
  }
  chunk_has_eval_false <- function(s) {
    m <- regexec("\\{\\s*[rR]([^}]*)\\}", s)
    opt <- regmatches(s, m)[[1]]
    if (length(opt) < 2) return(FALSE)
    grepl("(?i)\\beval\\s*=\\s*(false|f|0)\\b", opt[[2]], perl = TRUE)
  }
  ends_chunk <- function(s) {
    if (fence_ticks == "") return(FALSE)
    grepl(paste0("^\\s*", fence_ticks, "\\s*$"), s)
  }

  while (i <= length(lines)) {
    line <- lines[i]

    # Rmd fence management
    if (is_rmd) {
      if (!in_chunk) {
        sc <- starts_chunk(line)
        if (length(sc) > 1) {
          in_chunk    <- TRUE
          fence_ticks <- sc[[2]]
          chunk_runs  <- !chunk_has_eval_false(line)  # TRUE means eval happens
          i <- i + 1L
          next
        }
      } else if (ends_chunk(line)) {
        in_chunk    <- FALSE
        chunk_runs  <- TRUE
        fence_ticks <- ""
        i <- i + 1L
        next
      }
    }

    # We only parse functions:
    # - in .R files (everywhere)
    # - in .Rmd files, *inside* chunks (regardless of eval)
    in_parsable_region <- (!is_rmd) || (is_rmd && in_chunk)

    if (in_parsable_region &&
        grepl("^\\s*([a-zA-Z0-9_\\.]+)\\s*<-\\s*function\\s*\\(", line)) {

      fun_name <- sub("^\\s*([a-zA-Z0-9_\\.]+)\\s*<-\\s*function\\s*\\(.*", "\\1", line)

      sig_lines   <- line
      i <- i + 1L
      open_parens <- stringr::str_count(line, "\\(")
      close_parens<- stringr::str_count(line, "\\)")
      found_brace <- grepl("\\{", line)

      while ((open_parens > close_parens || !found_brace) && i <= length(lines)) {
        if (is_rmd && in_chunk && ends_chunk(lines[i])) break
        next_line    <- lines[i]
        sig_lines    <- c(sig_lines, next_line)
        open_parens  <- open_parens  + stringr::str_count(next_line, "\\(")
        close_parens <- close_parens + stringr::str_count(next_line, "\\)")
        if (!found_brace) found_brace <- grepl("\\{", next_line)
        i <- i + 1L
      }

      full_signature <- paste(sig_lines, collapse = " ")
      param_text <- sub("^.*<-\\s*function\\s*\\((.*)\\)\\s*\\{.*$", "\\1", full_signature)

      chunks[[length(chunks) + 1L]] <- tibble::tibble(
        function_name = fun_name,
        signature     = paste0(fun_name, "(", param_text, ")"),
        description   = paste(pending_description, collapse = "\n"),
        modified      = modified_time,
        instantiate   = if (!is_rmd) TRUE else chunk_runs  # FALSE for eval=FALSE chunks
      )

      pending_description <- character()

    } else {
      # Accumulate description lines from code regions only:
      if (!is_rmd || (is_rmd && in_chunk)) {
        pending_description <- c(pending_description, line)
      }
      i <- i + 1L
    }
  }

  if (length(chunks) == 0) return(tibble::tibble())
  dplyr::bind_rows(chunks)
}

# extract_function_chunks <- function(file_path) {
#   if (!file.exists(file_path)) return(tibble::tibble())
# 
#   lines <- readLines(file_path, warn = FALSE)
#   chunks <- list()
#   modified_time <- file.info(file_path)$mtime
# 
#   i <- 1
#   pending_description <- c()
# 
#   while (i <= length(lines)) {
#     line <- lines[i]
# 
#     # Detect start of a function definition
#     if (grepl("^\\s*([a-zA-Z0-9_\\.]+)\\s*<-\\s*function\\s*\\(", line)) {
#       fun_name <- sub("^\\s*([a-zA-Z0-9_\\.]+)\\s*<-\\s*function\\s*\\(.*", "\\1", line)
# 
#       sig_lines <- line
#       i <- i + 1
#       open_parens <- stringr::str_count(line, "\\(")
#       close_parens <- stringr::str_count(line, "\\)")
#       found_brace <- grepl("\\{", line)
# 
#       while ((open_parens > close_parens || !found_brace) && i <= length(lines)) {
#         next_line <- lines[i]
#         sig_lines <- c(sig_lines, next_line)
#         open_parens <- open_parens + stringr::str_count(next_line, "\\(")
#         close_parens <- close_parens + stringr::str_count(next_line, "\\)")
#         if (!found_brace) found_brace <- grepl("\\{", next_line)
#         i <- i + 1
#       }
# 
#       full_signature <- paste(sig_lines, collapse = " ")
#       param_text <- sub("^.*<-\\s*function\\s*\\((.*)\\)\\s*\\{.*$", "\\1", full_signature)
# 
#       chunks[[length(chunks) + 1]] <- tibble::tibble(
#         function_name = fun_name,
#         signature     = paste0(fun_name, "(", param_text, ")"),
#         description   = paste(pending_description, collapse = "\n"),
#         modified      = modified_time
#       )
# 
#       pending_description <- c()
#     } else {
#       pending_description <- c(pending_description, line)
#       i <- i + 1
#     }
#   }
# 
#   if (length(chunks) == 0) return(tibble::tibble())
# 
#   bind_rows(chunks)
# }
# 

#' Build or load the setup registry
#'
#' @param rmd_dir  Directory containing .Rmd and (optionally) an R/ subdir.
#' @param rebuild  Logical. If TRUE, completely recreate the CSV; if FALSE (default),
#'                 try to read an existing CSV and only rebuild if it is missing or invalid.
#' @param silent   Suppress messages.
#' @param ...      Back-compat: if a legacy `reload` argument is supplied, it will be mapped to `rebuild`.
build_setup_registry <- function(rmd_dir = .pkg_env$codedir, rebuild = FALSE, silent = TRUE, ...) {
  # --- Backward compatibility for legacy `reload` argument passed via ... ---
  dots <- list(...)
  if ("reload" %in% names(dots)) {
    if (!silent) warning("`reload` is deprecated; use `rebuild`. Treating `reload = TRUE` as `rebuild = TRUE`.")
    rebuild <- isTRUE(dots$reload)
  }

  csvfile <- file.path(rmd_dir, "setup_registry.csv")

  # --- Fast path: read cached CSV when not rebuilding ---
  if (!rebuild && file.exists(csvfile)) {
    rslt <- tryCatch({
      rslt <- read.csv(csvfile, stringsAsFactors = FALSE)

      # Normalize column types
      if ("required" %in% names(rslt))       rslt$required       <- as.logical(rslt$required)
      if ("instantiate_me" %in% names(rslt)) rslt$instantiate_me <- as.logical(rslt$instantiate_me)

      # Parse modified -> POSIXct if needed
      if ("modified" %in% names(rslt) && is.character(rslt$modified)) {
        # Best-effort parse; keep original if parsing fails
        parsed <- try(suppressWarnings(as.POSIXct(rslt$modified, tz = "UTC")), silent = TRUE)
        if (!inherits(parsed, "try-error")) rslt$modified <- parsed
      }

      # Minimal schema check; rebuild if clearly wrong
      needed <- c("filename", "signature", "function_name", "required", "instantiate_me", "modified")
      if (!all(needed %in% names(rslt))) stop("Cached registry missing required columns.")

      rslt
    }, error = function(e) {
      if (!silent) message("Cached registry could not be used: ", e$message, " â€” rebuilding.")
      NULL
    })

    if (!is.null(rslt)) return(rslt)
  } else if (rebuild && !silent) {
    message("Rebuilding registry from sources (rebuild = TRUE).")
  }

  # --- Rebuild path ---
  # Collect .Rmd files and base names
  rmd_files <- list.files(rmd_dir, pattern = "\\.Rmd$", full.names = TRUE)
  rmd_base  <- tools::file_path_sans_ext(basename(rmd_files))

  # Get all .R files in R/ subfolder, excluding those whose base name matches an .Rmd
  r_dir              <- file.path(rmd_dir, "R")
  r_files_all        <- if (dir.exists(r_dir)) list.files(r_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE) else character()
  r_files_base       <- tools::file_path_sans_ext(basename(r_files_all))
  r_files_to_include <- r_files_all[!(r_files_base %in% rmd_base)]

  # Combine and exclude files starting with a number
  all_files <- c(rmd_files, r_files_to_include)
  all_files <- all_files[!grepl("^\\d", basename(all_files))]

  # Extract
  rslt <- purrr::map_dfr(all_files, function(f) {
    funcs <- extract_function_chunks(f)
    if (is.null(funcs) || nrow(funcs) == 0) return(NULL)

    # Safe default if extractor doesn't provide `instantiate`
    inst_flag <- if ("instantiate" %in% names(funcs)) funcs$instantiate else TRUE

    tibble::tibble(
      filename        = basename(f),
      signature       = funcs$signature,
      description     = funcs$description,
      function_name   = funcs$function_name,
      required        = TRUE,
      instantiate_me  = inst_flag,
      modified        = funcs$modified
    )
  })

  if (!silent) cat("Saving registry at ", csvfile, "\n")
  utils::write.csv(rslt, file = csvfile, row.names = FALSE)

  rslt
}


# build_setup_registry <- function(rmd_dir=.pkg_env$codedir, reload = FALSE, silent = TRUE) {
# 
#   if (!reload) {
#     tryCatch ({
#       rslt <- suppressWarnings(read.csv(file.path(rmd_dir, "setup_registry.csv"), stringsAsFactors = FALSE))
#       rslt$required        <- as.logical(rslt$required)
#       rslt$instantiate_me  <- as.logical(rslt$instantiate_me)
#       class(rslt) <- "data.frame"
#       return(rslt)
#     }, error = function(e) {
#       message(" Couldn't load registry from csv, recreating\n")
#     })
#   } else {
#     message("Reloading registry from csv")
#   }
# 
#   # Step 1: Collect .Rmd files and base names
#   rmd_files <- list.files(rmd_dir, pattern = "\\.Rmd$", full.names = TRUE)
#   rmd_base  <- tools::file_path_sans_ext(basename(rmd_files))
# 
#   # Step 2: Get all .R files in R/ subfolder
#   r_dir <- file.path(rmd_dir, "R")
#   r_files_all <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
#   r_files_base <- tools::file_path_sans_ext(basename(r_files_all))
# 
#   # Step 3: Keep only .R files whose base name is NOT in .Rmd list
#   r_files_to_include <- r_files_all[!(r_files_base %in% rmd_base)]
# 
#   # Step 4: Combine all files to process and exclude those starting with a number
#   all_files <- c(rmd_files, r_files_to_include)
#   all_files <- all_files[!grepl("^\\d", basename(all_files))]
# 
#   # Step 5: Extract function chunks
#   rslt <- purrr::map_dfr(all_files, function(f) {
#     funcs <- extract_function_chunks(f)
#     if (nrow(funcs) == 0) return(NULL)
# 
#     tibble::tibble(
#       filename = basename(f),
#       signature = funcs$signature,
#       description = funcs$description,
#       function_name = funcs$function_name,
#       required = TRUE,
#       instantiate_me = TRUE,
#       modified = as.POSIXct(funcs$modified, format = "%Y-%m-%d %H:%M:%S")
#     )
#   })
# 
#   # Step 6: Save registry
#   csvfile <- file.path(rmd_dir, "setup_registry.csv")
#   if (!silent) cat(" Saving registry at ", csvfile, '\n')
#   write.csv(rslt, file = csvfile, row.names = FALSE)
# 
#   return(rslt)
# }
