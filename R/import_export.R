#' Read a CSV or (optionally encrypted) Excel sheet into a data frame
#'
#' @description
#' `get_sheet()` reads tabular data from a file path that points to either
#' a `.csv` or `.xlsx` file, with optional support for encrypted Excel workbooks.
#' It provides control over the starting row, number of rows, and verbosity,
#' and applies post-processing to convert date-like columns.
#'
#' @param filepath `character(1)`. Path to the input file. Must exist and have
#'   extension `.csv` or `.xlsx`.
#' @param encryptkey Optional `character(1)`. Encryption key or password for
#'   protected Excel files. Currently not used directly in this function, as
#'   the password is resolved via `get_encryptkey()`, but retained for
#'   interface compatibility.
#' @param startRow `integer(1)`. First row (1-based) to read from the file.
#'   Defaults to `1`.
#' @param rows `integer(1)` or `NULL`. Number of rows to read starting at
#'   `startRow`. If `NULL`, all available rows are read for Excel files and
#'   an infinite limit (`Inf`) is used for CSV. Defaults to `NULL`.
#' @param silent `logical(1)`. If `FALSE`, basic diagnostic information
#'   (file path, extension, start row, rows) is printed to the console.
#'   Defaults to `TRUE`.
#' @param test `logical(1)`. If `TRUE`, overrides `startRow`, `rows`, and
#'   `silent` with test values (`startRow = 5`, `rows = 185`, `silent = FALSE`)
#'   to facilitate quick, reproducible test reads. Defaults to `FALSE`.
#'
#' @details
#' For CSV input, the function uses [readr::read_csv()] with `n_max` equal to
#' `rows` (or `Inf` if `rows` is `NULL`). For Excel input, a plaintext password
#' is obtained via [get_encryptkey()], and `decrypt_excel()` is attempted first.
#' If passworded reading fails, `openxlsx::read.xlsx()` is used as a fallback.
#' The resulting data frame is then passed to [convert_date_cols()] to
#' normalize date columns.
#'
#' The Excel reader requires the **openxlsx** package; an informative error is
#' raised if it is not installed.
#'
#' @return
#' A `data.frame` (or tibble) containing the requested rows from the input
#' file, with date-like columns converted by `convert_date_cols()`.
#'
#' @seealso
#' [readr::read_csv()], [openxlsx::read.xlsx()], [decrypt_excel()],
#' [get_encryptkey()], [convert_date_cols()]
#'
#' @examples
#' \dontrun{
#' # Read all rows from a CSV
#' df_csv <- get_sheet("data/patients.csv")
#'
#' # Read 100 rows from an Excel sheet, starting at row 2
#' df_xlsx <- get_sheet("data/patients.xlsx", startRow = 2, rows = 100)
#'
#' # Use test mode for quick inspection
#' df_test <- get_sheet("data/patients.xlsx", test = TRUE)
#' }
get_sheet <- function(filepath,
                      encryptkey = NULL,
                      startRow   = 1,
                      rows       = NULL,
                      silent     = TRUE,
                      test       = FALSE) {

  if (test) {
    startRow   <- 5
    rows       <- 185
    silent     <- FALSE
  }

  if (!file.exists(filepath)) stop("File does not exist: ", filepath)

  ext <- tools::file_ext(filepath)
  if (is.null(rows) && ext == "csv") rows <- Inf
  if (!ext %in% c("xlsx", "csv")) stop("Invalid file type: ", ext)
  if (!silent) cat(filepath, '\n', ext, '\n', startRow, '\n', rows, '\n')

  if (ext == "csv") {
    return(readr::read_csv(filepath, show_col_types = FALSE, n_max = rows))
  }

  # xlsx path â€” resolve a usable plaintext password (env/param/cache)
  
  pw <- get_encryptkey()  # <- **plaintext or empty**

  # Ensure openxlsx is available for fallback

  if (!requireNamespace("xlsx", quietly = TRUE)) {
    install.packages("xlsx", dependencies = TRUE)
  }

  library(xlsx)
  
  row_range <- if (!is.null(rows)) startRow:(startRow + rows - 1) else NULL

  # Try passworded read first; if that fails, try without password
  df <- tryCatch({
    decrypt_excel(filepath, password = pw, startRow = startRow, rows = rows)
  }, error = function(e) {
    tryCatch({
      openxlsx::read.xlsx(filepath, sheet = 1, startRow = startRow, rows = row_range)
    }, error = function(e2) {
      stop("Failed to read Excel file: ", e2$message)
    })
  })

  return(convert_date_cols(df))
}

# get_sheet_ <- function(filepath, encryptkey = get_encryptkey(), startRow = 1, rows = NULL, silent = TRUE, test = FALSE) {
# 
#   if (test) {
#     filepath <- filepath
#     encryptkey <- get_encryptkey()
#     startRow <- 5
#     rows <- 185
#     silent <- FALSE
#   }
# 
#   if (!file.exists(filepath)) stop("File does not exist: ", filepath)
# 
#   ext <- tools::file_ext(filepath)
#   if (is.null(rows) && ext == "csv") rows <- Inf
# 
#   if (!ext %in% c("xlsx", "csv")) stop("Invalid file type: ", ext)
#   if (!silent) cat(filepath, '\n', ext, '\n', startRow, '\n', rows, '\n')
# 
#   if (ext == "csv") {
#     return(readr::read_csv(filepath, show_col_types = FALSE, n_max = rows))
#   }
# 
#   # Ensure openxlsx is available
#   if (ext == "xlsx" && !requireNamespace("openxlsx", quietly = TRUE)) {
#     stop("The 'openxlsx' package is required to read .xlsx files.")
#   }
# 
#   # Try decrypting, fall back to normal read
#   row_range <- if (!is.null(rows)) startRow:(startRow + rows-1) else NULL
#   df <- tryCatch({
#     decrypt_excel(filepath, password = encryptkey, startRow = startRow, rows = rows)
#   }, error = function(e) {
#     tryCatch({
#       openxlsx::read.xlsx(filepath, sheet = 1, startRow = startRow, rows = row_range)
#     }, error = function(e2) {
#       stop("Failed to read Excel file: ", e2$message)
#     })
#   })
# 
#   return(df)
# }
# 
# 
# 
# get_sheet_old <- function(filepath, encryptkey = get_encryptkey(), startRow = 1, rows=NULL, silent=TRUE, test=FALSE) {
# 
#   if(test) {
#     filepath <- ctmsfile
#     encryptkey <- get_encryptkey()
#     startRow   <- 5
#     rows=NULL
#     silent=FALSE
#   }
# 
#   if (!silent) print(filepath)
#   # Check if the file exists
#   if (!file.exists(filepath)) {
#     stop("File does not exist: ", filepath)
#   }
# 
#   ext <- tools::file_ext(filepath)
# 
#   if (is.null(rows) & ext=='csv') {
#     rows = Inf
#   }
# 
#   # Validate file type
#   valid_types <- c("xlsx", "csv")
#   if (!ext %in% valid_types) {
#     stop("Invalid file type. Supported types are: ", paste(valid_types, collapse = ", "))
#   }
# 
#   if (!silent) cat(filepath, '\n', ext, '\n', startRow, '\n', rows, '\n')
# 
#   # Read the file based on its type
#   if (ext == "csv") {
#     # Read the .csv file
#     df <- readr::read_csv(filepath, show_col_types = FALSE, n_max=rows)
#     return(df)
#   }
# 
#   # first attempt to with password
#   file_opened <- tryCatch({
#     df <- decrypt_excel(filepath, password=encryptkey, startRow = startRow, rows=rows)
#     TRUE
#   }, error = function(e) {
#     FALSE
#   })
# 
#   # second open without password, if first attempt failed
#   if (!file_opened) {
#       file_opened <- tryCatch({
#         df <- openxlsx::read.xlsx(filepath, sheet=1, startRow = startRow)
#         TRUE
#       }, error = function(e) {
#         FALSE
#       })
#   }
# 
#   return(df)
# }
# 
# # depreciated
# # get_excel_file <- function(filepath) {get_spreadsheet(filepath)}
# 

# library(fs)  # Make sure to load the fs package for dir_ls
# latest_file <- get_last_file_name(importdir, pattern = "(?i)all.*", ext='csv')

get_last_file_name <- function(path, ext = 'xlsx', pattern = '.*') {
  # Construct the full regex pattern
  full_pattern <- paste0('(?i).*', pattern, '.*\\.', ext, '$')
  
  # List files in the directory with the specified pattern
  files <- dir_ls(path, regexp = full_pattern)
  
  if (length(files) == 0) {
    return(NULL)  # Return NULL if no files match the pattern
  }
  
  # Get file information and sort by modification time
  file_info <- file.info(files)
  recent_file <- files[which.max(file_info$mtime)]
  
  return(recent_file)  # Return the name of the most recent file
}

# get_last_file(path=ctmsdir, startRow=5)
get_last_file <- function(path,
                          ext        = 'xlsx',
                          hasdate    = NULL,
                          pattern    = '',
                          encryptkey = NULL,
                          startRow   = 1,
                          rows       = NULL,
                          silent     = TRUE) {
  
  message(paste("silent =", silent))

  last_file <- NULL
  df <- data.frame()

  files <- dir_ls(path, type = "file", regexp = glue("(?i).*\\.{ext}$")) |> basename()
  filt_files <- files[grepl(pattern, files, perl = TRUE, ignore.case = TRUE)]
  file_paths <- path(path, filt_files)

  if (!length(file_paths)) {
    message("No files found with the specified extension.")
    return(df)
  }

  info <- file_info(file_paths)
  last_file <- info$path[which.max(info$modification_time)]
  
  
  if (!silent) {
    message(paste("Last file is:", last_file))
  }  

  # let get_sheet resolve plaintext via decrypt_encryptkey()
  df <- get_sheet(last_file, encryptkey = encryptkey, startRow = startRow, rows = rows)

  return(df)
}

get_last_file_old2 <- function(path, ext='xlsx', hasdate=NULL, pattern='', encryptkey=get_encryptkey(), startRow = 1) {
  
  if (FALSE) {
    path=ctmsdir
    startRow=5
    ext='xlsx'
    hasdate=NULL
    pattern=''
    encryptkey=get_encryptkey()
  }
  
  last_file <- NULL
  df <- data.frame()

  # List of files in directory with the specified extension
  files <- dir_ls(path, type = "file", regexp = glue("(?i).*\\.{ext}$")) |> basename()

  # Filter files based on the pattern (case insensitive)
  filt_files <- files[grepl(pattern, files, perl = TRUE, ignore.case = TRUE)]
  
  # Construct file paths
  file_paths <- path(path, filt_files)

  # Check if there are any files with the specified extension
  file_count <- length(file_paths)
  if (file_count == 0) {
    message("No files found with the specified extension.")

  } else {
    # Retrieve file info
    file_info <- file_info(file_paths)
    
    # Find the most recent file
    last_file <- file_info$path[which.max(file_info$modification_time)]
    
    # create a df
    df <- get_sheet(last_file, encryptkey, startRow = startRow)
    last_file  <- "C:/Users/cmshaw/OneDrive - Fred Hutchinson Cancer Research Center/Coding and Data Acquisition/CTMSDownloads/ProtocolSearch.xlsx"
    # encryptkey <- NULL
    startRow   <- 5
  }

  return(df)
}

get_last_file_old1 <- function(path, ext='xlsx', hasdate=NULL, encryptkey=get_encryptkey(), startRow = 1) {
  
  # List files in the directory with the specified extension
  hasdate <- if(!is.null(hasdate)) format(hasdate,'%Y%m%d')
  pattern <- paste0(hasdate, ".*\\.", ext, "$")
  files   <- dir_ls(path, regexp = pattern)
  files

  # Check if there are any files with the specified extension
  if (length(files) == 0) {
    stop("No files found with the specified extension.")
  }

  # Find the most recent file based on modification date
  file_info <- file_info(files)
  last_file <- file_info[which.max(file_info$modification_time), "path"]
  last_file <- as.character(last_file)

  df <- get_sheet(last_file, encryptkey, startRow = startRow)
  
  return(df)
}

# Function to read a password-protected excel file and return a df
# Reads a password-protected Excel file (uses xlsx backend)
decrypt_excel <- function(filepath,
                          password = NULL,
                          sheet    = 1,
                          silent   = TRUE,
                          startRow = 1,
                          rows     = NULL) {
  if (!file.exists(filepath)) stop("File does not exist: ", filepath)

  if (!requireNamespace("xlsx", quietly = TRUE)) {
    suppressMessages(
      suppressWarnings(
        install.packages("xlsx", dependencies = TRUE)
      )
    )
  }
  suppressMessages(library(xlsx))
  
  if (!silent) cat('Reading Excel file:', filepath, '\n')

  pw <- decrypt_encryptkey(password)  # <- **plaintext or empty**

  # xlsx::read.xlsx uses endRow (inclusive). Respect rows when provided.
  df <- xlsx::read.xlsx(
    file      = filepath,
    sheetIndex = sheet,
    password  = if (nzchar(pw)) pw else NULL,
    startRow  = startRow,
    endRow    = rows
  )

  df
}

decrypt_excel_old1 <- function(filepath, password = get_encryptkey(), sheet=1, silent=TRUE, startRow = 1, rows=NULL) {
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }

  library_loaded <- requireNamespace("xlsx", quietly = TRUE)
  if (!library_loaded) {
    stop("Package 'xlsx' is required but not installed")
  }
  
  if (!silent) {
    cat('Reading Excel file:', filepath, '\n')
  }
  
  df <- xlsx::read.xlsx(filepath, sheet, password = password, startRow = startRow, endRow = rows)

  if (!library_loaded) {
    detach("package:xlsx", unload=TRUE)
  }

  return(df)
}

# Create sheet xlsx or csv
make_sheet <- function(df,
                      path          = "C:/Users/cmshaw/Desktop/",
                      sheet         = "Sheet1",
                      filename      = "Results_",
                      ext           = "csv",
                      date          = TRUE,
                      time          = FALSE,
                      overwrite     = FALSE,
                      test          = FALSE,
                      null_as_blank = TRUE,
                      fix_decimals  = FALSE) {
  
  if (test) {
    test      <- TRUE
    df        <- Missing_from_Frozen
    path      <- path
    sheet     <- "Sheet1"
    filename  <- "Missing_from_Frozen"
    date      <- FALSE
    time      <- FALSE
    overwrite <- TRUE
    ext       <- "csv"
    null_as_blank <- FALSE
  }
  
  # Convert NA to "" if requested
  if (null_as_blank) {
    df <- df |>
      mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))
  }
  
  # standardize to decimal length
  if (fix_decimals) {
    df <- standardize_decimals(df)
  }
  
  
  # Generate date string if needed
  now <- if (date | time) format(Sys.time(), "_%Y%m%d_%H%M") else ""
  if (!time) {
    now <- substr(now,1,9)
  }
  
  # Ensure valid output format is provided
  ext <- match.arg(ext, c("xlsx", "csv"))
  
  # Determine outfile with path
  filepath <- dirname(filename)
  if (filepath == ".") {filepath <- path}
  outfile <- paste0(filepath, '/', basename(filename), now, ".", ext)

  
  if (ext == "csv") {write.csv(df, file = outfile, row.names = FALSE)}
  # # Update this section in make_sheet():
  # if (ext == "csv") {
  #   csv_filename <- if (sheet == "Sheet1") filename else sheet  # Use sheet name
  #   outfile <- paste0(path, '/', csv_filename, now, ".", ext)
  #   write.csv(df, file = outfile, row.names = FALSE)
  # }  

  # Ensure the openxlsx package is loaded
  if (ext == "xlsx" & !requireNamespace("openxlsx", quietly = TRUE)) {stop("openxlsx package required")}  

    
  if (ext == "xlsx") {

    # Create a new workbook and add data
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::writeData(wb, sheet, df)
    openxlsx::saveWorkbook(wb, outfile, overwrite = overwrite)  }
  
  return(outfile)
  
}
# make_sheet(test=TRUE)

write_to_file <- function(dfs, output_names = NULL, 
                          directory = getwd(), date_format = "", 
                          output_type = "xlsx", output_file = NULL, overwrite = TRUE) {
  # Ensure dfs is a list of data frames
  if (!is.list(dfs)) {
    dfs <- list(dfs)
  }
  
  # dfs = list(strong, match, review, unknown)

  # Determine the number of data frames
  n_dfs <- length(dfs)
  
  # Determine default output name type
  generic_output <- ifelse(tolower(output_type) == "xlsx", "Sheet", "File")
  
  # Handle excess or insufficient output names
  if (is.null(output_names)) {
    # Use names of the data frames (if any) or default to generic names
    output_names <- ifelse(is.null(names(dfs)) | any(names(dfs) == ""), 
                           paste0(generic_output, seq_len(n_dfs)), 
                           names(dfs))
  } else if (length(output_names) < n_dfs) {
    # Extend with default names if too few names
    extra_names <- paste0(generic_output, seq((length(output_names) + 1), n_dfs))
    output_names <- c(output_names, extra_names)
  } else if (length(output_names) > n_dfs) {
    # Truncate excess names
    output_names <- output_names[seq_len(n_dfs)]
    # Add empty data frames for excess names
    for (i in (n_dfs + 1):length(output_names)) {
      dfs[[i]] <- data.frame()  # Add empty data frame
    }
  }
  
  # Generate the base filename if not provided
  if (is.null(output_file)) {
    file_base <- if (n_dfs == 1) {
      # Use data frame name if only one df
      names(dfs)[1] %||% "data"
    } else {
      "workbook"
    }
    if (date_format != "") {
      date_suffix <- format(Sys.Date(), date_format)
      file_base <- paste0(file_base, "_", date_suffix)
    }
    output_file <- paste0(file_base, ".xlsx")
  }
  
  # Handle output type
  if (tolower(output_type) == "xlsx") {
    # Ensure the file name has .xlsx extension
    if (!grepl("\\.xlsx$", output_file, ignore.case = TRUE)) {
      output_file <- paste0(output_file, ".xlsx")
    }
    file_path <- file.path(directory, output_file)
    write_to_excel(dfs, output_names, file_path, overwrite=overwrite)
    message("Excel workbook saved to: ", file_path)
  } else if (tolower(output_type) == "csv") {
    write_to_csv(dfs, output_names, directory)
    message("CSV files saved to directory: ", directory)
  } else {
    stop("Unsupported output_type. Use 'xlsx' or 'csv'.")
  }
}


write_to_excel <- function(
  dfs,
  output_names = NULL,
  file_path = "output.xlsx",
  null_as_blank = FALSE,
  freeze_first_row = TRUE,
  add_timestamp = FALSE,
  overwrite = TRUE,
  same_sheet = FALSE) {
  # Ensure openxlsx is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed.")
  }

  # Validate input
  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` must be a non-empty list of data frames.")
  }

  # Fix extension
  ext <- tolower(tools::file_ext(file_path))
  if (ext == "") {
    file_path <- paste0(file_path, ".xlsx")
  } else if (ext != "xlsx") {
    warning("file_path had non-xlsx extension. Replacing with '.xlsx'.")
    file_path <- sub("\\.[^\\.]*$", "", file_path)
    file_path <- paste0(file_path, ".xlsx")
  }

  # Append timestamp if requested
  if (add_timestamp) {
    timestamp <- format(Sys.time(), "_%Y%m%d_%H%M")
    file_path <- sub("\\.xlsx$", paste0(timestamp, ".xlsx"), file_path)
  }

  # Determine sheet names
  if (is.null(output_names)) output_names <- names(dfs)
  if (is.null(output_names) || any(is.na(output_names) | output_names == "")) {
    output_names <- paste0("Sheet", seq_along(dfs))
  }

  # Clean and truncate sheet names
  output_names <- gsub("[\\[\\]\\:\\*\\?\\/\\\\]", "_", output_names)
  output_names <- substr(output_names, 1, 31)

  # Create workbook
  wb <- openxlsx::createWorkbook()
  sheetptr = 0

  # Write each sheet
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    if (!is.data.frame(df)) {
      stop(paste("Element", i, "in dfs is not a data frame."))
    }
    lastrow <- nrow(df) + 1 # location to start next sheet

    if (null_as_blank) {
      df[] <- lapply(df, \(x) ifelse(is.na(x), "", x))
    }

    if (!same_sheet || i==1) {
      sheetptr <- sheetptr + 1
      nextrow  <- 1
      openxlsx::addWorksheet(wb, sheetName = output_names[sheetptr])
    }

    # special case offset below previous data on same sheet
    openxlsx::writeData(wb, sheet = output_names[sheetptr], x = df, startRow = nextrow)

    nextrow <- nextrow + nrow(df) + 2
    
    if (freeze_first_row) {
      openxlsx::freezePane(wb, sheet = output_names[sheetptr], firstRow = TRUE)
    }
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, file = file_path, overwrite = overwrite)
  invisible(file_path)
}

write_to_excel_ <- function(
  dfs,
  output_names = NULL,
  file_path = "output.xlsx",
  null_as_blank = FALSE,
  freeze_first_row = TRUE,
  add_timestamp = FALSE,
  overwrite = TRUE
) {
  # Ensure openxlsx is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed.")
  }

  # Validate input
  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` must be a non-empty list of data frames.")
  }

  # Fix extension
  ext <- tolower(tools::file_ext(file_path))
  if (ext == "") {
    file_path <- paste0(file_path, ".xlsx")
  } else if (ext != "xlsx") {
    warning("file_path had non-xlsx extension. Replacing with '.xlsx'.")
    file_path <- sub("\\.[^\\.]*$", "", file_path)
    file_path <- paste0(file_path, ".xlsx")
  }

  # Append timestamp if requested
  if (add_timestamp) {
    timestamp <- format(Sys.time(), "_%Y%m%d_%H%M")
    file_path <- sub("\\.xlsx$", paste0(timestamp, ".xlsx"), file_path)
  }

  # Determine sheet names
  if (is.null(output_names)) output_names <- names(dfs)
  if (is.null(output_names) || any(is.na(output_names) | output_names == "")) {
    output_names <- paste0("Sheet", seq_along(dfs))
  }

  # Clean and truncate sheet names
  output_names <- gsub("[\\[\\]\\:\\*\\?\\/\\\\]", "_", output_names)
  output_names <- substr(output_names, 1, 31)

  # Create workbook
  wb <- openxlsx::createWorkbook()

  # Write each sheet
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    if (!is.data.frame(df)) {
      stop(paste("Element", i, "in dfs is not a data frame."))
    }

    if (null_as_blank) {
      df[] <- lapply(df, \(x) ifelse(is.na(x), "", x))
    }

    openxlsx::addWorksheet(wb, sheetName = output_names[i])
    openxlsx::writeData(wb, sheet = output_names[i], x = df)
    if (freeze_first_row) {
      openxlsx::freezePane(wb, sheet = output_names[i], firstRow = TRUE)
    }
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, file = file_path, overwrite = overwrite)
  invisible(file_path)
}

write_to_csv <- function(dfs, output_names, directory) {
  # Write each data frame to its own CSV file
  for (i in seq_along(dfs)) {
    file_name <- paste0(output_names[i], ".csv")
    file_path <- file.path(directory, file_name)
    write.csv(dfs[[i]], file_path, row.names = FALSE)
  }
}
