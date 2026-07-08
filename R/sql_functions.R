#' @title get_sql_dictionary
#' @description 
#' Retrieves the SQL Server data dictionary table from the specified schema and database.
#' Used to load metadata for structure management.
#'
#' @param schema SQL Server schema name.
#' @param dbname SQL database name.
#' @param ddname Table name for the data dictionary.
#' @param global Logical; if TRUE, assigns the result to global environment.
#'
#' @return Data frame containing the dictionary table.
#' @export
get_sql_dictionary<-function(schema=NULL
                           , dbname='HEMEDB'
                           , ddname='sqldd'
                           , global=TRUE
                           , reload=FALSE
                           , silent=TRUE)
{
  # A schema='ALL' returns all the data using the custom view [METADATA.DATADICTIONARY]
  # Fails if there is not a custom view in the target database.
  
  # Default schema to esteylist.
  if (is.null(schema))              {schema<-'esteylist'}
  # Default df name to 'sqldd'
  if (!exists('ddname'))            {ddname<-'sqldd'}
  if (ddname=='' | ddname=='sqldd') {ddname<-'sqldd'}
  # Default to global
  if (!exists('global'))            {global=TRUE}
  # Default to not reloading
  if (!exists('reload'))            {reload=FALSE}
  # Default to not reloading
  if (!exists('silent'))            {silent=TRUE}
  
  # Only load if not already in memory
  if (exists(ddname) & !reload) {
    if (!silent) {cat('skipping reload of sql server dictionary\n')}
    return(get(ddname))
  }
  
  cmd <- "SELECT * FROM METADATA.DATADICTIONARY ;"
  dd  <- sql_run_cmd(section=base_schema, cmd=cmd, engine="SQLServer")[[1]]$data

  # filter to schema unless ALL was was passed in
  if (!schema=='ALL') {dd <- dd |> filter(TABLE_SCHEMA==schema)}
  if (global) {
      assign(ddname, dd, envir = .GlobalEnv)  # Assign dd to global environment with specified name
      if (!silent) {cat('Assign sql dictionary to global environment\n')}    
  } else {
      assign(ddname, dd, envir = environment())  # Assign dd to current environment with specified name
      if (!silent) {cat('Assign sql dictionary to current environment\n')}
  }

  return(dd)
  
}
# x<-get_sql_dictionary(global=FALSE, ddname='ddcopy')

#' @title sql_get_table
#' @description 
#' Reads a full SQL Server table into R as a data frame.
#' Supports dynamic connection handling via icicle engine and section configuration.
#'
#' @param sch_tbl Fully qualified schema.table string.
#' @param section Connection section name.
#' @param engine Optional engine override.
#' @param filt Optional filtering expression (as string or quosure).
#' @param silent Logical; suppress messages.
#'
#' @return Data frame containing SQL Server table.
#' @export
# sql_get_table('non_ablative.aml_mds_tbi200', 'esteylist')
sql_get_table <- function(sch_tbl=NULL, section='esteylist', engine=NULL, filt=NULL, silent=TRUE) {
  # testing
  # sch_tbl <- 'esteylist_label.patient_list'
  # section    <- 'esteylist'

  # require sch_tbl
  if (is.null(sch_tbl)) {
    cat('Cannot process sql_get_table() without the sch_tbl parameter')
    return(FALSE)
  }

  # sort the parameters
  schema_split <- strsplit(str_replace_all(sch_tbl, "\\[|\\]", ""), "\\.")[[1]]
  schema     <- schema_split[1]
  tbl        <- schema_split[2]
  sch_tbl <- paste(glue("[{schema}].[{tbl}]"))
  
  # if no engine use the default
  if (is.null(engine)) {
    if (exists('eng')) {
      engine <- eng
    } else {
      cat('Cannot process sql_get_table() without the engine parameter or a global eng')
      return(FALSE)
    }
  }
  # engine <- if(is.null(engine)) 'global eng' else engine
  # if no section, use the section most appropriate for the engine
  if (engine=='SQLServer' & is.null(section)) {
    section <- schema
  }

  # grab the connection
  if (!silent) cat(section, engine, '\n')
  con <- db_con(section,engine,silent=silent)
  
  if (!silent) {
    con_name <- switch(engine
                     , SQLServer = 'Microsoft SQL Server'
                     , REDCap    = 'REDCap'
                     , MySQL     = 'MySQL'
                     , MariaDB   = 'MariaDBConnection'
                     , 'Unknown Engine')
    cat(  '\nParameters'
        , '\nSection:\t\t',         ?switchsection
        , '\nSchema/Table Pair:\t', tmpschtbl
        , '\nSchema:\t\t\t',        schema
        , '\nEngine:\t\t\t',        engine
        , '\nSilent:\t\t\t',        silent
        , '\nTable:\t\t\t',         tbl
        , '\nConnection:\t\t',      con_name
        ,'\n')
  }
  
  
  # determine filter
  if (is.character(filt)) {
    filt <- parse_expr(filt)  # Convert string filter to expression
  }
  
  df <- tryCatch({
    # db_con will automatically load the section parameters
    cmd <- in_schema(schema, tbl)
    df <- tbl(con, in_schema(schema, tbl))
    if (!is.null(filt)) {df <- df |> filter(!!filt)}
    df <- df |> collect()
  }, error = function(e) {
    df <- data.frame()
  })

  db_discon(con)
  
  return(df)
}
# con <- db_con('esteylist')
# x <- slotNames(con)
# df<-sql_get_table('esteylist','epicpatientdata.epicpatientdata', silent=TRUE)

#' @title sql_drop_tables
#' @description 
#' Drops one or more tables from SQL Server. 
#' Supports both base and label schemas based on icicle configuration.
#'
#' @param ... Internal use.
#'
#' @return None. Tables are dropped in SQL Server.
#' @export
sql_drop_tbl <- function(sch_tbl, tbl) {
  con <- db_con()
  
  if (str_detect(sch_tbl, '\\.')) {
      split_schema <- strsplit(sch_tbl, "\\.")[[1]]
      schema       <- split_schema[1]
      tbl          <- split_schema[2] 
  } else {
    schema <- sch_tbl
  }
  sch_tbl <- paste0(glue("[{schema}].[{tbl}]"))
  
  # Construct the SQL command to drop the table if it exists
  sqlcmd <- glue("DROP TABLE IF EXISTS {sch_tbl} ;")
  
  # Try-catch for transactional integrity
  tryCatch({
    # Transaction
    dbExecute(con, "BEGIN TRANSACTION")
    dbExecute(con, sqlcmd)
    dbExecute(con, "COMMIT TRANSACTION")
    message("Table ", tbl, " successfully dropped.")
  }, error = function(e) {
    # Rollback in case of error
    dbExecute(con, "ROLLBACK TRANSACTION")
    message("Error encountered during table drop: ", e$message)
  }, finally = db_discon(con))
  
}

#' @title sql_copy_tbl
#' @description 
#' Copies a full table from one schema/table to another within the same SQL Server connection.
#' Can optionally copy just the structure (no data) or copy the full data set.
#'
#' @param trgsch Target schema name.
#' @param trgtbl Target table name.
#' @param srcsch Source schema name.
#' @param srctbl Source table name.
#' @param data Logical; if TRUE, copies full data; if FALSE, copies structure only.
#'
#' @return None. Side effect: creates target table.
#' @export
sql_copy_tbl <- function(trgsch, trgtbl, srcsch, srctbl, data=FALSE) {

  # Drop the existing target table
  sql_drop_tbl(trgsch, trgtbl)
  
  # Construct the SQL command to copy the source table to the target with/without data
  if (data) {
    # all data
    sqlcmd <- glue("SELECT * INTO [{trgsch}].[{trgtbl}] FROM [{srcsch}].[{srctbl}];")  
  } else {
    # no data
    sqlcmd <- glue("SELECT * INTO [{trgsch}].[{trgtbl}] FROM [{srcsch}].[{srctbl}] WHERE 1=0;")  
  }
  
  # Try-catch for transactional integrity
  tryCatch({
    con <- db_con()
    # Transaction
    dbExecute(con, "BEGIN TRANSACTION")
    dbExecute(con, sqlcmd)
    dbExecute(con, "COMMIT TRANSACTION")
    message("Table ", trgtbl, " successfully copied from ", srctbl)
  }, error = function(e) {
    # Rollback in case of error
    dbExecute(con, "ROLLBACK TRANSACTION")
    message("Error encountered during table copy: ", e$message)
  })
  db_discon()
}

#' @title sql_desc_tbl
#' @description 
#' Gets or sets the extended MS_Description property for a SQL Server table.
#' Allows adding or updating table-level descriptions using SQL Server extended properties.
#'
#' @param sch_tbl Full table name in the format "schema.table".
#' @param desc Optional character string. If provided, sets the description; if NULL, retrieves current description.
#' @param con Optional active database connection. If NULL, opens default connection via \code{db_con()}.
#' @param silent Logical; if TRUE (default), suppresses informational messages.
#'
#' @return 
#' Returns current description if retrieving. Returns updated description if setting. Returns FALSE on failure.
#' @export
sql_desc_tbl <- function(sch_tbl, desc=NULL, con=NULL, silent=TRUE) {
  if (missing(sch_tbl)) stop("Please provide a schema and table destination.")
  if (is.null(con)) con <- db_con()
  
  split_schema <- unlist(strsplit(sch_tbl, "\\."))
  if (length(split_schema) != 2) {
    stop("Invalid `sch_tbl` format. Expected 'schema.table'.")  
  }
  
  schema <- split_schema[1]
  tbl <- split_schema[2]
  
  mode <- if(is.null(desc)) 'get' else 'set'
  
  # Query to get the MS_Description extended property from sys.extended_properties
  query <- glue("
      SELECT ep.value
      FROM sys.extended_properties ep
      WHERE ep.major_id = OBJECT_ID('{schema}.{tbl}')
        AND ep.name = 'MS_Description';
    ")
  
  rslt <- DBI::dbGetQuery(con, query)
  
# If description is missing, fetch the current description
  if (mode=='get') {
    if (nrow(rslt) > 0) {
      desc <- rslt[1,1]
    } else {
      if(!silent) cat(glue("No description found for {sch_tbl}"))
      desc <- NULL  # No description found
    }
    return(desc)
  }

  # Default action is 'add'
  action <- 'add'
  
  if (nrow(rslt)>0) {
    if (nzchar(rslt[1, 1])) { 
      # If it exists, switch to update
      action <- 'update'
    }
  }
  
  # Build the appropriate query based on the action
  query <- glue("
    EXEC sp_{action}extendedproperty 
        @name = N'MS_Description', 
        @value = N'{desc}', 
        @level0type = N'SCHEMA', @level0name = N'{schema}',
        @level1type = N'TABLE',  @level1name = N'{tbl}';
  ")
  
  if (!silent & action=='add') cat("The description", desc, "has been added to", sch_tbl, '\n')
  if (!silent & action=='update') cat("The description", desc, "has been added to", sch_tbl, '\n')

  # Execute the query
  tryCatch({
    DBI::dbExecute(con, query)
    return(desc)
    
  }, error = function(e) {
    print("Error occurred in executing query.")
    return(FALSE)
  })

}

# # Assume con is your ODBC connection and you've already created the table
# sch_tbl <- "frozen.allarrival"
# desc    <- "zebra arrivals for patients treated at our center and pre-abstraction."
# 
# # call setting
# rslt1 <- sql_desc_tbl(sch_tbl, desc=desc)
# # call getting
# rslt2 <- sql_desc_tbl(sch_tbl)

#' @title sql_store_rc
#' @description 
#' Full REDCap data storage manager into SQL Server.
#' Combines dropping tables, creating structures, resizing columns, and appending data.
#' Handles both base and label schemas.  
#' Creates empty SQL Server tables for the redcap data, adjusts the structure widening any
#' narrow columns as needed and populates the tables on SQL Server.
#'
#' @param paramsect Parameter list or configuration object from `get_configset()`.
#' @param section Optional configuration section name.
#' @param silent Logical; suppresses messages.
#'
#' @return Summary data frame of loaded tables and row counts.
#' @export

if (FALSE) sql_store_rc(section='esteylist', silent=FALSE)

sql_store_rc <-function( paramsect=NULL
                       , section=NULL
                       , silent=TRUE)  {
  
  if (FALSE) {
    paramsect = NULL; section='esteylist'; silent=FALSE
  }
  
  # Assure required parameters
  silent  <- if (exists("silent")) silent else TRUE
  
  # Use the section passed in the parameter object
  # if (!is.null(paramsect) & is.null(section)) section <- paramsect$section
  if (!is.null(paramsect) && is.null(section) && is.list(paramsect) && "section" %in% names(paramsect)) {
    section <- paramsect$section
  }
  
  # SECTION set via dialogue
  if (!silent & is.null(section)) {
    section <- showPrompt(title = "Replicate"
                      , message = "Enter configuration section:"
                      , default = section)
  }

  # get configurations if not passed in
  if (is.null(paramsect)) paramsect <- get_configset(section=section, silent=silent)

  # read in parameters from configurations section
  section      <- paramsect$section
  base_schema  <- paramsect$schema
  label_schema <- paste0(base_schema,'_label')
  retain       <- paramsect[['retain']]

  get_encryptkey('icicle')

  # Make sure the REDCap data dictionary is loaded for the current section's schema
  get_rc_dictionary(section,silent=silent)
  
  
  # create output summary table
  if (length(retain)>=1) {
    assign("summary_df", data.frame(
      Row     = 1:length(retain),
      Tables  = retain,
      Schemas = paste(base_schema, 'and', label_schema),
      Rows    = -999
    ), envir = .GlobalEnv)
  }  
  
  # Get SQL Server connection
  sql_con=db_con(section=section, engine='SQLServer')

  # Feedback for run
  cat('\014','\n')
  cat(red('MOVING SCHEMA',toupper(base_schema),'TO SQL SERVER\n'))
  
  loop_cnt  <-length(retain)

  label = FALSE
  for (label in c(FALSE, TRUE)) { # cat(label)}
    # target schema based on whether we do/don't have labels
    # schema <- if_else(label, label_schema, base_schema)
    schema <- if (label) label_schema else base_schema

    # Progress
    vers <- if (label) {'LABELED'} else {'CODED'}
    cat(red(glue('WORKING ON {vers} VERSION\n')))

    # pb <- get_progress_bar(loop_cnt, "LOAD DATA FRAMES IN R AND COPY TO SQL SERVER")

    for (i in 1:loop_cnt) {
        tbl = retain[i]
        # pb$tick() # info for user
        df <- get_rc_table(section, tbl, label, silent)
        
        if (!silent) {
          cat('\n', tbl, timestamp(), '\n\n')
        }
        
        # No need to store if empty?
        if (nrow(df)==0) next
        
        # Find summary row index for this table
        ind <- which(summary_df$Tables == tbl)

        # Store number of rows for summary
        summary_df$Rows[ind] <- nrow(df)        
        sch_tbl <- paste(schema, tbl, sep='.')
        altered_cols <- sql_insert_df(df, sch_tbl, silent=TRUE)
    }
    cat('\nLOADED AND SAVED', vers,'TABLES:', toupper(retain), '\n')

    
    # rm(pb)

    # Remove tables from memory
    suppressWarnings(rm(list=paramsect$retain))
    
    cat('\n')
  }
  
  # return from function
  return(summary_df)
}

# Define the custom conversion function
#' @title sql_convert_type
#' @description Converts one variable to match the class of a reference variable.
#' @param var Input variable.
#' @param reference_var Reference variable to match type.
#' @return Converted variable.
#' @export
sql_convert_type <- function(var, reference_var) {
  ref_class <- class(reference_var)
  
  if ("Date" %in% ref_class) {
    return(as.Date(var))  # Convert to Date if reference is Date
  } else if ("POSIXct" %in% ref_class) {
    return(as.POSIXct(var))  # Convert to POSIXct if reference is POSIXct
  } else if ("factor" %in% ref_class) {
    return(factor(var, levels = levels(reference_var)))  # Convert to factor with matching levels
  } else if ("logical" %in% ref_class) {
    return(as.logical(var))
  } else if ("integer" %in% ref_class) {
    return(as.integer(var))
  } else if ("numeric" %in% ref_class) {
    return(as.numeric(var))
  } else if ("character" %in% ref_class) {
    return(as.character(var))
  } else {
    # Handle unsupported types or fallback to standard conversion
    ref_type <- typeof(reference_var)
    return(as(var, ref_type))  # Fallback conversion based on reference's type
  }
}


# sql_resize(df, schema, tbl, chcols, limit)
#' @title sql_resize
#' @description Dynamically resizes character columns in SQL Server based on data frame content.
#' @param df Data frame.
#' @param schema Schema name.
#' @param tbl Table name.
#' @param limit Threshold width before resizing (default 250).
#' @return List of columns resized.
#' @export
sql_resize <- function(df, schema, tbl, limit = 250) {


  # # Create an empty table with the same structure as the data frame
  # emptydf <- df[0, ]
  # con <- db_con('esteylist') # db_con('sqlserver')
  # dbWriteTable(con, Id(schema = schema, table = tbl), emptydf, overwrite = TRUE, row.names = FALSE)
  # db_discon(con)

  # all column  names
  cols <- names(df)

  # Identify date-time columns
  dtcols <- cols[sapply(df, function(col) inherits(col, "Date") | inherits(col, "POSIXt"))]
  
  # Identify character columns
  chcols <- cols[sapply(df, is.character)]
  
  # Identify non-date-time columns (i.e., not in dtcols)
  non_dtcols <- setdiff(cols, dtcols)
  
  # Identify non-character columns (potential issue columns)
  non_chcols <- setdiff(names(df), chcols)
  
  # Replace NA's with blanks only for non-date-time columns (non_dtcols)
  df[non_dtcols] <- lapply(df[non_dtcols], function(x) replace(x, is.na(x), ""))

  # data frame with all column names and maximum column lengths
  df_col_len <- data.frame(
    COLUMN_NAME = names(df),
    DF_CHAR_MAX = sapply(df, function(x) max(nchar(as.character(x))))
  ) |> 
    filter(COLUMN_NAME %in% chcols) |> 
    filter(DF_CHAR_MAX > 50) |> 
    mutate(new_width = case_when(
      DF_CHAR_MAX >= 8000 ~ -1,
      DF_CHAR_MAX > limit ~ ceiling(DF_CHAR_MAX / 100) * 100,
      DF_CHAR_MAX < 10    ~ DF_CHAR_MAX,
      .default            = ceiling(DF_CHAR_MAX / 10) * 10
    ),
    new_width = if_else(new_width == -1, 'MAX', as.character(new_width))
  )
  # Get current column sizes from INFORMATION_SCHEMA
  check_cols <- paste(df_col_len$COLUMN_NAME, collapse = "', '")
  con <- db_con('esteylist') # db_con('sqlserver')
  sql_cmd <- glue("
    SELECT COLUMN_NAME, CHARACTER_MAXIMUM_LENGTH AS SQL_CHAR_MAX
    FROM INFORMATION_SCHEMA.COLUMNS 
    WHERE TABLE_SCHEMA = '{schema}' AND TABLE_NAME = '{tbl}' 
          AND COLUMN_NAME IN ('{check_cols}');")
  sql_col_len <- dbGetQuery(con, sql_cmd)
  db_discon(con)

  # df of just the columns that need to have their length adjusted
  adjust_col_len <- left_join(sql_col_len, df_col_len, by = 'COLUMN_NAME') |> 
    filter(SQL_CHAR_MAX < DF_CHAR_MAX & SQL_CHAR_MAX != -1)

  if (nrow(adjust_col_len) > 0) {
    con <- db_con('esteylist') # db_con('sqlserver')
    for (i in 1:nrow(adjust_col_len)) {
      col       <- adjust_col_len$COLUMN_NAME[i]
      new_width <- adjust_col_len$new_width[i]
      cmd <- glue("ALTER TABLE {schema}.{tbl} ALTER COLUMN [{col}] VARCHAR({new_width})")
      dbExecute(con, cmd)
    }
    db_discon(con)
  }
  
  adjust_col_list <- paste(unique(adjust_col_len$COLUMN_NAME), collapse = ", ")
  
  return(adjust_col_list)
}


#' @title sql_insert_df
#' @description 
#' Inserts a data frame into SQL Server. Dynamically resizes varchar fields as needed, supports overwrites and optional date-stamped copies.
#'
#' @param df Data frame to insert.
#' @param sch_tbl Fully qualified schema.table name (string).
#' @param limit Threshold for varchar resizing (default 250).
#' @param silent Logical; suppress messages.
#' @param test Logical; internal test mode.
#' @param date_suffix Format for optional date copy (default '%Y%m%d').
#' @param nrow Number of rows to process (default Inf for all rows).
#' @param batch_size Insert batch size (default 5000).
#' @param desc Optional description for metadata.
#' @param overwrite Logical; if TRUE, drops/recreates table before insert.
#'
#' @return Invisible TRUE on success.
#' @export
# sql_insert_df(chunk_df |> select(record, everything()), 'esteylist.redcap_log', overwrite=FALSE)  
# sql_insert_df(target_df, sch_tbl, overwrite=TRUE)
# sql_insert_df(prot_lkup, 'frozen.protocol_lkup', date_suffix = '%Y%m')
sql_insert_df <- function(df = NULL,
                          sch_tbl = NULL,
                          limit = 250,
                          silent = TRUE,
                          test = FALSE,
                          date_suffix = '%Y%m%d',
                          nrow = Inf,
                          batch_size = 5000,
                          desc='',
                          ## assumes delete and then append, false just append 
                          overwrite = TRUE ) {
  
  # Test mode setup for debugging
  if (test) {
    df          = df
    sch_tbl     = schema_tbl
    limit       = 250
    silent      = TRUE
    test        = FALSE
    date_suffix = date_suffix
    nrow        = Inf
    batch_size  = 5000
    desc        = ''
    overwrite = TRUE
  }
  
  # How many rows do we have?
  rowcnt <- min(nrow, nrow(df))
  # Subset df to rowcnt (if non-zero)
  if (rowcnt != 0) df <- df[1:rowcnt, ]

  # If df is empty and not in overwrite mode, return early
  if (rowcnt == 0  & !overwrite) {
    message("Data frame is empty, nothing to insert.")
    return(NULL)  # No need to proceed further
  }

  split_schema <- unlist(strsplit(sch_tbl, "\\."))
  if (length(split_schema) != 2) stop("Invalid `sch_tbl` format. Expected 'schema.table'.")

  # Local vars
  schema <- split_schema[1]
  tbl <- split_schema[2]
  make_dated_copy <- !is.null(date_suffix)

  if (overwrite) { # Overwrite removes the old table and creates a new empty table
    
    # Connection to database
    
    tryCatch({ # Drop the table if it exists
      sql_drop_tbl(schema, tbl)
      if (!silent) message(paste("Successfully dropped the table", tbl))
    
    }, error = function(e) { # Skip drop if it doesn't exist
      if (!silent) message(paste("Table", tbl, "did not exist. Skipping drop."))
    
    }, finally = { # Recreate the table with zero rows
      empty_df <- df[0, ]
      
      # Connect
      con <- db_con('esteylist', silent = TRUE)  # db_con('sqlserver')
      
      # Write
      dbWriteTable(con, Id(schema = schema, table = tbl), empty_df, row.names = FALSE, overwrite = TRUE, append = FALSE)
      if (!silent) message(paste("Successfully created empty table", tbl))
    
      # Disconnect
      db_discon('con')  # Ensure disconnection
    })
  }

  # Resize SQL columns to accomodate df widths
  resize_msg <- sql_resize(df, schema, tbl)
  if (!silent) message("Resized the column(s): ", resize_msg)  

  # Write in batches appending data, can be fairly slow using dbWriteTable()
  con <- db_con('esteylist', silent = TRUE)  # db_con('sqlserver')
  inserted_rows <- 0
  for (start_idx in seq(1, rowcnt, by = batch_size)) {
    if (!silent) cat(paste0('Working on batch:', start_idx, '\r'))
    
    end_idx  <- min(start_idx + batch_size - 1, rowcnt)
    batch_df <- df[start_idx:end_idx, ]

    tryCatch({ # Insert batch
      dbWriteTable(con, Id(schema = schema, table = tbl), batch_df, row.names = FALSE, overwrite = FALSE, append = TRUE)
      inserted_rows <- inserted_rows + nrow(batch_df)
      if (!silent) message(glue::glue("Successfully inserted {nrow(batch_df)} rows, now {inserted_rows} total rows."))
    
    }, error = function(e) { # Unable to insert batch
      print(e)
    
    })   
  }
  if (!silent) cat('\n')

  # Disconnect to ensure proper closure of the connection
  db_discon('con')
  
  
  if (!silent) message(glue("Data frame '{tbl}' with {rowcnt} rows moved to database schema '{schema}'"))
  
  # Optional dated copy utilizing SQL pass through for speed
  if (make_dated_copy) {
    datetbl <- paste0(tbl, "_", format(Sys.Date(), date_suffix))
    sql_copy_tbl(schema, trgtbl=datetbl, schema, srctbl=tbl, data=TRUE)
    if (!silent) message(paste("Dated copy of", tbl, "is", datetbl))
  }

  
  # Return the results
  invisible(TRUE)
}

#' @title sql_run_cmd
#' @description 
#' Executes one or more SQL Server commands dynamically.
#' Supports multi-statement batches, internal transaction handling, schema.table parsing, and result collection.
#'
#' @param con Optional active database connection. If NULL, opens connection dynamically.
#' @param section Connection section name from configuration (if opening connection dynamically).
#' @param cmd SQL command string, or vector of multiple SQL commands.
#' @param engine Optional database engine name (default uses global engine).
#' @param silent Logical; suppresses informational messages (default TRUE).
#' @param rslt_lst Internal: existing result list (for recursion).
#' @param test Logical; internal test mode.
#' @param newitem Internal recursion counter (default 0).
#'
#' @return A list of query results, each containing: item index, row count, SQL query string, and returned data frame.
#' @export
sql_run_cmd <- function(con=NULL
                , section='esteylist'
                , cmd=NULL
                , engine=.pkg_env$engine
                , silent=TRUE
                , rslt_lst=NULL
                , test=FALSE
                , newitem=0) {
  
  # testing
  if (FALSE) {
    section  <-'esteylist'
    cmd      <-sqlcmd
    engine   <-eng
    silent   <-FALSE
    rslt_lst <-list()
    test     <-FALSE
    con      <-NULL
    newitem  <-0
  }
  
    
  # Default query result
  rslt_df <- data.frame()

  # fail if no command (cmd)
  if (is.null(cmd)) {return (rslt_lst)}
  
  cmdset <- sql_parse(cmd)
  cmd    <- cmdset[1]
  cmdset <- cmdset[-1]
  
  if (!silent){
    cat('first command',cmd,'\n')
    cat('remaining commands',paste0(trimws(cmdset),'(n=',length(cmdset),')'),'\n')
  }

  # Meant for transactions on a single table
  if (is.null(engine) || engine == "") {
    engine <- .pkg_env$engine
    # if (exists("eng", envir = .GlobalEnv) && !is.null(eng) && eng != "") {
    #   engine <- eng
    # } else {
    #   engine <- 'MariaDB'
    # }
  }
  if (!silent) {cat('engine =',engine,'\n')}
  
  # Establish a connection
  if (!is.null(con)) {con <- con} # use connection passed in
  # if ( is.null(con)) {con <- db_con(section=section,engine=engine)} # create a new connection
  if ( is.null(con)) {con <- db_con(section=section)} # create a new connection
  if ( is.null(con)) { # no connection found/made
    stop("Failed to connect to the database.")
  }

  if (is.null(rslt_lst)) {rslt_lst <- list()}  
  
  if (!silent) {
    cat("Connection to SQL Server\n")
  }
  
  # Default flags
  executecmd <- TRUE
  fetchdata  <- TRUE
  hasinto    <- FALSE
  dropcmd     <- NULL
  fetchcmd    <- NULL
  sch_tbl_src <- NULL
  sch_tbl_trg <- NULL  
    
  tryCatch({
    
    # What is the first word in the SQL command?
    firstword <- strsplit(toupper(cmd), " ")[[1]][1]
    
    # Is the word "INTO" found in the SQL Server command, acts like CREATE
    hasinto  <- grepl("INTO", cmd, ignore.case = TRUE)
        
    if (hasinto || firstword %in% c('CREATE', 'INSERT', 'UPDATE', 'ALTER')) {
      # # Extract the first match
      # dbpath <- regmatches(cmd, regexpr("\\S*\\.\\S+", cmd))
      # dbpaths <- unlist(regmatches(cmd, gregexpr("\\b\\w+\\.\\w+\\b(?=\\s+(?!AS)\\w+|\\s*;)", cmd, perl=TRUE)))
  
      match       <- str_match(cmd, regex("\\binto\\b\\s+(.*?)\\s+\\bfrom\\b", ignore_case = TRUE))
      into_phrase <- str_split(match[, 2], "\\.", simplify = TRUE)

      match       <- str_match(cmd, regex("\\bfrom\\b\\s+(.*?)(\\s+|;|$)", ignore_case = TRUE))
      from_phrase <- str_split(match[,2], "\\.", simplify = TRUE)
      
      # # Find the components in the database pathing
      # path_comps <- parse_path_components(dbpaths)
  
      # Assign the components into
      into_phrase_len <- length(into_phrase)
      tbl <- into_phrase[into_phrase_len]
      sch <- into_phrase[into_phrase_len-1]
      if (into_phrase_len==3) {
        db      <- into_phrase[1]
        dbpath  <- paste(db, sch, tbl, sep='.')     
        sch_tbl_trg <- paste(db, sch, tbl, sep='.')     
      } else if (!is.null(sch)) {
        sch_tbl_trg <- paste(sch, tbl, sep='.')     
      }
      
      # Assign the components from
      from_phrase_len <- length(from_phrase)
      tbl <- from_phrase[from_phrase_len]
      sch <- from_phrase[from_phrase_len-1]
      if (from_phrase_len==3) {
        db      <- from_phrase[1]
        dbpath  <- paste(db, sch, tbl, sep='.')     
        sch_tbl_src <- paste(db, sch, tbl, sep='.')     
      } else if (!is.null(sch)) {
        sch_tbl_src <- paste(sch, tbl, sep='.')     
      }      
      
      
      # DROP Into
      dropcmd  <- paste0("DROP TABLE IF EXISTS ", sch_tbl_trg, " ;")
      
      # SELECT to fetch from changed table
      fetchcmd <- paste0("SELECT * FROM ", sch_tbl_src, " ;")
    }
    
    # Display fetch and execution code
    if (!silent) {
      if (isTRUE(fetchdata) && !is.null(fetchcmd)) cat(paste("Fetch command:\n", fetchcmd, '\n'))    
      if (isTRUE(hasinto) && !is.null(dropcmd))cat(paste("Drop command:\n", dropcmd, '\n'))    
      cat(paste("Executing command:\n", cmd, '\n'))
    }
    
    # Do we need to execute code in the finally?
    executecmd = TRUE # Except for SELECT
    fetchdata  = TRUE # Except for SELECT, DROP, DELETE
    
    if (!silent) {
      cat(  ' firstword =',   firstword,   '\n'
          , 'hasinto =',      hasinto,     '\n'
          , 'schema_table =', sch_tbl_src, '\n'
          , 'target_table =', sch_tbl_trg, '\n')
    }

    if (firstword == "SELECT" & !hasinto) {
      # Execute the command and get the data
      rslt_df <- suppressWarnings(dbGetQuery(con, cmd))
      executecmd = FALSE
      fetchdata  = FALSE

    } else if (firstword == "DROP")  {
      # DROP special case return and empty df
      executecmd = TRUE
      fetchdata  = FALSE

    } else if (firstword == "DELETE")  {
      # DELETE special case fetch before execute later
      rslt_df <- dbGetQuery(con, fetchcmd)
      executecmd = TRUE
      fetchdata  = FALSE
    
    } else if (firstword == "CREATE" || firstword == "SELECT") {
      # SELECT * INTO, CREATE special case do field typing prior to write
      dropfirst  = TRUE
      executecmd = TRUE
      fetchdata  = TRUE

    } else {
      # UPDATE, ALTER, INSERT
      executecmd = TRUE
      fetchdata  = TRUE
    }
    
  }, error = function(e) {
    executecmd = FALSE
    fetchdata  = FALSE   
    # Handle the error
    cat(paste('Skipped error for command:', cmd, '\n'))
    # stop(e)
  })

  # Drop first
  if (hasinto) {
    tryCatch ({dbExecute(con, dropcmd)}
              , error = function(e) 
                if (!silent) {cat("Couldn't execute command\n",cmd,"\n")})
  }
  # Execute the command
  if (executecmd) {
    tryCatch ({dbExecute(con, cmd)}
              , error = function(e) 
                if (!silent) {cat("Couldn't execute command\n",cmd,"\n")})
  }
  # Fetch data from table if fetchdata is TRUE
  if (fetchdata) {
    tryCatch(rslt_df <- dbGetQuery(con, fetchcmd)
             , error = function(e) 
               if (!silent) {cat("Couldn't fetch a dataframe\n")})
  }
  
  new_rslt_lst <- list( item=newitem
                      , rowcnt=nrow(rslt_df)
                      , query=cmd
                      , data=rslt_df)
  
  rslt_lst <- c(rslt_lst, list(new_rslt_lst))

  if (length(cmdset)>0) {
    # send the rest
    rslt_lst<-sql_run_cmd(con=con
                , cmd=cmdset
                , engine=engine
                , silent=silent
                , rslt_lst=rslt_lst
                , newitem=newitem+1)
  }
  
  return(rslt_lst)
  
}
  
# create a new connection}

sql_run_txt <- function(sql_txt, section = NULL, silent = TRUE) {
  
  # # sort the parameters
  # schema_split <- strsplit(str_replace_all(sch_tbl, "\\[|\\]", ""), "\\.")[[1]]
  # schema     <- schema_split[1]
  # tbl        <- schema_split[2]
  # sch_tbl <- paste(glue("[{schema}].[{tbl}]"))
  
  con <- db_con(section = section)
  on.exit(db_discon(con), add = TRUE)

  if (!silent) {
    cat("\nRunning SQL batch on section:", section, "\n")
  }

  DBI::dbExecute(con, sql_txt)
  invisible(TRUE)
}

# dosql(con=NULL, dbname='HEMEDB', cmd, engine=NULL, silent=TRUE)
#' @title dosql
#' @description 
#' Lightweight wrapper for \code{sql_run_cmd()}. 
#' Simplifies SQL execution interface by directly delegating parameters.  
#' Retained for backward compatibility.
#'
#' @param con Optional active database connection.
#' @param section Connection section name from configuration.
#' @param cmd SQL command string, or vector of multiple SQL commands.
#' @param engine Optional database engine name.
#' @param silent Logical; suppresses messages.
#' @param rslt_lst Internal: existing result list.
#' @param test Logical; internal test mode.
#' @param newitem Internal recursion counter.
#'
#' @return List of query results as returned by \code{sql_run_cmd()}.
#' @export
dosql <- function(con=NULL
                , section=NULL
                , cmd=NULL
                , engine=NULL
                , silent=TRUE
                , rslt_lst=NULL
                , test=FALSE
                , newitem=0) {
  return(sql_run_cmd(con, section, cmd, engine, silent, rslt_lst, test, newitem))
}

#' @title sql_parse
#' @description 
#' Splits multiple SQL statements concatenated together into individual SQL commands.
#' Removes empty commands, preserves proper statement separation, and returns a clean list of SQL queries.
#'
#' @param cmd Character string containing one or more SQL commands separated by semicolons.
#'
#' @return A character vector containing individual SQL commands.
#'
#' @examples
#' cmd <- "SELECT * FROM sch1.tmp1 WHERE karyo LIKE '%46,XY,t(2;3)%'; SELECT * FROM sch1.tmp2 ;"
#' sql_parse(cmd)
#'
#' @export
sql_parse <- function(cmd) {
  clean <- paste(cmd,';\n')
  clean <- gsub(";\\s*\n", ";\n", clean)
  clean <- trimws(strsplit(clean, ";\n")[[1]])
  clean <- clean[nzchar(clean)]  # Keep only non-empty strings
  return(clean)
}
