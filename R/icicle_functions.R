## ----knit_setup, include=FALSE----------------------------------------------------------------------------------------------------------------
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
cat('\n', msg, '\n')

onedir      <<- gsub("\\\\", "/", Sys.getenv('OneDrive', unset = NA))
codedir     <<- gsub("\\\\", "/", Sys.getenv('FROZEN_PATH', unset = NA))
mycodedir   <<- paste(path_dir(onedir), 'R Project', sep='/')
projdir     <<- rprojroot::find_rstudio_root_file()
scrapdir    <<- paste0(codedir,'/Scrap')
qcdir       <<- paste0(onedir, '/Patient Data Entry/Quality Control')
rcimportdir <<- paste0(onedir, '/Patient Data Entry/REDCap Imports')
datadir     <<- paste0(onedir, '/data')



## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------
set_packages <- function(silent=TRUE) {
  # List of required packages
  packages <- c(
    # Core Packages
    "fs", "ini", "R6", "glue", "rstudioapi", "yaml", "conflicted",
    # Database-related
    "DBI", "odbc", "RMariaDB",
    # Stats related
    "table1",
    # Configuration and Encryption
    "ConfigParser", "safer", "keyring",
    # Data Manipulation
    "tidyverse", "dbplyr", "janitor", "lubridate", "data.table",
    # REDCap-related
    "REDCapR",
    # Data Import/Export and Utility
    "openxlsx", "readr", "jsonlite",
    # Display Related
    "knitr", "kableExtra", "progress", "crayon", "consort", "ggplot2", "reshape2", "pheatmap", "gt", "webshot2", 
    # Send Email
    "mailR"
  )
  NA
  # Check which packages are not installed
  missing <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install missing packages
  if(length(missing)) {
    if(!silent) message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
  
  # Initialize a vector to collect names of packages that fail to load
  failed <- character()
  
  # Load libraries with error handling
  for(pkg in packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      failed <<- c(failed, pkg)
    })
  }
  

  # Manage package conflicts
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("lag", "dplyr")
  conflicted::conflict_prefer("%||%", "purrr")  
  conflicted::conflict_prefer("wday", "lubridate")  
  conflicted::conflict_prefer("month", "lubridate")  
  conflicted::conflict_prefer("year", "lubridate")  
  
  # Report packages that failed to load
  if(length(failed) > 0) {
    cat("These are the packages that did not load: ", paste(failed, collapse = ", "), "\n")
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_user <- function() {
  strsplit(system("whoami", intern = TRUE), "\\\\")[[1]][2]
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Helper Function to Retrieve or Set a Keyring Service
manage_keyring <- function(keyring = 'icicle', action = 'get') {
  service  <- paste0(keyring, '_service')
  username <- get_user()
  
  if (action == 'get') {
    tryCatch({
      keyring::key_get(service, username)
    }, error = function(e) {
      .set_keyring(keyring)
    })
  } else if (action == 'set') {
    pass <- rstudioapi::askForPassword(prompt = paste("Enter ", keyring, " key (aml*****!)"))
    keyring::keyring_create(service, password = pass)
    keyring::key_set_with_value(service, username, pass)
    pass
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Function to Unlock the Keyring
unlock_keyring <- function(keyring = 'icicle') {
  service  <- paste0(keyring, '_service')
  password <- manage_keyring(keyring)
  
  if (!is.null(password)) {
    keyring::keyring_unlock(service, password)
    assign('encryptkey', password, envir = .GlobalEnv)
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Internal function to Set a Keyring
.set_keyring <- function(keyring = 'icicle') {
  pass <- manage_keyring(keyring, action = 'set')
  assign('encryptkey', pass, envir = .GlobalEnv)
  return(pass)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Function to Retrieve Encryption Key
get_encryptkey <- function(keyring = 'icicle') {
  if (!exists("encryptkey", envir = .GlobalEnv) || is.null(encryptkey)) {
    encryptkey <- manage_keyring(keyring)
    assign('encryptkey', encryptkey, envir = .GlobalEnv)
  }
  return(encryptkey)
}

# get_encryptkey()


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_configfile <- function(
    srcfile = 'C:/Users/cmshaw/WorkingNotebooks/Config/config_jake.ini',
    trgdir = NULL,
    workdir = NULL,
    silent = TRUE) {

  # Explicitly load the necessary package
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("Package 'fs' is required but not installed. Please install it using install.packages('fs').")
  }

  workdir <- if (is.null(workdir)) getwd() else workdir

  # Determine target directory
  if (is.null(trgdir)) {
    trgdir <- workdir
  }

  # Ensure trgdir ends with /inst/extdata
  if (!grepl("/inst/extdata$", trgdir)) {
    trgdir <- file.path(trgdir, "inst/extdata")
  }

  # Create trgdir if it doesn't exist
  if (!fs::dir_exists(trgdir)) {
    fs::dir_create(trgdir)
    if (!silent) cat("Created directory:", trgdir, "\n")
  }

  # Construct the full target file path
  trgfile <- file.path(trgdir, 'config_.ini')

  if (fs::file_exists(trgfile)) {
    if (!silent) cat(trgfile, "found.\n")
    return(trgfile)
  }

  # File missing from target directory, copy it
  if (!silent) cat("Getting configuration file from:", srcfile, "\n")

  fs::file_copy(srcfile, trgfile)

  return(trgfile)

}


# library(fs)
get_configfile_ <- function(
    srcfile = 'C:/Users/cmshaw/WorkingNotebooks/Config/config_jake.ini',
    trgdir=NULL,
    silent=TRUE){
  
  # Figure out trgdir if not passed in
  if (exists('workdir')) {
    trgdir <- workdir
  } else {
    trgdir <- getwd()
  }
  
  # Construct the full target file path, defaults to working directory
  trgfile <- file.path(trgdir, 'config_.ini')
  if (file.exists(trgfile)) {
    if (!silent) {cat(trgfile,'found.\n')}
    return()
  }
  
  # File missing from target directory
  if (!silent) {
    cat('Getting configuration file from: ',srcfile,'\n')
  }
  file_copy(srcfile, trgfile)
}
# Trigger the auto-magical move of configuration file to working directory
get_configfile()


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_configproperty <- function(property, section, silent=TRUE)
{
  if(!silent) {cat('Current directory',getwd(),'\n')}
  encryptkey     <- get_encryptkey()  
  # configs        <- ConfigParser$new()
  # configs        <- configs$read('config_.ini')
  configs <- ConfigParser$new()$read(file.path(getwd(), 'config_.ini'))
  property_value <- tryCatch({
    configs$get(option = property, section = section)
  }, error = function(e) {
    NULL
  })
  
  return(property_value)
}
# get_configproperty('label','esteylist')


## ---------------------------------------------------------------------------------------------------------------------------------------------

# esteylist_params<-get_configset(section='esteylist')

get_configset <- function(section = 'esteylist', silent=TRUE)
{
  # Validate section is provided
  if (is.null(section)) {
    stop("Section parameter is required.")
  }
  
# Check if the current working directory is the expected one
  if (!codedir %in% getwd()) {
    if (!silent) {
      cat(paste0('Current working directory: "', getwd(), '" is not expected.\n',
                 'Expect the path: ', codedir, '\n'))
    }
    # Fall back to projdir if getwd() is not the expected path
    working_dir <- ifelse(file.exists(file.path(projdir, 'config_.ini')), projdir, getwd())
  } else {
    working_dir <- projdir # getwd()
  }
  
  # Print the working directory if not silent
  if (!silent) cat('Using working directory: "', working_dir, '"\n')
  
  # Get config information
  encryptkey <- get_encryptkey()  
  configdir  <- get_configfile(workdir=projdir)
  configs    <- ConfigParser$new()
  configs    <- configs$read(configdir)

  # Check if section exists in configs$data
  if (!(section %in% names(configs$data))) {
    stop(sprintf('Error: The section "%s" is not found.', section))
  }
  
  # Initialize params list special cases
  paramsect <- configs$data[[section]]
  
  # handle special cases
  if (is.character(paramsect$retain)) {
    paramsect$retain          <- unlist(strsplit(paramsect$retain, ", "))
  } else {paramsect$retain    <- list()}
  if (is.character(paramsect$keyfields)) {
    paramsect$keyfields       <- unlist(strsplit(paramsect$keyfields, ", "))
  } else {paramsect$keyfields <- list()}
  
  assign('params', paramsect, envir = .GlobalEnv)  
  
  return(paramsect)
}
# rslt <- get_configset('esteylist')


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_configset__depreciated <- function(section = NULL, silent=TRUE)
{
  # Validate section is provided
  if (is.null(section)) {
    stop("Section parameter is required.")
  }
  
  if (!codedir %in% getwd()) {
    if (!silent) {
      cat(paste0('Current working directory: "',getwd(),'" is not expected.\n'
                 ,'Expect the path: ',codedir,'\n'))
    }
  }
  
  if (!silent) {cat('Current working directory: "',getwd(),'" is not expected.\n')}
  
  # Get config information
  encryptkey <- get_encryptkey()  
  # configs    <- ConfigParser$new()
  # configs    <- configs$read('config_.ini')
  configs    <- ConfigParser$new()$read(file.path(getwd(), 'config_.ini'))

  # Check if section exists in configs$data
  if (!(section %in% names(configs$data))) {
    stop(sprintf('Error: The section "%s" is not found.', section))
  }
  
  # Initialize params list special cases
  paramsect <- configs$data[[section]]
  
  # handle special cases
  if (is.character(paramsect$retain)) {
    paramsect$retain          <- unlist(strsplit(paramsect$retain, ", "))
  } else {paramsect$retain    <- list()}
  if (is.character(paramsect$keyfields)) {
    paramsect$keyfields       <- unlist(strsplit(paramsect$keyfields, ", "))
  } else {paramsect$keyfields <- list()}
  
  assign('params', paramsect, envir = .GlobalEnv)  
  
  return(paramsect)
}
# rslt <- get_configset('esteylist')


## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(DBI)
# section <- 'mysql'
# engine <- 'MariaDB'
# c <- db_con('esteylist')

db_con <- function(section='esteylist'
                 , engine=NULL
                 , registry='con_pool'
                 , silent=TRUE)
{
  
  # Check if the engine parameter is provided or use the global variable or default to 'MariaDB'
  if (is.null(engine) || engine == "") {
    if (exists("eng", envir = .GlobalEnv) && !is.null(eng) && eng != "") {engine <- eng}
      else {engine <- 'MariaDB'}
  }
  
  # Validate section is provided
  if (is.null(section)) {
    stop("Section parameter is required.")
  }
  
  if (section=='SQLServer' | section=='MariaDB') {
    # engine was passed as section
    stop(glue("Revise you parameters, engine '{section}' was passed as section"))
  }

  # before attempting connection, assure that the config_.ini is in the local directory
  get_configfile()  
  
  # reuse valid connections stored in connection registry
  con <- .con_reg('get_con', section=section, registry=registry, silent=silent)
  if (!is.null(con)) {return(con)} 
  
  con <- switch(engine
                  , 'MariaDB'   = { if (!silent) cat("Connecting to MariaDB ... \n")
                                    .mariadb_con(section,silent=silent)
                                  }
                  , 'SQLServer' = { if (!silent) cat("Connecting to SQL Server ... \n")
                                    .sqlserv_con(section,silent=silent)
                                  }
                    
                  , 'REDCap'    = if (!silent) {cat("Aborting Connection\n")}
                  
                  , stop('Unsupported database engine')
                )
  
  if (!silent) {cat('Connection Successful\n')}
  
  attr(con, 'con_name') <- paste(section, engine, sep = "_")
  attr(con, 'con_eng')  <- engine
  
  # store connection to the connection registry object
  .con_reg('push', con=con, registry=registry, silent=silent)
  
  return(con)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
db_discon <- function(connection=NULL, keep=TRUE, registry='con_pool', silent=TRUE) {

  # Ensure connection is not NULL before proceeding
  if (is.null(connection)) {
    if (!silent) cat("No connection provided.\n")
    return(FALSE)
  }
  
  tryCatch({
    if (DBI::dbIsValid(connection)) {
      DBI::dbDisconnect(connection)
      if (!silent) cat("Connection successfully disconnected.\n")
    } else {
      if (!silent) cat("Connection was already invalid.\n")
    }
  }, warning = function(w) {
    if (!silent) cat('\nWarning: ', w$message, '\n')
  }, error = function(e) {
    if (!silent) cat('\nError: ', e$message, '\n')
  }, finally = {
    # Remove connection from environment if it was a name (string)
    if (!keep) {
      .con_reg('drop', con=connection, registry=registry, silent=silent)  # Remove the connection object by name
    }
    if (!silent) cat("Connection closed and removed from environment (if applicable).\n")
  })
}

# sample call
# print(db_discon(sample_con))
# print('done')


## ---------------------------------------------------------------------------------------------------------------------------------------------
# rslt <- .con_reg('get_con', section=section, registry=registry, silent=silent)
.con_reg <- function(action='get_con', con=NULL, section='', engine=NULL, registry='con_pool', silent=TRUE) {
  
  # Check if the engine parameter is provided or use the global variable or default to 'MariaDB'
  if (is.null(engine) || engine == "") {
    if (exists("eng", envir = .GlobalEnv) && !is.null(eng) && eng != "") {
      engine <- eng
    } else {
      engine <- 'MariaDB'
    }
  }

  if (!silent) cat("Action ", action, "\n")

  switch(action

        # get the environment object
        # requires registry, silent
        , 'get_env' = { if (!exists(registry, envir = .GlobalEnv)) {
                          # print('get_env')
                          assign(registry, new.env(), envir = .GlobalEnv)
                          if (!silent) cat("Created new connection registry in the global environment.\n")
                        }
                        # return new or existing registry environment
                        return(get(registry))
        } # get_env end

        # get the connection object from pool
        # requires section, engine, registry, silent
        , 'get_con' = { con_name <-paste(section, engine, sep='_')
                        pool <- .con_reg('get_env', registry=registry, silent=silent)
                        if (exists(con_name, envir = pool)) {
                          con <- get(con_name, envir = pool)
                          if (!DBI::dbIsValid(con)) {
                            if (!silent) cat("Cleaning up stale connection:", con_name, "\n")
                            remove(list = con_name, envir = pool)
                            con <- NULL
                          }                          
                        }
                        
                        # Re-establish the connection if no valid connection found
                        if (is.null(con)) {
                          if (!silent) cat("Attempting to create a new connection for:", con_name, "\n")
                          con <- .con_reg('create', section=section, engine=engine, registry=registry, silent=silent)
                          
                          if (!is.null(con)) {
                            .con_reg('push', con=con, registry=registry, silent=silent)
                          }
                        }                        
                                                
                        # return new or existing registry environment
                        return(con)
        } # get_con end

        # register connection in pool object
        # requires connection object, registry object name, silent
        , 'push' = {con_name <- attr(con, 'con_name')
                    # Store the connection in the registry
                    pool <- .con_reg('get_env', registry=registry, silent=silent)
                      
                    # Ensure con_name is valid
                    if (is.null(con_name) || con_name == "") {
                      stop("Invalid con_name: It is NULL or empty. Ensure the connection object has a valid 'con_name' attribute.")
                    }
                      
                      # Ensure pool is an environment
                    if (!inherits(pool, "environment")) {
                      stop("Connection registry (pool) is not an environment.")
                    }
                    
                    # print('push')
                    # print(class(con))  # Should be "Microsoft SQL Server" or similar
                    # print(typeof(con)) # Should be "S4"
                    
                    assign(con_name, con, envir = pool)
                    if (!silent) cat("New connection stored in the registry", "\n")
                    return(TRUE)
        } # push end

        # remove connection from pool object
        # requires connection object, registry object name, silent
        , 'drop' = {  pool <- .con_reg('get_env', registry)
                      con_name <- attr(con, 'con_name')
                      if (!silent) cat("Removing connection:", con_name, "\n")
                      # Find connection to remove
                      if (exists(con_name, envir = pool)) remove(list = con_name, envir = pool)
                      rslt <- TRUE
        } # drop end
        
        # clean by removing connections that are no longer valid
        # requires registry object name, silent
        , 'clean' = { pool <- get(registry)
                      con_objs <- ls(envir = pool)
                      for (con_name in con_objs) {
                        con <- get(con_name, envir = pool)
                        
                        if (!dbIsValid(con)) {
                          if (!silent) cat("Cleaning up stale connection:", con_name, "\n")
                          remove(list = con_name, envir = pool)
                        }
                      }
                      return(TRUE)
        }
        
        , 'default' = { 
          stop(glue("Invalid action parameter: {action}. Use 'get', 'push', 'clean'."))
          return(FALSE)
        } # clean end
        
  ) # switch end               
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(ConfigParser) # parse configuration file
# library(safer)        # encrypts text
# library(odbc)         # work with SQL Server
# Call this function via db_con(engine="SQLServer")
.sqlserv_con <- function(section=NULL
                      , silent=TRUE)
{ 
  if (!silent) {cat("Attempting to connect to SQL Server...\n")}

  # Check if a valid connection already exists and disconnect it
  # reuse_con <- check_existing_connection(section, engine, silent)
  # if (!is.null(reuse_con)) {return(reuse_con)}  
  
  # get configuration object
  params <- get_configset(section,silent=silent)

  con <- tryCatch(
    { dbConnect(odbc::odbc(),
                Driver = "ODBC Driver 17 for SQL Server",
                Server = "SQLPRDGEN01",
                Database = 'HEMEDB',
                trusted_connection = "yes")
    }, error = function(e) {
      cat("Error connecting to SQL Server:", e$message, "\n")
      NULL }
  )
  
  return(con)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(ConfigParser) # parse configuration file
# library(safer)        # encrypts text
# library(RMariaDB)     # work with mariadb
# This function expects a configuration file in the working directory
.mariadb_con <- function(section=NULL
                      , silent=TRUE)
{
  # get configuration object
  params <- get_configset(section,silent=silent)

  if (!silent) {
    if (exists('section')) cat('Configuration section',section,'\n')
    if (length(params)>2) {cat('Configurations loaded\n')}
    if (exists('encryptkey')) {cat('Encryption key exists\n')}
  }
  
  # mariadb needs a user, password, host, and port to connect
  schema <- params$schema
  user   <- params$user
  pass   <- params$encrypted_password
  host   <- params$host
  port   <- params$port
  
  if (!silent) {cat( ' Schema', schema, '\n'
                   , 'User', user, '\n'
                   , 'Password', decrypt_string(pass, key = encryptkey), '\n'
                   , 'Host', host, '\n'
                   , 'Port', port, '\n')
  }
  
  con    <- dbConnect(MariaDB()
                   , user = user
                   , password = decrypt_string(pass, key = encryptkey)
                   , host = host
                   , port = port
                   , dbname = schema)
  return(con)
}
# con <- db_con('esteylist','MariaDB')


## ----progress---------------------------------------------------------------------------------------------------------------------------------
progress_bar_custom <- R6::R6Class(
  "progress_bar_custom",
  inherit = progress::progress_bar,
  public = list(
    tick = function(len = 1) {
      old_echo <- getOption("echo")
      options(echo = TRUE)
      on.exit(options(echo = old_echo), add = TRUE)  # Ensure reset even on error
      super$tick(len)
      flush.console()  # Ensure the progress bar updates
    }
  )
)


## ---------------------------------------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
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
        , '\nSection:\t\t',         ?switcvhsection
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_copy_tbl <- function(schema, trgtbl, srctbl, data=FALSE) {

  # Drop the existing target table
  sql_drop_tbl(schema, trgtbl)
  
  # Construct the SQL command to copy the source table to the target with/without data
  if (data) {
    # all data
    sqlcmd <- glue("SELECT * INTO [{schema}].[{trgtbl}] FROM [{schema}].[{srctbl}];")  
  } else {
    # no data
    sqlcmd <- glue("SELECT * INTO [{schema}].[{trgtbl}] FROM [{schema}].[{srctbl}] WHERE 1=0;")  
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_desc_tbl <- function(schema, trgtbl, srctbl, data=FALSE) {

  # Construct the SQL command to copy the source table to the target with/without data
  if (data) {
    # all data
    sqlcmd <- glue("SELECT * INTO [{schema}].[{trgtbl}] FROM [{schema}].[{srctbl}];")  
  } else {
    # no data
    sqlcmd <- glue("SELECT * INTO [{schema}].[{trgtbl}] FROM [{schema}].[{srctbl}] WHERE 1=0;")  
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_store_rc <-function( paramsect=NULL
                       , section=NULL
                       , silent=TRUE) {
  
  # Assure required parameters
  silent  <- if (exists("silent")) silent else TRUE
  
  # Use the section passed in the parameter object
  if (!is.null(paramsect) & is.null(section)) section <- paramsect$section
  
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

  for (label in c(FALSE, TRUE)) {
    # target schema based on whether we do/don't have labels
    schema <- if_else(label, label_schema, base_schema)
    
    # Progress
    vers <- if (label) {'LABELED'} else {'CODED'}
    cat(red(glue('WORKING ON {vers} VERSION\n')))

    pb <- get_progress_bar(loop_cnt, "LOAD DATA FRAMES IN R AND COPY TO SQL SERVER")

    for (i in 1:loop_cnt) {
        tbl = retain[i]
        pb$tick() # info for user
        df <- get_rc_table(paramsect, tbl, label, silent)
        
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

    
    rm(pb)

    # Remove tables from memory
    suppressWarnings(rm(list=paramsect$retain))
    
    cat('\n')
  }
  
  # return from function
  return(summary_df)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Define the custom conversion function
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



## ---------------------------------------------------------------------------------------------------------------------------------------------
# sql_resize(df, schema, tbl, chcols, limit)
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



## ---------------------------------------------------------------------------------------------------------------------------------------------
# sql_insert_df(chunk_df |> select(record, everything()), 'esteylist.redcap_log', overwrite=FALSE)  
# sql_insert_df(target_df, sch_tbl, overwrite=TRUE)
sql_insert_df <- function(df = NULL,
                          sch_tbl = NULL,
                          limit = 250,
                          silent = TRUE,
                          test = FALSE,
                          date_suffix = NULL,
                          nrow = Inf,
                          batch_size = 5000,
                          ## assumes delete and then append, false just append 
                          overwrite = TRUE ) {
  
  # Test mode setup for debugging
  if (test) {
    df <- rslt_df
    
    sch_tbl <- 'esteylist.redcap_log'
    limit <- 250
    silent <- TRUE
    nrow = Inf
    date_suffix <- '%Y%m'
    batch_size = 3000
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
    })}

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
    sql_copy_tbl(schema, trgtbl=datetbl, srctbl=tbl, data=TRUE)
    if (!silent) message(paste("Dated copy of", tbl, "is", datetbl))
  }

  
  # Return the results
  return(TRUE)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# sql_insert_df(chunk_df |> select(record, everything()), 'esteylist.redcap_log', overwrite=FALSE)  
# sql_insert_df(target_df, sch_tbl, overwrite=TRUE)
sql_insert_df__X <- function(df = NULL,
                          sch_tbl = NULL,
                          limit = 250,
                          silent = TRUE,
                          test = FALSE,
                          date_suffix = NULL,
                          nrow = Inf,
                          batch_size = 5000,
                          overwrite = TRUE ## assumes delete and then append, false just append
                          ) {
  
  # Test mode setup for debugging
  if (test) {
    df <- target_df
    sch_tbl <- 'frozen.allarrival'
    limit <- 250
    silent <- FALSE
    nrow = Inf
    date_suffix <- '%Y%m'
    batch_size = 3000
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
  if (length(split_schema) != 2) {
    stop("Invalid `sch_tbl` format. Expected 'schema.table'.")  
  }
  
  # Local vars
  schema <- split_schema[1]
  tbl <- split_schema[2]
  rslt <- TRUE
  make_dated_copy <- !is.null(date_suffix)
  
  # Connection to database
  con <- db_con('esteylist', silent = TRUE)  # db_con('sqlserver')
  
  # Create/Delete empty table, fairly quick because no data
  if (overwrite) {
    tryCatch({
      # Try deleting all records (will fail if the table does not exist)
      dbExecute(con, glue("DELETE FROM [{schema}].[{tbl}]"))
      if (!silent) message(paste("Successfully deleted all records from", tbl))
    }, error = function(e) {
      # If deletion fails, assume the table does not exist and create an empty structure
      if (!silent) message(paste("Table", tbl, "did not exist. Creating empty structure."))
      empty_df <- df[0, ]
      dbWriteTable(con, Id(schema = schema, table = tbl), empty_df,
                   row.names = FALSE, overwrite = TRUE, append = FALSE)
    })
  }
  # Disconnect
  db_discon('con')  # Ensure disconnection
  
  
  # Return if there are no rows to process
  if (rowcnt==0) {
    db_discon('con')  # Ensure disconnection
    return(rslt)
  }  

  # check for oversize columns in import df, adjust width/type if needed
  resize_msg <- sql_resize(df, schema, tbl)
  if (!silent) message(resize_msg)  
  
  # con <- db_con('esteylist')
  # sql_cmd <- glue("
  #   SELECT COLUMN_NAME, CHARACTER_MAXIMUM_LENGTH 
  #   FROM INFORMATION_SCHEMA.COLUMNS 
  #   WHERE TABLE_SCHEMA = '{schema}' AND TABLE_NAME = '{tbl}';
  # ")
  # new_sql_col_len <- dbGetQuery(con, sql_cmd)
  # db_discon(con)
  
  
  # Connection to database
  con <- db_con('esteylist', silent = TRUE)  # db_con('sqlserver')
  # Write in batches appending
  # This can be fairly slow using dbWriteTable()
  if (!silent) message(paste("This can be slow, copying df to SQL Server"))      
  for (start_idx in seq(1, rowcnt, by = batch_size)) {
    # start_idx <- start_idx + if_else(start_idx==0, 1, batch_size)
    end_idx <- min(start_idx + batch_size - 1, rowcnt)
    batch_df <- df[start_idx:end_idx, ]
    tryCatch({
      dbWriteTable(con, Id(schema = schema, table = tbl), batch_df,
                   row.names = FALSE, overwrite = FALSE, append = TRUE)

      if (!silent) message(glue::glue("Successfully inserted {nrow(batch_df)} rows."))
    }, error = function(e) {
      rslt <<- e
      print(rslt)
    }, finally = {
    # Disconnect to ensure proper closure of the connection
      if (exists("con") && !is.null(con)) db_discon('con')
      if (!silent) message(paste("Done appending", rowcnt, "rows to", tbl, "structure"))      
  })   
  }

  # Make a dated copy?  Only do this if a date format was given
  # Utilizes SQL pass through for speed
  if (make_dated_copy) {
    datetbl <- paste0(tbl, "_", format(Sys.Date(), date_suffix))
    sql_copy_tbl(schema, trgtbl=datetbl, srctbl=tbl, data=TRUE)
  }

  
  # Return the results
  return(rslt)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_run_cmd <- function(con=NULL
                , section=NULL
                , cmd=NULL
                , engine=NULL
                , silent=TRUE
                , rslt_lst=NULL
                , test=FALSE
                , newitem=0) {
  
  # testing
  if (FALSE) {
    section<-'esteylist'
    engine   <-NULL
    silent   <-FALSE
    rslt_lst <-list()
    test     <-TRUE
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
    if (exists("eng", envir = .GlobalEnv) && !is.null(eng) && eng != "") {
      engine <- eng
    } else {
      engine <- 'MariaDB'
    }
  }
  if (!silent) {cat('engine =',engine,'\n')}
  
  # Establish a connection
  if (!is.null(con)) {con <- con} # use connection passed in
  if ( is.null(con)) {con <- db_con(section=section,engine=engine)} # create a new connection
  if ( is.null(con)) { # no connection found/made
    stop("Failed to connect to the database.")
  }

  if (is.null(rslt_lst)) {rslt_lst <- list()}  
  
  if (!silent) {
    cat("Connection to SQL Server\n")
  }
  
  tryCatch({
    
    # What is the first word in the SQL command?
    firstword <- strsplit(toupper(cmd), " ")[[1]][1]
    
    # Is the word "INTO" found in the SQL Server command, acts like CREATE
    hasinto  <- grepl("INTO", cmd, ignore.case = TRUE)
        
    if (hasinto || firstword %in% c('CREATE', 'INSERT', 'UPDATE', 'ALTER')) {
      # Extract the first match
      dbpath <- regmatches(cmd, regexpr("\\S*\\.\\S+", cmd))
      dbpaths <- unlist(regmatches(cmd, gregexpr("\\b\\w+\\.\\w+\\b(?=\\s+(?!AS)\\w+|\\s*;)", cmd, perl=TRUE)))
  
      
      # Find the components in the database pathing
      path_comps <- parse_path_components(dbpaths)
  
      # Assign the components
      tbl <- path_comps$tbl
      sch <- path_comps$sch
      db  <- path_comps$db
      if (!is.null(db)) {
        dbpath  <- paste(db, sch, tbl, sep='.')     
        sch_tbl <- paste(db, sch, tbl, sep='.')     
      } else if (!is.null(sch)) {
        sch_tbl <- paste(sch, tbl, sep='.')     
      }
  
      
      # SELECT to fetch from changed table
      fetchcmd <- paste0("SELECT * FROM ", sch_tbl, " ;")
  }
    # Display fetch and execution code
    if (!silent) {
      cat(paste("Fetch command:\n", fetchcmd, '\n'))    
      cat(paste("Executing command:\n", cmd, '\n'))
    }
    
    # Do we need to execute code in the finally?
    executecmd = TRUE # Except for SELECT
    fetchdata  = TRUE # Except for SELECT, DROP, DELETE
    
    if (!silent) {
      cat(  ' firstword =',   firstword,    '\n'
          , 'hasinto =',      hasinto,      '\n'
          , 'schema_table =', sch_tbl, '\n')
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
# dosql(con=NULL, dbname='HEMEDB', cmd, engine=NULL, silent=TRUE)
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_parse <- function(cmd) {
  clean <- paste(cmd,';\n')
  clean <- gsub(";\\s*\n", ";\n", clean)
  clean <- trimws(strsplit(clean, ";\n")[[1]])
  clean <- clean[nzchar(clean)]  # Keep only non-empty strings
  return(clean)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# get_rc_dictionary('esteylist')
get_rc_dictionary <- function(params_or_section=NULL
                       , ddname='rcdd'
                       , global=TRUE
                       , reload=FALSE
                       , silent=TRUE) {
  
  retval <- NULL
  
  # # testing
  # params_or_section <- section
  # reload <- FALSE
  # ddname <- 'rcdd'

  # Keep the data dictionary 'ddname' in memory
  if (!reload & is.data.frame(ddname)) {
    retval<-get(ddname)
    msg<-paste('Keeping',ddname,'in memory')
  }
  # Require a configurtion section or parameter list
  if (is.null(params_or_section)) {
    retval<-FALSE
    msg<-paste('Pass in either a parameter list or the name of configurations section\n')
    cat(msg)
  }
  # Return early
  if (!is.null(retval)) {
    if(!silent) {cat(msg,'\n')}
    return(retval)
  }

  # Get configuration params_or_section when section name passed in
  if (is.character(params_or_section)) {
    params_or_section<-get_configset(tolower(params_or_section),silent=silent)
  }
  
  # Read REDCap metadata into retval
  retval <- redcap_metadata_read(
    redcap_uri = params_or_section[['url']],
    token = decrypt_string(params_or_section[['encrypted_token']], encryptkey), 
    verbose = FALSE,
    config_options = NULL)$data
  
  # No dictionary found, return early
  if (is.null(retval)) {
    retval<-FALSE
    cat('Failed to load dictionary',ddname,'\n')
    return(retval)
  }
  retddname<-ddname
  
  # Assign dd to global or default environment
  if (global) {
    # in global environment
    assign(ddname, retval, envir = .GlobalEnv)  
    msg<-'Assigned redcap dictionary to global environment'
  } else {
    # in default environment
    assign(ddname, retval, envir = environment())  
    msg <- 'Assigned redcap dictionary to current environment'
  }

  if (!silent) {cat(msg,'\n')}
  return(retddname)
}
# get_rc_dictionary('dahlia', reload=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_rc_code <- function(fld=NULL, section='esteylist') {
  get_rc_dictionary(section)
  field_dd <- rcdd |>
    select(codes=select_choices_or_calculations, tbl=form_name, field_name, type=field_type) |>
    mutate( is_coded = case_when(
            codes>'' & type!='calc' ~ TRUE
            , .default=FALSE)
          , code = NA_character_
          , value = NA_character_) |>
    separate_longer_delim(codes, delim = "|") |>
    mutate(code=trimws(codes), code=str_replace(code, ',', '@@@')) |>
    separate(code, into = c("code", "value"), sep = "@@@") |>
    select(-codes)

  if (!is.null(fld)) {
    field_dd <- field_dd |> filter(field_name == fld)
  }
  return(field_dd |> select(tbl, fld=field_name, type, code, value))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_rc_log <- function(token=NULL
                       , url        = "https://redcap.iths.org/api/"
                       , logtype    = 'update'
                       , starttime  = NULL
                       , endtime    = Sys.time()
                       , section    = 'esteylist'
                       , excl_regex = '(?i)port\\b|api\\b|calculation\\b|quality\\b'
                       , weeks      = 1
                       , silent     = TRUE) {
  
  if(!silent) print('get_rc_log')

  if (FALSE) {
    token      = NULL
    url        = "https://redcap.iths.org/api/"
    logtype    = 'update'
    section    = 'esteylist'
    starttime  = chunk_start_date
    endtime    = chunk_end_date
    excl_regex = '(?i)port\\b|api\\b|calculation\\b|quality\\b'
    weeks      = 26
    silent     = FALSE
  }

  if (is.null(logtype)) logtype   = 'update'
  if (is.null(endtime)) endtime    = Sys.time()
  if (!is.null(weeks) & is.null(starttime) ) starttime = format(as.Date(endtime) - (weeks * 7), "%Y-%m-%d %H:%M")
  if (is.null(token)) {
    params <-get_configset(tolower(section),silent=silent)
    token <- decrypt_string(params[['encrypted_token']], encryptkey)
  }
  
  # format start/end as time
  starttime <- format(as.Date(starttime), "%Y-%m-%d %H:%M")
  endtime   <- format(as.Date(endtime),   "%Y-%m-%d %H:%M")
  
  incl_regex = paste0('(?i)',logtype)

  formData  <- list("token"=token,
                  content='log',
                  logtype='record_edit',
                  beginTime=starttime,
                  endTime=endtime,
                  format='json',
                  returnFormat='json')

  response <- httr::POST(url, body = formData, encode = "form")

  result   <- httr::content(response)
  
  df <- bind_rows(result)
  
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
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
      filter(str_detect(details, fld)) |>
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
      fld_expr <- paste0(fld_expr, glue('if_else(str_detect(details, "{fld}\\\\({code}\\\\) = "), "{code}", "NA"), '))
      fld_view <- paste0(fld_view, '\t', glue('if_else(str_detect(details, "{fld}\\\\({code}\\\\) = "), "{code}", "NA"), '), '\n')
      }
    
    # Wrap the expression with paste and str_c correctly
    fld_expr <- paste0('paste(str_c(', fld_expr, ' sep = " @@@ "))')
    fld_view <- paste0('paste(str_c(\n', fld_view, '\tsep = " @@@ "))')
    
    cat('\nField expression:\n', fld_view, '\n')

    # Use the dynamically generated codelist expression in mutate when of type checkbox
    rslt_df <- log_df |>
      filter(str_detect(details, fld)) |>
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



## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(dplyr) # likely part of standard libraries
# Find df widths for df list utilizing REDCap data dictionary and SQL Server INFORMATION SCHEMAS
# params <- get_configset('esteylist')
# get_width(params, label=TRUE, silent=TRUE)
get_width <- function( paramsect=NULL
                       , label=FALSE
                       , silent=TRUE) {

  # verify required parameters
  silent <- if (exists('silent')) silent else TRUE
  label  <- if (exists('label'))  label  else TRUE
  
  # Read in parameters from configurations section
  retain       <- paramsect$retain
  base_schema  <- paramsect$schema
  label_schema <- paste0(base_schema,'_label')

  schema <- if_else(label, label_schema, base_schema)

  get_rc_dictionary(paramsect$section,reload=TRUE)
  get_sql_dictionary(dbname=paramsect$dbname, schema=base_schema, reload=TRUE) 
  # get_sql_dictionary(dbname=paramsect$dbname, schema=schema, reload=TRUE)
  
  
    # get_sql_dictionary<-function(schema=NULL
    #                            , dbname='HEMEDB'
    #                            , ddname='sqldd'
    #                            , global=TRUE
    #                            , reload=FALSE
    #                            , silent=TRUE)  

  maxdf_ <- data.frame()

  loop_cnt <- length(retain)
  pb <- get_progress_bar(loop_cnt, "REVIEW COLUMNS")
  
  for (tbl in unlist(retain)) {
    tryCatch ({
      # make sure the table is in memory
      # if (!exists(tbl)) {get_rc_table(paramsect,tbl=tbl, label=label,silent=silent)}
      tmpdf   <- get(tbl)
      tmpdf <- data.frame(
          field_name = names(tmpdf),
          # sapply to get max number of characters
          max_char_length = sapply(tmpdf, function(col) max(nchar(as.character(col)), na.rm = TRUE)))
      # rownames(df1)<-NULL
      tmpdf <- suppressMessages(left_join(tmpdf,rcdd) |> filter(form_name==tbl & !field_type %in% c('checkbox','dropdown','radio')))
      success <- TRUE
    }, error=function(e){
      cat("Data frame",tbl,"not found\n")
      success <- FALSE
    })                              
    pb$tick()
    if (!success) {next}
    maxdf_ <- rbind(maxdf_,tmpdf)    
  }

  # Find maximum character lengths distinct column names
  sqldd_ <- sqldd |> 
    filter(TABLE_SCHEMA==schema & !grepl('___.*',COLUMN_NAME)) |>
    group_by(COLUMN_NAME) |>
    mutate( CHARACTER_MAXIMUM_LENGTH = max(CHARACTER_MAXIMUM_LENGTH, na.rm = TRUE, default = NA_real_)) |>
    ungroup() |>
    select(TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, CHARACTER_MAXIMUM_LENGTH, DATA_TYPE) |>
    distinct()
  
  # Perform the join, by table and column
  maxdf <- left_join(maxdf_, sqldd_, by = c('field_name'='COLUMN_NAME', 'form_name'='TABLE_NAME')) |>
    select(  TABLE_SCHEMA
           , form_name
           , field_name
           , field_type
           , field_label
           , select_choices_or_calculations
           , text_validation_type_or_show_slider_number
           , branching_logic
           , field_annotation
           , max_char_length
           , CHARACTER_MAXIMUM_LENGTH
           , DATA_TYPE
          ) 
  
  return(maxdf)
  
}

# maxdf <- get_width(section=section,retain)


## ---------------------------------------------------------------------------------------------------------------------------------------------
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
  # rm(list = ls(pattern = glue("^arrival\\d*$")), envir = .GlobalEnv)
  

  return(summary_df)
} # end func


## ---------------------------------------------------------------------------------------------------------------------------------------------
# section<-'diagnosis_for_aml_arrivals'
# params<-get_configset(section)
# x <- get_rc_table(params,'trm')
get_rc_table <- function(paramsect=NULL
                       , tbl=NULL
                       , label=FALSE
                       , silent=TRUE)
{
  
  # default return value
  df=data.frame()
  if (!silent) message("Loading df with ", tbl, " data")

  # require table name
  if (is.null(tbl)) {return(df)}
  # require parameter object
  if (is.null(paramsect)) {return(df)}  
  # assure dd is loaded
  if (!exists('rcdd')) get_rc_dictionary(paramsect, reload=TRUE, silent=TRUE)
  
  # set defaults
  api_token      <- paramsect$encrypted_token
  api_url        <- paramsect$url
  keyfields      <- paramsect$keyfields
  # let's assume that if keyfields is empty that the first field in the first redcap form is a keyfield
  if (length(keyfields)==0) keyfields = list('recordid')
  # is the project set up with a rectime for each table?
  rectime_column <- glue("{tbl}_rectime")
  rectime        <- rectime_column %in% rcdd$field_name
  # rectime        <- paramsect$rectime
  # rectime_column <- ifelse(rectime,paste0(tbl, "_rectime"),NA_character_)

  formData <- list(
    "token" = decrypt_string(api_token,encryptkey),
    content = 'record',
    action = 'export',
    format = 'json',
    type = 'flat',
    csvDelimiter = '',
    'forms[0]' = tbl,
    rawOrLabel = 'raw',  # <-- Default to 'raw'
    exportCheckboxLabel = 'true',
    exportSurveyFields = 'false',
    exportDataAccessGroups = 'false',
    returnFormat = 'json'
  )

  # # Add the key fields dynamically
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
  if (rectime) {
    df <- jsonlite::fromJSON(result) |> 
      filter(!is.na(.data[[rectime_column]]) & .data[[rectime_column]] != '') |>
      select(all_of(keyfields), -any_of(rectime_column), everything())
  } else {
    df <- jsonlite::fromJSON(result) |> 
      select(all_of(keyfields), everything())
  }

  return(df)
  
}
# rslt <- get_rc_row_ids('eln2022', label=FALSE, section='esteylist')


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Define the function
group_rc_chkbox <- function(df) {
  
  chkbx_cols <- df %>%
    colnames() %>%
    grep("___", ., value = TRUE) %>%               # Find columns with '___'
    grep("show", ., value = TRUE, invert = TRUE) %>%# Exclude columns containing 'show'
    sub("___.*", "", .) %>%                        # Remove suffix to get base column names
    unique()  
  
  # Ensure chkbx_cols is a character vector
  if (!is.character(chkbx_cols)) {
    stop("chkbx_cols must be a character vector.")
  }
  
  # Create or update summary columns for each col
  for (col in chkbx_cols) {
    df <- df %>%
      mutate(!!col := apply(select(df, starts_with(paste0(col, "___"))), 1, function(x) {
        paste(x[x != ""], collapse = " | ")
      }))
  }

  # # Create or update summary columns for each col
  # for (col in chkbx_cols) {
  #   df <- df %>%
  #     mutate(!!col := apply(select(df, starts_with(paste0(col, "___"))), 1, function(x) {
  #       # Improved apply function that handles empty strings and NAs
  #       paste(x[!is.na(x) & x != ""], collapse = " | ")
  #     }))
  # }
  
  # Find the position of the first checkbox column for each col
  col_order <- colnames(df)
  
  for (col in chkbx_cols) {
    chkbox1 <- grep(paste0("^", col, "___"), colnames(df), value = TRUE)[1]
    pos <- if (!is.na(chkbox1)) which(col_order == chkbox1) else ncol(df) + 1
    
    # Reorder columns to place the summary column before the first checkbox column
    col_order <- c(
      col_order[1:(pos - 1)],  # Columns before the first checkbox column
      col,  # The summary column
      col_order[(pos):length(col_order)]  # The checkbox columns and any remaining columns
    )
  }
  
  # Ensure columns are in the correct order and select them
  df %>% select(all_of(unique(col_order))) |> select(-contains('___'))
  
  
}



## ---------------------------------------------------------------------------------------------------------------------------------------------
# suppressWarnings(suppressMessages(library(REDCapTidieR)))
redcap_supertibble <- function(section='esteylist',forms=NULL){
  # this code expects a configuration file in the working directory
  encryptkey = get_encryptkey()  
  # configs<-ConfigParser$new()
  # configs<-configs$read('config_.ini') 
  configs <- ConfigParser$new()$read(file.path(getwd(), 'config_.ini'))
  
  url=configs$get(option='url',section=section)
  token=configs$get(option='encrypted_token',section=section)
  rc_tibble <- read_redcap(url, decrypt_string(token, key = encryptkey), forms=forms)  
  return(rc_tibble)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# get_sheet(ctmsfile, encryptkey=encryptkey, startRow=3)
get_sheet <- function(filepath, encryptkey = get_encryptkey(), startRow = 1, rows=NULL, silent=TRUE, test=FALSE) {
  
  if(test) {
    filepath <- ctmsfile
    encryptkey <- get_encryptkey()
    startRow   <- 5
    rows=NULL
    silent=FALSE
  }
  
  if (!silent) print(filepath)
  # Check if the file exists
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }
  
  ext <- tools::file_ext(filepath)
  
  if (is.null(rows) & ext=='csv') {
    rows = Inf
  }

  # Validate file type
  valid_types <- c("xlsx", "csv")
  if (!ext %in% valid_types) {
    stop("Invalid file type. Supported types are: ", paste(valid_types, collapse = ", "))
  }
  
  if (!silent) cat(filepath, '\n', ext, '\n', startRow, '\n', rows, '\n')

  # Read the file based on its type
  if (ext == "csv") {
    # Read the .csv file
    df <- readr::read_csv(filepath, show_col_types = FALSE, n_max=rows)
    return(df)
  }

  # first attempt to with password
  file_opened <- tryCatch({
    df <- decrypt_excel(filepath, password=encryptkey, startRow = startRow, rows=rows)
    TRUE
  }, error = function(e) {
    FALSE
  })

  # second open without password, if first attempt failed
  if (!file_opened) {
      file_opened <- tryCatch({
        df <- openxlsx::read.xlsx(filepath, sheet=1, startRow = startRow)
        TRUE
      }, error = function(e) {
        FALSE
      })
  }
  
  return(df)
}

# depreciated
get_excel_file <- function(filepath) {get_spreadsheet(filepath)}



## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(fs)  # Make sure to load the fs package for dir_ls
# latest_file <- get_last_file_name(importdir, pattern = "(?i)all.*", ext='csv')

get_last_file_name <- function(path, ext = 'xlsx', pattern = '.*', encryptkey = NULL) {
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
# get_last_file(path=ctmsdir, startRow=5)
get_last_file <- function(path, ext='xlsx', hasdate=NULL, pattern='', encryptkey=NULL, startRow = 1) {
  
  if (FALSE) {
    path=ctmsdir
    startRow=5
    ext='xlsx'
    hasdate=NULL
    pattern=''
    encryptkey=encryptkey
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
    encryptkey <- NULL
    startRow   <- 5
  }

  return(df)
}

get_last_file_old <- function(path, ext='xlsx', hasdate=NULL, encryptkey=NULL, startRow = 1) {
  
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

  df <- get_sheet(last_file,encryptkey, startRow = startRow)
  
  return(df)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Function to read a password-protected excel file and return a df
# df <- decrypt_excel(filepath, password=encryptkey, startRow = startRow, rows=rows)
decrypt_excel <- function(filepath, password, sheet=1, silent=TRUE, startRow = 1, rows=NULL) {
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

# # # Example usage
# epicdir <- paste0(amlroot,'/AML Database Resources/EPIC Downloads')
# filepath <- paste(epicdir,'CYTOGENETICS_2024_Thus_far_53_of_503_are_in_FH2298__CS_20240730_1248.xlsx', sep='/')
# 
# # filepath
# # encryptkey
# df <- decrypt_excel(filepath, sheet=1, password=encryptkey, silent=TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Create sheet xlsx or csv
make_sheet <- function(df,
                      path      = "C:/Users/cmshaw/Desktop/",
                      sheet     = "Sheet1",
                      filename  = "Results_",
                      ext       = "csv",
                      date      = TRUE,
                      time      = FALSE,
                      overwrite = FALSE,
                      test      = FALSE) {
  
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
write_to_file <- function(dfs, output_names = NULL, 
                          directory = getwd(), date_format = "", 
                          output_type = "xlsx", output_file = NULL) {
  # Ensure dfs is a list of data frames
  if (!is.list(dfs)) {
    dfs <- list(dfs)
  }
  
  dfs = list(strong, match, review, unknown)

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
    write_to_excel(dfs, output_names, file_path)
    message("Excel workbook saved to: ", file_path)
  } else if (tolower(output_type) == "csv") {
    write_to_csv(dfs, output_names, directory)
    message("CSV files saved to directory: ", directory)
  } else {
    stop("Unsupported output_type. Use 'xlsx' or 'csv'.")
  }
}

write_to_excel <- function(dfs, output_names, file_path) {
  # Create a workbook
  wb <- createWorkbook()
  
  # Add worksheets and write data frames to them
  for (i in seq_along(dfs)) {
    addWorksheet(wb, output_names[i])
    if (!is.data.frame(dfs[[i]])) {
      stop(paste("Element", i, "in dfs is not a data frame. Check input."))
    }
    writeData(wb, output_names[i], dfs[[i]]) # Ensure full data frame is passed
  }
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

write_to_csv <- function(dfs, output_names, directory) {
  # Write each data frame to its own CSV file
  for (i in seq_along(dfs)) {
    file_name <- paste0(output_names[i], ".csv")
    file_path <- file.path(directory, file_name)
    write.csv(dfs[[i]], file_path, row.names = FALSE)
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(knitr)
# library(kableExtra)
display_kable <- function(rslt=data.frame()
                        , title='Results')
{
  kable(rslt, format = "html", table.attr = "style='width:80%;'") |>
    kable_styling(
        bootstrap_options = 
          c("striped"
          , "hover"
          , "condensed"
          , "responsive")
      , full_width = F) |>
    add_header_above(setNames(ncol(rslt), title))
}
# summary_df <- data.frame(
#   Row = 1:length(retain),
#   Tables = retain,
#   Schemas = 'esteylist',
#   Rows = -999
# )
# display_kable(summary_df,'TEST HEADING')


## ----eval=FALSE, include=FALSE----------------------------------------------------------------------------------------------------------------
# # call the send.mail() function of mailR
# send.mail(
#   from         = "cmshaw@fredhutch.org",
#   to           = "cmshaws@fredhutch.org",
#   bcc          = "cmshaw@fredhutch.org",
#   subject      = "Hello World",
#   body         = "Hello World",
#   encoding     = "iso-8859-1",
#   html         = TRUE,
#   inline       = FALSE,
#   smtp         = list(host.name="smtp.fhcrc.org", port=25),
#   authenticate = FALSE,
#   timeout      = 60000,
#   send         = TRUE,
#   attach.files = NULL,
#   debug        = FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Combine rows based on specific conditions in the "treatment" column


treatment_mapping <- function(df_name) {
  pall_text <- "pall|hospice|treatment|comfort|support|surve|nothing|none|died|refuse|maint|condition|palii|watch"
  outside_text <- c('lfu', 'home', 'at home', 'rx at home')
  
  df <- get(df_name) |>
    mutate(
        temp_treatment         = tolower(trimws(treatment))
      , temp_response          = tolower(trimws(response))
      , temp_treatmentdate     = as.Date(treatmentdate)
      , temp_c1date            = as.Date(a_c1date)
      # , temp_c1date            = as.Date(cycle1treatmentdate)
      , temp_response          = tolower(trimws(response))
      , temp_treatmentlocation = tolower(trimws(treatmentlocation))
    ) |>
    mutate(
      treatmentstatus_mapped = case_when(
          is.na(temp_treatment)                                         ~ "Missing"
        , grepl("^\\?\\?", temp_treatment)                              ~ "Missing"
        , response=="DIED without treatment"                            ~ "No Treatment"
        , grepl("cr|mlfs",temp_response) &
            grepl(pall_text,temp_treatment)                             ~ "Remission or MLFS without Treatment Record"
        , grepl(pall_text,temp_treatment)                               ~ "No Treatment"
        , grepl("opinion|consult", temp_treatment) &
            grepl("cr|mlfs",temp_response)                              ~ "Remission or MLFS after 2nd Opinion Consult"
        , grepl("opinion|consult", temp_treatment)                      ~ "2nd Opinion Consult"
        , grepl(" vs |uncert", temp_treatment)                          ~ "Unclear"
        , temp_treatment %in% outside_text                              ~ "Unknowable"
        , grepl("unknow|no record", temp_treatment)                     ~ "Unknowable"
        , TRUE                                                          ~ "Treated")
    , treatmentdate_mapped = case_when(
          is.na(temp_treatmentdate) & !is.na(temp_c1date)               ~ temp_c1date
        , TRUE ~ temp_treatmentdate)
    , temp_treatmentlocation = str_replace(temp_treatmentlocation,"FHCRC","FHCC")
    , treatmentlocation_mapped = case_when(
        grepl('cr|mlfs', temp_response) & 
          treatmentstatus_mapped == '2nd Opinion'             ~ 'Remission or MLFS without Treatment Record'
        , is.na(temp_treatmentlocation) & 
          grepl("outside", temp_treatment)                    ~ "OUT"
        , is.na(temp_treatmentlocation) & 
          treatmentstatus_mapped == 'No Treatment'            ~ 'No Treatment'
        , is.na(temp_treatmentlocation) & 
          treatmentstatus_mapped == '2nd Opinion'             ~ 'No Treatment'
        , is.na(temp_treatmentlocation) & 
          treatmentstatus_mapped == 'Treated'                 ~ 'FHCC'
        , is.na(temp_treatmentlocation) & 
          treatmentstatus_mapped == 'Treated'                 ~ 'FHCC'
        , is.na(temp_treatmentlocation) & 
          grepl('home', temp_treatment) & 
          treatmentstatus_mapped == 'Unknowable'              ~ 'OUT'
        , is.na(temp_treatmentlocation) & 
          treatmentstatus_mapped == 'Unclear' & 
          grepl('resist|remiss', temp_response)               ~ 'FHCC'
         , grepl('fh',temp_treatmentlocation)                 ~ 'FHCC'
        , TRUE                                                ~ treatmentlocation)
  ) |>
  select(-starts_with("temp_"))
  return(df)
  
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
## response_mapping
response_mapping <- function(df_name)
{ get(df_name) |>
    # temporary variables
    mutate(
        temp_treatment_status   = tolower(trimws(treatmentstatus_mapped))
      , temp_responsedate       = as.Date(responsedate)
      , temp_arrivalflowdate    = as.Date(arrivalflowdate)
      , temp_cycle1responsedate = as.Date(a_c1respdate)
      , temp_arrivalflow        = as.character(arrivalflow)
      , temp_responsedate       = as.Date(responsedate)
      , temp_response           = tolower(trimws(response))
    ) |>
    mutate(
        responsedate_mapped = case_when(
          !is.na(temp_responsedate)        ~ temp_responsedate,
          !is.na(temp_arrivalflowdate) & 
            temp_arrivalflow == "0"        ~ temp_arrivalflowdate,
          !is.na(temp_cycle1responsedate)  ~ a_c1respdate)
      , response_mapped = case_when(
          grepl("no treatment|2nd opinion|unknowable", temp_treatment_status) ~ 'Not Applicable',
          is.na(temp_response) ~ 'Missing',
          temp_response %in% c("missing", "currently no response") ~ "Missing",
          grepl("outside|insuff|unknow", temp_response) ~ "Unknowable",
          grepl("cr", temp_response) ~ "Remission",
          grepl("partial|complete|remission|mlfs", temp_response) ~ "Responded",
          grepl("resist|progress|refract", temp_response) ~ "Resistant",
          TRUE ~ response
      )
    ) |>
    select(-starts_with("temp_"))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
## treatmentlocation_mapping
# modify the location of treatment utilizing information in response and treatment status.
# treatmentlocation_mapping <- function(df)
# { df |>
#     mutate(
#       temp_treatment         = tolower(trimws(treatment)),
#       temp_response          = tolower(trimws(response)),
#       temp_treatment_status  = tolower(trimws(treatment_status)),
#       temp_treatmentlocation = treatmentlocation  # Keep the original case for comparison
#     ) |>
#     mutate(treatmentlocation_status = case_when(
#       grepl('cr', temp_response) & treatment_status == '2nd Opinion' ~ 'Response without Treatment Record',
#       is.na(temp_treatmentlocation) & grepl("outside", temp_treatment, ignore.case = TRUE) ~ "OUT",
#       is.na(temp_treatmentlocation) & treatment_status == 'No Treatment' ~ 'No Treatment',
#       is.na(temp_treatmentlocation) & treatment_status == '2nd Opinion' ~ 'No Treatment',
#       is.na(temp_treatmentlocation) & treatment_status == 'Treated' ~ 'FHCC',
#       is.na(temp_treatmentlocation) & treatment_status == 'Treated' ~ 'FHCC',
#       is.na(temp_treatmentlocation) & grepl('home', temp_treatment) & treatment_status == 'Unknowable' ~ 'OUT',
#       is.na(temp_treatmentlocation) & treatment_status == 'Unclear' & grepl('resist|remiss', temp_response) ~ 'FHCC',
#       TRUE ~ treatmentlocation
#     )) |>
#     select(-temp_treatment, -temp_response, -temp_treatment_status, -temp_treatmentlocation)
# }
# 



## ---------------------------------------------------------------------------------------------------------------------------------------------
responsedate_calculated <- function(df)
{
  df <- df |>
    mutate(responsedate_calculated = case_when(
      !is.na(responsedate)                         ~ responsedate,
      !is.na(arrivalflowdate) & arrivalflow == "0" ~ arrivalflowdate,
      !is.na(cycle1responsedate)                   ~ cycle1responsedate
  ))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
## arrivaltype_mapping
arrivaltype_mapping <- function(df)
{ df |>
    # temporary variables
    mutate(
        temp_treatment_status   = tolower(trimws(treatmentstatus_mapped))
      , temp_responsedate       = as.Date(responsedate)
      , temp_arrivalflowdate    = as.Date(arrivalflowdate)
      , temp_cycle1responsedate = as.Date(cycle1responsedate)
      , temp_arrivalflow        = as.character(arrivalflow)
      , temp_responsedate       = as.Date(responsedate)
      , temp_response           = tolower(trimws(response))
    ) |>
    mutate(
        responsedate_mapped = case_when(
          !is.na(temp_responsedate)        ~ temp_responsedate,
          !is.na(temp_arrivalflowdate) & 
            temp_arrivalflow == "0"        ~ temp_arrivalflowdate,
          !is.na(temp_cycle1responsedate)  ~ cycle1responsedate)
      , response_mapped = case_when(
          grepl("no treatment|2nd opinion|unknowable", temp_treatment_status) ~ 'Not Applicable',
          is.na(temp_response) ~ 'Missing',
          temp_response %in% c("missing", "currently no response") ~ "Missing",
          grepl("outside|insuff|unknow", temp_response) ~ "Unknowable",
          grepl("cr", temp_response) ~ "Remission",
          grepl("partial|complete|remission|mlfs", temp_response) ~ "Responded",
          grepl("resist|progress|refract", temp_response) ~ "Resistant",
          TRUE ~ response
      )
    ) |>
    select(-starts_with("temp_"))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
calculate_trm <- function(df) {
  
  # Check if dataframe has at least 9 columns
  if (ncol(df) < 9) {stop("The dataframe must have at least 9 columns.")}
  
  # Make a working copy
  df_temp<-df

  # Assign column names
  names(df_temp) <- c("recordid", "PS", "age", "secondary", "platelets", "wbc", "blast", "albumin", "creatinine")

  # Calculate TRM
  df_temp <- df_temp %>%
    rowwise() %>%
    mutate(
      trm_score = 100 / (1 + exp(-(-4.08 + 0.89 * PS + 0.03 * age - 0.008 * platelets - 0.48 * albumin +
                            0.47 * secondary + 0.007 * wbc - 0.007 * blast + 0.34 * creatinine))),
      trm_score = round(trm_score, 3)
    ) %>%
    ungroup()

  # Add TRM Score back to starting df and return
  df <- df |> mutate(trm_score = df_temp$trm_score)
  return(df)
}
# df <- data.frame(
#   a = 1:3,
#   b = c(1, 2, 3),
#   c = c(45, 55, 60),
#   secondary = c(0, 1, 0),
#   platelets = c(150, 200, 100),
#   wbc = c(10, 15, 12),
#   blast = c(5, 3, 2),
#   albumin = c(3.5, 3.0, 3.2),
#   creatinine = c(1.2, 1.5, 1.1)
# )
# newdf<-calculate_trm(df)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# remove objects from environment
rmv <- function(type = 'chr', envir = .GlobalEnv)
{ obj_names <- ls(all = TRUE, envir = envir)
  objs <- mget(obj_names, envir = envir)
  
  if (type == 'chr') {rm(list = obj_names[sapply(objs, class) == "character"], envir = envir)} 
  else if (type == 'tib') {rm(list = obj_names[sapply(objs, function(x) "data.frame" %in% class(x))], envir = envir)} 
  else if (type == 'df') {rm(list = obj_names[sapply(objs, function(x) identical(class(x), "data.frame"))], envir = envir)} 
  else if (type == 'lst') {rm(list = obj_names[sapply(objs, class) == "list"], envir = envir)} 
  else if (type == 'log') {rm(list = obj_names[sapply(objs, class) == "logical"], envir = envir)}
  else if (type == 'all') {rm(list = ls(all = TRUE))}
}



## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(progress)
# library(glue)

# Function to create a custom progress bar with alternating symbols
get_progress_bar <- function(n = 100, msg = "Testing Progress Bar") {
  # Format for progress bar with uppercase message
  format <- glue("{toupper(msg)} [:bar] :percent in :elapsed")

  # Define custom R6 class extending progress_bar
  ProgressBarCustom <- R6::R6Class(
    "ProgressBarCustom",  # Class name
    inherit = progress::progress_bar,  # Inherit from base progress_bar class
    public = list(
      # Override tick() to alternate between "|" and "--"
      tick = function(len = 1) {
        old_echo <- getOption("echo")  # Save current echo option
        options(echo = TRUE)  # Set echo to TRUE for output
        on.exit(options(echo = old_echo), add = TRUE)  # Reset echo after function
        flush.console()  # Force console update
        super$tick(len)  # Call the original tick method
      }
    )
  )

  # Return instance of the custom progress bar
  return(ProgressBarCustom$new(format = format, total = n, clear = FALSE, width = 120))
}


get_progress_bar_old <- function(n=100,msg="Downloading") {
  format <- glue("{toupper(msg)} [:bar] :percent in :elapsed")
  return(progress_bar$new(format = format
                       , total  = n
                       , clear  = FALSE
                       , width  = 120))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
compare_df_columns <- function(df1, df2) {
  # Get column names and types
  cols_df1 <- tibble(Column = names(df1), Type = sapply(df1, class))
  cols_df2 <- tibble(Column = names(df2), Type = sapply(df2, class))
  
  # Join on 'Column' to compare types
  merged <- full_join(cols_df1, cols_df2, by = "Column", suffix = c("_df1", "_df2"))
  
  # List of columns with matching names and types
  matching_columns <- merged %>%
    filter(Type_df1 == Type_df2, !is.na(Type_df1), !is.na(Type_df2)) %>%
    pull(Column)
  
  # List of columns with the same names but different types
  different_types <- merged %>%
    filter(Type_df1 != Type_df2, !is.na(Type_df1), !is.na(Type_df2)) %>%
    select(Column, Type_df1, Type_df2) %>%
    as.data.frame()
  
  # List of columns only in one of the dataframes
  only_in_df1 <- merged %>%
    filter(is.na(Type_df2)) %>%
    pull(Column)
  
  only_in_df2 <- merged %>%
    filter(is.na(Type_df1)) %>%
    pull(Column)
  
  only_in_either <- data.frame(
    Column = c(only_in_df1, only_in_df2),
    Found_in = c(rep("df1", length(only_in_df1)), rep("df2", length(only_in_df2)))
  ) %>%
    arrange(Column)  # Sort alphabetically by Column
  
  # Output results to the screen
  cat("Matching Columns (same name and type):\n")
  print(matching_columns)
  
  cat("\nColumns with the same name but different types:\n")
  if (nrow(different_types) > 0) {
    print(different_types)
  } else {
    cat("None\n")
  }
  
  cat("\nColumns only in one of the dataframes (sorted alphabetically):\n")
  print(only_in_either)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_prev_tues <- function(date) {
  # Calculate the difference from Tuesday (1 = Sunday, 7 = Saturday, 3 = Tuesday)
  weekday <- wday(date)
  diff <- ifelse(weekday > 3, weekday - 3, weekday + 4)
  # Subtract to get the previous Tuesday
  return(date - days(diff))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)

create_heat_map <- function(df, true_color = "red", false_color = "green", missing_types = c(NA, "", "No answer entered")) {
  # Function to identify "missing" values based on the provided types
  is_missing <- function(value) {
    any(sapply(missing_types, function(x) {
      if (is.na(x)) {
        is.na(value)
      } else {
        value == x
      }
    }))
  }
  
  # Apply the missing function to create a new column for the heat map
  df_long <- df %>%
    mutate(row = row_number()) %>%
    pivot_longer(-row, names_to = "Variable", values_to = "Value") %>%
    mutate(Missing = sapply(Value, is_missing))
  
  # Create the heat map
  ggplot(df_long, aes(x = Variable, y = row)) +
    geom_tile(aes(fill = Missing), color = "white") +
    scale_fill_manual(values = c("TRUE" = true_color, "FALSE" = false_color)) +
    labs(title = "Heat Map of Missing Values",
         fill = "Missing") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
freq <- function(vec, title=NULL) {
  # title <- if (is.null(title)) deparse(substitute(vec))
  if (is.null(title)) {
    title <- deparse(substitute(vec))
  }

  # Header and footer for output
  header  <- data.frame(Value = title,   Frequency = "", Percentage = "")

  # Calculate frequency and percentage
  summary_df <- as.data.frame(table(vec))
  colnames(summary_df) <- c("Value", "Frequency")
  summary_df$Percentage <- round((summary_df$Frequency / sum(summary_df$Frequency)) * 100, 2)
  
  # Create a table with the title (or vector name if no title passed) on the first row
  result_df <- rbind(header, summary_df |> arrange(desc(Percentage)))
  
  return(result_df)
}




## ---------------------------------------------------------------------------------------------------------------------------------------------
neatfreq <- function(vec, title=NULL) {
  if (is.null(title)) {
    title <- deparse(substitute(vec))
  }

  # Header and footer for output
  header  <- data.frame(Value = title,   Frequency = "", Percentage = "")

  # Calculate frequency and percentage
  summary_df <- as.data.frame(table(vec))
  colnames(summary_df) <- c("Value", "Frequency")
  summary_df$Percentage <- round((summary_df$Frequency / sum(summary_df$Frequency)) * 100, 2)
  
  # Remove empty rows and rows with only whitespace or empty values
  summary_df <- summary_df[summary_df$Frequency > 0 & summary_df$Value != "", ]
  
  # Create a table with the title (or vector name if no title passed) on the first row
  result_df <- rbind(header, summary_df |> arrange(desc(Percentage)))
  
  # Formatting to improve readability
  result_df$Frequency <- format(result_df$Frequency, big.mark = ",")  # Add commas for thousands
  result_df$Percentage <- paste0(result_df$Percentage, "%")  # Add percentage sign
  
  # Output with kable for nicer formatting
  kable(result_df, format = "pipe", align = "lccc", caption = paste("Frequency Table for", title))
}



## ---------------------------------------------------------------------------------------------------------------------------------------------
# Returns directory files matching a regex pattern
file_regex <- function(path, pattern) {
  # List all files in the directory
  files <- list.files(path, full.names = FALSE)
  
  # Filter files by matching the pattern
  matching_files <- files[str_detect(basename(files), regex(pattern, ignore_case = TRUE))]
  
  return(matching_files)
}

# Example usage
# directory <- linkdir
# pattern <- "(i?)link"
# matching_files <- list_files_by_regex(directory, pattern)
# print(matching_files)


## ---------------------------------------------------------------------------------------------------------------------------------------------
close_dfs <- function() {
  # Get all objects in the global environment
  glob_objs <- ls(envir = .GlobalEnv)
  
  # Identify data frame objects
  df_objs <- glob_objs[sapply(glob_objs, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
  
  # Remove data frame objects
  if (length(df_objs) > 0) {
    rm(list = df_objs, envir = .GlobalEnv)
  }
  
  rm('glob_objs', 'df_objs')
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
keep_objs <- function(type = 'func', keep=NULL) { 

  # Get a list of all objects in the global environment
  glob_objs <- ls(envir = .GlobalEnv)
  
  # Check if 'keep' already exists in the global environment, if not, initialize it as an empty list
  if ("keep" %in% glob_objs) {
      keep <- get("keep", envir = .GlobalEnv)
  } else { # keep needs to be an empty list
    keep <- list()
  }

  
  # Identify which of those objects are data frames
  df_objs <- glob_objs[sapply(glob_objs, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
  
  # Identify which of those objects are functions (closures)
  func_objs <- glob_objs[sapply(glob_objs, function(x) is.function(get(x, envir = .GlobalEnv)))]
  
  # Identify which of those objects are atomic values (non-list types like vectors, numbers, etc.)
  atomic_objs <- glob_objs[sapply(glob_objs, function(x) is.atomic(get(x, envir = .GlobalEnv)))]  
  
  # Remove whitespace and split the 'type' into a vector (case-insensitive)
  types <- tolower(gsub("\\s+", "", type))  # Remove spaces and convert to lowercase
  types <- unlist(strsplit(types, ","))  # Split by commas
  
  # Include all types if 'all' is specified
  if ('all' %in% types) {
    keep <- glob_objs
  }
  
  # Add the appropriate objects based on the 'type' values
  if ('func' %in% types) {
    keep <-c(keep, func_objs)
  }
  if ('df' %in% types) {
    keep <- c(keep, df_objs)
  }
  if ('val' %in% types) {
    keep <- c(keep, atomic_objs)
  }
  
  # Remove duplicates in the final 'keep' list
  keep <- unique(keep)
  
  # Store the 'keep' list back into the global environment
  assign("keep", keep, envir = .GlobalEnv)
  
  return(keep)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
addrow <- function(df_name = 'todo', dir = '', file = '', action = '') {
  # If no parameters are passed, create an empty table
  if (dir == '' && file == '' && action == '') {
    # Create an empty data frame and assign it to the specified name in the global environment
    df <- data.frame(
      row = integer(0),
      dir = character(0),
      file = character(0),
      action = character(0),
      stringsAsFactors = FALSE
    )
    assign(df_name, df, envir = .GlobalEnv)
    return(df)
  }

  # Check if the dataframe exists in the global environment
  if (!exists(df_name, envir = .GlobalEnv)) {
    # Create a new dataframe with initial row and assign it to the specified name in the global environment
      df <- data.frame(
        row    = 1,
        dir    = dir,
        file   = file,
        action = action)
    assign(df_name, df, envir = .GlobalEnv)
  } else {
    # If the dataframe exists, retrieve it from the global environment
    df <- get(df_name, envir = .GlobalEnv)
    
    # Create a new row data frame
    df_ <- data.frame(
      row = nrow(df) + 1,
      dir = dir,
      file = file,
      action = action,
      stringsAsFactors = FALSE
    )
    
    # Bind the new row to the existing data frame
    df <- rbind(df, df_)
    assign(df_name, df, envir = .GlobalEnv)
  }
}


addrow_ <- function(df_name = 'df', dir = '', file = '', action = '') {
  # Get the current DataFrame from the global environment
  df <- get(df_name, envir = .GlobalEnv)
  
  if (is.null(df)) {
    # Create a new to-do list data frame and assign it to the specified name in the global environment
    df <- data.frame(
      row = 1,
      dir = dir,
      file = 'icicle_setup.R',
      action = 'icicle_setup',
      stringsAsFactors = FALSE
    )
    assign(df_name, df, envir = .GlobalEnv)
  } else {
    # Create a new row data frame
    df_ <- data.frame(
      row = nrow(df) + 1,
      dir = dir,
      file = file,
      action = action
    )
    
    # Bind the new row to the existing data frame
    df <- rbind(df, df_)
    assign(df_name, df, envir = .GlobalEnv)
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
addtodf <- function(df_name, ...) {
  # Convert df_name to character
  df_name <- deparse(substitute(df_name))
  
  # Capture the column-value pairs passed in as arguments
  args <- list(...)

  # Extract column names and values
  col_names <- lapply(args, function(x) unlist(str_split_i(x, " ", 1)))
  col_vals  <- lapply(args, function(x) unlist(trimws(str_replace(x, str_split_i(x, " ", 1), ""))))  

  # cat(col_names, col_names2)
  # cat(col_vals, col_vals2)
  
  # Create a new row as a named list
  new_row <- setNames(as.list(col_vals), col_names)
  
  # Check if the dataframe exists in the global environment
  if (!exists(df_name, envir = .GlobalEnv)) {
    
    # Set up the first row with a 1 as the order
    new_row$order <- 1  # Start order from 1

    # Create a new dataframe with the single row
    df <- as.data.frame(new_row, stringsAsFactors = FALSE)
    
  } else {
    # Retrieve the existing dataframe
    df <- get(df_name, envir = .GlobalEnv)
    
    # Ensure that all existing columns are in the new row, fill missing ones with NA
    for (col in setdiff(names(df), col_names)) {
      new_row[[col]] <- NA
    }
    
    # Ensure that new columns are added to the existing dataframe, fill existing rows with NA
    for (col in setdiff(col_names, names(df))) {
      df[[col]] <- NA
    }

    # Add the new row with the incremented order value
    new_row$order <-  max(df$order, na.rm = TRUE) + 1
    
    # Convert new_row to a dataframe and bind it to the existing dataframe
    df <- rbind(df, as.data.frame(new_row, stringsAsFactors = FALSE))
    
    
  }
  
  # Assign the updated dataframe back to the global environment
  assign(df_name, df, envir = .GlobalEnv)
}



## ---------------------------------------------------------------------------------------------------------------------------------------------

# Define a function to convert applicable character columns to Date
convert_date_cols <- function(df) {
  # newdf <- 
  newdf <- df %>%
    mutate(across(where(is.character), ~ {
      col <- .
      
      # Convert empty strings to NA to handle them properly
      col[col == ""] <- NA

      # Filter out NA values
      non_missing <- col[!is.na(col)]
      
      # Return unchanged if no non-missing values or if not all match the date pattern
      if (length(non_missing) == 0 || !all(grepl("^\\d{4}-\\d{2}-\\d{2}$", non_missing))) {
        return(col)
      }
      
      # Convert to Date if all values match the pattern
      as.Date(col, format = "%Y-%m-%d")
    }))
  
  return(newdf)
}


# Define a function to convert applicable character columns to Date
convert_date_cols_4 <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ {
      
      col <- .

      # Filter out NA and blank values for the column
      non_missing <- .[!is.na(.) & . != ""]
      
      # Return unchanged if there are no non-missing values
      if (length(non_missing) == 0) return(.)
      
      # Check if all remaining values start with a date pattern
      if (!all(grepl("^19\\d{2}-|^20\\d{2}-", non_missing)))  return(.)
      
      # Convert to Date if all values match the pattern
      as.Date(., format = "%Y-%m-%d")
    }))
}

# Define a function to convert applicable character columns to Date
convert_date_cols_3 <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ {
      # If there are non-missing values and all match the date pattern, convert to Date
      if (all(!is.na(.) & . != "" & grepl("^19\\d{2}-|^20\\d{2}-", .))) {
        as.Date(., format = "%Y-%m-%d")
      } else {
        .  # Return unchanged if conditions are not met
      }
    }))
}

# Define a function to convert applicable character columns to Date
convert_date_cols_2 <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ {
      # Return unchanged if there are missing or blank values or if not all match the date pattern
      if (any(is.na(.) | . == "" | !grepl("^19\\d{2}-|^20\\d{2}-", .))) {
        return(.)  # Return the column unchanged
      }
      
      # Convert to Date if all values match the pattern
      as.Date(., format = "%Y-%m-%d")
    }))
}



# Define a function to convert applicable character columns to Date
convert_date_cols_1 <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ {
      # Return the column unchanged if any values are missing, blank, or can't convert to date
      if (any(is.na(.) | . == "" | !grepl("^19\\d{2}-|^20\\d{2}-", .))) return(.) 
      
      # Convert to Date if all values match the expected date pattern
      as.Date(., format = "%Y-%m-%d")
    }))
}



# Define a function to convert applicable character columns to Date
convert_date_cols_0 <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ {
      # Assign the current column to a variable 'col'
      col <- .
      
      # Return the column unchanged if any values are missing, blank, or can't convert to date
      if (any(is.na(col) | col == "" | !grepl("^19\\d{2}-|^20\\d{2}-", col))) return(col)
      
      # Convert to Date if all values match the expected date pattern
      as.Date(col, format = "%Y-%m-%d")
    }))
}




## ---------------------------------------------------------------------------------------------------------------------------------------------
skewdates <- function(df = NULL, skewvalue = 100, rand = TRUE) {
  
  # Check if the dataframe is NULL
  if (is.null(df)) {stop("The data frame cannot be NULL.")}
  
  # Ensure skewvalue is numeric
  if (!is.numeric(skewvalue)) {stop("The skewvalue must be a numeric value.")}
  
  df$randcol <- sample(5:skewvalue, nrow(df), replace = TRUE)

  # Identify columns that are either Date or character types
  top <- df[1:10, ]
  cols <- names(top)
  
  # Identify date columns
  date_cols <- sapply(top, function(col) {
    if (inherits(col, "Date")) return(TRUE)  # Already Date class
    if (is.character(col)) {  # Try parsing character columns
      parsed_dates <- suppressWarnings(lubridate::ymd(col))
      if (sum(!is.na(parsed_dates)) >= 2) return(TRUE)
      parsed_dates <- suppressWarnings(lubridate::mdy(col))
      if (sum(!is.na(parsed_dates)) >= 2) return(TRUE)
    }
    FALSE
  })

  date_cols <- names(date_cols)[date_cols]

  # Add the skewvalue (in days) to all valid date columns
  for (col_index in which(sapply(df, function(col) inherits(col, "Date")))) {
    col_name <- colnames(df)[col_index]
    if (inherits(df[[col_name]], "Date")) {
      df[[col_name]] <- df[[col_name]] + df$randcol # Apply skew value
    }
  }

  df <- df |> select(-randcol)
  
  # Return the modified dataframe
  return(df)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
get_abbr <- function(text) {
  result = text
  
  # Check if there is a comma and rearrange the string accordingly
  if (str_detect(result, ",")) {
    # Split the string at the comma and move everything before the comma to the end
    parts  <- str_split(result, ",", simplify = TRUE)
    result <- paste(str_trim(parts[2]), str_trim(parts[1]))  # Ensure no leading/trailing spaces
  }

  # Remove non-alpha, keep space and comma
  result <- str_replace_all(result, "[^a-zA-Z\\s,]", "")
  
  # Replace each word with its first alphanumeric character (upper case)
  # result <- str_replace_all(result, "\\b([a-zA-Z0-9])[a-zA-Z0-9]*\\b", function(matched) {
  result <- str_replace_all(result, "\\b([a-zA-Z])[a-zA-Z]*\\b", function(matched) {
    toupper(substr(matched, 1, 1))
  })

  # Remove spaces
  result <- str_replace_all(result, " ", "")

  return(result)
}



## ---------------------------------------------------------------------------------------------------------------------------------------------
in_range <- function(x, range_str) {
  # Trim whitespace and validate input
  range_str <- gsub("\\s", "", range_str)
  
  # Extract first and last characters
  a <- substr(range_str, 1, 1)
  b <- substr(range_str, nchar(range_str), nchar(range_str))
  
  # Detect and validate separator ":"
  separator_count <- length(gregexpr(":", range_str)[[1]])
  if (separator_count != 1) {
    warning("Invalid range format: must contain exactly one ':' separator.")
    return(NA)
  }
  
  # Ensure a, b, and separator are valid
  if (!(a %in% c("(", "[")) || !(b %in% c(")", "]"))) {
    warning("Invalid range format: must start with '(' or '[' and end with ')' or ']'.")
    return(NA)
  }
  
  # Extract the range body and split into start and end
  range_body <- substr(range_str, 2, nchar(range_str) - 1)
  parts <- strsplit(range_body, ":")[[1]]
  if (length(parts) != 2) {
    warning("Invalid range format: must contain exactly two numeric values separated by ':'.")
    return(NA)
  }
  
  # Attempt to convert parts to numeric
  start <- suppressWarnings(as.numeric(parts[1]))
  end <- suppressWarnings(as.numeric(parts[2]))
  if (is.na(start) || is.na(end)) {
    warning("Invalid range: start and end values must be numeric.")
    return(NA)
  }
  
  # Determine the type of comparison
  checktype <- paste0(a, b)
  
  # Perform the comparison based on the range type
  result <- switch(checktype,
                   "[]" = x >= start & x <= end,
                   "[)" = x >= start & x < end,
                   "()" = x > start & x < end,
                   "(]" = x > start & x <= end,
                   { warning("Invalid range type."); NA }) # Default case
  
  return(result)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
extract_components <- function(str, delim = "\\.") {
  # Regex pattern: Finds the first substring containing the delimiter
  pattern <- paste0("\\S*", delim, "\\S+")
  
  # Extract the first match
  match <- regmatches(str, regexpr(pattern, str))
  
  # Split into components based on the specified delimiter
  comps <- unlist(strsplit(match, delim))
  
  return(comps)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
parse_path_components <- function(str, delim = "\\.") {
  # Step 1: Extract components using extract_components
  comps <- extract_components(str, delim)
  
  # Step 2: Assign components dynamically
  n <- length(comps)
  
  rslt <- list(
    tbl = if (n >= 1) comps[n],
    sch = if (n >= 2) comps[n - 1],
    db  = if (n >= 3) comps[n - 2]
  )
  
  return(rslt)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------

# Function to check for function references in files
check_function_references <- function(codedir, functions_to_check, file_extensions = c("R", "Rmd")) {
  # List all files in the directory and subdirectories, excluding hidden files
  file_list <- list_files_recursively(codedir)
  
  # Filter to keep only files with the specified extensions
  file_list <- file_list[fs::path_ext(file_list) %in% file_extensions]
  
  # Check each file using the custom function
  all_results <- do.call(rbind, lapply(file_list, function(file) process_file(file, functions_to_check)))
  
  # Convert to data.frame
  results_df <- as.data.frame(all_results, stringsAsFactors = FALSE)
  
  # Remove row names
  row.names(results_df) <- NULL

  return(results_df)
}


## ----eval=FALSE, include=FALSE----------------------------------------------------------------------------------------------------------------
# print(interactive())
# if (interactive()) {
#   # Set your root directory
#   codedir <- gsub("\\\\", "/", Sys.getenv('FROZEN_PATH', unset = NA))
# 
#   # Specify functions to check
#   funcs <- c("get_encryptkey", "unlock_keyring", file_extensions = c("Rmd"))
# 
#   # Run the check
#   rslt <- check_function_references(codedir, funcs)
# 
#   # Show results of storing REDCap to SQL
#   display_kable(rslt)
# }


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Function to list all non-hidden files recursively from a given directory
list_files_recursively <- function(dir_path) {
  # List all files and directories recursively
  all_files <- fs::dir_ls(dir_path, recurse = TRUE, type = "file")
  
  # Exclude hidden files, files/directories ending with '_', and files in 'archive' directories
  non_hidden_files <- all_files[!grepl("/\\.|/_$|/.*(archive|doc|temp|test|image|macro).*?/", all_files, ignore.case = TRUE)]
  
  return(non_hidden_files)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Function to search for function references in a file
search_in_file <- function(file_path, functions) {
  # Suppress warnings related to incomplete final lines
  lines <- suppressWarnings(readLines(file_path))
  result <- list()
  
  for (func in functions) {
    line_numbers <- which(grepl(func, lines))
    if (length(line_numbers) > 0) {
      result[[func]] <- line_numbers
    }
  }
  
  return(result)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Custom function to process a file
process_file <- function(file, functions_to_check) {
  file_name    <- basename(file)
  file_results <- search_in_file(file, functions_to_check)
  
  # Initialize an empty data frame to collect results
  results <- data.frame(
    'path'      = character(),
    'file'      = character(),
    'function'  = character(),
    'lines'     = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Collect results for each function
  for (func in names(file_results)) {
    for (line in file_results[[func]]) {
      results <- rbind(results, data.frame(
        'path'      = file,
        'file'      = file_name,
        'function'  = func,
        'lines'     = line,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}


## ----eval=FALSE, include=FALSE----------------------------------------------------------------------------------------------------------------
# extract_info <- function(row, target_colnames, target_value) {
#   colresults <- sapply(target_colnames, function(colname) {
#     if (!colname %in% names(row)) {
#       message(paste("Column", colname, "does not exist in the row."))
#       return(NA_character_)
#     } else {
#       # Extract the value of the specified column for the current row
#       col <- row[[colname]]
#       if (any(!is.na(col) & grepl(target_value, tolower(col)))) {
#         remarks <- gsub(".*\\(([^)]+)\\).*", "\\1", col)
#         return(ifelse(tolower(remarks) == target_value, colname, paste(colname, " (", remarks, ")", sep = "")))
#       }
#       return(NA_character_)
#     }
# 
#   })
#   return(paste(na.omit(colresults), collapse = ",\n"))
# }
# 
# mut_summary <- function(df, first_test_col='ABCB1', retain_cols='all') {
#   # Convert dataframe to data.table
#   tbl <- as.data.table(df %>% mutate(across(where(is.character), ~ na_if(., 'Not Done'))))
# 
#   # Identify columns to retain and test columns
#   test_start_col <- which(names(tbl) == first_test_col)
#   if (is.na(test_start_col)) stop("The specified first test column does not exist.")
# 
#   test_cols <- names(tbl)[test_start_col:length(names(tbl))]
# 
#   # Handle retain columns
#   if (is.null(retain_cols) || tolower(retain_cols) == 'all') {
#     retain_cols <- names(tbl)[1:(test_start_col-1)]
#   } else {
#     retain_cols <- unlist(strsplit(retain_cols, ",\\s*"))
#   }
# 
#   # Add the result columns to the retained ones
#   # retain_cols <- c(retain_cols, "pos_test", "bi_test", "neg_test", "vus_test")
#   retain_cols <- c(retain_cols, "pos_test", "neg_test", "vus_test")
# 
#   # Define values for comparison
#   positive_value  <- "positive"
#   biallelic_value <- "bi_allelic"
#   negative_value  <- "negative"
#   vus_value       <- "vus"
# 
#   # Create result columns using data.table's lapply for efficiency
#   tbl[, pos_test := apply(.SD, 1, function(row) extract_info(row, test_cols, positive_value)),   .SDcols = test_cols]
#   # tbl[, bi_test  := apply(.SD, 1, function(row) extract_info(row, test_cols, biallelic_value)),  .SDcols = test_cols]
#   tbl[, vus_test := apply(.SD, 1, function(row) extract_info(row, test_cols, vus_value)),        .SDcols = test_cols]
#   tbl[, neg_test := apply(.SD, 1, function(row) extract_info(row, test_cols, negative_value)), .SDcols = test_cols]
# 
#   # # Combine pos_test and bi_test, then clean up the result
#   # tbl[, pos_test := str_trim(paste(pos_test, bi_test, sep = ", "), side = "right")]
#   # # clean up leading/trailing commas
#   # tbl[, pos_test := str_replace(pos_test, "^,\\s?", "")]
#   # tbl[, pos_test := str_replace(pos_test, "\\s*,\\s*$", "")]
# 
# 
#   # Convert final result to a data.frame for compatibility with other workflows
#   final_df <- as.data.frame(tbl[, ..retain_cols])
# 
#   return(final_df)
# }
# 
# 
# 
# mut_summary_old2 <- function(df, first_test_col='ABCB1', retain_cols='all') {
#   # Convert dataframe to data.table
#   tbl <- as.data.table(df %>% mutate(across(where(is.character), ~ na_if(., 'Not Done'))))
# 
#   # Identify columns to retain and test columns
#   test_start_col <- which(names(tbl) == first_test_col)
#   if (is.na(test_start_col)) stop("The specified first test column does not exist.")
# 
#   test_cols <- names(tbl)[test_start_col:length(names(tbl))]
# 
#   # Handle retain columns
#   if (is.null(retain_cols) || tolower(retain_cols) == 'all') {
#     retain_cols <- names(tbl)[1:(test_start_col-1)]
#   } else {
#     retain_cols <- unlist(strsplit(retain_cols, ",\\s*"))
#   }
# 
#   # Add the result columns to the retained ones
#   retain_cols <- c(retain_cols, "pos_test", "bi_test", "neg_test", "vus_test")
# 
#   # Define values for comparison
#   positive_value  <- "positive"
#   biallelic_value <- "bi_allelic"
#   negative_value  <- "negative"
#   vus_value       <- "vus"
# 
#   # Create result columns using data.table's lapply for efficiency
#   tbl[, pos_test := lapply(.SD, function(col) extract_info(col, positive_value)),  .SDcols = test_cols]
#   tbl[, bi_test  := lapply(.SD, function(col) extract_info(col, biallelic_value)), .SDcols = test_cols]
#   tbl[, vus_test := lapply(.SD, function(col) extract_info(col, vus_value)),       .SDcols = test_cols]
#   tbl[, neg_test := lapply(.SD, function(col) extract_info(col, negative_value)),  .SDcols = test_cols]
# 
#   # Combine pos_test and bi_test, then clean up the result
#   tbl[, pos_test := str_trim(paste(pos_test, bi_test, sep = ", "), side = "right")]
#   tbl[, pos_test := str_replace(pos_test,",\\s*$", "")]
# 
#   # Convert final result to a data.frame for compatibility with other workflows
#   final_df <- as.data.frame(tbl[, ..retain_cols])
# 
#   return(final_df)
# }
# 
# 
# 
# mut_summary_old1 <- function(df, first_test_col='ABCB1', retain_cols='all') {
#   # Convert dataframe to data.table
#   tbl <- as.data.table(df %>% mutate(across(where(is.character), ~ na_if(., 'Not Done'))))
# 
#   # Identify columns to retain and test columns
#   test_start_col <- which(names(tbl) == first_test_col)
#   if (is.na(test_start_col)) stop("The specified first test column does not exist.")
# 
#   test_cols <- names(tbl)[test_start_col:length(names(tbl))]
#   if (is.null(retain_cols) | tolower(retain_cols)=='all') {
#     retain_cols <- names(df)[1:test_start_col-1]
#   } else {
#     retain_cols <- unlist(strsplit(retain_cols, ",\\s*"))
#   }
# 
#   # Check if all retain columns exist
#   if (any(!retain_cols %in% names(df))) stop("Some retain columns are not present in the dataframe.")
# 
#   # Extract the name of the first column
#   retain_cols <- c(retain_cols, "pos_test", "neg_test", "vus_test")
# 
#   # Define values for comparison
#   positive_value  <- "positive"
#   biallelic_value <- "bi_allelic"
#   negative_value  <- "negative"
#   vus_value       <- "vus"
# 
#   # Apply function to each row and create temp columns
#   tbl[, `:=`(
#     pos_test = apply(.SD, 1, function(row) extract_info(as.list(row), test_cols, positive_value)),
#     # bi_test  = apply(.SD, 1, function(row) extract_info(as.list(row), test_cols, biallelic_value)),
#     vus_test = apply(.SD, 1, function(row) extract_info(as.list(row), test_cols, vus_value)),
#     neg_test = apply(.SD, 1, function(row) extract_info(as.list(row), test_cols, negative_value))
#   ), .SDcols = test_cols]
# 
#   # Convert final_df to a data.frame
#   final_df <- as.data.frame(tbl[, ..retain_cols])
#   #
#   # final_df <- final_df %>%
#   # mutate(pos_test = paste(pos_test, bi_test, sep = ", ")) %>%
#   # mutate(pos_test = str_trim(str_replace(pos_test, ",\\s*$", "")))
# 
#   return(final_df)
# }
# 
# 


## ---------------------------------------------------------------------------------------------------------------------------------------------
# dosql(con=NULL, dbname='HEMEDB', cmd, engine=NULL, silent=TRUE)
dosql__ <- function(con=NULL
                , section=NULL
                , cmd=NULL
                , engine=NULL
                , silent=TRUE
                , rslt_lst=NULL
                , test=FALSE
                , newitem=0)
{
  
  # testing
  # if (test) {
  #   cmdset <-"
  #     SELECT * FROM scratch.temp1 ;
  #     SELECT * FROM scratch.temp2 ;
  #   "
  #   cmd<-cmdset
  #   engine   <-'SQLServer'
  #   silent   <-FALSE
  #   rslt_lst <-list()
  #   test     <-TRUE
  #   con      <-NULL
  # }
  
    
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
    if (exists("eng", envir = .GlobalEnv) && !is.null(eng) && eng != "") {
      engine <- eng
    } else {
      engine <- 'MariaDB'
    }
  }
  if (!silent) {cat('engine =',engine,'\n')}
  
  # Establish a connection
  if (!is.null(con)) {con <- con}                   # use connection passed in
  if ( is.null(con)) {con <- db_con(section=section,engine=engine)} # create a new connection
  if ( is.null(con)) {                              # no connection found/made
    stop("Failed to connect to the database.")
  }

  if (is.null(rslt_lst)) {rslt_lst <- list()}  
  
  if (!silent) {
    cat("Connection to SQL Server\n")
  }
  
  tryCatch({
    
    # What is the first word in the SQL command?
    firstword <- strsplit(toupper(cmd), " ")[[1]][1]
    
    # Is the word "INTO" found in the SQL Server command, acts like CREATE
    hasinto  <- grepl("INTO", cmd, ignore.case = TRUE)
        
    # find schema and table for result
    srchexpr <- regexpr("`?([[:alnum:]_]+)`?\\.(`?[[:alnum:]_]+`?)", cmd)
    schema_table <- regmatches(cmd,  srchexpr)

    # SELECT to fetch from changed table
    fetchcmd <- paste("SELECT * FROM ", schema_table, ";", sep = "")

    # Display fetch and execution code
    if (!silent) {
      cat(paste("Fetch command:\n", fetchcmd, '\n'))    
      cat(paste("Executing command:\n", cmd, '\n'))
    }
    
    # Do we need to execute code in the finally?
    executecmd = TRUE # Except for SELECT
    fetchdata  = TRUE # Except for SELECT, DROP, DELETE
    
    if (!silent) {
      cat(  ' firstword =',   firstword,    '\n'
          , 'hasinto =',      hasinto,      '\n'
          , 'schema_table =', schema_table, '\n')
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_parse <- function(cmd) {
  clean <- paste(cmd,';\n')
  clean <- gsub(";\\s*\n", ";\n", clean)
  clean <- trimws(strsplit(clean, ";\n")[[1]])
  clean <- clean[nzchar(clean)]  # Keep only non-empty strings
  return(clean)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# library(glue)  # If using glue for string interpolation, ensure it's loaded too
# library(stringr)

# get arrivals 1 to 15
get_arrivals <- function(tblcnt=11, silent=TRUE) { 
  
  # columns of interest
  patient_list_cols <- c("recordid",         "ptmrn",        "ptlastname", "ptbirthdate", "dxdate",    
                         "dxmorphdate",      "dxmorphsrc",   "dxmorph",    "dxmorphnum",  "dxmorphcell",
                         "dxflowdate",       "dxflowsrc",    "dxflow",     "dxflownum",
                         "uploaded_to_ctms", "uploaded_to_ctms_date",      "firstarrival",
                         "firstinduction",   "patient_list_rectime",       "primary_subject_id")

  specialty_note_cols <- c("recordid", "ptmrn", "ptlastname",     
                           "ptdeathdate", "ptlastchecked",
                           "lastknownalive", "lastknownremission", "lastknownactivedisease",   
                           "specialty_note_rectime")

  # tables of interest
  patient_list <- sql_get_table('esteylist_label.patient_list') |> 
      mutate(recordid = as.character(recordid)) |> 
      select(any_of(patient_list_cols))

  specialty_note <- sql_get_table('esteylist_label.specialty_note') |> 
      mutate(recordid = as.character(recordid)) |>
      select(any_of(specialty_note_cols))
  
  # tblcnt is the number of n = 1arrival tables
  

  pb <- get_progress_bar(tblcnt, "PROCESSING ARRIVALS")
  all_ <- data.frame()
  for (n in 1:tblcnt) {
    pb$tick()
    tbl <- paste0('a',n)
    # Execute the SQL command and store the result in a list
    df <- sql_get_table(glue('esteylist_label.arrival{n}')) |> mutate(recordid = as.character(recordid))
    df1 <- group_rc_chkbox(df)
    df2 <- df1 |> 
      mutate(arrivaltable       = glue("arrival{n}")
        , arrival               = as.integer(get(glue("arrivalnum{n}")))
        , arrival               = if_else(is.na(arrival),n,arrival)
        , rxline                = get(glue("rxline{n}"))
        , arrivaldate           = get(glue("arrivaldate{n}"))
        , arrivaltype           = get(glue("arrivaltype{n}"))
        , arrivalchar           = get(glue("arrivalchar{n}"))
        , arrivalsubchar        = get(glue("arrivalsubchar{n}"))
        , arrivalreason         = get(glue("arrivalreason{n}"))
        , arrivalkaryo          = get(glue("arrivalkaryo{n}"))
        
        , arrivalmorphdate      = get(glue("arrivalmorphdate{n}"))
        , arrivalmorphsrc       = get(glue("arrivalmorphsrc{n}"))
        , arrivalmorph          = get(glue("arrivalmorph{n}"))
        , arrivalmorphnum       = get(glue("arrivalmorphnum{n}"))
        , arrivalmorphcell      = get(glue("arrivalmorphcell{n}"))
        
        , arrivalflowdate       = get(glue("arrivalflowdate{n}"))
        , arrivalflowsrc        = get(glue("arrivalflowsrc{n}"))
        , arrivalflow           = get(glue("arrivalflow{n}"))
        , arrivalflownum        = get(glue("arrivalflownum{n}"))
        
        , treatmentlocation     = get(glue("a{n}treatmentlocation"))
        , treatmentdate         = get(glue("treatmentdate{n}"))
        , cycle1treatmentdate   = get(glue("a{n}c1date"))
        , treatment             = get(glue("treatment{n}"))
        , protocol              = get(glue("protocol{n}"))
        , rgnumber              = get(glue("rgnumber{n}"))
        
        , cycle1responsedate    = get(glue("a{n}c1respdate"))
        , responsedate          = get(glue("responsedate{n}"))
        , response              = get(glue("responsecategory{n}"))
        , mrd                   = get(glue("mrd{n}"))
        , responsemorphdate     = get(glue("responsemorphdate{n}"))
        , responsemorphsrc      = get(glue("responsemorphsrc{n}"))
        , responsemorph         = get(glue("responsemorph{n}"))
        , responsemorphnum      = get(glue("responsemorphnum{n}"))
        , responsemorphcell     = get(glue("responsemorphcell{n}"))
        , responseflowdate      = get(glue("responseflowdate{n}"))
        , responseflowsrc       = get(glue("responseflowsrc{n}"))
        , responseflow          = get(glue("responseflow{n}"))
        , responseflownum       = get(glue("responseflownum{n}"))
        , ancdate               = get(glue("ancdate{n}"))
        , anc                   = get(glue("anc{n}"))
        , pltdate               = get(glue("pltdate{n}"))
        , plt                   = get(glue("plt{n}"))
        
        , relapsedate           = get(glue("a{n}relapsedate"))
        , relapse               = get(glue("a{n}relapsedesc"))
        , nonrelapsedate        = get(glue("a{n}nonrelapsedate"))
        
        , form_complete         = get(glue("arrival{n}_complete"))
      )


    assign(tbl,df2 |>  select(recordid, ptmrn, ptlastname, (ncol(df1) + 1):ncol(df2)), envir = .GlobalEnv)
    all_ <- if (nrow(all_)==0) {
          get(tbl)
      } else {
          rbind(all_,get(tbl))
      }
    rm(list = ls(pattern = glue("^a\\d*$")))
    rm(list = ls(pattern = glue("^df.*$")))    
  }
  rm(pb) # progress bar
  
  # all_ <- all

  all <- left_join(all_, patient_list, by = join_by(recordid, ptmrn, ptlastname)) |> 
         left_join(specialty_note,    by = join_by(recordid, ptmrn, ptlastname)) |>
         filter(!grepl('(?i)patient', ptlastname)) |>
         arrange(ptmrn, treatmentdate) |>
         mutate(rownum=row_number(), treatmentdate_=as.Date(treatmentdate), treatmentdate=as.Date(treatmentdate)) |>
         select(rownum, everything())
  
  # previous to first arrival induction information
  induction_cols <- c("recordid",
      "ptmrn",
      "ptlastname",
      "arrivaltable",
      "arrival",
      "rxline",
      "treatmentlocation",
      "treatmentdate",
      "treatment",
      "responsedate",
      "response",
      "mrd",
      "relapsedate",
      "relapsedesc")

  induction <- sql_get_table(glue('esteylist_label.induction')) |> 
      mutate(recordid = as.character(recordid))
  
  induction <- group_rc_chkbox(induction)
  
  induction1 <- induction |>
      mutate(arrivaltable   = "induction"
        , arrival           = as.integer(-1)
        , rxline            = 'Ind'
        # , arrivaldate       = dxdate1
        , treatmentlocation = 'OUT'
        , treatmentdate     = dxtreatmentdate1
        , treatment         = dxtreatment1
        , responsedate      = dxresponsedate1
        , response          = dxresponse1
        , mrd               = dxmrd1
        , relapsedate       = inductionrelapsedate
        , relapse           = inductionrelapsenote
        , treatmentdate     = as.Date(treatmentdate)
      ) |> 
    select(any_of(induction_cols)) |> 
    left_join(patient_list,   by = join_by(recordid, ptmrn, ptlastname)) |>
    left_join(specialty_note, by = join_by(recordid, ptmrn, ptlastname)) |>
    filter(!is.na(treatmentdate)) |>
    arrange(ptmrn, treatmentdate) |>
    select(recordid, ptmrn, ptlastname, treatmentdate, arrivaltable, arrival, everything())

  
  # find the earliest treatment date in the arrivals
  all_firstrx <- all %>%
    filter(!is.na(treatmentdate_)) %>%
    group_by(recordid, ptmrn, ptlastname) %>%
    summarise(
      firsttreatmentdate = min(treatmentdate_),
      arrivalmin = min(arrival),
      .groups = 'drop'
    ) %>%
    left_join(all %>% select(recordid, ptmrn, ptlastname, rownum, treatmentdate_, treatment),
              by = c("recordid", "ptmrn", "ptlastname", "firsttreatmentdate" = "treatmentdate_")) %>%
    arrange(ptmrn, firsttreatmentdate) |>
    select(rownum, recordid, ptmrn, ptlastname, firsttreatmentdate, arrivalmin, everything())  

 remove_from_all <- inner_join(all_firstrx, induction1, by = join_by(recordid, ptmrn, ptlastname)) |>
    filter(str_detect(treatment.x, '(?i)consult|opinion|surveillance|2nd|out|(no)+t?\\streat')) |>
    select(rownum, recordid, ptmrn, ptlastname, 
           firsttreatmentdate, treatmentdate, 
           all_treatment = treatment.x, induction_treatment = treatment.y)
 
  all <- anti_join(all, remove_from_all, by='rownum') 
  all_firstrx <- anti_join(all_firstrx, remove_from_all, by='rownum') 
        
  remove_from_induction <- inner_join(all_firstrx, induction1, by = join_by(recordid, ptmrn, ptlastname)) |>
    mutate(days_apart = abs(firsttreatmentdate - treatmentdate)) |>
    # Filter to keep only rows where treatment dates are within 10 days of each other
    # filter(days_apart <= 60) |>
    mutate(value = case_when(
      mapply(function(x, y) grepl(toupper(y), toupper(x), fixed = TRUE), treatment.x, treatment.y) ~ 'drop_dup_ind_in_all',
      mapply(function(x, y) grepl(toupper(x), toupper(y), fixed = TRUE), treatment.x, treatment.y) ~ 'drop_dup_all_in_ind',
      days_apart > 35 ~ 'keep',
      str_detect(treatment.x, '(?i)re\\-?induction|out|(no)+t?\\streat') ~ 'drop_norx',
      days_apart <= 35 ~ 'drop_close',
      TRUE ~ 'review'
    )) |>
    filter(str_detect(value,'^drop_')) |>
    select(value, recordid, ptmrn, ptlastname, 
           days_apart, firsttreatmentdate, treatmentdate, 
           treatment.x, treatment.y,
           everything())
  
  induction1 <- anti_join(induction1, remove_from_induction, by='recordid') 


  # Use bind_rows to put the lists together
  arrivalset <- bind_rows(induction1, all) |>
    filter(!toupper(ptlastname)=="PATIENT") |>
    mutate(across(contains("date"), ymd)) |>
    mutate( arrival           = ifelse(is.na(arrival),0,arrival)
          , arrivaltype       = ifelse(arrival < 1, 'Outside', arrivaltype)
          , arrivalyyyymm     = format(arrivaldate, "%Y%m")
          , treatmentlocation = case_when(
                arrival < 1                                   ~ 'OUT',
                !is.na(arrivaltype) & 
                  str_detect(tolower(arrivaltype), 'salvage') ~ 'FHCC',
                TRUE                                          ~ treatmentlocation)
      ) |>
    arrange(recordid, ptmrn, arrivaldate) |>
    group_by(recordid) |>
      mutate(
        prevarrivaldate   = lag(arrivaldate),
        prevarrivaltype   = lag(arrivaltype),
        prevarrivalreason = lag(arrivalreason),
        prevarrivalmorph  = lag(arrivalmorph),
        prevarrivalflow   = lead(arrivalflow),
        
        nextarrivaldate   = lead(arrivaldate),
        nextarrivaltype   = lead(arrivaltype),
        nextarrivalreason = lead(arrivalreason),
        nextarrivalmorph  = lead(arrivalmorph),
        nextarrivalflow   = lead(arrivalflow)
      ) |>
    ungroup() |>
    select("recordid",        "ptmrn",            "ptlastname",         "ptbirthdate",
           "dxdate",          "dxmorphdate",      "dxmorphsrc",         "dxmorph",
           "dxmorphnum",      "dxmorphcell",      "dxflowdate",         "dxflowsrc",
           "dxflow",          "dxflownum",        
           "firstarrival",    "arrivaltable",     "arrival",
           "rxline",          "arrivaldate",      "arrivaltype",        "arrivalchar",
           "arrivalsubchar",  "arrivalmorphdate", "arrivalmorphsrc",    "arrivalmorph",
           "arrivalmorphnum", "arrivalmorphcell", "arrivalflowdate",    "arrivalflowsrc",
           "arrivalflow",     "arrivalflownum",   "treatmentlocation",  "treatmentdate",
           "treatment",       "responsedate",     "response",           "relapsedate",
           "relapse",         "ptdeathdate",      
           "ptlastchecked",   "lastknownalive",   "lastknownremission", "lastknownactivedisease", 
           "prevarrivaldate", "prevarrivaltype",  "prevarrivaltype",    "prevarrivaltype",   "prevarrivalflow",
           "nextarrivaldate", "nextarrivaltype",  "nextarrivalreason",  "nextarrivalmorph",  "nextarrivalflow",
           everything())


  
  return(arrivalset)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Modify SQL Server columns that are too narrow
# section<-'esteylist'
# params<-get_configset(section)
# sql_fit_columns(params, label=TRUE, silent=TRUE)

sql_fit_columns <- function( paramsect=NULL
                       , label=FALSE
                       , silent=TRUE) {
  
  # testing
  # paramsect<-get_configset('esteylist')
  
  # verify required parameters
  silent <- if (exists('silent')) silent else TRUE
  label  <- if (exists('label'))  label  else TRUE
  
  # Read in parameters from configurations section
  retain       <- paramsect$retain
  base_schema  <- paramsect$schema
  section      <- paramsect$section
  label_schema <- paste0(base_schema,'_label')
  
  schema <- if_else(label, label_schema, base_schema)
  
  failmsg <- ''
  
  if (is.null(retain)) {
    cat('\nNo list to fit\n')
    return(FALSE)
  }
  
  # Utilize INFO SCHEMA, rcdd and data frame tibble to determine starting size
  
  # Reload SQL dictionary - structure has likely changed since last check
  get_sql_dictionary(base_schema, reload=TRUE, silent=TRUE)
  
  # Function to calculate the width required for each column
  maxdf <- get_width(paramsect,label=label)  

  # Iterate rows in maxdf updating SQL server table to accommodate larger field data
  narrowdf <- maxdf |> 
    mutate(field_category = case_when(
      max_char_length > 255 &  max_char_length < 8000 ~ glue('varchar({ceiling(max_char_length / 100) * 100})')
      , max_char_length >= 8000 ~ 'varchar(MAX)'
      , .default=''
    )) |> 
    filter(max_char_length > 255) |>
    select(field_category, everything())
  
  # testing
  maxdf_ <<- maxdf
  narrowdf_ <<- narrowdf  
  
  if (nrow(narrowdf_)==0) {
    cat('\nRESIZE COLUMNS ... No rows to resize\n')
    return(data.frame())
  }

  loop_cnt <- nrow(narrowdf)
  pb <- get_progress_bar(loop_cnt, "RESIZE COLUMNS")
  
  i = 0
  for (i in 1:nrow(narrowdf)) {
    # i = i + 1
    # pb$tick()
    tbl     <- narrowdf$form_name[i]
    field   <- narrowdf$field_name[i]
    rctype  <- narrowdf$RC_Type[i]
    newtype <- narrowdf$field_category[i]
    
    # Build and Execute ALTER command 
    cmd <- glue("ALTER TABLE {schema}.{tbl} ALTER COLUMN {field} {newtype} NULL")
    print(cmd)
    dosql__(section=section, cmd=cmd, engine = 'SQLServer')    
    # 
    # tryCatch(dosql(section=section, cmd=cmd, engine = 'SQLServer')
    #          , error = function(e) {failmsg <- paste0(failmsg,"\n",tbl,".",field)})
  }

  # error message
  if (length(failmsg)>1) {cat(paste("Resizing failed for:",failmsg,"\n"))}

  return(narrowdf)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
.sql_drop_tables <- function( paramsect=NULL
                       , label=FALSE
                       , silent=TRUE)
{
  
  failmsg      <- ''
  retain <- tryCatch({paramsect$retain}, error = function(e) {NULL})
  if (is.null(retain)) {return()}

  base_schema  <- paramsect$schema
  label_schema <- paste0(base_schema,'_label')
  section      <- paramsect$section
  
  loop_cnt <- length(retain)*2
  pb <- get_progress_bar(loop_cnt, "DROP EXISTING")

  for (tbl in retain) {
    pb$tick()
    # drop tables from base_schema
    cmd <-  'DROP TABLE {base_schema}.{tbl} ;' |> glue()
    tryCatch(
        dosql__(section=section, cmd=cmd, engine='SQLServer', silent=silent)
        , error = function(e) {failmsg <- paste(failmsg,"\n",base_schema,'.',tbl)}
      )
    pb$tick()
    # drop tables from label_schema
    cmd <-  'DROP TABLE {label_schema}.{tbl} ;' |> glue()
    tryCatch(
        dosql__(section=section, cmd=cmd, engine='SQLServer', silent=silent)
        , error = function(e) {failmsg <- paste(failmsg,"\n",label_schema,'.',tbl)}
      )
    
  }

  # error messages
  if (length(failmsg)>1 & !silent) {cat(paste("Drop failed for:",failmsg,"\n"))}
  
  rm(pb)
  
  return("!")
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
.sql_create_structure <- function(paramsect=NULL
                       , label=FALSE
                       , silent=TRUE) { 
  
  # verify required parameters
  silent <- if (exists('silent')) silent else TRUE
  label  <- if (exists('label'))  label  else TRUE
  
  # Read in parameters from configurations section
  retain       <- paramsect$retain
  label_schema <- paste0(paramsect$schema,'_label')
  schema       <- if_else(label, label_schema, paramsect$schema)
  section      <- paramsect$section

  failmsg <- ''
  if (is.null(retain)) {
    cat('\nNo list to create\n')
    return(FALSE)
  }
  
  msg<-'Created empty structures for\n'

  sql_con=db_con(section=section, engine='SQLServer')
  # write zero length copy to SQL Server
  loop_cnt <- length(retain)
  pb <- get_progress_bar(loop_cnt, "CREATE EMPTY")
  
  for (tbl in retain) {
    pb$tick()
    emptydf <- get(tbl)[0,]
    msg<-paste0(msg,'\t',tbl,' (n=',nrow(get(tbl)),')')
    tryCatch({
        dbWriteTable(sql_con, Id(schema = schema, table = tbl), emptydf, row.names = FALSE, overwrite = TRUE)
      }
             , error = function(e) {failmsg <- paste(failmsg,"\n",tbl)})
  }
  db_discon(sql_con)
  
  # error messages
  if (length(failmsg)>1) {cat(paste("Create failed for:",failmsg,"\n"))}
  # general message
  if (!silent) {cat(msg,'\n')}
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_append_df <- function( paramsect=NULL
                       , label=FALSE
                       , silent=TRUE) {

  # Determine the section name based on the global environment if paramsect is NULL
  if (is.null(paramsect)) {
    if (exists("eng", envir = .GlobalEnv)) {
      # Convert the global environment variable 'eng' to lowercase
      paramsect <- tolower(get("eng", envir = .GlobalEnv))
    } else {
      stop("No paramsect provided and 'eng' not found in the global environment.")
    }
  }
  
  # Check if paramsect is a character string (configuration section name)
  if (is.character(paramsect)) {
    # Call get_configset to populate paramsect from the configuration section
    paramsect <- get_configset(section = paramsect, silent = silent)
  }  

  # Read in parameters from configurations section, paramsect
  retain       <- paramsect$retain
  base_schema  <- paramsect$schema
  section      <- paramsect$section
  label_schema <- paste0(base_schema,'_label')

  schema <- if_else(label, label_schema, base_schema)
  
  if (is.null(retain)) {
    cat('\nNo list to append\n')
    return(FALSE)
  }

  sql_con=db_con(section=section, engine='SQLServer')

  loop_cnt <-length(retain)
  pb <- get_progress_bar(loop_cnt, "APPEND DATA FRAMES")
  
  
  failmsg <- ''
  i = 0
  for (tbl in retain) {
    i = i + 1
    tbl = retain[i]
    pb$tick()
    # always write the table without the suffix '_label'
    trg <- gsub("_label", "", tbl)
    df  <- get(tbl)
    
    # Write df to SQL Database Table
    if (!silent) {cat('\ntable=',trg,'; df=',tbl,'; data frame exists?',nrow(df))}

    tryCatch(dbWriteTable(sql_con, Id(schema = schema, table = trg), df, row.names = FALSE, append = TRUE)
             , error = function(e) {failmsg <- paste0(failmsg,"\n",tbl)})
  }
  db_discon(sql_con)
  
  # error message
  if (length(failmsg)>1) {cat(paste("Append failed for:",failmsg,"\n"))}

  return()
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Helper function to test inserting a single column
# test=TRUE
test_column_insert <- function(df, nrow = 10, silent = TRUE, test = FALSE) {
  
  if (test) {
    nrow   <- 10
    silent <- TRUE
    df <- df_
  }

  # Define table and schema
  # schema <- 'scratch'
  # tbl    <- 'test_insert_columns'
  # sch_tbl <- paste0(schema,'.',tbl)
  table_id   <- Id(schema='scratch', table='test_insert_columns')
  
  # Skip test if nrow < 1
  if (nrow < 1) {
    return(tibble(column = names(df), status = "skipped"))
  }
  
  # Add row number to the data frame
  cols <- names(df)
  df <- df[1:nrow, ] |> mutate(row_num = row_number()) |> select(row_num, everything())
  
  # Establish a connection
  con <- db_con('esteylist') # Use appropriate connection function
  
  # Create or overwrite the scratch table with the initial empty structure
  dbWriteTable(con, table_id, df[0, ], overwrite = TRUE, row.names = FALSE)
  
  # Write only the row_num column to the existing table
  row_num_df <- df |> select(row_num)
  dbWriteTable(con, table_id, row_num_df, row.names = FALSE, append = TRUE)
  
  # Initialize results list
  results_list <- list()
  
  for (col in cols) {
    # col <- cols[[4]]
    # Limit the dataframe to nrow rows and select the column
    df_col <- df %>% select(row_num, !!sym(col)) %>% slice(1:nrow)
    
    # Extract the data class of the column as a character
    col_class <- tolower(as.character(class(df_col[[2]])))[[1]]
    print(col_class)
    
    is_num  <- col_class %in% c("numeric","integer","integer64")
    is_char <- col_class %in% c("posixct","character")
    is_date <- col_class %in% c("date")
    is_bool <- col_class %in% c("logical")
    
    # Create the values_string based on column type
    values_string <- switch(
      TRUE,
      is_date = {df_col[[2]] <- as.POSIXct(df_col[[2]])
                     paste0("(", df_col$row_num, ", '", format(df_col[[col]], "%Y-%m-%d"), "')", collapse = ",\n\t")
                },
      is_char = paste0("(", df_col$row_num, ", '", df_col[[col]], "')",            collapse = ",\n\t"),
      is_bool = paste0("(", df_col$row_num, ", ",  as.integer(df_col[[col]]), ")", collapse = ",\n\t"),
      is_num  = paste0("(", df_col$row_num, ", ",  df_col[[col]], ")",             collapse = ",\n\t"),
      # "character" = paste0("(", df_col$row_num, ", '", df_col[[col]], "')"                     , collapse = ",\n\t"),
      # "integer"   = paste0("(", df_col$row_num, ", ",  df_col[[col]], ")"                      , collapse = ",\n\t"),
      # "integer64" = paste0("(", df_col$row_num, ", ",  df_col[[col]], ")"                      , collapse = ",\n\t"),
      # "logical"   = paste0("(", df_col$row_num, ", ",  as.integer(df_col[[col]]), ")"          , collapse = ",\n\t"),
      stop("Unsupported column type")
    )    
    
    update_sql <- glue(
      "UPDATE t
       SET {col} = vals.{col}
         FROM  scratch.test_insert_columns AS t
         INNER JOIN (VALUES {values_string}) 
         AS vals(row_num, {col})
         ON t.row_num = vals.row_num ;"
    )
    
    # Attempt to execute the update statement
    col_status <- tryCatch({
      dbExecute(con, update_sql)
      
      if (!silent) cat("Successfully updated column:", col, "\n")
      "successful"
      
    }, error = function(e) {
      print(update_sql)
      if (!silent) {
        cat("Error updating column:", col, "\n")
        print(e)
      }
      "failed"
    })
    
    results_list[[col]] <- tibble(column = col, status = col_status)
  }
  
  # Close database connection
  db_discon(con)
  
  # Combine all results into a single tibble
  result_tibble <- bind_rows(results_list)
  
  return(result_tibble)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
sql_insert_df_depreciated <- function(df = NULL,
                          sch_tbl = NULL,
                          limit = 225,
                          silent = TRUE,
                          test = FALSE,
                          date_suffix = NULL,
                          batch_size = 10) {
  
  # test <- TRUE
  
  # 
  # Test mode setup for debugging
  # 
  if (test) {
    df<-widedf
    sch_tbl<-'transplant.transplantwide'
    limit<-25
    silent<-TRUE   
    date_suffix <- '%Y%m'
  }

  # 
  # Append date suffix to the table name if format provided
  # 
  if (!is.null(date_suffix)) {
    # Ensure the date_suffix is properly formatted
    date_suffix <- format(Sys.Date(), date_suffix)
    sch_tbl <- paste0(sch_tbl, "_", date_suffix)
  }
  
  # 
  # Set up the variables
  # 

  df           <- df |> mutate(across(contains("lastarrival"), ymd)) 
  dtcols       <- names(df)[sapply(df, is.Date)]
  chcols       <- names(df)[sapply(df, is.character)]
  df           <- df |> mutate(across(all_of(dtcols), ~ format(., "%Y-%m-%d")))
  df           <- df |> mutate(across(all_of(dtcols), ~ replace(., is.na(.), "")))
  split_schema <- strsplit(sch_tbl, "\\.")[[1]]
  schema       <- split_schema[1]
  tbl          <- split_schema[2]  
    

  # 
  # Create an empty table with the same structure as the data frame
  # 
  emptydf <- df[0,]
  con <- db_con('esteylist')
  dbWriteTable(con
               , Id(schema = schema, table = tbl)
               , emptydf
               , overwrite=TRUE
               , row.names=FALSE)
  
  # 
  # Modify the structure for columns with > limit characters
  # 
  for (col in chcols) {
    max_length <- max(nchar(df[[col]]), na.rm = TRUE)
    if (max_length > limit) {
      cmd <- glue("ALTER TABLE {sch_tbl} ALTER COLUMN [{col}] text")
      if (!silent) {cat(cmd,'\n')}
      dbExecute(con, cmd)
    }
  }

  
  # 
  # Insert one column at a time to test structure
  # 
  # Insert one column at a time
  for (col in names(df)) {
    df_col <- df[, col, drop = FALSE]  # Select the column as a data frame
    tryCatch({
      # Insert the column data
      dbWriteTable(con
                   , Id(schema = scrap, table = test_stru)
                   , df_col
                   , row.names = FALSE
                   , append = TRUE)
      cat("Successfully inserted column:", col, "\n")
    }, error = function(e) {
      cat("Error inserting column:", col, "\n")
      print(e)
    })
  }  
  
  

  # 
  # Append data from data frame
  # 
  dbWriteTable(con, Id(schema = schema, table = tbl), df, row.names = FALSE, append = TRUE)

  # Close the connection
  dbDisconnect(con)
}


## ----eval=FALSE, include=FALSE----------------------------------------------------------------------------------------------------------------
# library(reticulate)
# library("sqlparseR")
# 
# sql_split_blob <- function(cmd=NULL)
# { if (is.null(cmd)) {return(FALSE)}
#   if (reticulate::py_module_available("sqlparse")) {stmts <- sql_split(cmd)}
#       return(stmts)
# }


## ---------------------------------------------------------------------------------------------------------------------------------------------
library(DBI)
library(dplyr)

find_truncated_columns <- function(con, table_name, df) {
  failed_columns <- c()
  
  for (col in names(df)) {
    message("Testing column: ", col)
    
    # Identify the longest value in the column
    longest_row <- df %>%
      mutate(nchar_val = nchar(!!sym(col))) %>%
      filter(nchar_val == max(nchar_val, na.rm = TRUE)) %>%
      select(all_of(col)) %>%
      slice(1)  # Take only one row in case of ties
    
    # Create a test dataframe where all values are blank except for the longest value
    test_df <- df[1, ]  # Create a 1-row copy of df
    test_df[,] <- ""     # Fill all cells with empty strings
    test_df[[col]] <- longest_row[[1]]  # Set only the tested column
    
    # Attempt to write the test dataframe to SQL Server
    tryCatch({
      dbWriteTable(con, table_name, test_df, append = TRUE, overwrite = FALSE)
      message("✅ Column ", col, " written successfully")
    }, error = function(e) {
      message("❌ Column ", col, " FAILED: ", e$message)
      failed_columns <<- c(failed_columns, col)  # Track failing columns
    })
  }
  
  return(failed_columns)
}


