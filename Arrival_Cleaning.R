## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, fig.show = 'asis')
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## ----library--------------------------------------------------------------------------------------------
codedir <<- gsub("\\\\", "/", Sys.getenv('FROZEN_PATH', unset = NA))
source(paste0(codedir,'/icicle_setup.R')); icicle_setup()


## -------------------------------------------------------------------------------------------------------
# columns of interest
patient_list_cols <- c("recordid",         "ptmrn",        "ptlastname", "ptbirthdate", "dxdate",    
                       "dxmorphdate",      "dxmorphsrc",   "dxmorph",    "dxmorphnum",  "dxmorphcell",
                       "dxflowdate",       "dxflowsrc",    "dxflow",     "dxflownum",
                       "uploaded_to_ctms", "uploaded_to_ctms_date",      "firstarrival", "lastarrival",
                       "firstinduction",   "patient_list_rectime",       "primary_subject_id")

specialty_note_cols <- c("recordid", "ptmrn", "ptlastname",     
                         "ptdeathdate", "ptlastchecked",
                         "lastknownalive", "lastknownremission", "lastknownactivedisease",   
                         "specialty_note_rectime")

# tables of interest
patient_list <- sql_get_table('esteylist_label.patient_list') |> 
    filter(ptlastname != 'PATIENT') |>
    mutate(recordid = as.character(recordid)) |> 
    select(any_of(patient_list_cols))

specialty_note <- sql_get_table('esteylist_label.specialty_note') |> 
    filter(ptlastname != 'PATIENT') |>
    mutate(recordid = as.character(recordid)) |>
    select(any_of(specialty_note_cols))

# Make sure the REDCap data dictionary is loaded for the current section's schema
get_rc_dictionary('esteylist')


## -------------------------------------------------------------------------------------------------------
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
  # Apply the custom date conversion function across character columns
  convert_date_cols() |>
  # Apply the custom grouping of REDCap checkbox fields into a single column
  group_rc_chkbox() |>
  mutate(recordid         = as.character(recordid)
      , arrivaltable      = "induction"
      , orig_arrivaltable = "induction"
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

print('Done')


## -------------------------------------------------------------------------------------------------------
prior       = NULL
arrival     = NULL
allprior    = NULL
allarrival  = NULL
alltreat    = NULL
allnotreat  = NULL
noarrival   = NULL
allflavor   = NULL
schema      = 'frozen'
tblcnt = 11

for (n in 1:tblcnt) {
  # table for this loop
  tbl <- paste0('arrival',  n)
  
  # Apply the function to the arrival dataframe
  arrival <- sql_get_table(glue('esteylist_label.arrival{n}')) |>
    # remove test patients
    filter(ptlastname != 'PATIENT') |>
    # Apply the custom grouping of REDCap checkbox fields into a single column
    group_rc_chkbox() |>
    mutate(
      # Convert recordid to character type
        recordid = as.character(recordid)
      # Add a row number
      , row_number = row_number(), .before = 1    
      # Add a tablename column
      , arrivaltable = tbl
      , orig_arrivaltable = tbl
    ) |>
    # Apply the custom date conversion function across character columns
    convert_date_cols() |>
    # Apply multiple renames in one `rename_with`
    rename_with(~ {
      col_name <- .
      # Rename "a{n}" fields to "a_"
      col_name <- gsub(glue("^a{n}"), "a_", col_name)
      # Rename "arrival{n}note" fields specifically to "arrival_note"
      col_name <- gsub(glue("^arrival{n}note$"), "arrival_note", col_name)
      # Rename "arrival{n}" fields to "arrival"
      col_name <- gsub(glue("^arrival{n}"), "arrival", col_name)
      # Remove trailing "{n}" from other fields
      col_name <- gsub(glue("{n}$"), "", col_name)
      # Return the transformed column name
      col_name
    }) |>
    mutate(  arrivalcategory=NA_character_
           # using regular expressions to flag those that didn't get curative treatment
           , currativerx   = !grepl("(?i)spice|pall?i|comfort|consult|outside|support|opinion|surv|2nd\\s|none|not\\selig|not?\\streat", tolower(treatment))) |>
    select(row_number, recordid, ptmrn, ptlastname, arrivaldate, arrival_rectime, arrivaltable
           , treatment, currativerx, everything())

  arrival <- arrival |>
    left_join(patient_list, by = join_by(recordid, ptmrn, ptlastname)) |> 
    left_join(specialty_note, by = join_by(recordid, ptmrn, ptlastname)) |>
    select(row_number, recordid, ptmrn, ptlastname, primary_subject_id, arrivalcategory, everything())
  
  # Retype arrival to match SQL Server, removed code that did this twice
  if (n > 1) {
    arrival <- arrival |>
      mutate(across(
        .cols = intersect(names(arrival), names(alltreat)),
        .fns = ~ sql_convert_type(.x, alltreat[[cur_column()]])
      ))
  }

  # identify rows not associated with a treatment
  # notreat <- arrival |> filter(str_detect(treatment, '(?i)pice|pall|consult|opinion|surv|2nd\\s|(no)+t?\\streat'))
  notreat <- arrival |> filter(!currativerx) |> mutate(arrivalcategory='notreat')

  # keep only rows where there was treatment
  arrival  <- arrival |> filter(currativerx) |> mutate(arrivalcategory='treated')

  # combine, and make sure to remove dups
  prior <- bind_rows(lapply(ls(pattern = "^prior_"), get)) # prior <- bind_rows(prior, prior1, prior2, prior3) |> distinct()
  prior <- distinct(prior) |> mutate(arrivalcategory='prearrival', arrivaltable='prearrival')  
  
  # remove prior treatments from the arrival data
  prior_1 <- arrival |> filter(rxline                == 'Treatment prior to AML induction')
  prior_2 <- arrival |> filter(a_c1treatmentlocation == 'OUT' & a_treatmentlocation   == '')
  prior_3 <- arrival |> filter(a_treatmentlocation   == 'OUT')
  
  # combine, and make sure to remove dups
  prior <- bind_rows(lapply(ls(pattern = "^prior_"), get)) # prior <- bind_rows(prior, prior1, prior2, prior3) |> distinct()
  prior <- distinct(prior) |> mutate(arrivalcategory='prearrival', arrivaltable='prearrival')


  # Remove prior treatment data from the arrival data
  arrival <- anti_join(arrival, prior, by=join_by(row_number)) 
  
  # set empty structures if needed, usually in just the first loop, these empty structures should be the same
  if (is.null(allprior) & is.null(alltreat)) {
    # empty prior structure
    allprior   <- as.data.frame(prior)[0,]
    # empty arrival structure
    alltreat <- as.data.frame(arrival)[0,]
    # empty structure for no treatment
    allnotreat <- as.data.frame(notreat)
  }
  
  
  # Join arrivals -- arrivals with treatment
  alltreat <- bind_rows(alltreat, arrival) # |> mutate(row_number = row_number())
  
  # Join priors -- care prior to first arrival here
  allprior   <- bind_rows(allprior, prior) |> mutate(row_number = row_number() + nrow(alltreat))

  # Join no treatment -- arrivals that were not associated with treatment
  allnotreat <- bind_rows(allnotreat, notreat) |> mutate(row_number = row_number() + nrow(alltreat) + nrow(allprior))
  

}

allprior <- allprior |> select(row_number, recordid, ptmrn, ptlastname, arrivaldate, arrival_rectime, arrivaltable
           , treatment, currativerx, everything())
alltreat <- alltreat |> select(row_number, recordid, ptmrn, ptlastname, arrivaldate, arrival_rectime, arrivaltable
           , treatment, currativerx, everything())
allnotreat <- allnotreat |> select(row_number, recordid, ptmrn, ptlastname, arrivaldate, arrival_rectime, arrivaltable
           , treatment, currativerx, everything())


# clear out tables from last loop
rm(list = ls(pattern = "^prior"))
rm(list = ls(pattern = "^arrival"))
rm(list = ls(pattern = "^notreat"))


## -------------------------------------------------------------------------------------------------------
## Before binding allprior and induction check for duplicate introduction
allprior_  <- allprior
testbind   <- bind_rows(allprior_, induction) |> distinct(recordid, ptmrn, ptlastname, arrivaldate, arrivaltable, .keep_all = TRUE)
outlier    <- anti_join(allprior_, testbind)
outlierset <- left_join(outlier, allprior_, by=c('recordid', 'ptmrn', 'ptlastname', 'arrivaldate', 'arrivaltable')) |> 
  select(recordid, ptmrn, ptlastname
         , starts_with(c('orig_', 'arrivalcategory', 'arrivaldate', 'arrivaltable', 'treatment.', 'treatmentdate.')))

# These are duplicate rows
make_sheet(outlierset, qcdir, filename="multiple_outside_treatment_and_second_opinion_records")

# remove duplicate rows
allprior <- anti_join(allprior_, outlier)
allprior <- bind_rows(allprior, induction) |> 
  mutate(arrivaltable = 'prearrival',
         arrivalcategory = 'prearrival') |>
  arrange(recordid, arrivaldate, treatmentdate, arrivalnum) |>
  # Add a unique arrival identifier as row number
  mutate(row_number = row_number(), .before = 1)


allpreabstract <- anti_join(patient_list, bind_rows(alltreat, allprior, allnotreat)) |>
  distinct(recordid, ptmrn, ptlastname, dxdate, .keep_all = TRUE) |>
  mutate(arrivalcategory = 'preabstract', arrivaltable = 'preabstract')

testbind <- bind_rows(alltreat, allpreabstract) %>%
  distinct(recordid, ptmrn, ptlastname, .keep_all = TRUE) %>%
  mutate(row_number = row_number())
outlier  <- testbind %>% count(recordid, ptmrn, ptlastname) %>% filter(n > 1)

# Keeping the pre-abstraction with the treatment arrivals from alltreats
allarrival <- bind_rows(alltreat, allpreabstract) |> mutate(row_number = row_number())

zz <- allarrival |> group_by(recordid, ptmrn, ptlastname, arrivaldate) |> 
  mutate(cnt=n()) |> 
  ungroup() |> 
  select(row_number, recordid, ptmrn, ptlastname, arrivaldate, cnt)

outlier <- left_join(zz, allarrival, by=c('recordid', 'ptmrn', 'ptlastname')) |> 
  filter(cnt>1, row_number.x != row_number.y) |> 
  arrange(recordid, ptmrn, ptlastname, arrivaldate.y) |>
  select(row_number.x, row_number.y, recordid, ptmrn, ptlastname, cnt
         , arrivalcategory, arrivaldate.y, arrivaltable, any_of(names(allarrival)))

# These are duplicate rows ... multiple arrivals on a single date
make_sheet(outlier, qcdir, filename="multiple_arrivals_on_a_date")




## ----eval=FALSE, message=TRUE, include=FALSE------------------------------------------------------------
# cat("All arrival rows        ", nrow(allarrival), "\tallarrival\t(Treated", nrow(alltreat), glue("and Pre-Abstraction {nrow(allpreabstract)})"), "\n")
# cat("All no treatment rows   ", nrow(allnotreat), "\tallnotreat\n")
# cat("All prior arrival rows  ", nrow(allprior),   "\tallprior\n")
# # cat("All pats with no arrival", nrow(noarrival),  "\tnoarrival\n")
# freq(allnotreat$treatment)
# 
# rm(allpreabstract)
# rm(alltreat)
# rm(induction)


## -------------------------------------------------------------------------------------------------------
# identify earliest FHCC arrival
first_fhcc_arrival <- bind_rows(allarrival, allnotreat) |>
  mutate(min_date = pmin(arrivaldate, treatmentdate, na.rm = TRUE)) |>
  select(recordid, min_date) |>
  filter(!is.na(min_date)) |>
  group_by(recordid) |>
  summarise(firstfhccarrivaldate = min(min_date)  # Get min_date for each group
    , .groups = 'drop')

last_fhcc_arrival <- bind_rows(allarrival, allnotreat) |>
  mutate(max_date = pmax(arrivaldate, treatmentdate, na.rm = TRUE)) |>
  select(recordid, max_date) |>
  filter(!is.na(max_date)) |>
  group_by(recordid) |>
  summarise(lastfhccarrivaldate = max(max_date)         # Get max_date for each group            
    , .groups = 'drop')

allarrival <- left_join(allarrival, first_fhcc_arrival) |> left_join(last_fhcc_arrival)
allprior   <- left_join(allprior,   first_fhcc_arrival) |> left_join(last_fhcc_arrival)




## -------------------------------------------------------------------------------------------------------

all <- bind_rows(allprior, allarrival, allnotreat) |> 
  arrange(recordid, arrivaldate, treatmentdate, arrivalnum, arrivaltable ) |>
  mutate(row = row_number()) |>
  group_by(recordid, ptmrn, ptlastname) |>
  mutate(sequence = row_number(),
         prevarrivaldate=lag(arrivaldate),
         prevarrivalcategory=lag(arrivalcategory),
         prevtreatmentdate=lag(treatmentdate),
         prevtreatment=lag(treatment),
         nextarrivaldate=lead(arrivaldate),
         nextarrivalcategory=lead(arrivalcategory),
         nexttreatmentdate=lead(treatmentdate),
         nexttreatment=lead(treatment)) |>
  ungroup() |>
  select(row, sequence, recordid, ptmrn, ptlastname, primary_subject_id
         , arrivalcategory, firstfhccarrivaldate, lastfhccarrivaldate
         , starts_with(c('arrival', 'prev', 'next')), everything())



## -------------------------------------------------------------------------------------------------------
# Are there prior arrivals dated after first FHCC arrival/treatment

# Have staff review any of these where arrival date is after first FHCC arrival, but Rx date is not
late_outside_arrival <- allprior |>
  mutate(  priorarrivaldate    = arrivaldate
         , priortreatmentdate  = treatmentdate
         , afterfirstarrival   = priorarrivaldate   > firstfhccarrivaldate
         , afterfirsttreatment = priortreatmentdate > firstfhccarrivaldate
         , reviewreason = paste('Arrival date',arrivaldate
                                ,'is after first arrival on',firstfhccarrivaldate)) |>  
  filter(  afterfirstarrival & !afterfirsttreatment) |>
  select(  recordid, ptmrn, ptlastname
         , priorarrivaldate, priortreatmentdate, firstfhccarrivaldate, patient_list_rectime)
 
# Have staff review any of these where treatment date is after first FHCC arrival, but prior Rx arrival date is not
late_outside_treatment <- allprior |>
  mutate(  priorarrivaldate    = arrivaldate
         , priortreatmentdate  = treatmentdate
         , afterfirstarrival   = priorarrivaldate   > firstfhccarrivaldate
         , afterfirsttreatment = priortreatmentdate > firstfhccarrivaldate 
         , reviewreason = paste('Outside arrival on',priorarrivaldate,
                                'is before first arrival on',firstfhccarrivaldate,
                                'but associated rx on',priortreatmentdate,'is not.')) |>
  filter(  rxline!="" & afterfirsttreatment  & !afterfirstarrival) |>
  select(  recordid, ptmrn, ptlastname, reviewreason, rxline
         , priorarrivaldate, priortreatmentdate, firstfhccarrivaldate, patient_list_rectime)

# Discrepancy report
outside_rx_discreps <- bind_rows(late_outside_arrival, late_outside_treatment)
rm(list = ls(pattern = "^late_outside_"))

if (nrow(outside_rx_discreps) > 0){
  # Create excel for data cleaning
  make_sheet(outside_rx_discreps, scrapdir, 'errordata', 'outside_after_FHCC_arrival', 'xlsx', overwrite=TRUE )
  # Add to table of things to correct
  addrow('todo_df', scrapdir, 'outside_after_FHCC_arrival.xlsx', 'Review for arrival date, treatment date or treatment location errors.')
  cat('ERROR CHECK OUTSIDE treatments dated before the first arrival recorded at FHCC')
} else {
  cat('NO OUTSIDE treatments dated before the first arrival recorded at FHCC')
}




## -------------------------------------------------------------------------------------------------------
# Merge together all arrival from FHCC and all prior to arrival at FHCC
allarrival <- bind_rows(allarrival, allprior)  |> 
  arrange(recordid, arrivaldate, treatmentdate, arrivalnum, arrivaltable ) |>
  mutate(row_number = row_number())

cat('Merge prior arrivals with FHCC arrivals')


## -------------------------------------------------------------------------------------------------------
# Adding prev/next arrival information}
# Ranking arrivals
allarrival <- allarrival |>
  # mutate(arrival_old = arrival) |>
  arrange(recordid, ptmrn, pmax(arrivaldate, treatmentdate, na.rm = TRUE)) |>
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
        nextarrivalflow   = lead(arrivalflow),
        
        # Adding ranking
        arrival = row_number(),
        
        # Count all treatment plans
        allarrivalcnt = n(),

        # Count FHCC arrivals
        fhccarrivalcnt = sum(grepl("^arr", arrivaltable))
        
        
      ) |>
    ungroup() |>
    select(recordid, ptmrn, arrival, everything())

cat('Rank Patient Arrivals with prev/next')


## -------------------------------------------------------------------------------------------------------
# identify errors
missing_arrival_order_info <- allarrival |> 
  # bind_rows(allnotreat) |>
  left_join(patient_list |> select(recordid, patient_list_rectime))  |> 
  arrange(recordid, arrivaldate, treatmentdate, arrivalnum, arrivaltable ) |>
  filter(!grepl('^pre', arrivaltable)) |>
  filter(is.na(arrivaldate) | arrivalnum == "") |>
  filter(recordid > 1000 & arrivaltable != 'induction') |>
  arrange( ptlastname, ptmrn, arrivalnum ) |>
  select(  recordid, ptmrn, ptlastname
         , arrivalnum, arrivaltable, orig_arrivaltable, arrivaldate, arrival_rectime, arrivalnote
         , treatmentdate, treatmentnote, patient_list_rectime )

if (nrow(missing_arrival_order_info) > 0){
  # Create excel for data cleaning
  make_sheet(missing_arrival_order_info, scrapdir, 'nodate_or_nonumber', 'missing_arrival_order_info', 'xlsx', overwrite=TRUE )
  # Add to table of things to correct
  addrow('todo_df', scrapdir, 'missing_arrival_order_info.xlsx', 'Review for arrival date missing and arrival number order')
  cat('\nReport Missing Arrival Sequence number or Arrival Date')
} else {
  cat('\nNo Missing Arrival Sequence number or Arrival Date')
}


## -------------------------------------------------------------------------------------------------------
# Resize accommodate structure of all the records
cummarrival <- allarrival |> bind_rows(allnotreat) |> bind_rows(allprior) |> bind_rows(noarrival) |> distinct()
tempsch = 'scratch'
sql_drop_tbl(tempsch, 'temp')
sql_insert_df(cummarrival, 'scratch.temp')
# make empty copies
sql_copy_tbl(tempsch, 'allnotreat', 'temp', data=FALSE)
sql_copy_tbl(tempsch, 'allprior',   'temp', data=FALSE)
sql_copy_tbl(tempsch, 'allarrival', 'temp', data=FALSE)
sql_drop_tbl(tempsch, 'temp')

# insert allarrivals, allnotreat, and allprior twice, normal and again with a date suffix
x <- sql_insert_df(allarrival, 'frozen.allarrival', date_suffix='%Y%m')
x <- sql_insert_df(allnotreat, 'frozen.allnotreat', date_suffix='%Y%m') 
x <- sql_insert_df(allprior,   'frozen.allprior',   date_suffix='%Y%m') 


cat('SQL Server updated with allarrival, allnotreat, and allprior tables')

