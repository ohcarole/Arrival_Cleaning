idlist      <- c('recordid', 'ptmrn', 'ptlastname')

schema      <- 'frozen'
tblcnt      <- 11

arrival_    <- NULL
notreat_    <- NULL
prior_      <- NULL

allarrival  <- NULL
alloutnorx  <- NULL
allprior    <- NULL
alltreat    <- NULL
allnotreat  <- NULL

index <- n <- 0
# looping through each table arrival1 up to arrival11
for (n in 1:tblcnt-1) {
  if(index==0) {n=n+1} else {n=index}
  # table for this loop
  tbl <- paste0('arrival',  n)
  cat(tbl, '\n')

  # Apply the function to the arrival dataframe
  arrival_ <- sql_get_table(glue('esteylist_label.arrival{n}')) |>
    # remove test patients
    filter(ptlastname != 'PATIENT') |>
    # Apply the custom grouping of REDCap checkbox fields into a single column
    group_rc_chkbox() |>
    mutate(
      # Convert recordid to character type  (not sure if this is needed?)
      recordid = as.character(recordid)
      # Add a row number at the start of the df
      , row_number = row_number(),
      # Add a tablename column
      , arrivaltable = tbl
      , orig_arrivaltable = tbl
    ) |>
    # Apply the custom date conversion function across character columns
    convert_date_cols() |>
    # Apply multiple renames in one `rename_with`
    rename_with(~ {
      col_name <- . # col_name is a generic for all columns to meet the pattern
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
    mutate(across(everything(), as.character)) |>
    mutate(  arrivalcategory=NA_character_
             # using regular expressions to flag those that didn't get curative treatment
             , currativerx = is_currative_rx(treatment)) |>
    select(row_number, any_of(idlist)
           , arrivaldate, arrival_rectime, arrivaltable
           , treatment, starts_with('curr'), everything())

  arrival_ <- arrival_ |>
    left_join(patient_list,   by = idlist) |>
    left_join(specialty_note, by = idlist) |>
    select(row_number, any_of(idlist), primary_subject_id, arrivalcategory
           , arrival_rectime, arrivaltable, treatment, starts_with('curr'), everything())

  # outside and not currative
  outnorx_ <- arrival_ |>
    filter(!currativerx) |>
    filter(a_treatmentlocation == 'OUT' | a_c1treatmentlocation == 'OUT') |>
    mutate(arrivalcategory = 'outnotreat')

  # identify rows not associated with a treatment at FHCC
  # notreat_ <- arrival_ |> filter(str_detect(treatment, '(?i)pice|pall|consult|opinion|surv|2nd\\s|(no)+t?\\streat'))
  notreat_ <- arrival_ |> filter(!currativerx) |> mutate(arrivalcategory='notreat')

  # keep only rows where there was treatment at FHCC
  treat_  <- arrival_ |> filter(currativerx) |> mutate(arrivalcategory='treated')

  # remove prior treatments from the arrival data -- these are outside treatments that were missed above
  prior_ <- bind_rows(
    arrival_ |> filter(rxline == 'Treatment prior to AML induction'),
    arrival_ |> filter(a_c1treatmentlocation == 'OUT' & a_treatmentlocation == ''),
    arrival_ |> filter(a_treatmentlocation == 'OUT')
  ) |>
    distinct() |>
    mutate(arrivalcategory = 'prearrival', arrivaltable = 'prearrival')

  # Remove prior treatment data from the treated data
  treat_ <- anti_join(treat_, prior_, by=join_by(row_number))

  # Remove outside arrivals where no treatment occurred from prior and no treatment
  prior_ <- anti_join(prior_, outnorx_, by = join_by(row_number))
  notreat_ <- anti_join(notreat_, outnorx_, by = join_by(row_number))

  # set empty structures in the first loop
  if (n==1) allarrival <- alloutnorx <- allprior <- alltreat <- allnotreat <- as.data.frame(arrival_)[0,]

  allarrival <- bind_rows(allarrival, arrival_)
  alloutnorx <- bind_rows(alloutnorx, outnorx_) # outside care, no treatment
  alltreat   <- bind_rows(alltreat,   treat_)   # arrivals with treatment
  allprior   <- bind_rows(allprior,   prior_)   # care prior to first arrival here
  allnotreat <- bind_rows(allnotreat, notreat_) # arrivals that were not associated with treatment

  # clear out tables from last loop
  rm(list = ls(pattern = "^prior|^arrival|^treat|^combo|^outno|^empty"))

}

nrow(allarrival)
comboarrival <- bind_rows(alloutnorx=alloutnorx, alltreat=alltreat, allprior=allprior, allnotreat=allnotreat, .id='src') |>
  arrange(recordid, ptmrn, ptlastname, arrivaldate)
nrow(comboarrival)
missing_in_allarrival <- anti_join(comboarrival, allarrival,
                                   by = c(idlist, 'arrivaldate'))
nrow(missing_in_allarrival)

allarrival   <- allarrival |>
  mutate(across(
    .cols = names(allarrival),
    .fns  = ~ sql_convert_type(.x, alltreat[[cur_column()]]))) |>
  select(row_number, any_of(idlist), arrivaldate, arrival_rectime, arrivaltable
         , treatment, currativerx, everything())


# this just puts the columns in order
allprior   <- allprior |>
  mutate(across(
    .cols = intersect(names(allarrival), names(allprior)),
    .fns  = ~ sql_convert_type(.x, alltreat[[cur_column()]]))) |>
  select(row_number, any_of(idlist), arrivaldate, arrival_rectime, arrivaltable
                                 , treatment, currativerx, everything())
alltreat  <- alltreat |>
  mutate(across(
    .cols = intersect(names(allarrival), names(alltreat)),
    .fns  = ~ sql_convert_type(.x, alltreat[[cur_column()]]))) |>
  select(row_number, any_of(idlist), arrivaldate, arrival_rectime, arrivaltable
                                  , treatment, currativerx, everything())
allnotreat <- allnotreat |>
  mutate(across(
    .cols = intersect(names(allarrival), names(allnotreat)),
    .fns  = ~ sql_convert_type(.x, allnotreat[[cur_column()]]))) |>
  select(row_number, any_of(idlist), arrivaldate, arrival_rectime, arrivaltable
                                   , treatment, currativerx, everything())

