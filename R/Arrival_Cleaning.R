## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, fig.show = 'asis')
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## ----library------------------------------------------------------------------------------------
rm(crystals)
codedir <<- gsub("\\\\", "/", Sys.getenv('R_CODE', unset = NA))
oldcodedir <<- gsub("\\\\", "/", Sys.getenv('FROZEN_PATH', unset = NA))
source(paste0(codedir,'/icicle_setup.R'))
function_rmds = c(
      "icicle_functions.Rmd",
      "sql_functions.Rmd",
      "import_export.Rmd",
      "redcap_api.Rmd",
      "karyotype_function_set.Rmd",
      "timelink.Rmd")
icicle_setup(function_rmds = function_rmds)
idlist      <- c('recordid', 'ptmrn', 'ptlastname')


## ----helper-------------------------------------------------------------------------------------
# summarize_epic_treatment()
# this helper is used in Import EPIC Reports, so grabbing it from there
rmd_path <- "helper.Rmd"
source_cache(rmd_path, source=TRUE) # source=TRUE is the default, here for clarity


## -----------------------------------------------------------------------------------------------
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

local_ <- .pkg_env
get_rc_dictionary('esteylist', ddname = "rcdd_exp", reload = TRUE, explode = TRUE)
local_$rcdd_exp = rcdd_exp

# tables of interest
patient_list <- sql_get_table('esteylist_label.patient_list')
patient_list <- patient_list |> 
    filter(ptlastname != 'PATIENT') |>
    mutate(recordid = as.character(recordid)) |> 
    select(any_of(patient_list_cols)) |>
    convert_date_cols()

specialty_note <- sql_get_table('esteylist_label.specialty_note') |> 
    filter(ptlastname != 'PATIENT') |>
    mutate(recordid = as.character(recordid)) |>
    select(any_of(specialty_note_cols)) |>
    convert_date_cols()

project_specific_info <- sql_get_table('esteylist.project_specific_info') |> 
    filter(ptlastname != 'PATIENT') |>
    mutate(recordid = as.character(recordid)) |>
    group_rc_chkbox() |>
    select(any_of(idlist), special_population)



## -----------------------------------------------------------------------------------------------
# previous to first arrival induction information
induction_cols <- c("recordid",
    "ptmrn",
    "ptlastname",
    "arrivaltable",
    "redcap_table",
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

# checking to assure that the induction table is for outside induction information
# disease_features <- get_rc_table('diagnosis_for_aml_arrivals', 'disease_features')|>
#   group_rc_chkbox() |>
#   convert_date_cols() |>
#   filter(df_inductionlocation == 2)
# df_induction <- get_rc_table('diagnosis_for_aml_arrivals', 'induction') |>
#   group_rc_chkbox() |>
#   convert_date_cols() |>
#   select(diagnosisid, ptmrn, recordid, starts_with('in_cycle1'))
# xx <- inner_join(disease_features, df_induction, by = 'ptmrn') |>
#   filter(!is.na(in_cycle1date))

induction_ <- sql_get_table('esteylist_label.induction')

# zz <- induction <- induction_ |> group_rc_chkbox()


local_ <- .pkg_env
get_rc_dictionary('esteylist', ddname = 'rcdd_exp', reload=TRUE, explode=TRUE)
local_$rcdd_exp <- rcdd_exp

# tic("group_rc_chkbox_ timing")
# zz <- induction <- induction_ |> group_rc_chkbox_()
# toc()
# 
# tic("group_rc_chkbox timing")
# zz <- induction <- induction_ |> group_rc_chkbox()
# toc()
# 
# tic("group_rc_chkbox_dt timing")
# zz <- induction <- induction_ |> group_rc_chkbox()
# toc()

induction <- induction_ |>
  mutate(recordid         = as.character(recordid)
      , arrivaltable      = "induction"
      , redcap_table      = "induction"
      , orig_arrivaltable = "induction"
      , arrival           = as.integer(-1)
      , rxline            = 'Ind'
      # , arrivaldate       = dxdate1
      , treatmentlocation = ''
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
  # removed group_rc_chkbox, there weren't any check box fields
  left_join(patient_list,   by = join_by(recordid, ptmrn, ptlastname)) |>
  left_join(specialty_note, by = join_by(recordid, ptmrn, ptlastname)) |>
  mutate(treatmentlocation = if_else(str_detect(firstinduction, 'First AML induction here'), 'FHCC', 'OUT')) |>
  filter(!is.na(treatmentdate)) |>
  arrange(ptmrn, treatmentdate) |>
  # Apply the custom date conversion function across character columns
  convert_date_cols() |>
  select(recordid, ptmrn, ptlastname, 
         treatmentdate, arrivaltable, redcap_table, arrival, responsecategory=response, everything())

induction <- rx_stand(induction, 'treatment')


print('Done')


## -----------------------------------------------------------------------------------------------
# make sure the exploded dictionary is in scope
local_ <- .pkg_env
get_rc_dictionary('esteylist', ddname = "rcdd_exp", reload = TRUE, explode = TRUE)
local_$rcdd_exp = rcdd_exp

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

# Apply the function to the arrival dataframe
arrival_ <- sql_get_table(glue('esteylist_label.arrival{n+1}'))
  
# looping through each table arrival1 up to arrival11
for (n in 1:tblcnt-1) {
  # designed to allow stepping through easier
  if (nrow(arrival_)==0) next
  if(index==0) {n=n+1} else {n=index}

  # table for this loop
  tbl <- paste0('arrival',  n)
  cat(tbl, '\n')

  arrival_ <- arrival_ |>
    # remove test patients
    filter(ptlastname != 'PATIENT') |>
    # Apply the custom grouping of REDCap checkbox fields into a single column
    group_rc_chkbox() |>
    mutate(
      # Convert recordid to character type  (not sure if this is needed?)
      recordid = as.character(recordid)
      # Add a row number at the start of the df
      , row_number = row_number() + 10000*n,
      # Add a tablename column
      , arrivaltable = tbl
      , redcap_table = tbl
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
      # Rename "arrival{n}_" fields to "arrival_"
      col_name <- gsub(glue("^arrival{n}_"), "arrival_", col_name)
      # Rename "arrival{n}" fields to "arrival_"
      col_name <- gsub(glue("^arrival{n}"), "arrival_", col_name)
      # Remove trailing "{n}" from other fields
      col_name <- gsub(glue("{n}$"), "", col_name)
      # Return the transformed column name
      col_name
    }) |>
    mutate(across(everything(), as.character)) |>
    mutate(  arrivalcategory=NA_character_
             # using regular expressions to flag those that didn't get currative treatment at fhcc
             , currativerx_fhcc = is_currative_rx_fhcc(treatment)) |>
    select(row_number, any_of(idlist)
           , any_of(c('arrivaldate', 'arrival_rectime', 'arrivaltable')), 
           redcap_table, treatment, starts_with('curr'), everything())
  
  cols <- arrival_ |> select(ends_with("date")) |> names()

  arrival_ <- arrival_ |>
    left_join(patient_list,   by = idlist) |>
    left_join(specialty_note, by = idlist) |>
    left_join(project_specific_info, by = idlist) |>
    convert_columns('Date', cols) |>
    # convert_date_cols(skip_cols = c("row_number")) |>
    select(row_number, any_of(idlist), primary_subject_id
           , special_population
           , any_of(c('arrivalcategory', 'arrival_rectime', 'arrivaltable'))
           , redcap_table, treatment, starts_with('curr'), everything())
  
  # # outside and not currative
  # outnorx_ <- arrival_ |>
  #   filter(!currativerx_fhcc) |>
  #   filter(a_treatmentlocation == 'OUT' | a_c1treatmentlocation == 'OUT') |>
  #   mutate(arrivalcategory = 'outnotreat')
  # 
  # # identify rows not associated with a treatment at FHCC
  # # notreat_ <- arrival_ |> filter(str_detect(treatment, '(?i)pice|pall|consult|opinion|surv|2nd\\s|(no)+t?\\streat'))
  # notreat_ <- arrival_ |> filter(!currativerx_fhcc) |> mutate(arrivalcategory='notreat')
  # 
  # # keep only rows where there was treatment at FHCC
  # treat_  <- arrival_ |> filter(currativerx_fhcc) |> mutate(arrivalcategory='treated')
  # 
  # # remove prior treatments from the arrival data -- these are outside treatments that were missed above
  # prior_ <- bind_rows(
  #   arrival_ |> filter(rxline == 'Treatment prior to AML induction'),
  #   arrival_ |> filter(a_c1treatmentlocation == 'OUT' & a_treatmentlocation == ''),
  #   arrival_ |> filter(a_treatmentlocation == 'OUT')
  # ) |>
  #   distinct() |>
  #   mutate(arrivalcategory = 'prearrival', arrivaltable = 'prearrival', redcap_table=tbl)
  # 
  # # Remove prior treatment data from the treated data
  # treat_ <- anti_join(treat_, prior_, by=join_by(row_number))
  # 
  # # Remove outside arrivals where no treatment occurred from prior and no treatment
  # prior_ <- anti_join(prior_, outnorx_, by = join_by(row_number))
  # notreat_ <- anti_join(notreat_, outnorx_, by = join_by(row_number))

  # set empty structures in the first loop
  if (n==1) {
    allarrival <- alloutnorx <- allprior <- alltreat <- allnotreat <- as.data.frame(arrival_)[0,]
  }
  
  # includes all attempts including arrival w/o treatment and outside treatment
  allarrival <- bind_rows(allarrival, arrival_)
  # the sum of these should match the all arrival records
  # alltreat   <- bind_rows(alltreat,   treat_)   # arrivals with treatment
  # allprior   <- bind_rows(allprior,   prior_)   # outside care prior to first arrival here
  # alloutnorx <- bind_rows(alloutnorx, outnorx_) # outside care, no treatment
  # allnotreat <- bind_rows(allnotreat, notreat_) # arrivals here not associated with treatment

  # clear out tables from last loop
  rm(list = ls(pattern = "^prior|^arrival|^treat|^combo|^outno|^empty"))
  
  # Next set
  arrival_ <- sql_get_table(glue('esteylist_label.arrival{n+1}'))
  
}

# # this just puts the columns in order
# alloutnorx <- alloutnorx |>
#   mutate(category = 'alloutnorx', 
#          across(
#           .cols = intersect(names(allarrival), names(allprior)),
#           .fns  = ~ sql_convert_type(.x, alltreat[[cur_column()]]))) |>
#   convert_date_cols() |>
#   select(row_number, 
#          any_of(c(idlist, 'category', 'arrivaldate', 'arrival_rectime', 'arrivaltable'
#                 , 'redcap_table', 'treatment', 'currativerx_fhcc'))
#                 , everything())
# 
# allprior <- allprior |>
#   mutate(category = 'allprior', 
#          across(
#           .cols = intersect(names(allarrival), names(allprior)),
#           .fns  = ~ sql_convert_type(.x, alltreat[[cur_column()]]))) |>
#   convert_date_cols() |>
#   select(row_number, 
#          any_of(c(idlist, 'category', 'arrivaldate', 'arrival_rectime', 'arrivaltable'
#                 , 'redcap_table', 'treatment', 'currativerx_fhcc'))
#                 , everything())
# allprior <- bind_rows(allprior, induction) |> mutate(category = 'allprior')
# 
# alltreat <- alltreat |>
#   mutate(category = 'alltreat', 
#          across(
#           .cols = intersect(names(allarrival), names(alltreat)),
#           .fns  = ~ sql_convert_type(.x, alltreat[[cur_column()]]))) |>
#   convert_date_cols() |>
#   select(row_number, 
#          any_of(c(idlist, 'category', 'arrivaldate', 'arrival_rectime', 'arrivaltable'
#                 , 'redcap_table', 'treatment', 'currativerx_fhcc'))
#                 , everything())
# allnotreat <- allnotreat |>
#   mutate(category = 'allnotreat', 
#          across(.cols = intersect(names(allarrival), names(allnotreat)),
#                 .fns  = ~ sql_convert_type(.x, allnotreat[[cur_column()]]))) |>
#   convert_date_cols() |>
#   select(row_number, 
#          any_of(c(idlist, 'category', 'arrivaldate', 'arrival_rectime', 'arrivaltable'
#                 , 'redcap_table', 'treatment', 'currativerx_fhcc'))
#                 , everything())

allpreabstract <- anti_join(patient_list, allarrival) |>
  left_join(project_specific_info |> select(any_of(idlist), special_population)) |>
  mutate(category = 'Patient has not been abstracted in REDCap',
         special_population = coalesce(special_population, ''),
         arrivaltable = 'patient_list',
         redcap_table = 'patient_list') |>
  filter(str_detect(special_population, '(?i)^older aml|mucor', negate=TRUE)) |>
  filter( recordid > 0
        & str_detect(ptlastname, "(?i)patient", negate=TRUE)
        & uploaded_to_ctms == "Yes")


allarrival   <- bind_rows(allarrival, allpreabstract) |>
  mutate(across(
           .cols = names(allarrival),
           .fns  = ~ sql_convert_type(.x, alltreat[[cur_column()]]))) |>
  mutate(category = case_when(
      row_number %in% alloutnorx$row_number ~ "Outside Arrival without Treatment",
      row_number %in% allprior$row_number   ~ "Prior Treatment Outside",
      row_number %in% allnotreat$row_number ~ "No Treatment",
      row_number %in% alltreat$row_number   ~ "FH Treatment",
      row_number %in% allpreabstract        ~ "Not yet abstracted",
      TRUE ~ category  # retain existing value if no match
    )
  ) |>
  convert_date_cols() |>
  select(row_number, 
         any_of(c(idlist, 'category', 'arrivaldate', 'arrival_rectime', 'arrivaltable'
                , 'redcap_table', 'treatment', 'currativerx_fhcc'))
                , everything())

allarrival <- bind_rows(allarrival, induction |> mutate(category='Prior Treatment Outside')) |>
  mutate(row_number=row_number()+10000,
    currativerx_any  = coalesce(currativerx_fhcc, is.na(currativerx_fhcc) & arrivaltable=="induction"))

rslt <- nrow(allarrival) == nrow(alloutnorx) + nrow(alltreat) + nrow(allprior) + nrow(allnotreat) + nrow(allpreabstract)
cat("All records accounted for?", rslt)

rm(list=ls(pattern = '^allarrival_2025'))

backup_name.1 <- paste0('allarrival_', as.character(now()))
assign(backup_name.1, allarrival)
allarrival <- get(backup_name.1)


## -----------------------------------------------------------------------------------------------
rm(allarrival)
allarrival <- get(backup_name.1) |> 
  filter(str_detect(arrivaltable, "patient_list", negate=TRUE)) |>
  mutate(arrival_num = case_when(
      arrivaltable == "induction" ~ 0L,
      str_detect(arrivaltable, "\\d+") ~ parse_integer(str_extract(arrivaltable, "\\d+")),
      TRUE ~ NA_integer_)) |>
  select(any_of(idlist), treatment, -ends_with('_calc'), arrivaltable, arrival_num, everything())

# BACKBONE
allarrival  <- add_backbone(allarrival, treatment, ties = "all")
# INTENSITY
allarrival <- classify_intensity(allarrival, treatment, output = "label")
# MARK TKI'S
allarrival <- tag_tkis(allarrival)
# Arrange so that we can review the pertinent variables
allarrival <- allarrival |> select(any_of(idlist), ends_with(c('calc','tki')), everything())

# All of the data have been gathered at this point, but all arrival is arranged
# with one row per arrival, we need to go wide to accommodate the multiple
# arrival forms in REDCap.

# WIDE BACKBONE
backbone_wide <- allarrival |>
  filter(!is.na(arrival_num)) |>
  distinct(across(all_of(idlist)), arrival_num, treatmentbackbone_calc, .keep_all = FALSE) |> 
  tidyr::pivot_wider(
    names_from   = arrival_num,
    values_from  = treatmentbackbone_calc,
    names_prefix = "treatmentbackbone_calc",
    values_fn    = ~ dplyr::first(stats::na.omit(.)),
    values_fill  = NA
  )

# WIDE INTENSITY
intensity_wide <- allarrival |>
  filter(!is.na(arrival_num)) |>
  distinct(across(all_of(idlist)), arrival_num, treatmentintensity_calc, .keep_all = FALSE) |> 
  tidyr::pivot_wider(
    names_from   = arrival_num,
    values_from  = treatmentintensity_calc,
    names_prefix = "treatmentintensity_calc",
    values_fn    = ~ dplyr::first(stats::na.omit(.)),
    values_fill  = NA
  )

# WIDE BCR::ABL TKI
bcrabltki_wide <- allarrival |>
  filter(!is.na(arrival_num) & !is.na(treatmentbcrabl_tki)) |>
  distinct(across(all_of(idlist)), arrival_num, treatmentbcrabl_tki, .keep_all = FALSE) |> 
  tidyr::pivot_wider(
    names_from   = arrival_num,
    values_from  = treatmentbcrabl_tki,
    names_prefix = "treatmentbcrabl_tki",
    values_fn    = ~ dplyr::first(stats::na.omit(.)),
    values_fill  = NA
  )

# WIDE FLT3 TKI
flt3tki_wide <- allarrival |>
  filter(!is.na(arrival_num) & !is.na(treatmentflt3_tki)) |>
  distinct(across(all_of(idlist)), arrival_num, treatmentflt3_tki, .keep_all = FALSE) |> 
  tidyr::pivot_wider(
    names_from   = arrival_num,
    values_from  = treatmentflt3_tki,
    names_prefix = "treatmentflt3_tki",
    values_fn    = ~ dplyr::first(stats::na.omit(.)),
    values_fill  = NA
  )

# COMBINE WIDE DATA
classified_treatment <- intensity_wide |>
  full_join(backbone_wide, by=idlist) |>
  full_join(bcrabltki_wide, by=idlist) |>
  full_join(flt3tki_wide, by=idlist)
  

# QC Check, one record per patient/recordid
length(unique(classified_treatment$recordid)) == nrow(classified_treatment)

# Output to import directory
make_sheet(classified_treatment, rcimportdir, 
           filename = 'classified_treatment_import', 
           time=TRUE, 
           overwrite = TRUE, 
           null_as_blank = TRUE)

join_by <- c(idlist, 'row_number')

allarrival <- allarrival |> select(any_of(join_by), ends_with(c('calc','tki')))

allarrival <- get(backup_name.1) %>%
  left_join(allarrival, by=join_by, suffix = c(".x", "")) |>
  select(any_of(names(allarrival)), treatment, arrivaltable, category, everything(), -ends_with('.x')) |>
  mutate(category=if_else(arrivaltable == 'patient_list', 'Abstraction of Arrivals not Complete', category))

backup_name.2 <- paste0('allarrival_', as.character(now()))
assign(backup_name.2, allarrival)


## -----------------------------------------------------------------------------------------------
runall <- coalesce(.pkg_env$runall, TRUE)
if (runall) source_cache('C:/Users/cmshaw/R Project/Arrival_Cleaning/Arrival_Cleaning Part2.Rmd', source=TRUE)

