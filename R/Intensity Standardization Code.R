# Backbone patterns + high/low groupings
get_pattern_map <- function() {

  HYPH <- "[-‐-–—]"

  single = c(
    sorafenib     = "^soraf(a|e|i)nib\\b|^sora\\b|^9510\\b|^sorafe\\b|^soraf\\b|^nexavar\\b",
    quizartinib   = "^quizartinib\\b",
    crenolanib    = "^crenolanib\\b|^creno\\b|^crenol\\b|^9351\\b",
    sel24_b489    = "^sel24\\s*-\\s*B489\\b|^sel24\\b",
    sunitinib     = "^sunitinib\\b",
    midostaurin   = "^midostaurin\\b|^rydapt\\b",
    cabozantinib  = "^cabozantinib\\b",
    G_749         = "^g\\s*-\\s*749\\b",
    lestaurtinib  = "^lestaurtinib\\b",
    gilteritinib  = "^gilteritinib|^xospata\\b",
    tandutinib    = "^tandutinib\\b",
    amg_925       = "^amg\\s*-\\s*925\\b",

    imatinib      = "^bimatinib\\b|^gleevec\\b|^imkeldi\\b",
    nilotinib     = "^nilotinib\\b|^tasigna\\b|^danziten\\b",
    dasatinib     = "^dasatinib\\b|^sprycel\\b|^phyrago\\b",
    bosutinib     = "^bosutinib\\b|^bosulif\\b",
    ponatinib     = "^ponatinib\\b|^iclusig\\b",
    asciminib     = "^asciminib\\b|^scemblix\\b",

    ivosidenib    = "^ivosidenib\\b|^ivo\\b",
    olutasidenib  = "^olutasidenib\\b",
    enasidinib    = "^enasidinib\\b",
    gilteritinib  = "^gilteritinib\\b|^gilt|^xospata\\b",
    lenalidomide  = "^len(a|o)lid",
    revlamid      = "^revlamid\\b",
    rituximab     = "^rituximab\\b",
    myelotarg     = "^myelotarg\\b|^go\\b|^gemtuzumab\\b",
    ruxolitinib   = "^ruxolitinib\\b|^rux\\b|^jakafi\\b|^jak\\b|\\bjak\\s*2\\b")

  single_rx <- paste(unname(single), collapse = "|")

  flt3_tki = c(
    sorafenib    = "soraf(a|e|i)nib|sora\\b|9510|sorafe\\b|soraf\\b|nexavar",
    quizartinib  = "quizartinib",
    crenolanib   = "crenolanib|creno\\b|crenol\\b|9351",
    sel24_b489   = "sel24\\s*-\\s*B489|sel24\\b",
    sunitinib    = "sunitinib",
    midostaurin  = "midostaurin|rydapt",
    cabozantinib = "cabozantinib",
    G_749        = "g\\s*-\\s*749",
    lestaurtinib = "lestaurtinib",
    gilteritinib = "gilteritinib|xospata",
    tandutinib   = "tandutinib",
    amg_925      = "amg\\s*-\\s*925")

  bcrabl_tki = c(
    imatinib = "\\bimatinib\\b|\\bgleevec\\b|\\bimkeldi\\b",
    nilotinib = "\\bnilotinib\\b|\\btasigna\\b|\\bdanziten\\b",
    dasatinib = "\\bdasatinib\\b|\\bsprycel\\b|\\bphyrago\\b",
    bosutinib = "\\bbosutinib\\b|\\bbosulif\\b",
    ponatinib = "\\bponatinib\\b|\\biclusig\\b",
    asciminib = "\\basciminib\\b|\\bscemblix\\b"
  )

  standard7_3 <- c("7\\s*[+]\\s*3|3\\s*[+]\\s*7", # 7 + 3
                   "5\\s*[+]\\s*2|2\\s*[+]\\s*5", # 5 + 2
                   "4\\s*[+]\\s*3|3\\s*[+]\\s*4", # 3 + 4
                   "\\bs(o|0)106\\b", # so106 sometimes typed with a zero
                   "2588", # decitabine +7+3 trial
                   "9011") # 7 + 3 trial

  standard7_3 <- paste(standard7_3, collapse = "|")

  norx <- c("\\bnone\\b|\\bno\\s+treatment\\b|not\\s+treated|no\\s*rx",
            "\\bpall?.*|paliiative",
            "hospice|comfort|supportive",
            "2nd|opinion|consult\\b",
            "unknown|no record",
            "\\bobserv.*",
            "\\bsurveil.*|survil.*",
            "not\\s*eligible",
            "^outside$|to\\s*outside",
            "\\bdied\\b",
            "\\bhome\\b",
            "\\bnothing\\b")
  norx <- paste(norx, collapse = "|")


  # gclam_base = c("(?:gclam",
  #           "gclam|g\\s*-?\\s*clam",
  #           "gcalm|g\\s*-?\\s*calm",
  #           "\\bgcla\\b|\\bclam\\b",
  #           "\\bclag\\b|\\bclag\\s*-?\\s*m?\\b",
  #           "\\b2734\\b|\\bFH2734\\b|2734\\s*\\(?off",
  #           "\\bFH10000\\b))",
  #           "\\b7971\\b")
  # gclam_base = paste(gclam_base, collapse='|')
  # mini_gclam <- paste0("^(?=.*\\bmini\\b)(?=.*(?:", gclam_base, "))")
  # gclam      <- paste0("^(?!.*\\bmini\\b)(?=.*(?:", gclam_base, "))")  

  
  gclam_base <- c(
    "(?:gclam",
    "gclam|g\\s*-?\\s*clam",
    "gcalm|g\\s*-?\\s*calm",
    "\\bgcla\\b|\\bclam\\b",
    "\\bclag\\b|\\bclag\\s*-?\\s*m?\\b",
    "\\b2734\\b|\\bFH2734\\b|2734\\s*\\(?off",
    "\\bFH10000\\b))",
    "\\b7971\\b"
  )
  gclam_base <- paste(gclam_base, collapse = "|")
  
  mini_gclam <- paste0("^(?=.*\\bmini\\b)(?=.*(?:", gclam_base, "))")
  gclam      <- paste0("^(?!.*\\bmini\\b)(?=.*(?:", gclam_base, "))")  


  # 1) Backbone -> regex
  map <- c(
    reduced       = "\\breduced\\b",
    mini_gclam    = mini_gclam,
    gclam         = gclam, # see above
    clac          = "gclac|g?\\s*-?\\s*clac|\\b(fh)?7144\\b|\\b(fh)?6562\\b|\\b(fh)?2335\\b",
    mec           = "\\bd?\\s*-?\\s*mec\\b|2652|GMI-1271|14070",
    hidac         = "\\bhi?dac\\b|High dose ARA-C",
    iap           = "\\bIAP\\b|\\b2674\\b|\\b2674\\s?\\(?off\\b|\\bIA\\b",
    flag_ida      = "(?=.*flag)(?=.*ida)|\\bflag\\b|\\bfai\\b",
    bend_ida      = "(?=.*bend)(?=.*ida)|2413",
    hma           = "azacitidine|azacitadine|\\baza\\b|\\bazac.*|\\bvidaza\\b|dacogen|decit|inqovi|onureg|2288|9019|2566\\s*\\(td\\)",
    standard7_3   = standard7_3,
    vyxeos        = "vyxeos|CPX\\s*-\\s*351|\\b(fh)?2642\\b",
    flam          = "\\bflam\\b|2315",
    atra          = "\\batra\\b",
    hct           = "\\bhct\\b|\\ballo|\\btbi\\b|transplant|flu|\\b7617\\b|\\bbu(-|\\s|\\+)?cy\\b",
    ldac          = "\\bldac\\b|low\\s*dose\\s*ara-?c|\\b(fh)?2302\\b|\\b2566\\s*\\(ta\\)",
    tose          = "\\btosedostat\\b|\\b2566\\b(?!\\s*\\()|2566\\s*\\(ta\\)|2566\\s*\\(td\\)|s0919|so919",
    hypercvad     = paste0(
                      "\\bhyper\\s*", HYPH, "?\\s*cva?d\\b",  # Hyper-CVAD / Hyper CVD
                      "|\\bh\\s*",    HYPH, "?\\s*cva?d\\b",  # H-CVAD / H CVD
                      "|\\bhcva?d\\b",                        # HCVAD / HCVD
                      "|\\bhypercvad\\b"),                    # contiguous HYPERCVAD
    sgn_cd33      = "2690|9233|\\bsgn\\b|sgn-cd33",
    single        = single_rx,
    norx          = norx)

  # 2) Intensity-ish groupings (adding APL/NoRx bucket)
  groups <- list(
    high = c("gclam","clac","iap","flag_ida","hct","flam","mec"),
    int  = c("vyxeos","reduced","standard7_3","hidac","hypercvad"),
    low  = c("mini_gclam","ldac","bend_ida","hma","sgn_cd33","single","tose"),
    apl  = c("atra"),
    norx = c("norx")
  )

  # 3) Global precedence (include apl/norx so you can prioritize them if desired)
  precedence <- unique(c(groups$high, groups$int, groups$low, groups$apl, groups$norx, groups$unk))

  # 4) Pre-computed collapsed patterns per group (build for all groups)
  patterns <- lapply(groups, function(keys) paste0(unname(map[keys]), collapse = "|"))
  pm_ <- list(map = map,
             groups = groups,
             precedence = precedence,
             patterns = patterns,
             flt3_tki = flt3_tki,
             bcrabl_tki = bcrabl_tki)
  list(map = map, groups = groups, precedence = precedence, patterns = patterns, flt3_tki = flt3_tki, bcrabl_tki = bcrabl_tki)
}

#' Classify AML treatment intensity from a free-text column
#'
#' @param data    A data.frame / tibble.
#' @param col     Unquoted name of the text column containing treatment text.
#' @param new_col Name of the output column. Defaults to "<col>_intensity" (label output)
#'                or "<col>_high" (binary output).
#' @param output  "label" -> "high"/"low"/NA  |  "binary" -> 1/0/NA
#' @param patterns Optional list with elements "high" and "low" (regex strings).
#'                 If NULL, built-in defaults (based on your list) are used.
#' @param ignore_case Logical; case-insensitive matching (default TRUE).
#'
#' @return data with a new column appended.
classify_intensity <- function(data, col, new_col = NULL,
                               output   = c("label", "number"),
                               priority =  c("high","int","low","apl","norx"),
                               ignore_case = TRUE) {
  stopifnot(is.data.frame(data))
  output   <- match.arg(output)
  priority <- unique(match.arg(priority, c("high","int","low","apl","norx"), several.ok = TRUE))

  col_sym  <- rlang::ensym(col)
  col_name <- rlang::as_string(col_sym)
  if (is.null(new_col)) {
    new_col <- paste0(col_name, if (output == "label") "intensity_calc" else "_intensity_num")
  }

  pm <- get_pattern_map()
  if (!is.list(pm)) stop("get_pattern_map() must return a list.")

  pats <- pm$patterns

  # Compile regexes once
  rx <- lapply(pats, function(p) stringr::regex(p, ignore_case = ignore_case))

  txt <- dplyr::coalesce(as.character(data[[col_name]]), "")

  # Resolve by priority: first level that matches wins
  lab <- rep(NA_character_, length(txt))
  for (lvl in priority) {
    if (!is.null(rx[[lvl]])) {              # <-- crucial guard
      hit <- stringr::str_detect(txt, rx[[lvl]])
      hit[is.na(hit)] <- FALSE
      lab[is.na(lab) & hit] <- lvl
    }
  }

  data[[new_col]] <- if (output == "label") {
    lab
  } else {
    # Map labels to ordinal numbers: low=1, int=2, high=3
    map_num <- c(norx=0L, low=1L, int=2L, high=3L, apl=8L)
    unname(map_num[lab])
  }

  data
}

# Tag each row with the backbone name(s) based on get_pattern_map()
# - Precedence = order of names in get_pattern_map()$map (or the named vector it returns)
# - ties = "first" -> first match only; "all" -> join all matches in order
# zz_2 <- add_backbone(zz_, treatment, ties = "first")

add_backbone <- function(data, col, new_col = NULL, ignore_case = TRUE,
                         ties = c("first", "all"), sep = "; ",
                         backbone_order = NULL, ignore_whitespace = TRUE) {

  # browser()

  stopifnot(is.data.frame(data))
  ties <- match.arg(ties)

  col_sym  <- rlang::ensym(col)
  col_name <- rlang::as_string(col_sym)
  if (is.null(new_col)) new_col <- paste0(col_name, "backbone_calc")

  pm <- get_pattern_map()
  rx_map <- pm$map

  # Use map’s default precedence unless an override is supplied
  order_vec <- if (is.null(backbone_order)) pm$precedence else {
    stopifnot(all(backbone_order %in% names(rx_map)))
    backbone_order
  }
  # keep order, but don't drop any keys
  order_vec <- c(order_vec, setdiff(names(rx_map), order_vec))
  rx_map <- rx_map[order_vec]

  txt <- dplyr::coalesce(as.character(data[[col_name]]), "")

  if (ignore_whitespace) {
    txt <- str_squish(txt) # trims and collapses all whitespace
  }

  det_mat <- vapply(
    X = rx_map,
    FUN = function(rx) stringr::str_detect(txt, stringr::regex(rx, ignore_case = ignore_case)),
    FUN.VALUE = logical(length(txt))
  )

  if (is.null(dim(det_mat))) {
    det_mat <- matrix(det_mat, ncol = 1L)
    colnames(det_mat) <- names(rx_map)[1]
  }

  out <- if (ties == "first") {
    idx <- max.col(det_mat * 1L, ties.method = "first")
    ifelse(rowSums(det_mat) == 0, NA_character_, names(rx_map)[idx])
  } else {
    apply(det_mat, 1, function(row) {
      hits <- names(rx_map)[which(row)]
      if (length(hits) == 0) NA_character_ else paste(hits, collapse = sep)
    })
  }

  data[[new_col]] <- out
  data
}

# Helper: return matched pattern names collapsed with `sep` (or NA if none)
.detect_from_patterns <- function(txt, patterns, ignore_case = TRUE, sep = " | ") {
  stopifnot(is.character(txt))
  stopifnot(is.character(patterns), !is.null(names(patterns)))

  nm <- names(patterns)

  # For each row of text, check which patterns hit
  hits <- lapply(
    seq_along(txt),
    function(i) {
      nm[ vapply(
        patterns,
        function(rx) stringr::str_detect(txt[i], stringr::regex(rx, ignore_case = ignore_case)),
        logical(1)
      ) ]
    }
  )

  vapply(hits, function(h) if (length(h)) paste(h, collapse = sep) else NA_character_, character(1))
}


tag_tkis <- function(data        = allarrival,
                     col         = treatment,
                     bcrabl_col  = "treatmentbcrabl_tki",
                     flt3_col    = "treatmentflt3_tki",
                     ignore_case = TRUE,
                     sep = " | ") {

  # browser()

  stopifnot(is.data.frame(data))

  col_sym  <- rlang::ensym(col)
  col_name <- rlang::as_string(col_sym)

  pm <- get_pattern_map()
  if (is.null(pm$bcrabl_tki) || is.null(pm$flt3_tki)) {
    stop("get_pattern_map() must return $bcrabl_tki and $flt3_tki named pattern vectors.")
  }

  txt <- dplyr::coalesce(as.character(data[[col_name]]), "")
  txt <- stringr::str_squish(txt)

  data[[bcrabl_col]] <- .detect_from_patterns(txt, pm$bcrabl_tki, ignore_case, sep)
  data[[flt3_col]]   <- .detect_from_patterns(txt, pm$flt3_tki,   ignore_case, sep)

  return(data)
}

# zz_ <- tag_tkis() |> select(any_of(idlist), ends_with(c('_calc','_tki')), everything())


is_currative_rx_fhcc <- function(treatment) {
  # Normalize input
  tx <- as.character(treatment)
  tx <- ifelse(is.na(tx) | trimws(tx) == "", NA_character_, tx)

  # Patterns indicating non-curative or unrelated treatments
  pattern <- paste(
    "no\\s+rx",        # "no rx"
    "no\\s+rec",       # "no rec"
    "spice",           # hospice (as written)
    "pall?(i|a)",      # palli/palla/pali/pala
    "unknown",
    "comfort",
    "consult",
    "outside",
    "support",
    "opinion",
    "survillance?",    # as written
    "watch\\s*wait",
    "2nd\\s",
    "none",
    "not\\selig",
    "not?\\streat",
    sep = "|"
  )

  # grepl is vectorized; NA in tx -> NA in hit
  hit <- grepl(pattern, tx, ignore.case = TRUE, perl = TRUE)

  # TRUE if no non-curative indicators; FALSE if non-curative; NA stays NA
  res <- ifelse(is.na(hit), NA, !hit)
  return(res)
}


is_currative_rx_fhcc__ <- function(treatment) {

  # Patterns indicating non-currative or unrelated treatments
  pattern <- paste(
    "no\\s+rx",       # Explicitly "no rx" — no treatment given
    "no\\s+rec",      # "no record" or "no recommendation"
    "spice",          # hospice
    "pall?(i|a)",     # "palli", "palla", "pali", or "pala" — palliative
    "unknown",        # Treatment unknown
    "comfort",        # Comfort care
    "consult",        # Consult only — no intent to treat
    "outside",        # Treated outside, not actionable here
    "support",        # Supportive care
    "opinion",        # Second opinion only, not treatment
    "survillance?",   # Surveillance
    "watch\\s*wait",  # Surveillance
    "2nd\\s",         # Second opinion only, not treatment
    "none",           # No treatment
    "not\\selig",     # Not eligible for treatment
    "not?\\streat",   # Not treated / no treatment given
    sep = "|"
  )

  # Return TRUE if no non-currative indicators found (i.e., likely currative intent)
  !grepl(pattern, treatment, ignore.case = TRUE)
}



build_arrivalupdates <- function(rg_df = RG_Number_Updates,
                                 forms = 1:11,
                                 idlist = c("recordid", "ptmrn", "ptlastname")) {

  if (FALSE) {
    rg_df = RG_Number_Updates
    forms = 1:11
    idlist = c("recordid", "ptmrn", "ptlastname")
  }

  arrival_list <- list()

  i = 1
  for (i in forms) {
    form_label <- paste0("arrival", i)

    arrival <- rg_df |>
      filter(arrivaltable == form_label) |>
      mutate(ptlastname = toupper(ptlastname)) |>
      select(recordid, ptmrn, ptlastname, rgnumber, `Short.Title`, `PI.Name`) |>
      rename(
        !!paste0("rgnumber", i)     := rgnumber,
        !!paste0("short_title", i) := `Short.Title`,
        !!paste0("pi_name", i)     := `PI.Name`
      )

    if (nrow(arrival)==0) next
    arrival_list[[form_label]] <- arrival

  }

  # Create master ID list
  all_ids <- bind_rows(arrival_list) |>
    distinct(across(all_of(idlist))) |>
    select(any_of(idlist))

  # Left join each arrival sheet by ID
  arrivalupdates <- reduce(
    arrival_list,
    .f = ~ left_join(.x, .y, by = idlist),
    .init = all_ids
  )

  # Explicitly ordered joins
  arrivalupdates <- all_ids
  for (form_label in names(arrival_list)) {
    arrivalupdates <- left_join(arrivalupdates, arrival_list[[form_label]], by = idlist)
  }

  return(arrivalupdates)
}




rx_stand <- function(df=induction, col='treatment') {
  # df <- induction
  # col <- 'treatment'
  new <- paste0(col, '_clean')

  rslt <- df |>
    mutate(
      tmp_text  = .data[[col]],
      tmp_right = str_replace_all(tmp_text, "\\|.*$", ""),
      tmp_found = str_detect(tmp_right, '\\('),
      tmp_paren = if_else(tmp_found, str_extract(tmp_right, '\\(.*\\)|\\[.*\\]'), 'NO PARENTHETICAL STATEMENT FOUND'),

      # ---------------------------------
      # General Removal of Non-Essential Annotations
      # ---------------------------------
      tmp_text = tmp_right |>
        str_replace_all("(?i)re-induction","reinduction") |>
        str_replace_all("Not on treatment","NOTRX") |>
        str_remove_all("(?i)\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}") |>
        str_remove_all("(?i)\\d{4}-\\d{2}-\\d{2}") |>
        str_remove_all("\\d{1,2}[/-]\\d{4}") |>

        str_remove_all("\\(([^)]*(mg|daily|x\\d+|day|SQ|IV|PO)[^)]*)\\)") |>

        # str_remove_all("(?i)^Ind\\b") |>
        str_remove_all("(?i)\\b\\d+\\s?mg(\\/m2)?\\b") |>
        str_remove_all("(?i)\\bmg(\\/m2)?\\b") |>
        str_remove_all("(?i)\\bm2\\b") |>
        str_remove_all("(/)?m2") |>
        str_remove_all("(/)?m²") |>
        str_remove_all("(?i)\\b(daily|x\\d+\\s?days|Q\\d+\\s?days)\\b") |>
        str_remove_all("(?i)\\b(Inj|IV|PO)\\b") |>
        str_remove_all("(?i)\\b(IND|INDUCTION|RE-INDUCTION|REINDUCTION|CONSOLIDATION|MAINTENANCE|OUTSIDE|RESISTANT|REFRACTORY|RELAPSED|CLINICAL|TRIAL|PROTOCOL|DAY|DAYS|ON)\\b") |>

        str_replace_all("(?i)\\b(/?ARAC/?)\\b",  "+cytarabine ") |>
        str_replace_all("(?i)\\b(/?IDA/?)\\b",   "+idarubicin ") |>
        str_replace_all("(?i)\\b(/?DAUNO/?)\\b", "+daunorubicin"),
      tmp_slash  = str_detect(tmp_text, '/'),
      tmp_status = tmp_text
    ) |>

    select(-tmp_found) |>

    mutate(tmp_text = tmp_status |>
      # ---------------------------------
      # Pattern-Based Simplification C#, S# S#C# x#
      # ---------------------------------
      str_remove_all("(?i)\\bS\\d{1,3}C\\d{1,3}\\b\\s+with\\b") |>
        str_remove_all("(?i)\\bS\\d{1,3}C\\d{1,3}\\b") |>
        str_remove_all("(?i)\\bC\\d{1,3}\\b\\s+with\\b") |>
        str_remove_all("(?i)\\bC\\d{1,3}\\b") |>
        str_remove_all("(?i)\\bS\\d{1,3}\\b") |>
        str_remove_all("(?i)\\bX\\d{1,2}\\b") |>

        # ---------------------------------
      # Simple Syntax Standardization
      # ---------------------------------
      str_replace_all("/", " with ") |>
        str_replace_all("(?i)\\b(plus|and)\\b", "+") |>
        str_replace_all('\\d+\\-\\d+', '')  |>
        str_replace_all('\\s+\\d{4,}', '')  |>

        # ---------------------------------
        # Synonym Normalization
        # ---------------------------------
        str_replace_all("(?i)low.?dose",                     "low dose") |>  # Normalize low dose
        str_replace_all("(?i)high.?dose",                    "high dose") |>  # Normalize high dose


        str_replace_all("(?i)\\bgemtuzumab ozogamicin\\b",  "GO") |>  # Normalize GO synonyms
        str_replace_all("(?i)\\bgemtuzumab\\b",             "GO") |>  # Normalize GO synonyms
        str_replace_all("(?i)\\bmylotarg\\b",               "GO") |>  # Normalize GO synonyms

        str_replace_all("(?i)\\bvyx\\w*",                   "vyxeos") |>  # Normalize vyx... to vyxeos
        str_replace_all("(?i)\\bCPX[- ]?351\\b",            "vyxeos") |>  # Normalize CPX-351 to vyxeos
        str_replace_all("(?i)vyxeos vyxeos",                "vyxeos") |>  # Normalize vyxeos

        str_replace_all("(?i)\\bvidaza\\b",                 "azacitidine") |>  # Normalize Vidaza
        str_replace_all("(?i)\\bCC[- ]?486\\b",             "azacitidine") |>  # Normalize CC-486
        str_replace_all("(?i)\\bOnureg\\b",                 "azacitidine") |>  # Normalize Onureg
        str_replace_all("(?i)\\baza\\b",                    "azacitidine") |>  # Normalize azacitidine
        str_replace_all("(?i)azacitadine",                  "azacitidine") |>  # Normalize azacitidine
        str_replace_all("(?i)azacitidine",                  "azacitidine") |>  # Normalize azacitidine
        str_replace_all("(?i)azacitidine solo",             "azacitidine") |>  # Normalize azacitidine

        str_replace_all("(?i)\\bVC\\b",                      "vincristine") |>

        str_replace_all("(?i)\\bORAL DECITABINE\\b",         "decitabine") |>
        str_replace_all("(?i)\\bdacogen\\b",                 "decitabine") |>  # Normalize Dacogen to deci
        str_replace_all("(?i)\\bDecitabine\\b",              "decitabine") |>  # Normalize Decitabine to deci


        str_replace_all("(?i)\\bINV\\b",                     "decitabine+cedazuridine") |>
        str_replace_all("(?i)\\bINQOVI\\b",                  "decitabine+cedazuridine") |>
        str_replace_all("(?i)\\bASTX727\\b",                 "decitabine+cedazuridine") |>
        str_replace_all("(?i)\\bDECITABINE/CEDAZURIDINE\\b", "decitabine+cedazuridine") |>



        str_replace_all("(?i)\\bRuxolitinib\\b",             "ruxolitinib") |>

        str_replace_all("(?i)\\bDexamethasone\\b",           "dexamethasone") |>  # Normalize dexamethasone to deci

        str_replace_all("(?i)\\bdaunomycin\\b",              "daunorubicin") |>  # Normalize daunomycin to dauno
        str_replace_all("(?i)\\bDuanorubicin\\w*",           "daunorubicin") |>  # Normalize daunorubicin
        str_replace_all("(?i)\\Daunorubicin\\w*",            "daunorubicin") |>  # Normalize daunorubicin
        str_replace_all("(?i)\\Danurubicin\\w*",             "daunorubicin") |>  # Normalize daunorubicin
        str_replace_all("(?i)\\dauno\\w*",                   "daunorubicin") |>  # Normalize daunorubicin
        str_replace_all("(?i)\\Danunorubicin\\w*",           "daunorubicin") |>  # Normalize daunorubicin


        str_replace_all("(?i)\\b(ara\\-?c|arac)\\b",         "cytarabine") |>  # Normalize ara-c, arac to cytarabine
        str_replace_all("(?i)\\bcytar\\w*",                  "cytarabine") |>  # Normalize cytarabine
        str_replace_all("(?i)cytarabine",                    "cytarabine") |>  # Normalize cytarabine



        str_replace_all("(?i)\\bida\\w*",                    "idarubicin") |>  # Normalize idarubicin
        str_replace_all("(?i)\\bclad\\w*",                   "cladrabine") |>  # Normalize cladrabine
        str_replace_all("(?i)\\bvene\\w*",                   "venetoclax") |>  # Normalize venetoclax

        str_replace_all("(?i)\\bmido\\w*",                   "midostaurin") |>  # Normalize midostaurin
        str_replace_all("(?i)\\bmito\\w*",                   "mitoxantrone") |>  # Normalize mitoxantrone
        str_replace_all("(?i)\\bmetho\\w*",                  "methotrexate") |>  # Normalize methotrexate
        str_replace_all("(?i)\\bmtx\\w*",                    "methotrexate") |>  # Normalize methotrexate

        str_replace_all("(?i)\\bVincristine\\w*",            "vincristine") |>  # Normalize vincristine
        str_replace_all("(?i)\\bPrednisone\\w*",             "prednisone") |>  # Normalize prednisone

        str_replace_all("(?i)\\bUproleselan\\w*",            "uproleselan") |>  # Normalize uproleselan
        str_replace_all("(?i)\\b(GMI\\-?1271)\\b",           "uproleselan") |>  # Normalize uproleselan
        str_replace_all("(?i)\\bhypercvad\\b",               "HyperCVAD") |>  # Normalize HyperCVAD

        str_replace_all("(?i)low.?dose\\scytarabine",        "LDAC") |>  # Normalize LDAC
        str_replace_all("(?i)high.?dose\\scytarabine",       "HiDAC") |>  # Normalize HiDAC
        str_replace_all("(?i)hidac",                         "HiDAC") |>  # Normalize HiDAC

        # ---------------------------------
      # Cleanup and Deduplication
      # ---------------------------------
      str_remove_all("[^A-Za-z0-9+ ]") |>
        str_replace_all("\\s*\\+\\s*", "+") |>
        str_replace_all("(?i)(?<=\\b)with(?=\\b)", "+") |> # str_replace_all("\\s+with\\s+", "+") |>
        str_replace_all("(?i)\\b(\\w+)\\+\\1\\b", "\\1") |>
        str_replace_all("\\++", "+") |>
        str_replace_all("^\\++", "") |>
        str_replace_all("\\s\\++", " ") |>
        str_replace_all("\\++$", "") |>
        str_replace_all("NOTRX", "Not on treatment") |>
    str_squish()) |>
    mutate(tmp_text = case_when(
      # Remove ARAC/IDA/DAUNO from 7+3
      str_detect(tmp_text, "7\\+3") ~ tmp_text |>
      str_remove_all("\\+?(cytarabine|idarubicin|daunorubicin)"),

      # Remove gcsf|cladrabine|cytarabine|methotrexate from GCLAM
      str_detect(tmp_text, "GCLAM") ~ tmp_text |>
      str_remove_all("\\+?(gcsf|cladrabine|cytarabine|mitoxantrone)"),

      # Remove fosfamide-doxorubicin-cisplatin from IAP
      str_detect(tmp_text, "IAP") ~ tmp_text |>
      str_remove_all("\\+?(ifosfamide|doxorubicin|cisplatin)"),

      # Default case — keep as is
      TRUE ~ tmp_text)) |>
    mutate(tmp_text = if_else(tmp_slash, tmp_right, tmp_text))

  rslt <- rslt |> mutate(!!new := tmp_text) |> relocate(!!new, .after = all_of(col)) |> select(-starts_with('tmp_'))

  return(rslt)
}

#  zz <- rx_stand(allarrival, 'treatment')
