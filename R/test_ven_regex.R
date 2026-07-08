# Quick regression test for the venetoclax ('ven') backbone regex added to
# get_pattern_map() in "Intensity Standardization Code.R".
#
# Compares the OLD state (no 'ven' pattern existed -> venetoclax was never
# detected as a backbone, so HMA+venetoclax was indistinguishable from HMA-alone)
# against the NEW 'ven' regex, using the 2000+ real distinct treatment strings in
# distinct_treatments.txt as the test corpus.
#
# Run from the repo root:  Rscript "R/test_ven_regex.R"

corpus_path <- "distinct_treatments.txt"
if (!file.exists(corpus_path)) {
  corpus_path <- file.path("..", "distinct_treatments.txt")
}
tx <- readLines(corpus_path, warn = FALSE, encoding = "UTF-8")

# NEW ven regex — must stay in sync with the `ven` entry in get_pattern_map().
ven <- paste0(
  "\\bvene\\w*",           # venetoclax, vene, and other vene* spellings
  "|\\bven[eo]?toclax\\b", # ventoclax / venotoclax typos
  "|\\bven\\d*\\b",        # standalone 'ven' token, incl. day-count suffix (Ven, VEN14)
  "|\\bABT[- ]?199\\b",    # ABT-199 / ABT199
  "|\\bGDC[- ]?0199\\b"    # GDC-0199 / GDC0199
)

new_hits <- grepl(ven, tx, ignore.case = TRUE, perl = TRUE)

cat("Corpus size:              ", length(tx), "\n")
cat("OLD ven-backbone matches: ", 0L, "(no 'ven' pattern existed)\n")
cat("NEW ven regex matches:    ", sum(new_hits), "\n\n")

# --- False-positive guard: common words that contain 'ven' must NOT match -----
neg <- c("seven days", "intravenous cytarabine", "prevention trial",
         "given daily", "convention", "eleven")
stopifnot(!any(grepl(ven, neg, ignore.case = TRUE, perl = TRUE)))
cat("False-positive guard passed (no match on: ",
    paste(neg, collapse = ", "), ")\n", sep = "")

# --- Recall guard: spec-required variants must all match ----------------------
pos <- c("venetoclax", "Venetoclax", "ventoclax", "venotoclax", "vene",
         "Ven", "Ven/Aza", "VEN14", "DEC10/VEN21",
         "ABT-199", "ABT199", "GDC-0199")
stopifnot(all(grepl(ven, pos, ignore.case = TRUE, perl = TRUE)))
cat("Recall guard passed (all spec variants matched).\n\n")

cat("Sample of newly-detected strings:\n")
print(utils::head(tx[new_hits], 15))
