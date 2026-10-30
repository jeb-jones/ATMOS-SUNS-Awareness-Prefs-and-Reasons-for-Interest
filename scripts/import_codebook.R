# import_codebook.R
# Parse the ATMOS/SUNS 2026 SAS-style codebook (.XLSX) into two tidy tables
# and (optionally) attach variable + value labels to the analysis dataset.
#
# Outputs:
#   codebook_vars   - one row per variable (name, label, format, type, range)
#   codebook_values - one row per variable x coded value (value, value_label)
#
# Packages: readxl, dplyr, tidyr, stringr (haven only if you apply labels)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

cb_path <- "data/ATMOS_SUNS 2026 Dataset Codebook.XLSX"   # adjust to your project

# ---- 1. Read the raw sheet ---------------------------------------------------
# The first 9 rows are a title block; row 10 holds the column headers.
raw <- read_excel(cb_path, sheet = "Codebook", skip = 9,
                  col_types = "text", .name_repair = "minimal")
names(raw) <- c("variable", "label", "format", "type", "value", "freq_category")

# Line breaks inside cells come from PDF-style wrapping; collapse them.
squish <- function(x) str_squish(str_replace_all(x, "[\r\n]+", " "))

cb <- raw |>
  mutate(across(everything(), squish)) |>
  filter(if_any(everything(), ~ !is.na(.x) & .x != "")) |>   # drop spacer rows
  # Value-label rows have a blank variable name: carry the variable info down
  # (only within a variable, so a blank format isn't inherited from the one above)
  mutate(var_id = cumsum(!is.na(variable))) |>
  group_by(var_id) |>
  fill(variable, label, format, type, .direction = "down") |>
  ungroup() |>
  select(-var_id)

# ---- 2. Variable-level table -------------------------------------------------
ranges <- cb |>
  filter(freq_category == "Range") |>
  mutate(range = value) |>
  separate_wider_regex(value, c(range_min = ".*?", "\\s*to\\s*", range_max = ".*"),
                       too_few = "align_start") |>
  # Numeric where possible (SAS dates stay as day counts from 1960-01-01);
  # date-formatted ranges like "05/15/1986 to ..." remain readable in `range`
  mutate(across(c(range_min, range_max), ~ suppressWarnings(as.numeric(.x)))) |>
  select(variable, range, range_min, range_max)

codebook_vars <- cb |>
  distinct(variable, .keep_all = TRUE) |>
  transmute(
    variable,
    label,
    format,
    type,
    storage   = if_else(str_detect(type, "^Char"), "character", "numeric"),
    length    = as.integer(str_extract(type, "\\d+$")),
    free_text = variable %in% cb$variable[cb$value == "**OTHER**"]
  ) |>
  left_join(ranges, by = "variable") |>
  mutate(position = row_number(), .before = 1)

# ---- 3. Value-label table ----------------------------------------------------
codebook_values <- cb |>
  filter(!is.na(freq_category),
         freq_category != "Range",
         is.na(value) | value != "**OTHER**") |>
  transmute(
    variable,
    format,
    value       = suppressWarnings(as.numeric(value)),   # NA = SAS missing (.)
    value_label = freq_category,
    sas_missing = freq_category == "SAS missing (.)"
  )

# Quick checks
message(nrow(codebook_vars), " variables; ",
        n_distinct(codebook_values$variable), " with value labels")

# Save tidy versions for reuse (fast to reload, easy to diff)
dir.create("codebook", showWarnings = FALSE)
write.csv(codebook_vars,   "codebook/codebook_vars.csv",   row.names = FALSE)
write.csv(codebook_values, "codebook/codebook_values.csv", row.names = FALSE)
saveRDS(list(vars = codebook_vars, values = codebook_values),
        "codebook/codebook.rds")

# ---- 4. (Optional) Apply labels to the dataset -------------------------------
# Adds a "label" attribute to every column (shows in RStudio's View()) and
# converts coded numeric variables to haven_labelled so as_factor() works.
apply_codebook <- function(df, vars = codebook_vars, values = codebook_values) {
  # Match case-insensitively and ignore leading dots/underscores, since R
  # renames SAS names like _NCHS_RURAL2023 to ._nchs_rural2023 or X_NCHS...
  name_key <- function(x) toupper(sub("^(X(?=[._]))?[._]+", "", x, perl = TRUE))
  lookup <- setNames(names(df), name_key(names(df)))
  
  for (i in seq_len(nrow(vars))) {
    col <- lookup[name_key(vars$variable[i])]
    if (is.na(col)) next
    attr(df[[col]], "label") <- vars$label[i]
    
    vl <- values |> filter(variable == vars$variable[i], !sas_missing)
    if (nrow(vl) > 0 && is.numeric(df[[col]])) {
      df[[col]] <- haven::labelled(
        as.numeric(df[[col]]),
        labels = setNames(vl$value, vl$value_label),
        label  = vars$label[i]
      )
    }
  }
  df
}

# Example:
# atmos <- haven::read_sas("data/atmos_suns2026_analysis_id.sas7bdat")
# atmos <- apply_codebook(atmos)
# haven::as_factor(atmos$RACEA)          # "Not Checked" / "Checked"
# codebook_vars |> filter(str_detect(label, regex("PrEP", ignore_case = TRUE)))

# ---- 5. Search helpers -------------------------------------------------------
# One row per variable with its value labels collapsed into a single string
codebook_lookup <- codebook_vars |>
  left_join(
    codebook_values |>
      filter(!sas_missing) |>
      group_by(variable) |>
      summarise(values = paste0(value, " = ", value_label, collapse = "; "),
                .groups = "drop"),
    by = "variable"
  ) |>
  select(position, variable, label, format, values, range, storage)

# cb_search("prep")                 -> any variable whose name, label or values match
# cb_search("^STIG", in = "variable") -> regex on variable names only
cb_search <- function(pattern, in_cols = c("variable", "label", "values")) {
  hits <- Reduce(`|`, lapply(in_cols, function(col)
    str_detect(coalesce(codebook_lookup[[col]], ""),
               regex(pattern, ignore_case = TRUE))))
  codebook_lookup[hits, ]
}

# cb_var("RACEA") -> full label and value codes for one variable, printed readably
cb_var <- function(var) {
  v <- codebook_lookup |> filter(toupper(variable) == toupper(var))
  if (nrow(v) == 0) return(message("No variable named ", var))
  cat(v$variable, " (", v$storage, if (!is.na(v$format)) paste0(", ", v$format), ")\n",
      str_wrap(v$label, 80), "\n", sep = "")
  vals <- codebook_values |> filter(toupper(variable) == toupper(var), !sas_missing)
  if (nrow(vals)) cat(paste0("  ", vals$value, " = ", vals$value_label), sep = "\n")
  if (!is.na(v$range)) cat("  Range:", v$range, "\n")
  invisible(v)
}

# Interactive, searchable table in the RStudio Viewer (needs the DT package)
cb_browse <- function() {
  DT::datatable(codebook_lookup, filter = "top", rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE))
}