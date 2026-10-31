# analysis_tables.R
# Table 1 (demographics by PrEP awareness) and descriptive tables for
# awareness, willingness, preferences, and reasons for not wanting injectable PrEP.
#
# Assumes `as_2026_clean` is the output of clean_as_data().

library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(gtsummary)
library(forcats)

theme_gtsummary_compact()

# ---- 0. Analysis variables ---------------------------------------------------

# Drop the long question stem from check-all labels ("Man:How else do you..." -> "Man")
short_label <- function(df, vars) {
  for (v in intersect(vars, names(df))) {
    lab <- attr(df[[v]], "label")
    if (!is.null(lab)) attr(df[[v]], "label") <- str_squish(sub(":.*$", "", lab))
  }
  df
}

# 0/1 item -> factor with both levels, so items nobody checked still summarize
as_01_factor <- function(x) {
  structure(factor(x, levels = c(0, 1)), label = attr(x, "label"))
}

gender_vars <- c("gender_id_man", "gender_id_woman", "gender_id_tm",
                 "gender_id_tf", "gender_id_nb", "gender_id_oth")
stop_vars   <- grep("^prepstop_reason_", names(as_2026_clean), value = TRUE)
reason_vars <- paste0("inj_prep_noreas_", letters[1:19], "_comb")
dom_vars    <- c("dom_need", "dom_safety", "dom_inject",
                 "dom_know", "dom_access", "dom_stigma")

analysis <- as_2026_clean %>%
  # 0/1 items kept numeric still carry value labels; gtsummary wants plain numbers
  mutate(across(where(haven::is.labelled), remove_val_labels)) %>%
  short_label(c(gender_vars, stop_vars)) %>%
  mutate(
    # Aware of at least one modality (NA only if missing all three)
    aware_any = as.integer(if_any(c(aware_qd, aware_od, aware_inj), ~ .x == 1)),
    aware_any_f = factor(aware_any, levels = c(1, 0),
                         labels = c("Aware of PrEP", "Not aware of PrEP")),
    
    prep_hx = factor(case_when(prep_ever == 0 ~ "Never used PrEP",
                               prep_ever == 2 ~ "Former PrEP user",
                               prep_ever == 1 ~ "Current PrEP user"),
                     levels = c("Never used PrEP", "Former PrEP user",
                                "Current PrEP user")),
    
    # Pairwise choices (asked of non-users willing to use exactly two modalities)
    pair_qd_od  = factor(prep_qdxod,  1:2, c("Daily oral", "On-demand oral")),
    pair_qd_inj = factor(prep_qdxinj, 1:2, c("Daily oral", "Injectable")),
    pair_od_inj = factor(prep_odxinj, 2:3, c("On-demand oral", "Injectable")),
    
    # Who answered the injectable reasons items
    reason_group = case_when(
      prep_group == "Daily oral"  ~ "Current daily oral PrEP users",
      prep_group == "Not current" ~ "Not currently using PrEP"
    )
  ) %>%
  set_variable_labels(
    aware_any    = "Aware of any PrEP modality",
    prep_hx      = "PrEP use history",
    pair_qd_od   = "Daily vs on-demand oral",
    pair_qd_inj  = "Daily oral vs injectable",
    pair_od_inj  = "On-demand oral vs injectable"
  )

# ---- 1. Table 1: demographics by overall PrEP awareness ----------------------
# Edit this list to taste; check-all items (gender identity) show % checked
demo_vars <- c("age", "agecat", "newrace", "region", "nchs_rural",
               "trans_id", gender_vars, "birthsex",
               "inscat", "employment", "food_sec", "homeless_p12m",
               "seehcp", "lasthivtest12", "uas", "anysti_dx")
demo_vars <- intersect(demo_vars, names(analysis))

tbl1 <- analysis %>%
  filter(!is.na(aware_any_f)) %>%
  select(aware_any_f, all_of(demo_vars)) %>%
  mutate(across(any_of(gender_vars), as_01_factor)) %>%
  tbl_summary(
    by = aware_any_f,
    type = list(any_of(gender_vars) ~ "dichotomous"),
    value = list(any_of(gender_vars) ~ "1"),
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"),
    missing_text = "Missing"
  ) %>%
  add_overall() %>%
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  bold_labels() %>%
  modify_caption("**Table 1. Participant characteristics by PrEP awareness**")

# ---- 2. Awareness ------------------------------------------------------------
tbl_aware <- analysis %>%
  filter(!is.na(prep_hx)) %>%
  select(prep_hx, aware_any, aware_qd, aware_od, aware_inj, n_aware) %>%
  tbl_summary(
    by = prep_hx,
    type = list(c(aware_any, aware_qd, aware_od, aware_inj) ~ "dichotomous",
                n_aware ~ "categorical"),
    statistic = list(all_dichotomous() ~ "{n}/{N} ({p}%)"),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  bold_labels() %>%
  modify_caption("**Table 2. Awareness of PrEP modalities by PrEP use history**")

# ---- 3. Willingness ----------------------------------------------------------
# Current users are coded willing for the modality they use
tbl_will <- analysis %>%
  filter(!is.na(prep_hx)) %>%
  select(prep_hx, will_qd, will_od, will_inj, n_willing) %>%
  tbl_summary(
    by = prep_hx,
    type = list(c(will_qd, will_od, will_inj) ~ "dichotomous",
                n_willing ~ "categorical"),
    statistic = list(all_dichotomous() ~ "{n}/{N} ({p}%)"),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  bold_labels() %>%
  modify_caption("**Table 3. Willingness to use PrEP modalities by PrEP use history**")

# ---- 4. Preferences (participants not currently using PrEP) -----------------
# preferred: willing to use one -> that one; willing to use 2+ -> stated choice
tbl_pref <- analysis %>%
  filter(prep_group == "Not current", !is.na(prep_hx)) %>%
  mutate(prep_hx = fct_drop(prep_hx)) %>%
  select(prep_hx, preferred, pref_basis) %>%
  tbl_summary(by = prep_hx, missing_text = "Missing") %>%
  add_overall() %>%
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  bold_labels() %>%
  modify_caption("**Table 4. Preferred PrEP modality among participants not currently using PrEP**")

# Full rankings among those willing to use all three
tbl_rank <- analysis %>%
  filter(if_all(c(prep_rank_qd, prep_rank_od, prep_rank_inj), ~ !is.na(.x))) %>%
  transmute(across(c(prep_rank_qd, prep_rank_od, prep_rank_inj),
                   ~ factor(.x, 1:3, c("Ranked 1st", "Ranked 2nd", "Ranked 3rd")))) %>%
  set_variable_labels(prep_rank_qd  = "Daily oral PrEP",
                      prep_rank_od  = "On-demand oral PrEP",
                      prep_rank_inj = "Injectable PrEP") %>%
  tbl_summary() %>%
  bold_labels() %>%
  modify_caption("**Table 5. Rankings among participants willing to use all three modalities**")

# ---- 5. Reasons for not wanting injectable PrEP ------------------------------
reasons <- analysis %>%
  filter(will_inj == 0, !is.na(reason_group),
         if_any(all_of(reason_vars), ~ !is.na(.x)))

# Order items from most to least endorsed
item_order <- reasons %>%
  summarise(across(all_of(reason_vars), ~ mean(.x == 1, na.rm = TRUE))) %>%
  pivot_longer(everything()) %>%
  arrange(desc(value)) %>%
  pull(name)

reason_table <- function(vars) {
  reasons %>%
    select(reason_group, all_of(vars)) %>%
    mutate(across(any_of(item_order), as_01_factor)) %>%
    tbl_summary(
      by = reason_group,
      type = list(all_of(vars) ~ "dichotomous"),   # domains are TRUE/FALSE, items 0/1
      value = list(any_of(item_order) ~ "1"),
      statistic = list(all_dichotomous() ~ "{n}/{N} ({p}%)"),
      missing = "no"
    ) %>%
    add_overall()
}

tbl_reasons <- tbl_stack(
  list(reason_table(dom_vars), reason_table(item_order)),
  group_header = c("Domains", "Individual reasons")
) %>%
  modify_caption("**Table 6. Reasons for not wanting to use injectable PrEP**")
# Note: "I don't trust the pharmaceutical industry" was not asked of current
# daily oral users, so its denominator differs (shown in n/N).

# Reasons for stopping PrEP (former users)
tbl_stop <- analysis %>%
  filter(prep_ever == 2) %>%
  select(all_of(stop_vars)) %>%
  mutate(across(everything(), as_01_factor)) %>%
  tbl_summary(type = everything() ~ "dichotomous", value = everything() ~ "1",
              missing = "no") %>%
  modify_caption("**Table 7. Reasons for stopping PrEP among former users**")

# ---- 6. Export to Word -------------------------------------------------------
flextable::save_as_docx(
  "Table 1" = as_flex_table(tbl1),
  "Table 2" = as_flex_table(tbl_aware),
  "Table 3" = as_flex_table(tbl_will),
  "Table 4" = as_flex_table(tbl_pref),
  "Table 5" = as_flex_table(tbl_rank),
  "Table 6" = as_flex_table(tbl_reasons),
  "Table 7" = as_flex_table(tbl_stop),
  path = "results/prep_tables.docx"
)


tables <- list(
  "Table 1" = tbl1,
  "Table 2" = tbl_aware,
  "Table 3" = tbl_will,
  "Table 4" = tbl_pref,
  "Table 5" = tbl_rank,
  "Table 6" = tbl_reasons,
  "Table 7" = tbl_stop
)

titles <- c(
  "Table 1" = "Participant characteristics by PrEP awareness",
  "Table 2" = "Awareness of PrEP modalities by PrEP use history",
  "Table 3" = "Willingness to use PrEP modalities by PrEP use history",
  "Table 4" = "PrEP modality preferences among participants not currently using PrEP",
  "Table 5" = "Rankings among participants willing to use all three modalities",
  "Table 6" = "Reasons for not wanting to use injectable PrEP",
  "Table 7" = "Reasons for stopping PrEP among former users"
)

wb   <- createWorkbook()
bold <- createStyle(textDecoration = "bold", wrapText = TRUE, valign = "top")

for (nm in names(tables)) {
  tb <- as_tibble(tables[[nm]], col_labels = TRUE)
  # Strip gtsummary's markdown (**bold**) and line breaks from the headers
  names(tb) <- str_squish(gsub("\\*\\*|__|\\n", " ", names(tb)))
  
  addWorksheet(wb, nm)
  writeData(wb, nm, paste0(nm, ". ", titles[[nm]]), startRow = 1)
  addStyle(wb, nm, createStyle(textDecoration = "bold", fontSize = 12), rows = 1, cols = 1)
  writeData(wb, nm, tb, startRow = 3, headerStyle = bold)
  
  setColWidths(wb, nm, cols = 1, widths = 60)
  setColWidths(wb, nm, cols = 2:ncol(tb), widths = 18)
  freezePane(wb, nm, firstActiveRow = 4, firstActiveCol = 2)
}

saveWorkbook(wb, "results/prep_tables.xlsx", overwrite = TRUE)

wb_formatted <- NULL
for (nm in names(tables)) {
  ht <- as_hux_table(tables[[nm]]) %>%
    huxtable::set_caption(paste0(nm, ". ", titles[[nm]])) %>%
    huxtable::set_caption_pos("topleft")
  
  wb_formatted <- huxtable::as_Workbook(ht, Workbook = wb_formatted, sheet = nm,
                                        write_caption = TRUE)
}
openxlsx::saveWorkbook(wb_formatted, "results/prep_tables_formatted.xlsx", overwrite = TRUE)
