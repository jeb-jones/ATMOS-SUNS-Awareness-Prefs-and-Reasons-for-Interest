library(dplyr)
library(rlang)
library(forcats)
library(labelled)

source('scripts/import_codebook.R')

# Helper to safely rename a column if it exists
safe_rename <- function(df, old, new) {
  if (old %in% names(df)) {
    df <- df %>% rename(!!new := !!sym(old))
  }
  df
}

# Codebook-derived variable lists (lower case, leading _ dropped, to match the cleaned names)
vars_dk_pna <- codebook_values %>%
  filter(value %in% c(777, 999)) %>%
  pull(variable) %>% unique() %>% tolower() %>% sub("^_", "", .)

keep_numeric <- codebook_vars %>%
  filter(format %in% c("CHKNCHK", "YESNO", "PHQSCALE", "MOSTAGREE")) %>%
  pull(variable) %>% tolower() %>% sub("^_", "", .)

prep_vars <- c(
  "prep_ever", "current_prep_rx", "od_prep_current",
  "qd_prep_aware", "qd_prep_aware2", "qd_prep_aware3",
  "od_prep_aware", "od_prep_aware2", "od_prep_aware3",
  "inj_prep_aware", "inj_prep_aware2", "inj_prep_aware3",
  "prep_will1", "prep_will2", "inj_prep_will",
  "prep_switch", "prep_switch2",
  "prep_switch_od_qd", "prep_switch_od_inj",
  "prep_switch4", "prep_switch5",
  "prep_rank_qd", "prep_rank_od", "prep_rank_inj",
  "prep_qdxod", "prep_qdxinj", "prep_odxinj"
)

# Main function to clean raw data
clean_as_data <- function(raw_data) {
  
  # 1. Labels and names ---------------------------------------------------------
  df <- raw_data %>%
    apply_codebook() %>%
    rename_with(tolower) %>%
    safe_rename("._nchs_rural2023", "nchs_rural2023") %>%
    safe_rename("._inscat", "inscat") %>%
    safe_rename("._agecat", "agecat") %>%
    safe_rename("._age5yr", "age5yr") %>%
    safe_rename("._hivstat", "hivstat") %>%
    safe_rename("._lasthivtest12", "lasthivtest12") %>%
    safe_rename("._raceomb", "raceomb") %>%
    safe_rename("._insprivate", "insprivate") %>%
    safe_rename("._inspublic", "inspublic") %>%
    safe_rename("._censdiv", "censdiv") %>%
    safe_rename("._m_m_no", "m_m_no") %>%
    safe_rename("._insaca", "insaca")
  
  # 2. Recodes and derived variables (while everything is still numeric) --------
  df <- df %>%
    # recode all 777 and 999 as NA
    mutate(across(any_of(vars_dk_pna), ~ replace(.x, .x %in% c(777, 999), NA))) %>%
    
    mutate(inj_prep_will_raw   = inj_prep_will,
           prep_switch2_raw    = prep_switch2,
           current_prep_rx_raw = current_prep_rx) %>%
    
    # limit to hiv-negative or unknown, sex in p12m, reported race, and 50 states + DC
    filter(hiv_status != 1,
           sexp12m_oral == 1 | sexp12m_anal == 1 | sexp12m_vag == 1,
           newrace %in% c(1, 2, 3, 4),
           region %in% c(1, 2, 3, 4)) %>%
    
    mutate(
      respondent_id = as.double(respondent_id),
      # combine micropolitan and non-core (non-core too sparse)
      nchs_rural = factor(as.numeric(nchs_rural2023),
                          levels = 1:6,
                          labels = c("Large central metro",
                                     "Large fringe metro",
                                     "Medium metro",
                                     "Small metro",
                                     "Micropolitan and non-core",
                                     "Micropolitan and non-core")),
      
      across(all_of(prep_vars), ~ as.numeric(haven::zap_labels(.x))),
      
      prep_group = case_when(
        prep_ever == 1 & current_prep_rx %in% 3:4 ~ "Injectable",
        prep_ever == 1 & od_prep_current == 1     ~ "On-demand",
        prep_ever == 1 & current_prep_rx %in% 1:2 ~ "Daily oral",
        prep_ever == 1                            ~ "Current, type unknown",
        TRUE                                      ~ "Not current"
      ),
      
      # Awareness: current users are aware of the modality they take
      aware_qd  = if_else(prep_group == "Daily oral", 1,
                          coalesce(qd_prep_aware, qd_prep_aware2, qd_prep_aware3)),
      aware_od  = if_else(prep_group == "On-demand", 1,
                          coalesce(od_prep_aware, od_prep_aware2, od_prep_aware3)),
      aware_inj = if_else(prep_group == "Injectable", 1,
                          coalesce(inj_prep_aware, inj_prep_aware2, inj_prep_aware3)),
      
      # Willingness: current users are willing to use the modality they take;
      # 1/2-coded switch items are shifted to 0/1
      will_qd  = if_else(prep_group == "Daily oral", 1,
                         coalesce(prep_will1, prep_switch_od_qd - 1, prep_switch4 - 1)),
      will_od  = if_else(prep_group == "On-demand", 1,
                         coalesce(prep_will2, prep_switch, prep_switch5 - 1)),
      will_inj = if_else(prep_group == "Injectable", 1,
                         coalesce(inj_prep_will, prep_switch2, prep_switch_od_inj - 1)),
      
      n_aware   = aware_qd + aware_od + aware_inj,
      n_willing = will_qd + will_od + will_inj,
      
      # Stated choice among participants not currently on PrEP who were willing
      # to use 2+ modalities (ranked 1st of three, or chosen from a pair)
      first_choice = case_when(
        prep_rank_qd  == 1 ~ "Daily oral",
        prep_rank_od  == 1 ~ "On-demand oral",
        prep_rank_inj == 1 ~ "Injectable",
        prep_qdxod == 1 | prep_qdxinj == 1 ~ "Daily oral",
        prep_qdxod == 2 | prep_odxinj == 2 ~ "On-demand oral",
        prep_qdxinj == 2 | prep_odxinj == 3 ~ "Injectable"
      ),
      
      # Preferred modality (participants not currently on PrEP):
      # willing to use one -> that one; willing to use 2+ -> stated choice
      preferred = factor(case_when(
        prep_group != "Not current"    ~ NA_character_,
        n_willing == 0                 ~ "Not willing to use any modality",
        n_willing == 1 & will_qd  == 1 ~ "Daily oral",
        n_willing == 1 & will_od  == 1 ~ "On-demand oral",
        n_willing == 1 & will_inj == 1 ~ "Injectable",
        n_willing >= 2                 ~ first_choice
      ), levels = c("Daily oral", "On-demand oral", "Injectable",
                    "Not willing to use any modality")),
      
      pref_basis = factor(case_when(
        prep_group != "Not current"           ~ NA_character_,
        n_willing == 1                        ~ "Willing to use only one",
        n_willing >= 2 & !is.na(prep_rank_qd) ~ "Ranked all three",
        n_willing >= 2 & (!is.na(prep_qdxod) | !is.na(prep_qdxinj) |
                            !is.na(prep_odxinj)) ~ "Chose between two"
      ), levels = c("Willing to use only one", "Chose between two", "Ranked all three")),
      
      first_choice = factor(first_choice,
                            levels = c("Daily oral", "On-demand oral", "Injectable"))
    ) %>%
    set_variable_labels(
      aware_qd     = "Daily oral PrEP",
      aware_od     = "On-demand oral PrEP",
      aware_inj    = "Injectable PrEP",
      n_aware      = "Number of modalities aware of",
      will_qd      = "Daily oral PrEP",
      will_od      = "On-demand oral PrEP",
      will_inj     = "Injectable PrEP",
      n_willing    = "Number of modalities willing to use",
      first_choice = "Stated choice among modalities willing to use",
      preferred    = "Preferred PrEP modality",
      pref_basis   = "Basis for preference",
      nchs_rural   = "NCHS urban-rural classification"
    )
  
  # 3. Reasons for disinterest in injectable PrEP -------------------------------
  # combine across current daily oral users (noreas2) and non-users (noreas)
  suffixes <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                "l", "m", "n", "o", "p", "q", "s")
  
  for (s in suffixes) {
    df[[paste0("inj_prep_noreas_", s, "_comb")]] <-
      coalesce(df[[paste0("inj_prep_noreas_", s)]],
               df[[paste0("inj_prep_noreas2_", s)]])
  }
  
  # create domain-level grouping
  df <- df %>%
    mutate(
      inj_prep_noreas_r_comb = inj_prep_noreas_r,
      dom_need    = if_any(c(inj_prep_noreas_a_comb, inj_prep_noreas_k_comb, inj_prep_noreas_l_comb), ~ .x == 1),
      dom_safety  = if_any(all_of(paste0("inj_prep_noreas_", c("b","c","d","e"), "_comb")), ~ .x == 1),
      dom_inject  = if_any(all_of(paste0("inj_prep_noreas_", c("f","g"), "_comb")), ~ .x == 1),
      dom_know    = inj_prep_noreas_h_comb == 1,
      dom_access  = if_any(all_of(paste0("inj_prep_noreas_", c("i","j"), "_comb")), ~ .x == 1),
      dom_stigma  = if_any(all_of(paste0("inj_prep_noreas_", c("m","n","o","p","q"), "_comb")), ~ .x == 1)
    ) %>%
    set_variable_labels(
      dom_need   = "Low perceived need or prefer other prevention methods",
      dom_safety = "Medication safety and side-effect concerns",
      dom_inject = "Injection pain, discomfort, or needle fear",
      dom_know   = "Insufficient knowledge about injectable PrEP",
      dom_access = "Access and logistical barriers",
      dom_stigma = "Anticipated stigma and privacy concerns",
      inj_prep_noreas_a_comb = "I prefer to use other methods to protect myself from HIV.",
      inj_prep_noreas_b_comb = "I’m worried about side effects of the injections.",
      inj_prep_noreas_c_comb = "I’m worried about long-term effects of injectable PrEP.",
      inj_prep_noreas_d_comb = "I’m worried injectable PrEP might interfere with other medications I take.",
      inj_prep_noreas_e_comb = "I prefer not to have medication in my body for an extended period.",
      inj_prep_noreas_f_comb = "The injections may be painful or uncomfortable.",
      inj_prep_noreas_g_comb = "I’m afraid of needles.",
      inj_prep_noreas_h_comb = "I don’t know enough about injectable PrEP to make a decision.",
      inj_prep_noreas_i_comb = "I don’t have reliable transportation to get to my local healthcare provider.",
      inj_prep_noreas_j_comb = "It may be difficult to keep the injection schedule.",
      inj_prep_noreas_k_comb = "I’m in a monogamous relationship with an HIV-negative partner.",
      inj_prep_noreas_l_comb = "I’m not sexually active.",
      inj_prep_noreas_m_comb = "I’m worried people will think that I have HIV when going to the clinic for the injection.",
      inj_prep_noreas_n_comb = "I’m worried people will think I am very sexually active because I’m getting injectable PrEP.",
      inj_prep_noreas_o_comb = "I’m worried about being judged by clinic staff.",
      inj_prep_noreas_p_comb = "I’m worried about my privacy with someone who I live with.",
      inj_prep_noreas_q_comb = "I’m worried about my privacy on my parent’s health insurance plan.",
      inj_prep_noreas_r_comb = "I don’t trust the pharmaceutical industry.",
      inj_prep_noreas_s_comb = "Another reason."
    )
  
  # 4. Convert to factors (last, so the numeric recodes above still work) -------
  # labelled variables -> factors, except binary/scale items kept numeric;
  # then drop empty levels (e.g., prefer not to answer / don't know), keeping
  # variable labels (plain droplevels() strips them)
  df <- df %>%
    mutate(across(where(haven::is.labelled) & !any_of(keep_numeric) &
                    !ends_with("_comb"),          # combined 0/1 reason items stay numeric
                  haven::as_factor),
           across(where(is.character), as.factor),
           across(where(is.factor),
                  ~ structure(droplevels(.x), label = attr(.x, "label"))))
  
  return(df)
}