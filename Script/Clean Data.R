library(dplyr)
library(rlang)
library(forcats)
library(labelled)


# Helper to safely rename a column if it exists
safe_rename <- function(df, old, new) {
  if (old %in% names(df)) {
    df <- df %>% rename(!!new := !!sym(old))
  }
  df
}

# Main function to clean AMIS raw data
clean_as_data <- function(raw_data) {
  
  df <- raw_data %>%
    rename_with(tolower) %>%
    mutate(across(where(is.character), as.factor)) %>%
    
    
    # Safe renames
    safe_rename("._nchs_rural2023", "nchs_rural") %>%
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
  
  if (nrow(df) == 0) {
    stop("Dataset is empty after filtering — check variable types and filter logic")
  }
  
  ##### START HERE #####
  
  df <- df %>%
    mutate(inj_prep_will_raw   = inj_prep_will,
           prep_switch2_raw    = prep_switch2,
           current_prep_rx_raw = current_prep_rx)
  # select variables depending on year
  #df <- df %>%
  # select(respondent_id, agecat, newrace, employment,
  #        nchs_rural, region, inscat, food_sec, 
  #        hiv_status, lasthivtest12, uas, uas_discord12, 
  #        prep_ever, prep_used, current_prep_rx, prep_intend,
  #        preprx_p12m_t,preprx_p12m_d, preprx_p12m_a, preprx_p12m_ref, preprx_p12m_dk,
  #        od_prep_current, qd_prep_aware, prep_will1, od_prep_aware,
  #        otherpreprx_a, otherpreprx_t, otherpreprx_d,
  #        prep_will2, inj_prep_aware, inj_prep_will, prep_rank_qd,
  #        prep_rank_od, prep_rank_inj, prep_qdxod, prep_qdxinj, prep_odxinj,
  #        od_prep_aware2, prep_use_od, prep_switch, inj_prep_aware2,
  #        prep_switch2, prep_switch3, qd_prep_aware2, prep_use_qd,
  #        prep_switch_od_qd, inj_prep_aware3, prep_switch_od_inj, prep_switch7,
  #        qd_prep_aware3,prep_use_qd2, prep_switch_4, otc_doprep_inj,
  #        od_prep_aware3, prep_use_od2, prep_switch5, prep_switch6,
  #        anysti_dx, sexp12m_oral, sexp12m_anal, sexp12m_vag) 
  
  
  # Filter + basic preprocessing
  df <- df %>%
    #limit to hiv-negative or unknown, sex in p12m, reported race, and 50 states + DC
    filter(hiv_status != 1,
           sexp12m_oral == 1 | sexp12m_anal == 1 | sexp12m_vag == 1,
           newrace %in% c(1,2,3,4),
           region %in% c(1,2,3,4)) %>%
    # Recodes
    #create indicator for education and change to factor
    mutate(
      respondent_id = as.double(respondent_id),
      nchs_rural = factor(nchs_rural,
                          levels = c(1,2,3,4,5,6),
                          labels = c("Large central metro",
                                     "Large fringe metro",
                                     "Medium metro",
                                     "Small metro",
                                     "Micropolitan and non-core",  #combine micropolitan and non-core (non-core too sparse)
                                     "Micropolitan and non-core")), #combine micropolitan and non-core (non-core too sparse)
      #recode don't know as no;
      prep_ever = ifelse(prep_ever == 999, 0, prep_ever),
      inj_prep_will = ifelse(inj_prep_will == 999, 0, inj_prep_will),
      inj_prep_aware = ifelse(inj_prep_aware == 999, 0, inj_prep_aware),
      prep_switch2 = ifelse(prep_switch2 == 999, 0, prep_switch2),
      other_prep = ifelse(other_prep == 999, 0, other_prep),
      food_sec = ifelse(food_sec == 999, 0, food_sec),
      current_prep_rx = ifelse(current_prep_rx == 999, 0, current_prep_rx),
      #recode prefer not to answer as missing
      prep_ever = ifelse(prep_ever == 777, NA, prep_ever),
      inj_prep_will = ifelse(inj_prep_will == 777, NA, inj_prep_will),
      inj_prep_aware = ifelse(inj_prep_aware == 777, NA, inj_prep_aware),
      current_prep_rx = ifelse(current_prep_rx == 777, NA, current_prep_rx),
      food_sec = ifelse(food_sec == 777, NA, food_sec),
      #prep use variables as factors
      prep_ever = as.factor(prep_ever),
      current_prep_rx = as.factor(current_prep_rx),
      #note that current users were not asked about willingness
      #and willingness can't be inferred from current rx
      #for oral formulations b/c we don't know if people were 
      #taking daily or OD
      #could try to infer willingness based on current OD use (od_prep_current)
      #but this is difficult becuase only tells us for sure about OD willingness
      #among those reporting OD (can't make inference about those not using it
      #because they might be willing), provides no info about daily oral willingness
      la_prep_will = case_when(
        current_prep_rx %in% c(3,4) | inj_prep_will == 1 | prep_switch2 == 1 ~ 1,
        is.na(current_prep_rx) & is.na(inj_prep_will) & is.na(prep_switch2) ~ NA_real_,
        TRUE ~ 0
      ),
      
      #create new variables from prep_ever
      #0 = No
      #1 = Yes, and I'm currently taking it
      #2 = Yes, but I stopped taking it
      #777 = I prefer not to answer
      #999 = Don't know
      prep_ever_dichotomous = case_when(prep_ever %in% c(1,2) ~ 1, is.na(prep_ever) ~ NA_real_, TRUE ~0),
      prep_current_dichotomous = case_when(prep_ever == 1 ~ 1, is.na(prep_ever) ~ NA_real_, TRUE ~ 0),
      #combine LA PrEP willingness variables among those who are not taking PrEP (inj_prep_will)
      #and those who are taking oral PrEP (prep_switch2)
      
      inj_prep_will = as.factor(inj_prep_will),
      
      #past 12 months prep use
      truvada_p12m = case_when(
        other_prep == 1 & otherpreprx_t == 1 ~ 1,
        other_prep == 1 & otherpreprx_t == 0 ~ 0,
        .default = other_prep
      ),
      descovy_p12m = case_when(
        other_prep == 1 & otherpreprx_d == 1 ~ 1,
        other_prep == 1 & otherpreprx_d == 0 ~ 0,
        .default = other_prep
      ),
      apretude_p12m = case_when(
        other_prep == 1 & otherpreprx_a == 1 ~ 1,
        other_prep == 1 & otherpreprx_a == 0 ~ 0,
        .default = other_prep
      ),
      yeztugo_p12m = case_when(
        other_prep == 1 & otherpreprx_y == 1 ~ 1,
        other_prep == 1 & otherpreprx_y == 0 ~ 0,
        .default = other_prep
      ),
      
      
      
      #recode variables then set to factors
      #age
      agecat = factor(case_when(
        agecat == 1 ~ '15-24',
        agecat == 2 ~ '25-29',
        agecat == 3 ~ '30-39',
        agecat == 4 ~ '40 or older'
      )),
      #race
      newrace = factor(case_when(
        newrace == 1 ~ 'Non-Hispanic Black',
        newrace == 2 ~ 'Hispanic',
        newrace == 3 ~ 'Non-Hispanic White',
        newrace == 4 ~ 'Other, multiple, unknown',
      )),
      #region
      region = factor(case_when(
        region == 1 ~ 'Northeast',
        region == 2 ~ 'Midwest',
        region == 3 ~ 'South',
        region == 4 ~ 'West'
      )),
      #insurance
      inscat = factor(case_when(
        inscat == 1 ~ 'None',
        inscat == 2 ~ 'Private only',
        inscat == 3 ~ 'Public only',
        inscat == 4 ~ 'Other/multiple'
      )),
      current_prep_rx = factor(case_when(
        current_prep_rx == 1 ~ "Truvada",
        current_prep_rx == 2 ~ "Descovy",
        current_prep_rx == 3 ~ "Apretude",
        current_prep_rx == 4 ~ "Yeztugo",
        current_prep_rx == 777 ~ "Prefer not to answer",
        current_prep_rx == 999 ~ "Don't know",
        prep_ever == 0 | prep_ever == 2  ~ "No PrEP", #never or former PrEP user
        TRUE ~ NA_character_
      )),
      food_sec = factor(case_when(
        food_sec == 0 ~ "No",
        food_sec == 1 ~ "Yes"
      )),
      prep_ever_dichotomous = factor(case_when(
        prep_ever_dichotomous == 0 ~ "No",
        prep_ever_dichotomous == 1 ~ "Yes"
      )),
      hrmnform_shots = factor(case_when(
        hrmnform_shots == 0 ~ "No",
        hrmnform_shots == 1 ~ "Yes"
      )),
      #if no hormone use ever, then hrmnform_shots = no
      hrmnform_shots = case_when(
        everhormones == 0 ~ "No",
        TRUE ~ hrmnform_shots
      ),
      #if no hormone use p12m, then hrmnform_shots = no
      hrmnform_shots = case_when(
        hormonesp12m == 0 ~ "No",
        TRUE ~ hrmnform_shots
      ),
      uas = factor(case_when(
        uas == 0 ~ "No",
        uas == 1 ~ "Yes"
      ))
    )
  
  #reasons for disinterest
  #check how many are missing for all
  # twist_2026_clean %>%
  #   summarise(n_all_na = sum(if_all(starts_with("inj_prep_noreas"), is.na)),
  #             n_any_response = sum(if_any(starts_with("inj_prep_noreas"), ~ !is.na(.x))),
  #             n_total = n())
  #n=665 missing for all
  #use coalesce to combine across those with a history of PrEP use and those without
  suffixes <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                "l", "m", "n", "o", "p", "q", "s")
  for (s in suffixes) {
    df[[paste0("inj_prep_noreas_", s, "_comb")]] <-
      coalesce(df[[paste0("inj_prep_noreas_", s)]],
               df[[paste0("inj_prep_noreas2_", s)]])
  }
  
  
  #create domain-level grouping
  df <- df %>%
    mutate(
      inj_prep_noreas_r_comb = inj_prep_noreas_r,
      dom_need    = if_any(c(inj_prep_noreas_a_comb, inj_prep_noreas_k_comb, inj_prep_noreas_l_comb), ~ .x == 1),
      dom_safety  = if_any(paste0("inj_prep_noreas_", c("b","c","d","e"), "_comb"), ~ .x == 1),
      dom_inject  = if_any(paste0("inj_prep_noreas_", c("f","g"), "_comb"), ~ .x == 1),
      dom_know    = inj_prep_noreas_h_comb == 1,
      dom_access  = if_any(paste0("inj_prep_noreas_", c("i","j"), "_comb"), ~ .x == 1),
      dom_stigma  = if_any(paste0("inj_prep_noreas_", c("m","n","o","p","q"), "_comb"), ~ .x == 1)
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
  
  #set character vars as factor
  df <- df %>%
    mutate(across(where(is.character), as.factor))
  
  return(df)
}


