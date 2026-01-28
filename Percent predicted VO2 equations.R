# ============================================================
# Predicted VO2 (ml/min) and Percent Predicted Calculator
#
# Equations (all return ml/min unless noted):
#   1) Cooper 1984 (weight-based, children)  -> ml/min
#   2) Cooper 1984 (height-based, children)  -> ml/min
#   3) Hansen/Wasserman 1984 (clinical ET)   -> ml/min
#   4) FRIEND 2017 (Myers; ml/kg/min -> ml/min)
#   5) FRIEND 2018 (De Souza; ml/kg/min -> ml/min; treadmill/cycle)
#
# Units expected:
#   sex               = Male/Female (or M/F; normalized)
#   age               = years
#   height            = cm
#   weight            = kg
#   test_mode         = Treadmill/Cycle (FRIEND 2018 only; default Treadmill)
#   Peak_VO2_ml_min   = measured peak VO2 (ml/min)
#
# Notes:
#   - Prefer Source() rather than Run in interactive R sessions.
#   - Helper functions + equations are defined first.
#   - Bulk DF calculator provided for reuse.
#   - Interactive prompt runs only when interactive().
# ============================================================


# ============================================================
# Helpers
# ============================================================

normalize_sex <- function(sex) {
  s <- tolower(trimws(as.character(sex)))
  if (s %in% c("m", "male")) return("Male")
  if (s %in% c("f", "female")) return("Female")
  stop("sex must be Male/Female (or M/F).")
}

normalize_test_mode <- function(test_mode) {
  t <- tolower(trimws(as.character(test_mode)))
  if (is.na(t) || t == "" || t %in% c("t", "tm", "treadmill")) return("Treadmill")
  if (t %in% c("c", "cy", "cycle", "bike", "erg", "ergometer")) return("Cycle")
  stop("test_mode must be Treadmill or Cycle (or T/C).")
}

read_num <- function(prompt) {
  x <- suppressWarnings(as.numeric(readline(prompt)))
  if (is.na(x)) stop(paste0("Expected a numeric value for: ", prompt))
  x
}

assert_required_cols <- function(df, required) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("DF is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}


# ============================================================
# Equations (all return ml/min)
# ============================================================

# ------------------------------------------------------------
# Cooper — WEIGHT-based VO2max (children)
# J Appl Physiol 1984
# ------------------------------------------------------------
pred_cooper_weight <- function(sex, weight) {
  sex <- normalize_sex(sex)
  if (sex == "Male") {
    52.8 * weight - 303.4
  } else {
    28.5 * weight + 288.2
  }
}

# ------------------------------------------------------------
# Cooper — HEIGHT-based VO2max (children)
# Am Rev Respir Dis 1984
# ------------------------------------------------------------
pred_cooper_height <- function(sex, height) {
  sex <- normalize_sex(sex)
  if (sex == "Male") {
    43.6 * height - 4547.1
  } else {
    22.5 * height - 1837.8
  }
}

# ------------------------------------------------------------
# Hansen/Wasserman — Predicted values for clinical exercise testing
# Am Rev Respir Dis 1984;129(2 Pt 2):S49–S55
# Implemented in the ATS/ACCP (2003) recommended simplified form:
#   Peak VO2 (ml/min) = weight_used × (50.75 − 0.37 × age)
# where weight_used = min(actual, ideal) if actual<than ideal.
# ------------------------------------------------------------
pred_hansen1984 <- function(sex, age, weight, height) {
  sex <- normalize_sex(sex)
  
  pred_wt <- if (sex == "Male") (0.79 * height - 60.7) else (0.65 * height - 42.8)
  wt_used <- if (weight >= pred_wt) pred_wt else weight
  
  if (sex == "Male") {
    wt_used * (50.75 - 0.37 * age)
  } else {
    (wt_used + 43) * (22.78 - 0.17 * age)
  }
}

# ------------------------------------------------------------
# FRIEND 2017 (Myers) — outputs ml/kg/min in paper; convert to ml/min here
# Screenshot formula:
#   VO2max (ml/kg/min) = 79.9 - 0.39*age - 13.7*gender(0=male,1=female) - 0.127*weight[lbs]
# Converted to ml/min by multiplying by weight (kg).
# ------------------------------------------------------------

pred_friend2017_myers <- function(sex, age, weight) {
  sex <- normalize_sex(sex)
  
  weight_lbs <- weight * 2.20462
  gender01 <- if (sex == "Male") 0 else 1
  
  vo2_ml_kg_min <- 79.9 -
    0.39 * age -
    13.7 * gender01 -
    0.127 * weight_lbs
  
  vo2_ml_kg_min * weight
}

# -------------------------------------------------------------------
# friend2018_desouza — Adult reference equation
# De Souza et al
# Eur J Prev Cardiol 2018
# Originally ml/kg/min → converted here to ml/min
# -------------------------------------------------------------------

pred_friend2018_desouza <- function(sex, age, weight, height, test_mode = "Treadmill") {
  sex <- normalize_sex(sex)
  test_mode <- normalize_test_mode(test_mode)
  
  weight_lbs <- weight * 2.20462
  height_in  <- height / 2.54
  
  sex_term  <- if (sex == "Male") 1 else 2
  mode_term <- if (test_mode == "Treadmill") 1 else 2
  
  vo2_ml_kg_min <-
    45.2 -
    0.35 * age -
    10.9 * sex_term -
    0.15 * weight_lbs +
    0.68 * height_in -
    0.46 * mode_term
  
  vo2_ml_kg_min * weight
}

# ============================================================
# Generic wrapper: predicted + percent predicted
# ============================================================

calc_pred_and_percent <- function(equation,
                                  sex, age, weight, height,
                                  peak_vo2_ml_min,
                                  test_mode = "Treadmill") {
  equation <- tolower(trimws(equation))
  
  pred <- switch(
    equation,
    "cooper_weight"        = pred_cooper_weight(sex, weight),
    "cooper_height"        = pred_cooper_height(sex, height),
    "hansen1984"           = pred_hansen1984(sex, age, weight, height),
    "friend2017_myers"     = pred_friend2017_myers(sex, age, weight),
    "friend2018_desouza"   = pred_friend2018_desouza(sex, age, weight, height, test_mode)

    stop("Unknown equation. Use: cooper_weight, cooper_height, hansen1984, friend2017_myers, friend2018_desouza")
  )
  
  if (!is.finite(pred) || pred <= 0) {
    stop("Predicted VO2 is non-positive/invalid (check inputs/range).")
  }
  
  perc <- (peak_vo2_ml_min / pred) * 100
  list(pred_ml_min = pred, percent_pred = perc)
}


# ============================================================
# Bulk DF calculator
#   - Adds predicted columns (all ml/min)
#   - Adds combined pred_VO2 (Hansen if age < 18 else FRIEND2018)
#   - Adds perc_pred_VO2 if measured column present
# ============================================================

add_pred_vo2_columns <- function(DF,
                                 sex_col = "sex",
                                 age_col = "Age",
                                 height_col = "height",
                                 weight_col = "weight",
                                 test_mode_col = "test_mode",
                                 peak_vo2_col = "Peak_VO2_ml_min") {
  DF <- as.data.frame(DF)
  
  required <- c(sex_col, age_col, height_col, weight_col)
  assert_required_cols(DF, required)
  
  sex_norm <- vapply(DF[[sex_col]], normalize_sex, character(1))
  
  test_mode_vec <- if (test_mode_col %in% names(DF)) DF[[test_mode_col]] else "Treadmill"
  test_mode_norm <- vapply(test_mode_vec, normalize_test_mode, character(1))
  
  age    <- DF[[age_col]]
  height <- DF[[height_col]]
  weight <- DF[[weight_col]]
  
  # unit conversions (stored for transparency; FRIEND equations use these internally)
  DF$weight_lbs <- weight * 2.20462
  DF$height_in  <- height / 2.54
  
  # predicted columns (ml/min)
  DF$pred_VO2_cooper_weight      <- mapply(pred_cooper_weight, sex_norm, weight)
  DF$pred_VO2_cooper_height      <- mapply(pred_cooper_height, sex_norm, height)
  DF$pred_VO2_hansen1984         <- mapply(pred_hansen1984, sex_norm, age, weight, height)
  DF$pred_VO2_friend2017_myers   <- mapply(pred_friend2017_myers, sex_norm, age, weight)
  DF$pred_VO2_friend2018_desouza <- mapply(pred_friend2018_desouza, sex_norm, age, weight, height, test_mode_norm)

  # combined logic: <18 -> Hansen; >=18 -> FRIEND2018 (De Souza)
  DF$pred_VO2 <- ifelse(age < 18, DF$pred_VO2_hansen1984, DF$pred_VO2_friend2018_desouza)
  
  # percent predicted if measured column exists
  if (peak_vo2_col %in% names(DF)) {
    DF$perc_pred_VO2 <- (DF[[peak_vo2_col]] / DF$pred_VO2) * 100
  }
  
  DF
}

# ============================================================
# Interactive calculator (runs only when interactive())
# ============================================================

if (interactive()) {
  
  cat("\nPredicted VO2 (ml/min) + Percent Predicted Calculator\n")
  cat("----------------------------------------------------\n")
  
  sex    <- normalize_sex(readline("Sex (Male/Female or M/F): "))
  age    <- read_num("Age (years): ")
  height <- read_num("Height (cm): ")
  weight <- read_num("Weight (kg): ")
  
  cat("\nChoose equation:\n")
  cat("  1 = Cooper 1984 (weight-based, children) [ml/min]\n")
  cat("  2 = Cooper 1984 (height-based, children) [ml/min]\n")
  cat("  3 = Hansen 1984 (clinical exercise testing) [ml/min]\n")
  cat("  4 = FRIEND 2017 (Myers; age/sex/weight only; ml/kg/min -> ml/min)\n")
  cat("  5 = FRIEND 2018 (adult; ml/kg/min -> ml/min) [requires test mode]\n")

  choice <- trimws(readline("Enter 1/2/3/4/5: "))
  
  equation <- switch(
    choice,
    "1" = "cooper_weight",
    "2" = "cooper_height",
    "3" = "hansen1984",
    "4" = "friend2017_myers",
    "5" = "friend2018_desouza",
    stop("Choice must be 1, 2, 3, 4, or 5.")
  )
  
  test_mode <- "Treadmill"
  if (equation == "friend2018_desouza") {
    test_mode <- normalize_test_mode(readline("Test mode (Treadmill/Cycle) [default Treadmill]: "))
  }
  
  peak_vo2 <- read_num("Measured Peak VO2 (ml/min): ")
  
  res <- calc_pred_and_percent(
    equation = equation,
    sex = sex, age = age, weight = weight, height = height,
    peak_vo2_ml_min = peak_vo2,
    test_mode = test_mode
  )
  
  cat("\nRESULTS\n")
  cat("-------\n")
  cat(sprintf("Equation: %s\n", equation))
  cat(sprintf("Predicted VO2: %.1f ml/min\n", res$pred_ml_min))
  cat(sprintf("Measured VO2:  %.1f ml/min\n", peak_vo2))
  cat(sprintf("Percent predicted: %.1f %%\n\n", res$percent_pred))
}