# ============================================================
# Predicted VO2 (ml/min) and Percent Predicted Calculator
#
# Equations (all return ml/min unless noted):
#   1) Cooper 1984 (weight-based, children)  -> ml/min
#   2) Cooper 1984 (height-based, children)  -> ml/min
#   3) Hansen/Wasserman 2001 (textbook clinical ET) -> ml/min
#   4) Wasserman 1999 simplified (Pistea 2016 table; cycle) -> ml/min
#   5) FRIEND 2017 (Myers; ml/kg/min -> ml/min)
#   6) FRIEND 2018 (De Souza; ml/kg/min -> ml/min; treadmill/cycle)
#   7) Blackie 1989 (adults >55 years; cycle ergometry) -> ml/min
#   8) Jones 1985 (cycle ergometry) -> ml/min
#   9) Koch 2009 / SHIP (age >65 implementation from Pistea table) -> ml/min
#  10) Hakola 2011 (ages 57-78; cycle ergometry) -> ml/min
#
# Units expected:
#   sex               = Male/Female (or M/F; normalized)
#   age               = years
#   height            = cm
#   weight            = kg
#   test_mode         = Treadmill/Cycle
#   Peak_VO2_ml_min   = measured peak VO2 (ml/min)
#
# Automatic equation-selection logic:
#   age < 18                         -> Cooper 1984 height-based
#   age >= 18                        -> FRIEND 2018 De Souza
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
# Cooper - WEIGHT-based VO2max (children)
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
# Cooper - HEIGHT-based VO2max (children)
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
# Hansen/Wasserman 2001 - clinical exercise testing
# Output: ml/min (converted from L/min)
# Age floor: 30 y
# ------------------------------------------------------------
pred_hansen2001 <- function(sex, age, weight, height) {
  sex <- normalize_sex(sex)

  age_used <- max(age, 30)

  if (sex == "Male") {
    ideal_wt <- 0.79 * height - 60.7
    wt_diff  <- weight - ideal_wt

    base <- 0.0337 * height - 0.000165 * age_used * height - 1.963

    vo2_L <- if (weight >= ideal_wt) {
      base + 0.006 * wt_diff
    } else {
      base + 0.014 * wt_diff
    }

  } else {
    ideal_wt <- 0.65 * height - 42.8
    wt_diff  <- weight - ideal_wt

    vo2_L <- 0.001 * height * (14.783 - 0.11 * age_used) +
      0.006 * wt_diff
  }

  vo2_L * 1000
}

# ------------------------------------------------------------
# Wasserman 1999 simplified equations
# Reproduced in Pistea et al. Table 1
# Cycle ergometry
#
# Men:
#   VO2max = (50.72 - 0.372*age) * weight
#
# Women:
#   VO2max = (22.78 - 0.17*age) * (weight + 43)
#
# Inputs:
#   age    = years
#   weight = kg
#
# Output: ml/min
#
# This is retained as a separate equation and does not replace the
# Hansen/Wasserman textbook implementation above.
# ------------------------------------------------------------
pred_wasserman1999_simplified <- function(sex, age, weight) {
  sex <- normalize_sex(sex)

  if (sex == "Male") {
    (50.72 - 0.372 * age) * weight
  } else {
    (22.78 - 0.17 * age) * (weight + 43)
  }
}

# ------------------------------------------------------------
# FRIEND 2017 (Myers)
# Original output: ml/kg/min; converted to ml/min
# ------------------------------------------------------------
pred_friend2017_myers <- function(sex, age, weight) {
  sex <- normalize_sex(sex)

  weight_lbs <- weight * 2.20462
  gender01   <- if (sex == "Male") 0 else 1

  vo2_ml_kg_min <- 79.9 -
    0.39 * age -
    13.7 * gender01 -
    0.127 * weight_lbs

  vo2_ml_kg_min * weight
}

# ------------------------------------------------------------
# FRIEND 2018 (De Souza)
# Original output: ml/kg/min; converted to ml/min
# ------------------------------------------------------------
pred_friend2018_desouza <- function(
  sex,
  age,
  weight,
  height,
  test_mode = "Treadmill"
) {
  sex       <- normalize_sex(sex)
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

# ------------------------------------------------------------
# Blackie 1989 - adults >55 years, cycle ergometry
#
# Inputs:
#   age    = years
#   height = cm
#   weight = kg
#
# Original output: L/min
# Function output: ml/min
#
# Men:
#   VO2max = 0.0142*height - 0.0494*age +
#            0.00257*weight + 3.015
#
# Women:
#   VO2max = 0.0142*height - 0.0115*age +
#            0.00974*weight - 0.651
# ------------------------------------------------------------
pred_blackie1989 <- function(sex, age, weight, height) {
  sex <- normalize_sex(sex)

  if (!is.finite(age) || age <= 55) {
    stop("Blackie 1989 is intended for adults older than 55 years.")
  }

  if (sex == "Male") {
    vo2_L_min <-
      0.0142 * height -
      0.0494 * age +
      0.00257 * weight +
      3.015
  } else {
    vo2_L_min <-
      0.0142 * height -
      0.0115 * age +
      0.00974 * weight -
      0.651
  }

  vo2_L_min * 1000
}


# ------------------------------------------------------------
# Jones 1985 - cycle ergometry
# Pistea et al. Table 1
#
# VO2max (L/min) = 0.046*height - 0.021*age - 0.62*sex - 4.31
# Sex coding: male = 0, female = 1
# Function output: ml/min
# ------------------------------------------------------------
pred_jones1985 <- function(sex, age, height) {
  sex <- normalize_sex(sex)
  sex01 <- if (sex == "Male") 0 else 1

  vo2_L_min <-
    0.046 * height -
    0.021 * age -
    0.62 * sex01 -
    4.31

  vo2_L_min * 1000
}

# ------------------------------------------------------------
# Koch 2009 - SHIP study, cycle ergometry
# Pistea et al. Table 1 implementation for adults >65 years
#
# The Pistea table codes:
#   age >65 years = 5
#   sex: male = 1, female = 2
#   BMI <=25 = 0; BMI >25 = 1
#
# The equation returns ml/kg/min; converted here to ml/min.
# This implementation is intentionally restricted to age >65 because
# Pistea only provides the age-category coding needed for that group.
# ------------------------------------------------------------
pred_koch2009_pistea <- function(sex, age, weight, height) {
  sex <- normalize_sex(sex)

  if (!is.finite(age) || age <= 65) {
    stop(
      paste(
        "The Pistea-table Koch 2009 implementation is restricted",
        "to adults older than 65 years because the table only provides",
        "the age-category code for that group."
      )
    )
  }

  bmi <- weight / ((height / 100)^2)
  age_code <- 5
  sex_code <- if (sex == "Male") 1 else 2
  bmi_code <- if (bmi <= 25) 0 else 1

  vo2_ml_kg_min <-
    47.7565 -
    0.988 * age_code -
    0.2356 * age_code^2 -
    8.8697 * sex_code +
    2.3597 * bmi_code -
    2.0308 * age_code * bmi_code -
    3.7405 * sex_code * bmi_code +
    0.2512 * age_code * sex_code +
    1.3797 * age_code * sex_code * bmi_code

  vo2_ml_kg_min * weight
}

# ------------------------------------------------------------
# Hakola 2011 - adults aged 57-78 years, cycle ergometry
# Pistea et al. Table 1
#
# Men:   VO2max = 4.846 - 0.039*age (L/min)
# Women: VO2max = 3.475 - 0.031*age (L/min)
# Function output: ml/min
# ------------------------------------------------------------
pred_hakola2011 <- function(sex, age) {
  sex <- normalize_sex(sex)

  if (!is.finite(age) || age < 57 || age > 78) {
    warning(
      "Hakola 2011 was derived in adults aged 57-78 years; prediction is extrapolated."
    )
  }

  vo2_L_min <- if (sex == "Male") {
    4.846 - 0.039 * age
  } else {
    3.475 - 0.031 * age
  }

  vo2_L_min * 1000
}


# ============================================================
# Generic wrapper: predicted + percent predicted
# ============================================================

calc_pred_and_percent <- function(
  equation,
  sex,
  age,
  weight,
  height,
  peak_vo2_ml_min,
  test_mode = "Treadmill"
) {
  equation <- tolower(trimws(equation))

  pred <- switch(
    equation,
    "cooper_weight"      = pred_cooper_weight(sex, weight),
    "cooper_height"      = pred_cooper_height(sex, height),
    "hansen2001"         = pred_hansen2001(sex, age, weight, height),
    "wasserman1999_simplified" = pred_wasserman1999_simplified(
      sex, age, weight
    ),
    "friend2017_myers"   = pred_friend2017_myers(sex, age, weight),
    "friend2018_desouza" = pred_friend2018_desouza(
      sex, age, weight, height, test_mode
    ),
    "blackie1989"        = pred_blackie1989(
      sex, age, weight, height
    ),
    "jones1985"          = pred_jones1985(
      sex, age, height
    ),
    "koch2009_pistea"    = pred_koch2009_pistea(
      sex, age, weight, height
    ),
    "hakola2011"         = pred_hakola2011(
      sex, age
    ),
    stop(
      paste(
        "Unknown equation. Use:",
        "cooper_weight, cooper_height, hansen2001,",
        "wasserman1999_simplified, friend2017_myers,",
        "friend2018_desouza, blackie1989,",
        "jones1985, koch2009_pistea, hakola2011"
      )
    )
  )

  if (!is.finite(pred) || pred <= 0) {
    stop("Predicted VO2 is non-positive/invalid (check inputs/range).")
  }

  perc <- (peak_vo2_ml_min / pred) * 100

  list(
    pred_ml_min = pred,
    percent_pred = perc
  )
}


# ============================================================
# Bulk DF calculator
#
# Adds:
#   - all individual predicted VO2 columns
#   - selected pred_VO2
#   - pred_VO2_equation
#   - perc_pred_VO2 when measured VO2 is available
#
# Selection:
#   age < 18                      -> Cooper 1984 height-based
#   age >= 18                     -> FRIEND 2018
# ============================================================

add_pred_vo2_columns <- function(
  DF,
  sex_col       = "sex",
  age_col       = "Age",
  height_col    = "height",
  weight_col    = "weight",
  test_mode_col = "test_mode",
  peak_vo2_col  = "Peak_VO2_ml_min"
) {
  DF <- as.data.frame(DF)

  required <- c(sex_col, age_col, height_col, weight_col)
  assert_required_cols(DF, required)

  sex_norm <- vapply(
    DF[[sex_col]],
    normalize_sex,
    character(1)
  )

  test_mode_vec <- if (test_mode_col %in% names(DF)) {
    DF[[test_mode_col]]
  } else {
    rep("Treadmill", nrow(DF))
  }

  test_mode_norm <- vapply(
    test_mode_vec,
    normalize_test_mode,
    character(1)
  )

  age    <- as.numeric(DF[[age_col]])
  height <- as.numeric(DF[[height_col]])
  weight <- as.numeric(DF[[weight_col]])

  DF$weight_lbs <- weight * 2.20462
  DF$height_in  <- height / 2.54

  DF$pred_VO2_cooper_weight <- mapply(
    pred_cooper_weight,
    sex_norm,
    weight
  )

  DF$pred_VO2_cooper_height <- mapply(
    pred_cooper_height,
    sex_norm,
    height
  )

  DF$pred_VO2_hansen2001 <- mapply(
    pred_hansen2001,
    sex_norm,
    age,
    weight,
    height
  )

  DF$pred_VO2_wasserman1999_simplified <- mapply(
    pred_wasserman1999_simplified,
    sex_norm,
    age,
    weight
  )

  DF$pred_VO2_friend2017_myers <- mapply(
    pred_friend2017_myers,
    sex_norm,
    age,
    weight
  )

  DF$pred_VO2_friend2018_desouza <- mapply(
    pred_friend2018_desouza,
    sex_norm,
    age,
    weight,
    height,
    test_mode_norm
  )

  DF$pred_VO2_jones1985 <- mapply(
    pred_jones1985,
    sex_norm,
    age,
    height
  )

  # Koch/Pistea can only be calculated with the age >65 category
  # explicitly supplied in the Pistea table.
  DF$pred_VO2_koch2009_pistea <- mapply(
    function(sex_i, age_i, weight_i, height_i) {
      if (
        any(is.na(c(age_i, weight_i, height_i))) ||
        !is.finite(age_i) ||
        age_i <= 65
      ) {
        return(NA_real_)
      }

      pred_koch2009_pistea(
        sex = sex_i,
        age = age_i,
        weight = weight_i,
        height = height_i
      )
    },
    sex_norm,
    age,
    weight,
    height
  )

  DF$pred_VO2_hakola2011 <- mapply(
    function(sex_i, age_i) {
      if (is.na(age_i) || !is.finite(age_i)) {
        return(NA_real_)
      }
      suppressWarnings(pred_hakola2011(sex_i, age_i))
    },
    sex_norm,
    age
  )

  # Calculate Blackie only when age >55.
  # Cycle mode is enforced when selecting the combined equation.
  DF$pred_VO2_blackie1989 <- mapply(
    function(sex_i, age_i, weight_i, height_i) {
      if (
        any(is.na(c(age_i, weight_i, height_i))) ||
        !is.finite(age_i) ||
        age_i <= 55
      ) {
        return(NA_real_)
      }

      pred_blackie1989(
        sex = sex_i,
        age = age_i,
        weight = weight_i,
        height = height_i
      )
    },
    sex_norm,
    age,
    weight,
    height
  )

  # Automatic equation-selection logic:
  # age <18  -> Cooper 1984 height-based
  # age >=18 -> FRIEND 2018 De Souza
  DF$pred_VO2 <- ifelse(
    age < 18,
    DF$pred_VO2_cooper_height,
    DF$pred_VO2_friend2018_desouza
  )

  DF$pred_VO2_equation <- ifelse(
    age < 18,
    "Cooper 1984 height-based",
    "FRIEND 2018 De Souza"
  )

  if (peak_vo2_col %in% names(DF)) {
    DF$perc_pred_VO2 <- (
      as.numeric(DF[[peak_vo2_col]]) / DF$pred_VO2
    ) * 100
  }

  DF
}


# ============================================================
# All-equation single-patient calculator
# ============================================================

safe_prediction <- function(expr) {
  tryCatch(
    suppressWarnings(as.numeric(expr)),
    error = function(e) NA_real_
  )
}

calculate_all_vo2_predictions <- function(
  sex,
  age,
  height,
  weight,
  peak_vo2_ml_min,
  test_mode = "Treadmill"
) {
  sex <- normalize_sex(sex)
  test_mode <- normalize_test_mode(test_mode)

  predictions <- c(
    "Cooper 1984 weight-based" = safe_prediction(
      pred_cooper_weight(sex, weight)
    ),
    "Cooper 1984 height-based" = safe_prediction(
      pred_cooper_height(sex, height)
    ),
    "Hansen/Wasserman 2001" = safe_prediction(
      pred_hansen2001(sex, age, weight, height)
    ),
    "Wasserman 1999 simplified" = safe_prediction(
      pred_wasserman1999_simplified(sex, age, weight)
    ),
    "FRIEND 2017 Myers" = safe_prediction(
      pred_friend2017_myers(sex, age, weight)
    ),
    "FRIEND 2018 De Souza" = safe_prediction(
      pred_friend2018_desouza(sex, age, weight, height, test_mode)
    ),
    "Blackie 1989" = safe_prediction(
      pred_blackie1989(sex, age, weight, height)
    ),
    "Jones 1985" = safe_prediction(
      pred_jones1985(sex, age, height)
    ),
    "Koch 2009 / SHIP (Pistea implementation)" = safe_prediction(
      pred_koch2009_pistea(sex, age, weight, height)
    ),
    "Hakola 2011" = safe_prediction(
      pred_hakola2011(sex, age)
    )
  )

  age_appropriate <- c(
    age < 18,
    age < 18,
    age >= 18,
    if (sex == "Male") age >= 34 && age <= 74 else age >= 29 && age <= 73,
    age >= 18,
    age >= 18,
    age > 55 && age <= 80,
    age >= 15 && age <= 71,
    age > 65 && age <= 80,
    age >= 57 && age <= 78
  )

  mode_appropriate <- c(
    TRUE,
    TRUE,
    TRUE,
    test_mode == "Cycle",
    TRUE,
    TRUE,
    test_mode == "Cycle",
    test_mode == "Cycle",
    test_mode == "Cycle",
    test_mode == "Cycle"
  )

  age_range <- c(
    "<18 years",
    "<18 years",
    "Adults; age 30 substituted when younger than 30",
    if (sex == "Male") "34-74 years" else "29-73 years",
    ">=18 years",
    ">=18 years",
    "56-80 years",
    "15-71 years",
    "66-80 years in this Pistea-table implementation",
    "57-78 years"
  )

  results <- data.frame(
    Equation = names(predictions),
    Predicted_VO2_ml_min = unname(predictions),
    Percent_predicted = ifelse(
      is.finite(predictions) & predictions > 0,
      peak_vo2_ml_min / predictions * 100,
      NA_real_
    ),
    Age_appropriate = age_appropriate,
    Mode_appropriate = mode_appropriate,
    Age_range = age_range,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  results$Overall_applicable <-
    results$Age_appropriate & results$Mode_appropriate &
    is.finite(results$Predicted_VO2_ml_min)

  results$Status <- ifelse(
    !is.finite(results$Predicted_VO2_ml_min),
    "Not calculable for entered age",
    ifelse(
      results$Age_appropriate,
      ifelse(
        results$Mode_appropriate,
        "Age appropriate",
        "Age appropriate; mode differs"
      ),
      "Outside derivation age range"
    )
  )

  results
}

plot_all_vo2_predictions <- function(
  results,
  measured_vo2_ml_min,
  patient_age,
  test_mode,
  main = NULL
) {
  keep <- is.finite(results$Predicted_VO2_ml_min)
  plot_df <- results[keep, , drop = FALSE]

  if (nrow(plot_df) == 0) {
    stop("No finite predicted VO2 values are available to plot.")
  }

  if (is.null(main)) {
    main <- sprintf(
      "Peak VO2 Reference-Equation Comparison\nAge %.1f years | %s ergometry",
      patient_age,
      test_mode
    )
  }

  # Keep the equations in the calculator's intended order, but plot from
  # bottom to top so the first equation appears at the top of the figure.
  plot_df$plot_y <- rev(seq_len(nrow(plot_df)))

  age_ok <- plot_df$Age_appropriate
  mode_ok <- plot_df$Mode_appropriate

  # Age appropriateness controls red intensity, as requested.
  point_colors <- ifelse(
    age_ok,
    "#C00000",
    "#F3B3B3"
  )

  # Filled circles = age and mode appropriate.
  # Filled triangles = age appropriate but test mode differs.
  # Open circles = outside derivation age range.
  point_pch <- ifelse(
    !age_ok,
    1,
    ifelse(mode_ok, 19, 17)
  )

  all_values <- c(measured_vo2_ml_min, plot_df$Predicted_VO2_ml_min)
  x_range <- range(all_values, finite = TRUE)
  x_span <- diff(x_range)
  if (!is.finite(x_span) || x_span == 0) x_span <- max(x_range[2] * 0.20, 500)

  # Extra margin on both sides inside the plotting region for value
  # labels: predictions below measured VO2 get their label drawn to the
  # left (pos = 2), predictions at/above get theirs drawn to the right
  # (pos = 4), so both edges need room reserved for label text.
  x_limits <- c(
    max(0, x_range[1] - 0.28 * x_span),
    x_range[2] + 0.33 * x_span
  )

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    mar = c(5.2, 12.5, 5.0, 2.0) + 0.1,
    xaxs = "i",
    yaxs = "i"
  )

  # x_limits (computed above) already reserves extra right-side margin
  # for the value labels; use it directly instead of recomputing a
  # symmetric buffer that doesn't account for label width.
  x_min <- x_limits[1]
  x_max <- x_limits[2]

  plot(
    xlim = c(x_min, x_max),
    NA,
    ylim = c(0.45, nrow(plot_df) + 0.75),
    xlab = "Peak VO2 (ml/min)",
    ylab = "",
    yaxt = "n",
    main = main,
    bty = "o",
    las = 1
  )

  # Light row guides improve comparison without dominating the figure.
  abline(
    h = plot_df$plot_y,
    col = "grey92",
    lwd = 1
  )

  # Measured VO2 is the common clinical reference, not another category.
  abline(
    v = measured_vo2_ml_min,
    col = "black",
    lty = 3,
    lwd = 1.4
  )

  # Short segments show the direction and magnitude of each difference.
  segments(
    x0 = measured_vo2_ml_min,
    y0 = plot_df$plot_y,
    x1 = plot_df$Predicted_VO2_ml_min,
    y1 = plot_df$plot_y,
    col = ifelse(age_ok, "#D9A0A0", "#F1DADA"),
    lwd = 2
  )

  points(
    plot_df$Predicted_VO2_ml_min,
    plot_df$plot_y,
    pch = point_pch,
    cex = ifelse(age_ok, 1.35, 1.20),
    lwd = ifelse(age_ok, 1.2, 1.6),
    col = point_colors
  )

  # A black X at the top of the measured reference line.
  points(
    measured_vo2_ml_min,
    nrow(plot_df) + 0.48,
    pch = 4,
    cex = 1.55,
    lwd = 2.3,
    col = "black",
    xpd = NA
  )
  label_gap <- max(20, 0.015 * (x_max - x_min))
  label_x <- rep(NA_real_, nrow(results))
  finite_labels <- is.finite(results$Predicted_VO2_ml_min)

  label_x[finite_labels] <- pmin(
    results$Predicted_VO2_ml_min[finite_labels] + label_gap,
    measured_vo2_ml_min - label_gap
  )
  right_of_measured <- is.finite(results$Predicted_VO2_ml_min) &
    results$Predicted_VO2_ml_min >= measured_vo2_ml_min

  label_x[right_of_measured] <-
    results$Predicted_VO2_ml_min[right_of_measured] + label_gap

  text(
    adj = 0,
    measured_vo2_ml_min,
    nrow(plot_df) + 0.48,
    labels = sprintf("  Measured: %.0f ml/min", measured_vo2_ml_min),
    pos = 4,
    cex = 0.82,
    font = 2,
    col = "black",
    xpd = NA
  )

  axis(
    2,
    at = plot_df$plot_y,
    labels = plot_df$Equation,
    las = 1,
    tick = FALSE,
    cex.axis = 0.82,
    line = -0.4
  )

  # Label each point with absolute prediction and percent predicted.
  value_labels <- ifelse(
    is.finite(plot_df$Percent_predicted),
    sprintf(
      "%.0f ml/min  |  %.0f%% predicted",
      plot_df$Predicted_VO2_ml_min,
      plot_df$Percent_predicted
    ),
    sprintf("%.0f ml/min", plot_df$Predicted_VO2_ml_min)
  )

  # Put labels away from the measured-reference line:
  # predictions below measured VO2 are labeled on the left;
  # predictions at or above measured VO2 are labeled on the right.
  label_position <- ifelse(
    plot_df$Predicted_VO2_ml_min < measured_vo2_ml_min,
    2,
    4
  )

  text(
    plot_df$Predicted_VO2_ml_min,
    plot_df$plot_y,
    labels = value_labels,
    pos = label_position,
    offset = 0.65,
    cex = 0.73,
    col = ifelse(age_ok, "#8B0000", "#C98F8F"),
    xpd = NA
  )

  legend(
    bg = "white",
    box.col = "black",
    bty = "o",
    "bottomright",
    inset = c(0.01, 0.01),
    legend = c(
      "Age and mode appropriate",
      "Age appropriate; mode differs",
      "Outside derivation age range",
      "Measured VO2"
    ),
    pch = c(19, 17, 1, 4),
    col = c("#C00000", "#C00000", "#F3B3B3", "black"),
    pt.cex = c(1.1, 1.1, 1.1, 1.2),
    pt.lwd = c(1, 1, 1.5, 2),
    cex = 0.74
  )

  mtext(
    "Red intensity indicates age applicability; horizontal distance from the dotted line shows disagreement with measured VO2.",
    side = 1,
    line = 3.7,
    cex = 0.68,
    col = "grey35"
  )

  invisible(plot_df)
}

print_vo2_prediction_results <- function(results, measured_vo2_ml_min) {
  display <- results
  display$Predicted_VO2_ml_min <- ifelse(
    is.finite(display$Predicted_VO2_ml_min),
    round(display$Predicted_VO2_ml_min, 1),
    NA_real_
  )
  display$Percent_predicted <- ifelse(
    is.finite(display$Percent_predicted),
    round(display$Percent_predicted, 1),
    NA_real_
  )

  cat("\nMEASURED PEAK VO2\n")
  cat("-----------------\n")
  cat(sprintf("%.1f ml/min\n\n", measured_vo2_ml_min))

  cat("ALL PREDICTION EQUATIONS\n")
  cat("------------------------\n")
  print(
    display[, c(
      "Equation",
      "Predicted_VO2_ml_min",
      "Percent_predicted",
      "Age_appropriate",
      "Mode_appropriate",
      "Status",
      "Age_range"
    )],
    row.names = FALSE,
    right = FALSE
  )

  cat("\nPrimary automatic equation under the current rule:\n")
  primary <- if (isTRUE(results$Age_appropriate[
    results$Equation == "Cooper 1984 height-based"
  ])) {
    "Cooper 1984 height-based"
  } else {
    "FRIEND 2018 De Souza"
  }
  cat(sprintf("  %s\n\n", primary))
}



# ------------------------------------------------------------
# Tibble formatting helper
# ------------------------------------------------------------
as_nice_vo2_tibble <- function(results_df) {
  display <- results_df

  display$Predicted_VO2_ml_min <- round(
    as.numeric(display$Predicted_VO2_ml_min),
    0
  )
  display$Percent_predicted <- round(
    as.numeric(display$Percent_predicted),
    1
  )

  display <- display[, c(
    "Equation",
    "Predicted_VO2_ml_min",
    "Percent_predicted",
    "Age_appropriate",
    "Mode_appropriate",
    "Overall_applicable",
    "Age_range",
    "Status"
  ), drop = FALSE]

  names(display) <- c(
    "Equation",
    "Predicted VO2 (ml/min)",
    "Percent predicted",
    "Age appropriate",
    "Mode appropriate",
    "Overall appropriate",
    "Derivation age range",
    "Interpretation"
  )

  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(display)
  } else {
    display
  }
}


# ============================================================
# Interactive calculator: runs when the file is sourced
# interactively in R/RStudio
# ============================================================

if (interactive()) {

  cat("\nAll-Equation Predicted VO2 Calculator\n")
  cat("====================================\n")
  cat("Enter the patient/test information once.\n")
  cat("The program will calculate every available equation,\n")
  cat("percent predicted, age applicability, and a comparison plot.\n\n")

  sex <- normalize_sex(
    readline("Sex used by the reference equations (Male/Female or M/F): ")
  )
  age <- read_num("Age (years): ")
  height <- read_num("Height (cm): ")
  weight <- read_num("Weight (kg): ")
  test_mode <- normalize_test_mode(
    readline("Test mode (Treadmill/Cycle) [default Treadmill]: ")
  )
  peak_vo2 <- read_num("Measured peak VO2 (ml/min): ")

  all_vo2_results <- calculate_all_vo2_predictions(
    sex = sex,
    age = age,
    height = height,
    weight = weight,
    peak_vo2_ml_min = peak_vo2,
    test_mode = test_mode
  )


  # Create and display a clean tibble-style summary.

  # ----------------------------------------------------------
  # Compact console output
  # ----------------------------------------------------------
  console_summary <- data.frame(
    Equation = all_vo2_results$Equation,
    Pred_ml_min = round(all_vo2_results$Predicted_VO2_ml_min, 0),
    Percent_pred = round(all_vo2_results$Percent_predicted, 1),
    Age_OK = ifelse(all_vo2_results$Age_appropriate, "Yes", "No"),
    Mode_OK = ifelse(all_vo2_results$Mode_appropriate, "Yes", "No"),
    Status = all_vo2_results$Status,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Shorten labels only for console readability.
  console_summary$Equation <- sub(
    "Koch 2009 / SHIP \\(Pistea implementation\\)",
    "Koch 2009 / SHIP",
    console_summary$Equation
  )

  console_summary$Status <- ifelse(
    console_summary$Status == "Outside derivation age range",
    "Outside age range",
    ifelse(
      console_summary$Status == "Not calculable for entered age",
      "Not calculable",
      console_summary$Status
    )
  )

  cat("\nMEASURED PEAK VO2\n")
  cat("-----------------\n")
  cat(sprintf("%.1f ml/min\n", peak_vo2))

  cat("\nPREDICTION SUMMARY\n")
  cat("------------------\n")
  print(
    console_summary,
    row.names = FALSE,
    right = FALSE,
    max = nrow(console_summary)
  )

  age_reference <- data.frame(
    Equation = all_vo2_results$Equation,
    Age_range = all_vo2_results$Age_range,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  age_reference$Equation <- sub(
    "Koch 2009 / SHIP \\(Pistea implementation\\)",
    "Koch 2009 / SHIP",
    age_reference$Equation
  )

  cat("\nDERIVATION AGE RANGES\n")
  cat("---------------------\n")
  for (i in seq_len(nrow(age_reference))) {
    cat(sprintf(
      "%-29s %s\n",
      age_reference$Equation[i],
      age_reference$Age_range[i]
    ))
  }

  primary_equation <- if (age < 18) {
    "Cooper 1984 height-based"
  } else {
    "FRIEND 2018 De Souza"
  }

  cat("\nPRIMARY AUTOMATIC EQUATION\n")
  cat("--------------------------\n")
  cat(primary_equation, "\n")

  last_vo2_console_summary <- console_summary
  last_vo2_age_reference <- age_reference

  last_vo2_tibble <- as_nice_vo2_tibble(all_vo2_results)


  if (exists("last_vo2_tibble")) {
    last_vo2_tibble_compact <- if (
      requireNamespace("tibble", quietly = TRUE)
    ) {
      tibble::as_tibble(console_summary)
    } else {
      console_summary
    }
  }

  plot_all_vo2_predictions(
    results = all_vo2_results,
    measured_vo2_ml_min = peak_vo2,
    patient_age = age,
    test_mode = test_mode
  )

  # Store the last inputs and results in the global environment for reuse.
  last_vo2_inputs <- list(
    sex = sex,
    age = age,
    height_cm = height,
    weight_kg = weight,
    test_mode = test_mode,
    measured_peak_vo2_ml_min = peak_vo2
  )

  assign("last_vo2_inputs", last_vo2_inputs, envir = .GlobalEnv)
  assign("last_vo2_results", all_vo2_results, envir = .GlobalEnv)
  assign("last_vo2_tibble", last_vo2_tibble, envir = .GlobalEnv)
  assign("last_vo2_console_summary", console_summary, envir = .GlobalEnv)
  assign("last_vo2_age_reference", age_reference, envir = .GlobalEnv)
  if (exists("last_vo2_tibble_compact")) {
    assign("last_vo2_tibble_compact", last_vo2_tibble_compact, envir = .GlobalEnv)
  }

  cat("Results are also available as: last_vo2_results\n")
  cat("Formatted tibble is available as: last_vo2_tibble\n")
  cat("Inputs are also available as:  last_vo2_inputs\n\n")
}
