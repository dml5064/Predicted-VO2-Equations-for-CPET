# ===================================================================
# Reference equations for predicted VO2max (ml/min)
# ===================================================================
# Units assumed:
#   weight = kg
#   height = cm
#   Age    = years
#
# Output:
#   pred_VO2_* = predicted VO2max in ml/min
# ===================================================================


# -------------------------------------------------------------------
# Unit conversions (used by FRIEND)
# -------------------------------------------------------------------
DF$weight_lbs <- DF$weight * 2.20462   # kg -> lb
DF$height_in  <- DF$height / 2.54      # cm -> in


# -------------------------------------------------------------------
# Cooper 1984 — WEIGHT-based VO2max (children)
# J Appl Physiol 1984
# VO2max (ml/min) as a function of body weight (kg)
# -------------------------------------------------------------------
DF$pred_VO2_cooper_weight <- with(DF, ifelse(
  sex == "Male",
  52.8 * weight - 303.4,   # boys
  28.5 * weight + 288.2    # girls
))


# -------------------------------------------------------------------
# Cooper 1984 — HEIGHT-based VO2max (children)
# Am Rev Respir Dis 1984
# VO2max (ml/min) as a function of height (cm)
# -------------------------------------------------------------------
DF$pred_VO2_cooper_height <- with(DF, ifelse(
  sex == "Male",
  43.6 * height - 4547.1,  # boys
  22.5 * height - 1837.8   # girls
))


# -------------------------------------------------------------------
# Hansen 1984 — Predicted values for clinical exercise testing
# Am Rev Respir Dis 1984;129(2 Pt 2):S49–S55
# VO2peak (ml/min)
# -------------------------------------------------------------------
DF$pred_VO2_hansen1984 <- with(DF, {
  pred_wt <- ifelse(sex == "Male",
                    0.79 * height - 60.7,
                    0.65 * height - 42.8)
  
  wt_used <- ifelse(weight >= pred_wt, pred_wt, weight)
  
  ifelse(sex == "Male",
         wt_used * (50.75 - 0.37 * Age),
         (wt_used + 43) * (22.78 - 0.17 * Age))
})


# -------------------------------------------------------------------
# FRIEND — Adult reference equation
# Eur J Prev Cardiol 2018
# Originally ml/kg/min → converted here to ml/min
# -------------------------------------------------------------------
DF$pred_VO2_friend <- with(DF, {
  vo2_ml_kg_min <-
    45.2 -
    0.35 * Age -
    10.9 * ifelse(sex == "Male", 1, 2) -
    0.15 * weight_lbs +
    0.68 * height_in -
    0.46 * ifelse(test_mode == "Treadmill", 1, 2)
  
  vo2_ml_kg_min * weight
})


# -------------------------------------------------------------------
# Example combined logic (optional)
#   <18 years  -> Hansen 1984
#   >=18 years -> FRIEND
# All outputs in ml/min
# -------------------------------------------------------------------
DF$pred_VO2 <- with(DF, ifelse(
  Age < 18,
  pred_VO2_hansen1984,
  pred_VO2_friend
))


# -------------------------------------------------------------------
# Percent predicted VO2max (absolute)
# Assumes Peak_VO2_ml_min is measured VO2max in ml/min
# -------------------------------------------------------------------
DF$perc_pred_VO2 <- (DF$Peak_VO2_ml_min / DF$pred_VO2) * 100

# calc_pred_vo2.R
# ============================================================
# Interactive calculator for predicted VO2 (ml/min) and %pred
# Equations:
#   1) Cooper 1984 (weight-based, children) -> ml/min
#   2) Cooper 1984 (height-based, children) -> ml/min
#   3) Hansen 1984 (Predicted values for clinical exercise testing) -> ml/min
#   4) FRIEND 2018 (adult; ml/kg/min converted to ml/min)
#
# Units:
#   sex    = Male/Female (or M/F)
#   Age    = years
#   height = cm
#   weight = kg
#   test_mode = Treadmill/Cycle (FRIEND only; default Treadmill)
#   Peak_VO2_ml_min = measured peak VO2 in ml/min
# ============================================================

# ---------- helpers ----------
normalize_sex <- function(sex) {
  s <- tolower(trimws(sex))
  if (s %in% c("m", "male")) return("Male")
  if (s %in% c("f", "female")) return("Female")
  stop("sex must be Male/Female (or M/F).")
}

normalize_test_mode <- function(test_mode) {
  t <- tolower(trimws(test_mode))
  if (t == "" || t %in% c("t", "tm", "treadmill")) return("Treadmill")
  if (t %in% c("c", "cy", "cycle", "bike", "erg", "ergometer")) return("Cycle")
  stop("test_mode must be Treadmill or Cycle (or T/C).")
}

read_num <- function(prompt) {
  x <- suppressWarnings(as.numeric(readline(prompt)))
  if (is.na(x)) stop(paste0("Expected a numeric value for: ", prompt))
  x
}

# ---------- equations (all return ml/min) ----------
pred_cooper_weight <- function(sex, weight) {
  if (sex == "Male") {
    52.8 * weight - 303.4
  } else {
    28.5 * weight + 288.2
  }
}

pred_cooper_height <- function(sex, height) {
  if (sex == "Male") {
    43.6 * height - 4547.1
  } else {
    22.5 * height - 1837.8
  }
}

pred_hansen1984 <- function(sex, age, weight, height) {
  pred_wt <- if (sex == "Male") (0.79 * height - 60.7) else (0.65 * height - 42.8)
  wt_used <- if (weight >= pred_wt) pred_wt else weight
  
  if (sex == "Male") {
    wt_used * (50.75 - 0.37 * age)
  } else {
    (wt_used + 43) * (22.78 - 0.17 * age)
  }
}

pred_friend2018 <- function(sex, age, weight, height, test_mode = "Treadmill") {
  # FRIEND equation is ml/kg/min; convert to ml/min by multiplying by weight (kg)
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

calc_pred_and_percent <- function(equation, sex, age, weight, height, peak_vo2_ml_min, test_mode = "Treadmill") {
  pred <- switch(
    equation,
    "cooper_weight" = pred_cooper_weight(sex, weight),
    "cooper_height" = pred_cooper_height(sex, height),
    "hansen1984"    = pred_hansen1984(sex, age, weight, height),
    "friend2018"    = pred_friend2018(sex, age, weight, height, test_mode),
    stop("Unknown equation.")
  )
  
  if (!is.finite(pred) || pred <= 0) stop("Predicted VO2 is non-positive or invalid (check inputs/range).")
  perc <- (peak_vo2_ml_min / pred) * 100
  
  list(pred_ml_min = pred, percent_pred = perc)
}

# ---------- interactive prompts ----------
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
cat("  4 = FRIEND 2018 (adult; ml/kg/min -> ml/min) [requires test mode]\n")

choice <- trimws(readline("Enter 1/2/3/4: "))

equation <- switch(
  choice,
  "1" = "cooper_weight",
  "2" = "cooper_height",
  "3" = "hansen1984",
  "4" = "friend2018",
  stop("Choice must be 1, 2, 3, or 4.")
)

test_mode <- "Treadmill"
if (equation == "friend2018") {
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

