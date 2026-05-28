# Predicted VO₂ Reference Equations (ml/min)

This repository provides a structured, reusable implementation of commonly used **reference equations for predicted peak VO₂**, with outputs standardized to **absolute VO₂ units (ml/min)** and optional **percent predicted** calculations.

The code supports:

* **Batch processing** of datasets/data frames
* **Interactive, one-off calculations** for individual patients
* Clear separation between **equation definitions**, **helper functions**, and **execution logic**
* Standardized handling of sex, test mode, unit conversions, predicted VO₂, and percent predicted VO₂

---

## Included Reference Equations

All predicted values are returned in **ml/min**.

### 1. Cooper 1984 — Children

Two pediatric Cooper equations are implemented.

#### Weight-based equation

* Input: sex, weight
* Output: predicted VO₂ in ml/min

Reference:

Cooper, D. M., and D. Weiler-Ravell. 1984. “Gas Exchange Response to Exercise in Children.” *American Review of Respiratory Disease*, 129: S47–S48.

#### Height-based equation

* Input: sex, height
* Output: predicted VO₂ in ml/min

Reference:

Sietsema, K. E., Stringer, W. W., Sue, D. Y., and Ward, S. 2020. *Wasserman & Whipp's Principles of Exercise Testing and Interpretation*, 6th ed. Lippincott Williams & Wilkins.

---

### 2. Hansen/Wasserman 2001 — Clinical Exercise Testing

* Input: sex, age, weight, height
* Output: predicted VO₂ in ml/min
* Internal calculation is performed in L/min and converted to ml/min
* For adults younger than 30 years, age is floored at 30 years

Reference:

Hansen, J. E. 2001. Predicted values for clinical exercise testing, as cited in Sietsema, K. E., Stringer, W. W., Sue, D. Y., and Ward, S. 2020. *Wasserman & Whipp's Principles of Exercise Testing and Interpretation*, 6th ed. Lippincott Williams & Wilkins.

---

### 3. FRIEND Registry 2017 — Myers

* Input: sex, age, weight
* Original equation outputs VO₂ in ml/kg/min
* Converted internally to ml/min by multiplying by body weight in kg
* Weight is converted internally from kg to lb

---

### 4. FRIEND Registry 2018 — De Souza

* Input: sex, age, weight, height, test mode
* Original equation outputs VO₂ in ml/kg/min
* Converted internally to ml/min by multiplying by body weight in kg
* Weight is converted internally from kg to lb
* Height is converted internally from cm to inches
* Supports treadmill and cycle ergometer modes

Reference:

de Souza e Silva, C. G., Kaminsky, L. A., Arena, R., Christle, J. W., Araújo, C. G. S., Lima, R. M., Ashley, E. A., and Myers, J. 2018. “A Reference Equation for Maximal Aerobic Power for Treadmill and Cycle Ergometer Exercise Testing: Analysis from the FRIEND Registry.” *European Journal of Preventive Cardiology*, 25: 742–750.

---

## Expected Units and Inputs

| Variable          |    Units / Format | Notes                                     |
| ----------------- | ----------------: | ----------------------------------------- |
| `sex`             |     Male / Female | Case-insensitive; `M` / `F` accepted      |
| `Age`             |             years | Numeric                                   |
| `height`          |                cm | Numeric                                   |
| `weight`          |                kg | Numeric                                   |
| `test_mode`       | Treadmill / Cycle | Used by FRIEND 2018; default is Treadmill |
| `Peak_VO2_ml_min` |            ml/min | Measured peak VO₂                         |

---

## Output Variables

The bulk calculator adds predicted VO₂ columns in **ml/min**:

| Output column                 | Description                                         |
| ----------------------------- | --------------------------------------------------- |
| `weight_lbs`                  | Weight converted from kg to lb                      |
| `height_in`                   | Height converted from cm to inches                  |
| `pred_VO2_cooper_weight`      | Cooper pediatric weight-based predicted VO₂         |
| `pred_VO2_cooper_height`      | Cooper pediatric height-based predicted VO₂         |
| `pred_VO2_hansen2001`         | Hansen/Wasserman predicted VO₂                      |
| `pred_VO2_friend2017_myers`   | FRIEND 2017 Myers predicted VO₂                     |
| `pred_VO2_friend2018_desouza` | FRIEND 2018 De Souza predicted VO₂                  |
| `pred_VO2`                    | Combined default predicted VO₂                      |
| `perc_pred_VO2`               | Percent predicted VO₂, if measured VO₂ is available |

The combined `pred_VO2` column uses:

* `pred_VO2_hansen2001` for patients younger than 18 years
* `pred_VO2_friend2018_desouza` for patients 18 years or older

Percent predicted is calculated as:

```r
perc_pred_VO2 = Peak_VO2_ml_min / pred_VO2 * 100
```

---

## Main Functions

### Helper functions

| Function                 | Purpose                                                |
| ------------------------ | ------------------------------------------------------ |
| `normalize_sex()`        | Standardizes sex input to `Male` or `Female`           |
| `normalize_test_mode()`  | Standardizes test mode input to `Treadmill` or `Cycle` |
| `read_num()`             | Reads numeric values for interactive use               |
| `assert_required_cols()` | Confirms required data-frame columns are present       |

---

### Individual equation functions

| Function                    | Equation                                            |
| --------------------------- | --------------------------------------------------- |
| `pred_cooper_weight()`      | Cooper pediatric weight-based equation              |
| `pred_cooper_height()`      | Cooper pediatric height-based equation              |
| `pred_hansen2001()`         | Hansen/Wasserman clinical exercise testing equation |
| `pred_friend2017_myers()`   | FRIEND 2017 Myers equation                          |
| `pred_friend2018_desouza()` | FRIEND 2018 De Souza equation                       |

Each equation function returns predicted VO₂ in **ml/min**.

---

### Generic single-patient calculator

```r
calc_pred_and_percent(
  equation,
  sex,
  age,
  weight,
  height,
  peak_vo2_ml_min,
  test_mode = "Treadmill"
)
```

Supported equation names:

```r
"cooper_weight"
"cooper_height"
"hansen2001"
"friend2017_myers"
"friend2018_desouza"
```

The function returns a list with:

```r
pred_ml_min
percent_pred
```

---

### Bulk data-frame calculator

```r
add_pred_vo2_columns(
  DF,
  sex_col = "sex",
  age_col = "Age",
  height_col = "height",
  weight_col = "weight",
  test_mode_col = "test_mode",
  peak_vo2_col = "Peak_VO2_ml_min"
)
```

This function:

1. Validates required columns
2. Normalizes sex and test mode
3. Adds unit conversion columns
4. Calculates predicted VO₂ using all implemented equations
5. Adds a combined default `pred_VO2` column
6. Adds `perc_pred_VO2` when measured peak VO₂ is available

---

## Example: Bulk Use

```r
DF_with_pred <- add_pred_vo2_columns(
  DF,
  sex_col = "sex",
  age_col = "Age",
  height_col = "height",
  weight_col = "weight",
  test_mode_col = "test_mode",
  peak_vo2_col = "Peak_VO2_ml_min"
)
```

---

## Example: Single-Patient Use

```r
res <- calc_pred_and_percent(
  equation = "friend2018_desouza",
  sex = "Male",
  age = 35,
  weight = 75,
  height = 178,
  peak_vo2_ml_min = 2500,
  test_mode = "Treadmill"
)

res$pred_ml_min
res$percent_pred
```

---

## File Structure

This script contains:

* Helper and normalization functions
* Individual VO₂ prediction equations
* A generic predicted VO₂ + percent predicted wrapper
* A reusable bulk data-frame calculator
* An optional interactive calculator that runs only when `interactive()` is `TRUE`

---

## Notes

* All final predicted values are returned in **ml/min**.
* FRIEND equations are originally expressed in **ml/kg/min** and are converted internally to **ml/min**.
* FRIEND equations use internal unit conversions for weight and/or height where required.
* The interactive prompt is intended for one-off use in R sessions.
* For reproducible scripts and batch analyses, prefer calling the functions directly rather than relying on the interactive prompt.
