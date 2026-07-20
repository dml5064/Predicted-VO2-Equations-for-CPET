# Predicted VO₂ Reference Equations for CPET

This repository provides a structured R implementation of commonly used reference equations for predicted peak oxygen uptake during cardiopulmonary exercise testing (CPET).

All final predictions are standardized to **absolute peak VO₂ in mL/min**. The tool supports:

- Individual equation calculations
- Calculation of all available equations from one set of patient inputs
- Percent-predicted peak VO₂
- Age- and test-mode applicability flags
- Batch processing of data frames
- A compact console summary
- A horizontal comparison plot of measured and predicted peak VO₂

---

## Default equation-selection rule

The combined `pred_VO2` output uses:

- **Age <18 years:** Cooper 1984 height-based equation
- **Age ≥18 years:** FRIEND 2018 De Souza equation

The other equations are still calculated and retained as separate outputs. They are not used automatically unless selected directly.

---

## Required inputs

| Variable | Units / format | Notes |
|---|---:|---|
| `sex` | Male / Female | Case-insensitive; `M` and `F` accepted |
| `age` or `Age` | years | Numeric |
| `height` | cm | Numeric |
| `weight` | kg | Numeric |
| `test_mode` | Treadmill / Cycle | Used for mode-specific applicability and FRIEND 2018 |
| `Peak_VO2_ml_min` | mL/min | Measured peak VO₂ |

---

## Included reference equations

All functions return predicted peak VO₂ in **mL/min**.

### 1. Cooper 1984 — weight-based pediatric equation

#### Male

```text
Predicted VO₂ = 52.8 × weight − 303.4
```

#### Female

```text
Predicted VO₂ = 28.5 × weight + 288.2
```

Inputs: sex and weight in kg.

Function:

```r
pred_cooper_weight()
```

---

### 2. Cooper 1984 — height-based pediatric equation

#### Male

```text
Predicted VO₂ = 43.6 × height − 4547.1
```

#### Female

```text
Predicted VO₂ = 22.5 × height − 1837.8
```

Inputs: sex and height in cm.

Function:

```r
pred_cooper_height()
```

This is the default equation for patients younger than 18 years.

---

### 3. Hansen/Wasserman 2001 — textbook clinical equation

This is the detailed Hansen/Wasserman equation reproduced in the Wasserman exercise-testing textbook. It is retained separately from the simplified Wasserman equation below.

#### Male

```text
Ideal weight = 0.79 × height − 60.7
```

Base prediction:

```text
0.0337 × height − 0.000165 × age × height − 1.963
```

Weight adjustment:

```text
If actual weight ≥ ideal weight:
  + 0.006 × (actual weight − ideal weight)

If actual weight < ideal weight:
  + 0.014 × (actual weight − ideal weight)
```

#### Female

```text
Ideal weight = 0.65 × height − 42.8
```

```text
Predicted VO₂ =
0.001 × height × (14.783 − 0.11 × age)
+ 0.006 × (actual weight − ideal weight)
```

The internal result is calculated in L/min and converted to mL/min.

For adults younger than 30 years, the equation uses age 30.

Function:

```r
pred_hansen2001()
```

---

### 4. Wasserman 1999 — simplified equation

This is the simplified Wasserman equation reproduced in Pistea et al. It is included as a **separate equation** and does not replace the detailed Hansen/Wasserman textbook implementation.

#### Male

```text
Predicted VO₂ = (50.72 − 0.372 × age) × weight
```

#### Female

```text
Predicted VO₂ = (22.78 − 0.17 × age) × (weight + 43)
```

The output is already in mL/min.

Function:

```r
pred_wasserman1999_simplified()
```

Age ranges represented in the source populations:

- Male: 34–74 years
- Female: 29–73 years

This equation is treated as cycle-ergometry specific in the applicability display.

---

### 5. FRIEND Registry 2017 — Myers

```text
VO₂max (mL/kg/min) =
79.9 − 0.39 × age − 13.7 × sex − 0.127 × weight(lb)
```

Sex coding:

- Male = 0
- Female = 1

The result is converted from mL/kg/min to mL/min by multiplying by body weight in kg.

Function:

```r
pred_friend2017_myers()
```

---

### 6. FRIEND Registry 2018 — De Souza

```text
VO₂max (mL/kg/min) =
45.2
− 0.35 × age
− 10.9 × sex term
− 0.15 × weight(lb)
+ 0.68 × height(in)
− 0.46 × mode term
```

Coding:

- Sex term: male = 1, female = 2
- Mode term: treadmill = 1, cycle = 2

The result is converted from mL/kg/min to mL/min by multiplying by body weight in kg.

Function:

```r
pred_friend2018_desouza()
```

This is the default equation for patients aged 18 years or older.

Reference:

de Souza e Silva CG, Kaminsky LA, Arena R, et al. A reference equation for maximal aerobic power for treadmill and cycle ergometer exercise testing: analysis from the FRIEND registry. *European Journal of Preventive Cardiology*. 2018;25:742–750.

---

### 7. Blackie 1989 — adults older than 55 years

Cycle-ergometry equation.

#### Male

```text
VO₂max (L/min) =
0.0142 × height
− 0.0494 × age
+ 0.00257 × weight
+ 3.015
```

#### Female

```text
VO₂max (L/min) =
0.0142 × height
− 0.0115 × age
+ 0.00974 × weight
− 0.651
```

The result is converted from L/min to mL/min.

Function:

```r
pred_blackie1989()
```

Derivation range used by the tool: 56–80 years.

Reference:

Blackie SP, Fairbarn MS, McElvaney GN, et al. Prediction of maximal oxygen uptake and power during cycle ergometry in subjects older than 55 years of age. *American Review of Respiratory Disease*. 1989;139:1424–1429.

---

### 8. Jones 1985

Cycle-ergometry equation.

```text
VO₂max (L/min) =
0.046 × height
− 0.021 × age
− 0.62 × sex
− 4.31
```

Sex coding:

- Male = 0
- Female = 1

The result is converted from L/min to mL/min.

Function:

```r
pred_jones1985()
```

Derivation range used by the tool: 15–71 years.

---

### 9. Koch 2009 / SHIP — Pistea implementation

The current implementation uses the categorical coding reproduced in the Pistea table:

- Age >65 years = 5
- Male = 1
- Female = 2
- BMI ≤25 kg/m² = 0
- BMI >25 kg/m² = 1

Because the Pistea table only provides the age-category code needed for adults older than 65 years, the current implementation is intentionally restricted to that group.

Function:

```r
pred_koch2009_pistea()
```

For younger patients, the result is returned as `NA`.

---

### 10. Hakola 2011

Cycle-ergometry equation.

#### Male

```text
VO₂max (L/min) = 4.846 − 0.039 × age
```

#### Female

```text
VO₂max (L/min) = 3.475 − 0.031 × age
```

The result is converted from L/min to mL/min.

Function:

```r
pred_hakola2011()
```

Derivation range: 57–78 years.

---

## Main functions

### Normalization and validation

| Function | Purpose |
|---|---|
| `normalize_sex()` | Standardizes sex to `Male` or `Female` |
| `normalize_test_mode()` | Standardizes mode to `Treadmill` or `Cycle` |
| `read_num()` | Reads numeric values in the interactive workflow |
| `assert_required_cols()` | Confirms required data-frame columns exist |
| `safe_prediction()` | Returns `NA` rather than stopping when an equation cannot be calculated |

---

### Individual equation functions

```r
pred_cooper_weight()
pred_cooper_height()
pred_hansen2001()
pred_wasserman1999_simplified()
pred_friend2017_myers()
pred_friend2018_desouza()
pred_blackie1989()
pred_jones1985()
pred_koch2009_pistea()
pred_hakola2011()
```

---

### Generic single-equation calculator

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
"wasserman1999_simplified"
"friend2017_myers"
"friend2018_desouza"
"blackie1989"
"jones1985"
"koch2009_pistea"
"hakola2011"
```

The returned list contains:

```r
pred_ml_min
percent_pred
```

Percent predicted is calculated as:

```r
percent_pred = measured_peak_vo2 / predicted_peak_vo2 * 100
```

---

## Calculate all equations for one patient

```r
calculate_all_vo2_predictions(
  sex,
  age,
  height,
  weight,
  peak_vo2_ml_min,
  test_mode = "Treadmill"
)
```

This function returns one row per equation with:

- Equation name
- Predicted peak VO₂ in mL/min
- Percent predicted
- Age appropriateness
- Test-mode appropriateness
- Overall applicability
- Derivation age range
- Interpretation/status

---

## Interactive Source workflow

When the file is sourced interactively in R or RStudio, the user enters:

1. Sex
2. Age
3. Height in cm
4. Weight in kg
5. Test mode
6. Measured peak VO₂ in mL/min

The tool then:

- Calculates all available equations
- Calculates percent predicted for each
- Flags age and mode applicability
- Prints a compact console summary
- Prints derivation-age ranges separately
- Identifies the primary default equation
- Generates the comparison plot

Objects retained in the global environment:

```r
last_vo2_inputs
last_vo2_results
last_vo2_tibble
last_vo2_console_summary
last_vo2_age_reference
last_vo2_tibble_compact
```

---

## Comparison plot

The interactive plot displays:

- Measured peak VO₂ as a black X and dotted vertical reference line
- Age- and mode-appropriate equations as dark red filled circles
- Age-appropriate equations with a mode mismatch as dark red triangles
- Equations outside the derivation age range as light red open circles
- Horizontal segments showing the difference between measured and predicted VO₂
- Predicted VO₂ and percent predicted labels below each point
- An x-axis spanning the lowest and highest finite values with additional padding
- A white legend box with a black outline

Equations that return `NA` are retained in tabular output but omitted from the plot.

---

## Bulk data-frame calculator

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

The function:

1. Validates required columns
2. Normalizes sex and test mode
3. Adds unit-conversion columns
4. Calculates all implemented equations
5. Adds the default combined `pred_VO2`
6. Adds the name of the selected default equation
7. Calculates percent predicted when measured peak VO₂ is present

---

## Bulk output columns

| Output column | Description |
|---|---|
| `weight_lbs` | Weight converted from kg to lb |
| `height_in` | Height converted from cm to inches |
| `pred_VO2_cooper_weight` | Cooper weight-based prediction |
| `pred_VO2_cooper_height` | Cooper height-based prediction |
| `pred_VO2_hansen2001` | Detailed Hansen/Wasserman textbook prediction |
| `pred_VO2_wasserman1999_simplified` | Simplified Wasserman prediction |
| `pred_VO2_friend2017_myers` | FRIEND 2017 prediction |
| `pred_VO2_friend2018_desouza` | FRIEND 2018 prediction |
| `pred_VO2_blackie1989` | Blackie prediction |
| `pred_VO2_jones1985` | Jones prediction |
| `pred_VO2_koch2009_pistea` | Koch/SHIP Pistea implementation |
| `pred_VO2_hakola2011` | Hakola prediction |
| `pred_VO2` | Default age-based prediction |
| `pred_VO2_equation` | Name of the default equation used |
| `perc_pred_VO2` | Percent predicted using the default equation |

---

## Example: bulk use

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

## Example: individual equation

```r
res <- calc_pred_and_percent(
  equation = "wasserman1999_simplified",
  sex = "Male",
  age = 61,
  weight = 75,
  height = 178,
  peak_vo2_ml_min = 1292,
  test_mode = "Cycle"
)

res$pred_ml_min
res$percent_pred
```

---

## Example: all-equation calculation

```r
all_results <- calculate_all_vo2_predictions(
  sex = "Male",
  age = 61,
  height = 178,
  weight = 75,
  peak_vo2_ml_min = 1292,
  test_mode = "Cycle"
)

all_results
```

---

## References

- Blackie SP, Fairbarn MS, McElvaney GN, et al. Prediction of maximal oxygen uptake and power during cycle ergometry in subjects older than 55 years of age. *Am Rev Respir Dis*. 1989;139:1424–1429.
- Cooper DM, Weiler-Ravell D. Gas exchange response to exercise in children. *Am Rev Respir Dis*. 1984;129:S47–S48.
- de Souza e Silva CG, Kaminsky LA, Arena R, et al. A reference equation for maximal aerobic power for treadmill and cycle ergometer exercise testing: analysis from the FRIEND registry. *Eur J Prev Cardiol*. 2018;25:742–750.
- Hansen JE. Predicted values for clinical exercise testing, as reproduced in *Wasserman & Whipp's Principles of Exercise Testing and Interpretation*.
- Jones NL, Makrides L, Hitchcock C, et al. Normal standards for an incremental progressive cycle ergometer test. *Am Rev Respir Dis*. 1985;131:700–708.
- Pistea C, Lonsdorfer E, Doutreleau S, et al. Maximal aerobic capacity in ageing subjects: actual measurements versus predicted values. *ERJ Open Res*. 2016;2:00068-2015.
- Sietsema KE, Stringer WW, Sue DY, Ward S. *Wasserman & Whipp's Principles of Exercise Testing and Interpretation*. 6th ed. Lippincott Williams & Wilkins; 2020.

---

## Notes

- All final predictions are expressed in **mL/min**.
- Percent predicted is measured divided by predicted, multiplied by 100.
- Age and mode flags indicate applicability to the source population; they do not independently validate an equation for a specific clinical population.
- Equations derived from cycle ergometry should not be assumed interchangeable with treadmill reference values.
- The detailed Hansen/Wasserman and simplified Wasserman equations are intentionally retained as separate implementations.
- For reproducible analyses, call the functions directly rather than relying only on the interactive Source workflow.
