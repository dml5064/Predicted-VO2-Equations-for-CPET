# Predicted VO₂ Reference Equations (ml/min)

This repository provides a structured, reusable implementation of commonly used **reference equations for predicted peak VO₂**, with outputs in **absolute units (ml/min)** and optional **percent predicted** calculations.

The code is designed to support:
- **Batch processing** of datasets (data frames)
- **Interactive, one-off calculations** for individual patients
- Clear separation between *definitions* (equations) and *execution*

---

## Included Reference Equations

All predicted values are returned in **ml/min**.

1. **Cooper et al. 1984 (Children)**
   - Weight-based equation  
   - Height-based equation
Cooper, D. M., and D. Weiler-Ravell. 1984. 'Gas exchange response to exercise in children', Am Rev Respir Dis, 129: S47–8.

2. **Hansen/Wasserman et al. 1984**
   - Clinical exercise testing reference values
Sietsema, Kathy E, William W Stringer, Darryl Y Sue, and Susan Ward. 2020. Wasserman & Whipp's: Principles of Exercise Testing and Interpretation 6th edition (Lippincott Williams & Wilkins).

3. **FRIEND Registry 2018 (Adults)**
   - Originally ml/kg/min  
   - Converted internally to ml/min
de Souza, E. Silva C. G., L. A. Kaminsky, R. Arena, J. W. Christle, C. G. S. Araújo, R. M. Lima, E. A. Ashley, and J. Myers. 2018. 'A reference equation for maximal aerobic power for treadmill and cycle ergometer exercise testing: Analysis from the FRIEND registry', Eur J Prev Cardiol, 25: 742–50.


---

## Expected Units and Inputs

| Variable | Units | Notes |
|--------|------|------|
| `sex` | Male / Female | Case-insensitive; M/F accepted |
| `Age` | years | Numeric |
| `height` | cm | |
| `weight` | kg | |
| `test_mode` | Treadmill / Cycle | FRIEND only; default = Treadmill |
| `Peak_VO2_ml_min` | ml/min | Measured value |

---

## File Structure

This single file contains:
- Helper and normalization functions
- Individual reference equations
- A generic wrapper for predicted + percent predicted
- A bulk data-frame calculator
- An interactive calculator (runs only in interactive sessions)

