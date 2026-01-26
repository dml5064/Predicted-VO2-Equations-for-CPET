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

2. **Hansen et al. 1984**
   - Clinical exercise testing reference values  

3. **FRIEND Registry 2018 (Adults)**
   - Originally ml/kg/min  
   - Converted internally to ml/min  

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

