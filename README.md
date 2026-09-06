# Labend 🧪

Labend is a comprehensive, interactive R Shiny application designed to automate and simplify daily laboratory calculations and data analysis workflows. It serves as an all-in-one digital assistant for molecular biology and biochemistry labs.

## ✨ Key Features & Modules

### 1. 🧮 Molarity Calculator
Quickly perform routine lab calculations for preparing solutions.
- **Find Mass:** Calculate required mass given volume and concentration.
- **Find Volume:** Calculate required volume given mass and concentration.
- **Find Concentration:** Determine the molarity from mass and volume.

### 2. 🧬 ProtParam & A280
Analyze protein sequences to obtain physical and chemical parameters.
- Calculate extinction coefficients, molecular weights, and theoretical pI.
- Accurately determine protein concentration using A280 absorbance readings.

### 3. 📈 Growth Curve Analysis
Plot and analyze bacterial or yeast growth curves over time to determine growth kinetics and doubling times.

### 4. ⚖️ Protein Quantification
Automated analysis for BCA or Bradford assays.
- Generate standard curves with $R^2$ evaluation.
- Calculate unknown sample concentrations automatically.
- Generate Western Blot (WB) prep recipes based on the calculated concentrations.

### 5. 🖼️ WB Densitometry
Analyze Western Blot ImageJ densitometry data.
- Normalize target protein band intensities against loading controls.
- Generate clean, publication-ready bar plots of the normalized results.

### 6. 🔥 DSF (Differential Scanning Fluorimetry) Analysis
Analyze protein thermal shift assays.
- Plot raw melt curves and negative derivative (-dF/dT) plots.
- Automatically calculate and report the melting temperature (Tm) of proteins.

## 🚀 How to Run

1. Open `Labend.Rproj` in RStudio.
2. Open `Labend.R`.
3. Click the **"Run App"** button in RStudio, or execute `shiny::runApp("Labend.R")` in the console.

## 🛠 Prerequisites

Ensure you have the following R packages installed before running:
`shiny`, `dplyr`, `ggplot2`, `tidyr`, `DT`, `bslib`, `readxl`, `tools`, `plotly`

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
