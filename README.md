# Efficiency‑TFP Analysis Kit v3.0

**MAPS Project — Deliverable 4.2 (WP4)**  
*Models, Assessment and Policies for Sustainability*  
*Horizon Europe — Grant Agreement No. 101137914*

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This repository provides an automated R pipeline to test whether **Total Factor Productivity (TFP)** is statistically explained by **final‑to‑useful exergy efficiency**, following the econometric methodology of Santos et al. (2021) and De Ketelaere et al. (2026).

The kit runs a complete eight‑step cointegration analysis and produces a formatted multi‑sheet Excel workbook plus a publication‑ready PDF report.

## Features

- Output elasticities (α_K, α_L) from income shares or default values (0.3/0.7)
- Labour inputs: unadjusted (`emp × avh`) and quality‑adjusted (`× hc`)
- TFP: Solow residual (K_stock or K_services), normalised to `z = ln(x/x₀)+1`
- Unit root tests: ADF (BIC lag selection, EViews‑consistent) and PP (Newey‑West)
- VAR(p) estimation with residual diagnostics (Portmanteau, LM, Jarque‑Bera)
- Johansen cointegration: **Hz** (no deterministic term) and **Hc** (restricted constant)
- Useful exergy intensity constancy analysis (trend + stationarity + CV)
- Output: formatted Excel workbook + structured PDF report

## Requirements

- R version 4.1 or later
- Internet connection (for automatic package installation)

## Quick Start

```r
# Source the main script
source('eff_tfp_v3.R')

# Standard mode: provide full macro + exergy data
results <- run_analysis('austria.xlsx')

# Pre‑loaded mode: TFP and efficiency series already computed
results <- run_analysis('portugal.xlsx')

# Built‑in Portugal replication (Santos et al. 2021)
run_portugal_santos2021()

Input Data Formats

Two Excel templates are supported:

    Standard Template (1_Standard_Template) – requires columns:
    year, gdp, k_stock (or k_services), emp, x_final, x_useful, plus optional factor‑share and labour columns.

    Pre‑loaded Template (2_Preloaded_Template) – requires only:
    year, tfp_unadj (or z_tfp_unadj), eff (or z_eff).

See data_template_v3.xlsx for examples.
Output Files
File pattern	Description
eff_tfp_results_<country>.xlsx	Nine‑sheet Excel workbook with elasticities, TFP series, unit root tests, VAR diagnostics, Johansen results, intensity constancy, and estimated TFP/GDP.
eff_tfp_report_<country>.pdf	Complete PDF report (A4) with charts, tables, and verdict banners.
Repository Contents
File	Description
eff_tfp_v3.R	Main analysis script
data_template_v3.xlsx	Excel template for user data
austria.xlsx	Example: Austria 1960–2019 (full macro data)
usa.xlsx	Example: USA 1960–2019 (full macro data)
portugal.xlsx	Example: Portugal 1960–2014 (pre‑loaded mode)
eff_tfp_report_*.pdf	Sample PDF outputs for Austria, Portugal, USA
README_EfficiencyTFP_Kit_v3.docx	Detailed user guide
MAPS_PartB_Final.pdf	MAPS project proposal (background)
License

Code and example data © MAPS Project (Horizon Europe, Grant Agreement No. 101137914).
Distributed under the MIT License – see LICENSE file for details.
Citation

If you use this kit in your research, please cite:

    Santos, J., Borges, A.S., Domingos, T. (2021). Exploring the links between total factor productivity and energy efficiency: Portugal, 1960–2014. Energy Economics, 101, 105407.

    De Ketelaere, J., Santos, J., Domingos, T. (2026, under review). Total Factor Productivity is fully explained by changes in physical energy efficiency: an exergy economics approach. Energy Economics.

Contact

MAPS Project – Horizon Europe Grant Agreement No. 101137914
