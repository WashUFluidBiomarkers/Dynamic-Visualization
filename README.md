# Alzheimer's Disease Biomarker Visualization Tools

<!-- badges: start -->
<!-- badges: end -->

<p align="center">
  <img src="raindrop_animation_demo.gif" alt="Longitudinal biomarker trajectories" width="450">
</p>

> Interactive Shiny applications for visualizing longitudinal Alzheimer's disease biomarker trajectories from ADNI supporting head-to-head comparisons across multiple assay platforms.

---

## Overview

This repository contains three Shiny applications for exploring plasma and imaging biomarker trajectories in Alzheimer's disease research. The applications use ADNI data to create dynamic visualizations showing how biomarkers change over time. All three Siny apps support different biomarker variables across multiple platforms including C2N Precivity, Fujirebio Lumipulse, ALZpath Quanterix, Janssen LucentAD, Roche NeuroToolKit, and Quanterix Simoa assays, as well as amyloid PET, tau PET, structural MRI, and CSF measures.

The visualization tools enable researchers to explore longitudinal patterns and compare different biomarker modalities.

## Interactive Tools

🔗 **[Raindrop Viewer](https://amyloid.shinyapps.io/raindrop/)** - Explore biomarker trajectories aligned by baseline rank

🔗 **[Time Trails Viewer](https://amyloid.shinyapps.io/timetrails/)** - Visualize simultaneous biomarker trajectories with directional arrows

🎬 **[Movie Generator](https://amyloid.shinyapps.io/movie/)** - Generate MP4 videos of animated biomarker trajectories

## Clone this repository

git clone https://github.com/WashUFluidBiomarkers/adni_visualization_tools.git



## Applications

### Raindrop Viewer (`Raindrop/`)

Interactive visualization showing longitudinal biomarker trajectories aligned by baseline rank order. Participants are arranged on the x-axis by their baseline biomarker value, with longitudinal changes displayed as "raindrops" rising or falling over time.

**Key Features:**
- Baseline rank-ordered participant display
- Animated time slider (0-10 years of follow-up)
- Color-coded disease stage stratification (5-level grouping)
- Customizable threshold lines for clinical cutoffs
- Optional baseline value indicators
- Support for 21 variables
- MP4 video export functionality

### Time Trails Viewer (`TimeTrails/`)

Interactive trajectory viewer showing simultaneous longitudinal changes in two biomarkers with directional arrows indicating disease progression over time.

**Key Features:**
- Simultaneous X-Y plotting of any two biomarkers
- Animated trajectories with progression arrows
- Time slider with smooth animation controls (0-10 years)
- Play/pause functionality
- Color-coded by baseline or longitudinal grouping
- MP4 video export functionality

### Movie Generator (`Movie/`)

Application for automated MP4 video generation, triggered via URL parameters from the interactive viewers. Generates high-quality videos at 1250×750 pixels.

**Key Features:**
- URL parameter-driven configuration
- Automatic download trigger on page load
- Supports both raindrop and time trail formats

## Key Components

### Supported Biomarkers

The applications support **21 different variables** organized into three categories:

**Plasma Biomarkers (15):**
- **C2N PrecivityAD2**: plasma %p-tau217 (ratio), p-tau217 (pg/mL), Aβ42/Aβ40
- **Fujirebio Lumipulse**: plasma p-tau217 (pg/mL), Aβ42/Aβ40
- **ALZpath Quanterix**: plasma p-tau217 (pg/mL)
- **Janssen LucentAD Quanterix**: plasma p-tau217 (pg/mL)
- **Roche NeuroToolKit**: plasma p-tau181 (pg/mL), Aβ42/Aβ40, GFAP (ng/mL), NfL (pg/mL)
- **Quanterix Neurology 4-Plex**: plasma p-tau181 (pg/mL), GFAP (pg/mL), Aβ42/Aβ40, NfL (pg/mL)

**Imaging Biomarkers (4):**
- Amyloid PET Centiloid
- Tau PET Mesial-Temporal (Early Braak)
- Tau PET Temporo-Parietal (Late Braak)
- Cortical Thickness (Meta ROI)

**Clinical/CSF Measures (2):**
- Clinical Dementia Rating Sum of Boxes (CDR-SB)
- CSF Aβ42/p-tau181 ratio (Roche Elecsys)

### Helper Functions (`Needed_Functions.R`)

Each application folder contains a `Needed_Functions.R` file with core utility functions. While the files share common functions, they contain app-specific variations optimized for each visualization type.

**Core Functions:**
- **`create_groups_EST()`** - Creates color-coded disease stage groups for biomarkers where higher values indicate higher risk
- **`create_groups_ratio_EST()`** - Creates groups for ratio biomarkers where lower values indicate higher risk
- **`extract_comma_sep()`** - Parses comma-separated threshold strings for custom cutoff values
- **`decimalplaces()`** - Calculates appropriate decimal precision for slider steps
- **`create_raindrop_plot_pro()`** - Generates raindrop-style trajectory plots with time animation
- **`create_timepath_plot()`** - Generates dual-axis trajectory plots with directional arrows
- **`create_raindrop_plot_movie()`** - Video rendering function for raindrop animations
- **`create_timepath_plot_video()`** - Video rendering function for time trail animations

**Note:** The three `Needed_Functions.R` files differ slightly due to app-specific variants.

## Technical Details

### Dependencies

**Core Packages:**
- `shiny` - Interactive web application framework
- `tidyverse` - Data manipulation and visualization
- `data.table` - High-performance data operations
- `ggplot2` - Grammar of graphics plotting

**Visualization:**
- `ggforce` - Extended ggplot2 functionality
- `ggbreak` - Axis breaks and transformations
- `RColorBrewer` - Color palettes
- `ggrastr` - Rasterized plotting layers
- `khroma` - Scientific color schemes

**Performance:**
- `cachem` - Plot caching (200 MB session cache)
- `ragg` - High-quality, fast PNG rendering

**Video Generation:**
- `av` - FFmpeg video encoding
- `magick` - Image processing

### Data Requirements

**Input File:** `ADNI_Raindrop_Data_Test_v3.rdata`

**Required Variables per Biomarker:**
- `EST_[biomarker]` - Estimated biomarker value
- `BL_EST_[biomarker]` - Baseline estimated value
- `N_[biomarker]` - Number of observations

**Core Variables:**
- `RID` - Participant ID
- `time` - Years since baseline (0-10)

## Usage

Launch each application independently:

Launch Raindrop Viewer
shiny::runApp("Raindrop/")

Launch Time Trails Viewer
shiny::runApp("TimeTrails/")

Launch Movie Generator (use URL parameters to configure)
shiny::runApp("Movie/")


## References

Schindler SE, Petersen KK, Saef B, Tosun D, Shaw LM, Zetterberg H, Dage JL, Ferber K, Triana-Baltzer G, Du-Cuny L, Li Y, Coomaraswamy J, Baratta M, Mordashova Y, Saad ZS, Raunig DL, Ashton NJ, Meyers EA, Rubel CE, Rosenbaugh EG, Bannon AW, Potter WZ; Alzheimer's Disease Neuroimaging Initiative (ADNI) Foundation for the National Institutes of Health (FNIH) Biomarkers Consortium Plasma Aβ and Phosphorylated Tau as Predictors of Amyloid and Tau Positivity in Alzheimer's Disease Project Team. **Head-to-head comparison of leading blood tests for Alzheimer's disease pathology.** *Alzheimers Dement*. 2024 Nov;20(11):8074-8096. doi: 10.1002/alz.14321

---

<p align="center">
  <img src="washu_logo.png" height="70" alt="Washington University School of Medicine"/>
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="knight_adrc_logo.png" height="70" alt="Knight ADRC"/>
</p>

<p align="center">
  <strong>Washington University School of Medicine</strong><br>
  Knight Alzheimer Disease Research Center
</p>

<!--
---

## Citation

If you use these visualization tools in your research, please cite:


-->




