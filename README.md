# Dynamic-Visualization

Overview
This repository contains three complementary Shiny applications for exploring plasma and imaging biomarker trajectories in Alzheimer's disease research. The applications use ADNI data to create dynamic visualizations showing how biomarkers change over time, colored by disease stage groups. All three apps support multiple biomarker platforms including C2N PrecivityAD2, Fujirebio Lumipulse, Roche NeuroToolKit, Quanterix assays, and PET imaging.​

Repository Structure
text
├── Raindrop/
│   ├── app.R                    - Interactive raindrop viewer
│   └── Needed_Functions.R       - Helper functions for raindrop plots
├── TimeTrails/
│   ├── app.R                    - Time trail trajectory viewer  
│   └── Needed_Functions.R       - Helper functions for trajectory plots
├── Movie/
│   ├── app.R                    - MP4 video generator (headless)
│   └── Needed_Functions.R       - Helper functions for video rendering
└── README.md
Applications
1. Raindrop Viewer (Raindrop/)
Interactive visualization showing longitudinal biomarker trajectories aligned by baseline rank. Participants are ordered on the x-axis by their baseline value rank, with longitudinal changes displayed as "raindrops" falling over time.​

Key Features:

X-axis: Baseline rank order of any selected biomarker​

Y-axis: Longitudinal biomarker values over time​

Animated time slider showing biomarker evolution (0-10 years)​

Optional baseline value indicators​

Color-coded disease stage groups (5 categories from low to high risk)​

Customizable threshold lines for clinical cutoffs​

MP4 video export via URL parameter passing​

Interactive Demo: Explore the raindrop biomarker viewer

2. Time Trails Viewer (TimeTrails/)
Interactive dual-axis trajectory viewer showing simultaneous longitudinal changes in two biomarkers with directional arrows indicating disease progression.​

Key Features:

Simultaneous X-Y plotting of any two biomarkers​

Animated trajectories with directional arrows showing progression​

Time slider (0-10 years) with smooth animation controls​

Play/pause functionality for dynamic viewing​

Color-coded by baseline or longitudinal grouping variables​

Cached plotting for improved performance​

Help documentation and references panel​

MP4 video generation link​

Interactive Demo: Explore the time trail biomarker viewer

3. Movie Generator (Movie/)
Headless application for automated MP4 video generation, triggered via URL parameters from the interactive viewers. Generates high-quality videos (1250x750px, 100 DPI) at 5 frames per second.​

Key Features:

URL parameter-driven configuration​

Automatic download trigger on load​

Supports both raindrop and time trail video formats​

Progress notifications during rendering​

No visible UI - purely functional endpoint​

Key Components
Supported Biomarkers
The applications support 21 different biomarker variables across blood-based, imaging, and clinical measures:​

Plasma Biomarkers (15):

C2N PrecivityAD2: plasma %p-tau217, p-tau217 (pg/mL), Aβ42/Aβ40​

Fujirebio Lumipulse: plasma p-tau217, Aβ42/Aβ40​

ALZpath Quanterix: plasma p-tau217​

Janssen LucentAD: plasma p-tau217​

Roche NeuroToolKit: plasma p-tau181, Aβ42/Aβ40, GFAP, NfL​

Quanterix Neurology 4-Plex: plasma p-tau181, GFAP, Aβ42/Aβ40, NfL​

Imaging Biomarkers (4):

Amyloid PET Centiloid​

Tau PET Mesial-Temporal (Early)​

Tau PET Temporo-Parietal (Late)​

Cortical Thickness (Meta ROI)​

Clinical/CSF (2):

Clinical Dementia Rating Sum of Boxes​

CSF Aβ42/p-tau181 (Roche Elecsys)​

Helper Functions (Needed_Functions.R)
Each app folder contains a Needed_Functions.R file with shared utility functions. While similar, these files contain app-specific variations:​

Core Functions:

create_groups_EST() - Creates color-coded disease stage groups where higher values indicate higher risk​

create_groups_ratio_EST() - Creates groups for ratio biomarkers where lower values indicate higher risk​

extract_comma_sep() - Parses comma-separated threshold strings​

decimalplaces() - Calculates decimal precision for slider steps​

create_raindrop_plot_pro() - Generates raindrop-style trajectory plots​

create_timepath_plot() - Generates dual-axis trajectory plots with arrows​

create_raindrop_plot_movie() - Video rendering function for raindrop animations​

create_timepath_plot_video() - Video rendering function for trajectory animations​

Note: The three Needed_Functions.R files differ in size (30,439 - 31,986 characters) due to app-specific customizations and function variants.​

Technical Details
Dependencies:

Core: shiny, tidyverse, data.table, ggplot2​

Visualization: ggforce, ggbreak, RColorBrewer, ggrastr​

Performance: cachem (plot caching), ragg (high-quality rendering)​

Video: av, magick​

UI Enhancement: khroma (color palettes)​

Data Requirements:

Input: ADNI_Raindrop_Data_Test_v3.rdata containing longitudinal biomarker estimates​

Variables required per biomarker: EST_[biomarker], BL_EST_[biomarker], N_[biomarker]​

Participant ID: RID​

Time variable: time (years since baseline)​

Performance Optimizations:

Session-level plot caching (200 MB memory cache)​

renderCachedPlot() for frequently accessed visualizations​

Pre-computed variable lookup tables​

Reactive programming to minimize redundant calculations​

Clone This Repository
bash
git clone https://github.com/WashUFluidBiomarkers/adni_visualization_tools.git
Usage
Each application can be launched independently:

r
# Launch Raindrop Viewer
shiny::runApp("Raindrop/")

# Launch Time Trails Viewer  
shiny::runApp("TimeTrails/")

# Launch Movie Generator (headless - use URL parameters)
shiny::runApp("Movie/")
References
1. Schindler SE, Petersen KK, Saef B, Tosun D, Shaw LM, Zetterberg H, Dage JL, Ferber K, Triana-Baltzer G, Du-Cuny L, Li Y, Coomaraswamy J, Baratta M, Mordashova Y, Saad ZS, Raunig DL, Ashton NJ, Meyers EA, Rubel CE, Rosenbaugh EG, Bannon AW, Potter WZ; Alzheimer's Disease Neuroimaging Initiative (ADNI) Foundation for the National Institutes of Health (FNIH) Biomarkers Consortium Plasma Aβ and Phosphorylated Tau as Predictors of Amyloid and Tau Positivity in Alzheimer's Disease Project Team. Head-to-head comparison of leading blood tests for Alzheimer's disease pathology. Alzheimers Dement. 2024 Nov;20(11):8074-8096. https://pubmed.ncbi.nlm.nih.gov/39394841/​

Washington University School of Medicine | Knight Alzheimer Disease Research Center
