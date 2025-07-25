# Retinal Wave Preliminary Analysis

## Overview

This R Shiny application performs preliminary interactive analysis of retinal wave propagation patterns. It processes time-series calcium imaging data to understand how calcium activity propagates through interconnected non-neural supporting cells in the retina.

## Analysis Goal

The primary goal of this application is to:
- Detect and characterize spontaneous waves of calcium activity in retinal tissue
- Quantify the transmission probability of signals between neighboring ommatidia
- Visualize spatiotemporal patterns of wave propagation

## How It Works

### 1. Data Input and Preprocessing
- **Input Format**: Accepts Fiji-exported CSV files containing fluorescence intensity measurements from regions of interest (ROIs)
- **ROI Management**: Automatically assigns unique identifiers to each ROI and handles duplicate labels
- **Spatial Mapping**: Extracts X,Y coordinates to create spatial maps of ROI locations
- **Optional Renaming**: Supports custom ROI naming through an upload table for better organization

### 2. Signal Binarization
- **Gaussian Mixture Model**: Uses a two-component Gaussian mixture model (via `mclust` package) to classify activity states
- **State Classification**: Separates fluorescence signals into ON (active) and OFF (inactive) states
- **Probability Calculation**: Computes the probability of each ROI being in an ON state at each time frame
- **Threshold Adjustment**: Allows interactive adjustment of the probability threshold for ON/OFF classification
- **Model Persistence**: Can save and reuse trained mixture models for consistent analysis across sessions

### 3. Hexagonal Network Analysis

Given the fly retina is a hexagon tile, most ommatidia have 6 neighbors except the ones on the border of
field of view. To assign immediate neighbors per ommatidium, we implemented:

- **Nearest Neighbor Detection**: Identifies the 6 nearest neighbors for each ROI
- **Mutual Nearest Neighbors (MNN)**: Creates bidirectional connections between ROIs that are mutual nearest neighbors
- **Background Removal**: Filters out ROIs that are active more than 50% of the time (likely background)

### 4. Transmission Probability Analysis
- **Conditional Probability**: Calculates P(neighbor ON | source ON) for each connected pair
- **Temporal Binning**: Divides the time series into bins to analyze transmission patterns over time
- **Directional Analysis**: Can analyze directional or merged bidirectional transmission
- **All-Pairs Analysis**: Optional calculation for all possible ROI pairs (Very slow!!!)

### 5. Sanity check Tools

#### Binarization Check
- Time series plots showing raw intensity and binarized states
- Distribution plots of intensity values and ON probabilities
- Interactive selection of specific ROIs and time windows

#### Transmission Probability
- Line plots showing transmission probability trends over time bins
- Customizable point sizes and line widths for clarity

#### Firing Summary
- Heat maps showing firing frequency per ROI per time bin
- Interactive Plotly visualizations with hover information
- Customizable color scales with optional manual limits

#### Link Summary
- **Network Visualization**: Shows transmission strength as weighted edges between ROIs
- **Hub Analysis**: Identifies ROIs with high total link strength (potential hub neurons)
- **Vector Field**: Displays directional transmission as a vector field
- **Wave Annotation**: Tracks wave propagation including de novo initiations and collisions

## Technical Implementation

### Dependencies
- **Core**: R, Shiny, shinybusy
- **Data Processing**: data.table, dplyr, tidyr
- **Visualization**: ggplot2, plotly, patchwork, RColorBrewer
- **Statistical Analysis**: mclust
- **Export**: xlsx, Cairo (for server environments)
