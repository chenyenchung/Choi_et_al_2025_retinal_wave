# Retinal Wave Analysis Pipeline

> Please also see: [Choi *et al.* 2025](https://www.biorxiv.org/content/10.1101/2025.06.02.657513v1.abstract)

This pipeline analyzes spontaneous calcium waves in retinal tissue, specifically examining
how calcium activity propagates through interconnected non-neural supporting cells in the
*Drosophila* retina. The analysis workflow includes automated segmentation, signal
processing, network analysis, and spatial characterization of ommatidial structures.

## Pipeline Components

### 1. ImageJ Macros (`1_imagej_macro/`)
Automated segmentation and ROI extraction from microscopy images
- **roi_segmentation.ijm**: Automated segmentation using threshold and watershed algorithms
- **roi_adjust.ijm**: Batch adjustment of ROI sizes
- **roi_load.ijm**: Creates ROIs from CSV coordinates

### 2. ROI Decluster (`2_decluster/`)
R Shiny app for merging overlapping ROIs
- Semi-automatic merging of nearby ROIs using hierarchical clustering
- Interactive visualization and adjustment of merging parameters
- Export of cleaned ROI coordinates

### 3. Preliminary Processing (`3_preliminary_processing/`)
R Shiny app for calcium wave analysis
- **Signal Binarization**: Gaussian mixture model to classify ON/OFF states
- **Hexagonal Network Analysis**: Identifies nearest neighbors in fly retina hexagonal pattern
- **Transmission Probability**: Calculates signal propagation between connected ommatidia
- **Wave Tracking**: Identifies wave initiation sites and collision events
- **Interactive Visualizations**: Heat maps, network graphs, and vector fields

### 4. Ommatidial Size Analysis (`4_ommatidial_size_summarize/`)
R Shiny app for spatial characterization of ommatidial organization
- 3D to 2D projection of ommatidial positions
- Anatomical axis alignment using reference landmarks
- Distance and area measurements between neighboring ommatidia
- Asymmetry analysis and spatial binning for regional characterization

## Requirements

- **ImageJ/Fiji**: For running segmentation macros
- **R**: Version 4.0+ with the following packages:
  - Core: shiny, shinybusy
  - Data processing: data.table, dplyr, tidyr
  - Visualization: ggplot2, plotly, patchwork, RColorBrewer
  - Statistical: mclust
  - Spatial: area
  - Export: xlsx, Cairo

## Citation

If you use this pipeline in your research, please cite:
[bioRxiv preprint link](https://www.biorxiv.org/content/10.1101/2025.06.02.657513v1.abstract)
