# Ommatidial Distance Pre-processing Tool

This R Shiny application analyzes and visualizes ommatidial (unit eye) spatial organization
by calculating distances, areas, and structural patterns between neighboring ommatidia.

## Analysis Goal

- Measuring distances between neighboring ommatidia
- Estimating ommatidial areas
- Analyzing structural asymmetry and axis orientation
- Binning ommatidia spatially for regional analysis

## How It Works

### 1. Data Input
- Accepts Excel (.xls, .xlsx) or CSV files containing:
  - The input files are manually labeled retinal microscopy in Imaris of:
      - 3D coordinates (x, y, z) of ommatidial centers
      - Annotation labels ("match" column) for anatomical reference points

### 2. Reference Point Selection
- Users can manually label anatomical landmarks to orient the sample:
  - Center point
  - Dorsal (D) and Ventral (V) points for D-V axis
  - Anterior (A) and Posterior (P) point pairs for A-P axis

### 3. Processing Pipeline

#### Neighbor Identification
- Calculates 3D distances between all ommatidia
- Identifies 6 nearest neighbors within a user-defined threshold
- Filters to retain only ommatidia with sufficient neighbors

#### Spatial Projection
- Projects 3D coordinates onto 2D using PCA
- Rotates coordinate system to anatomical axes:
  - Centers at the designated center point
  - Rotates so dorsal points upward
  - Ensures anterior points leftward

#### Measurements
- **Median neighbor distance**: Average distance to 6 nearest neighbors
- **Estimated area**: Polygon area formed by neighboring ommatidia
- **Axis angles**: Orientation of diagonal connections
- **Asymmetry**: Distance differences between diagonal neighbor pairs

#### Spatial Binning
- Divides the eye into rectangular bins for regional analysis
- Assigns each ommatidium to a bin based on its aligned coordinates

### 4. Visualization
- Reference point positions with anatomical orientation
- Heat maps showing:
  - Median neighbor distances
  - Estimated ommatidial areas
  - Individual neighbor connections
  - Axis orientations
  - Asymmetry patterns

### 5. Data Export
The app generates four CSV files:
- **centers_with_avgdist_and_estarea.csv**: Ommatidial positions with median distances and areas
- **individual_dist.csv**: All pairwise neighbor distances
- **axis_angle.csv**: Diagonal axis orientations
- **asymmetry_per_axis.csv**: Asymmetry measurements for each diagonal axis

## Technical Implementation

Built with R Shiny using:
- `tidyr`, `dplyr` for data manipulation
- `area` for polygon area calculations
- `ggplot2`, `patchwork` for visualization
- `viridisLite`, `RColorBrewer` for color schemes
- PCA for dimensionality reduction
- Trigonometric transformations for spatial alignment
