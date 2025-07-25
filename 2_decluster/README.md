# ROI Merger for Retinal Wave Analysis

## What it does

This Shiny app helps merge nearby Regions of Interest (ROIs) in retinal imaging data. When segmenting ommatidial ROIs, multiple ROI might overlap.
This tool merge overlapping ROIs in a semi-automatic fashion with user discretion to facilitate downstream analyses.

## How it works

1. **Upload your data**: Upload a CSV file containing X and Y coordinates of your ROIs
2. **Set merging distance**: Use the slider to control the merging diameter (1-30 units) - ROIs within this distance will be merged together
3. **View results**: The plot shows your ROIs for your preview
4. **Download merged data**: Click the download button to save the merged ROI coordinates as a new CSV file

The app uses hierarchical clustering to identify and merge nearby points, replacing groups of close ROIs with their average position.
