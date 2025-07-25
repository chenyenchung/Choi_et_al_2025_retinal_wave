# ImageJ Macros for Retinal Wave Analysis

## roi_segmentation.ijm
**Purpose:** Automated segmentation of regions of interest (ROIs) from z-stack images.

**How it works:**
- Creates maximum intensity projection from user-selected slice range
- Applies threshold to identify ommatidium outlines
- Smooths with Gaussian blur and removes noise with second threshold
- Separates touching objects using watershed algorithm (3x)
- Identifies particles >10 pixels as ROIs and measures them
- Generate a Result table that can be exported.
  - Further adjustment and merging of ROIs can be done with `roi_adjust` or `2_decluster`

## roi_adjust.ijm
**Purpose:** Batch adjustment of ROI sizes in ROI Manager.

**How it works:**
- Prompts for enlargement value (negative to shrink)
- Iterates through all ROIs in ROI Manager
- Applies size adjustment to each ROI
- Updates ROI Manager with modified ROIs

## roi_load.ijm
**Purpose:** Creates ROIs from a CSV file containing X,Y coordinates.

**How it works:**
- Loads coordinate data from CSV file (X and Y columns)
- Creates point selections at coordinates
- Enlarges points by 15 pixels to create circular ROIs
- Applies watershed to separate overlapping regions
- Adds resulting particles as ROIs to ROI Manager

