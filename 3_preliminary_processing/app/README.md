### What's new?

#### 2023-12-01

- New feature

  - Now maximum values for color scales for link strength, link summary, and
  firing summary can be set in tab *Set color scale*.


#### 2023-11-26

- New features

  - Transmission can now be calculated for all pairs of ROIs if *Calculate for
  all pairs* are selected under the *P(Transmission)* tab.
  
    - If there are more than 30 ROIs in the measurement table, a warning will
    be shown. You can still proceed but the server might crash due to excessive
    calculation being done for all pairs.
    
  - After uploading the measurement data, *Rename ROIs* tab will appear. You can
  download a CSV file containing all ROIs in the raw data and enter a preferred
  name for each ROI in the *name* column. This CSV file can then be uploaded
  with *Upload a rename table* selected (you still need to upload the raw
  measurement table to re-run preprocessing again. It will appear after the
  upload for the renaming table).
  
  
- Changes

  - Maximum bin number is now 200.
  
#### 2023-11-20

- New features

  - The following are downloadable as a CSV file besides visualization:
  
    1. The number of firing per bin per ROI

    2. The intensity of each link per bin per ROI
    
  - Allow using a pre-saved mixture model for binarization
  

#### 2023-08-26

- Bug fix

  - Now the vector field map is log-normalized if transmission frequency is shown (was
  showing raw counts and the capping function was incorrectly changing the direction
  of transmission).

#### 2023-08-20

- Changes
  - When the direction of trasmission is not considered, the vector map is now
  displayed as bidirectional to better represent merging.

#### 2023-08-19

- New feature
  - Now either transmission probability and number can be used to define link
  strength.
    - To address high-transmission number outliers (when using frequency),
    an optional cap value (everything beyond it will be squished and replaced by
    this value) is provided for frequency vector sums.
  
- Changes
  - Now threshold adjustment has a step size of 0.01 (used to be 0.1).
  
- Bug fix
  - Now in the link strength plot, high link strength will be correctly colored
  (link strength > 0.5 was incorrectly capped).
  - Now in the vector field map, 0-length vector is omitted (used to show the
  arrow head).

#### 2023-08-09

- New feature:
  - Export full ROI map as a labeled image (**Check Binarization > Show ROI map**)
  - Show distribution of intensity and correspondence to P(On) (**Check
  Binarization > Show intensity distribution**)
  - Show total link strength per ROI (i.e., hub potential) with link summary (
  **P(Transmission) > Show link summary**
  )
  - Show directionality of link as a vector field with link summary (
  **P(Transmission) > Show link summary**
  )
  
- Changes
  - Now firing summary is colored using the same palette (plasma).
  
- Bug fix:
  - Now values above the cap of color scale will be colored with the color for
  max value (used to be dark grey).

#### 2023-07-24

- New feature:
  - Now every plot can be downloaded as PDF/PNG files.
  - You can see where the ROI is in **Check Binarization > Where's my ROI?**
  
- Changes:
  - Now you can adjust the size of point and lines in the summary plots.
  
- Bug fix:
  - Now small bin number no longer results in small plot height.
  - Minimal bin number is now 1 (was 2).
  

### Usage

To run a preliminary analysis, please upload a Fiji-exported csv file from
**Upload a file**. When the file is uploaded, it will be pre-processed and
preliminarilily binarized. You will be brought to the **Preview** output tab,
which would show the first few rows of the table uploaded.

- Note that the file format has to be correct. If the app crashes upon
upload (and shows a reconnect message at bottom right), it is likely to be
a file format issue.


After pre-processing, **ROI to examine** under the **Check Binarization** tab
will be populated with ROI numbers to facilitate decision of threshold.

When clicking **Show trace**, the peaks that are the closest together will be
shown, so you can decide if binarization threshold is too stringent and resulted
in splitting a peak into fragments.

You can change the binarization threshold with **Probability threshold to be
considered ON**. Clicking **Binarize with new cutoff** will update the data.

When threshold looks good enough, move on to tab **P(Transmission)**. Here,
you can examine the transmission probability for the neighbors of each ROI (
selectable with **ROI to examine**). Transmission probability is calculated 
per bin, and the number of bins can be changed in **Split frames into ? bins**.

Clicking **Plot transmission** will plot fitted trends of transmission
probability per neighbor of the selected ommatidium.

Clicking **Show firing summary** will show the total ON frame per bin for
each ommatidium as an interactive heat map. Hovering your cursor on each
ommatidium will show the exact number of frames when it is ON.

Clicking **Show link summary** will show the overall transmission tendency as
_links_ between ommatidia. This visualization takes long and you might want to
go have a cup of tea.
