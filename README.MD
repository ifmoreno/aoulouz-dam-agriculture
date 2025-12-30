# Dam Infrastructure and Agricultural Productivity  
### Data Acquisition and Analysis Pipeline

This repository contains the scripts used to acquire, process, and prepare data for an empirical study on the impacts of large-scale hydraulic infrastructure on agricultural productivity in arid and semi-arid regions.

The project focuses on the Aoulouz dam in Morocco’s Souss-Massa region and examines how dam construction alters water access and agricultural outcomes across space. The analysis relies on remote sensing data, climatic variables, and topographical controls, and is designed to support econometric evaluation of spatially heterogeneous treatment effects.

---

## Project Overview

Water availability is a critical determinant of agricultural productivity in water-scarce environments. Large-scale dams represent a common policy response to rainfall variability and water shortages, yet the spatial distribution of their benefits within downstream regions remains poorly understood.

This project studies how agricultural productivity responds to dam construction as a function of distance from the dam. Agricultural outcomes are proxied using vegetation indices derived from satellite imagery, combined with precipitation and terrain characteristics. The scripts in this repository implement the data acquisition and preparation pipeline used to construct the analytical dataset for subsequent econometric analysis.

---

## Data Sources

- **Landsat 5 Thematic Mapper:** Vegetation and water indices
- **CHIRPS:** Precipitation
- **Topographical data:** Elevation and slope
- **Google Earth Engine:** Raster access and preprocessing

---

## Repository Structure and Scripts

- `get_data_GEE.ipynb`  
  Downloads raster datasets from Google Earth Engine for local processing.

- `01.Calculate_NDVI_Landsat_5.R`  
  Computes the Normalized Difference Vegetation Index (NDVI) from Landsat 5 imagery.

- `02.1.ndvi_l5_extraction.R`  
  Extracts NDVI values and aggregates them by parcel, month, and year.

- `02.2.topography.R`  
  Extracts and computes topographical variables for each parcel.

- `02.4.ndwi_precip_extraction.R`  
  Extracts and aggregates NDWI and precipitation data by parcel, month, and year.

- `03.data_prep.R`  
  Cleans, merges, and harmonizes all datasets into a unified analytical file.

- `04.3_model_28.10.R`  
  Implements the econometric analysis models.

- `06.data_vis.R`  
  Generates figures and visualizations for the paper.

---

## Reproducibility

- Python dependencies required for data acquisition are listed in `requirements.txt`
- Access to Google Earth Engine is required to run the data download notebook
- Downstream analysis is conducted in R using the processed datasets

---

## Notes

This repository contains code only. Raw raster data and intermediate outputs are not included due to size and access constraints.
