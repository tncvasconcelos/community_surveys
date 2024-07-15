# 
Exploring data on pollination surveys of plant communities along an environmental gradient


on git ignore (files are too large for the repo):
- POWO data
- gbif points from Dorey et al.
- environmental layers

Note: packages rgeos, rgdal, and maptools were unfortunately retired as of October 2023. Much of the code here depends on functions from these three packages and, although the functionalities of these packages have been migrated to other packages, I still have to make changes to the code so that it works in computers that dont already have rgeos, rgdal, and maptools installed. 

----
Description of folders: 
 
- **data/** 

Includes table with the studies and community data included in the analyses, as well as a table with studies that had to be removed from the analyses (among other reasons for not presenting number of species with bee flowers)

- **files_for_maps/** 

Some files used as a reference for plotting maps.

- **plots/** 

A folder to organize plots for visual inspection of data and results. 

- **spatial_regression_results/**

Some results from correlation analyses.

- **TWDG/**

 
- **WWF_ecoregions/** 


- **results/** 

  

----
Scripts:

> 00_utility.R

Functions that can be sourced to perform tasks in other scripts.

> 01_map_mismatch.R


> 02_map_surveys.R


> 03_extracting_env_means.R


> 04_spatial_corr_biome_vars.R


> 05_spatial_corr_env_vars.R


> 06_spatial_corr_biome_interaction.R


> 07_spatial_corr_env_vars_interaction.R



----
  
**Climate layers from:**
  
Karger, et al. (2017). Climatologies at high resolution for the earth’s land surface areas. Scientific data, 4(1), 1-20.  
  
Trabucco, A., & Zomer, R. (2018). Global aridity index and potential evapo- transpiration (ET0) climate database v2. CGIAR Consortium for Spatial Information (CGIAR-CSI). Published online, available from the CGIAR-CSI GeoPortal at https://cgiarcsi.community
  
  
**Filtered GBIF points from:**
  
Dorey, et al. (2023). A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data, 10(1), 747.

