Comparative Study in Grass Flammability
=======================================

Project goal
------------


- Investigate variation in flammability among various grass species  
- Determine what plant traits lead to grass flammability variation  
- Link plant life history strategy to flammability variation
- Determine ecological impacts of different grasses on woody plants  

Data
------

Each data file referred to has an associate machine readable metadata file. If the file is named as file.csv, the metadata file is file-metadata.csv. The metadata files describe each variable(column) in the data files. To separately store data collected in different year for different project, all data files are named in following format: 20xx-measurement-type.csv or 20xx-measurement-type-meatdata.csv

### Sample data 


#### species  
data/species.csv file contains information about studied species, which includes family, genus, specific epithet, 4-5 digits USDA species code, sp.id for labeling sample, display name, photosynthesis type, number of pair collected and sampling area. Along with it is the metadata for describing each variable in 20xx-species.csv file.  
#### specimens georeference 
For each plant, we log geographical location and store the data at data/20xx-specimen-location.csv along with metadata at data/20xx-specimen-location-metadata.csv

### Plant trait measurements   

#### Fuel moisture content and canopy architecture

For each paired samples, we collected ~25g sub-sample for measuring field moisture content. Plant moisture content will also be measured right before burning trial on paired samples by taking leaf samples with minimizing damage to plant structure. 

There are two traits used to characterize plant canopy architecture: biomass density and biomass height ratio.

- Biomass density: total above ground biomass/plant volume.

Total above ground biomass is measured for each plant. For unburned plants, it is directly measured by cutting the biomass at 10 cm plant height, oven-dry at 60 Celsius degree for 72 hours and weighed each section. For plants assigned for burning trials, it is measured  by weighing plants before burn, and after burn without fuel residuals. 
Plant width (two perpendicular width, measured 3 times for each at each locations and use averaged value) at base and canopy, and plant height is measured to calculate plant volume with assuming each plant as a truncated cone.

- Biomass height ratio: biomass above 10 cm plant height to biomass below 10 cm plant height

One of each paired samples is cut at 10 cm plant height. Dry mass of each section was taken. Due to its destructive characteristics, this trait is predicted for plants assigned for burning trials by measuring predictor traits on both paired plants, which include tiller number, total above ground biomass. Species is also included as one predictor. Model is trained with data from unburned plants and fit model then is used to predict the trait for plants assigned for burning with same predictors. 

Data for fmc and canopy traits is stored at data/20xx-canopy-fmc.csv along with meta data at data/20xx-canopy-fmc-metadata.csv

#### Leaf traits

- leaf area: 3 leaf samples are collected from each plant when fully 
expend. Leaf area is scanned and analyzed with imageJ. For grasses with needle or very fine leaves, to calculate leaf area, we measure two perpendicular diameters of 10cm-long needle section.

- leaf mass: collected lead samples are oven-dried after area measurement is taken, and then weighed to obtain dry mass.

Data is stored at data/20xx-leaf-traits.csv along with meta data at data/20xx-leaf-traits-metadata.csv


#### Burning  trial  

Burning  data contains three files:
  
- HOBO data which contains flame temperature(C) at 4 different heights: soil surface, 10cm, 20cm and 40cm. It is stored at data/hobo/.csv, named by trial date and location temperature is taken
- Balance data which contains fuel biomass during burning for every second and it is stored at data/balance/.csv
- Burning trials measurements. This file includes trial date, trial reference number which based on trial order, sample ID, temperature(F), humidity and wind speed right before each trial, start time for each trial, initial weight(pot, dirt, plant) and weight after trial(pot only, fuel residual was removed), also weight before burn on burn table (pot, dirt, fuel residuals, and below ground biomass) and final weight after burn (pot, dirt, and below ground biomass).
   
Flammability measurements are maximum flame height, time to ignition, flaming and glowing duration. The time point when ignited cotton ball was removed is recored also, fuel residual collected from soil surface was weighted.  

