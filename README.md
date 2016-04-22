## Comparative Study in Grass Flammability

### Project goal:  

* Investigate variation in flammability among various grass species  
* Determine what plant traits lead to grass flammability variation  
* Uncover mechanisms that lead to enhanced flammability evolution  

## Data  

Each data file referred to has an associate machine readable metadata file. If the file is named as file.csv, the metadata file is file-metadata.csv. The metadata files describe each variable(column) in the data files.  

### Sample data 

#### species  
data/species.csv file contains information about studied species, which includes family, genus, specific epithet, 4-5 digits USDA species code, Sp.ID for labeling sample, display name, photosynthesis type, number of pair collected and sampling area. Along with it is the metadata for describing each variable in species.csv file.  
#### specimens georeference 

During 2015 summer, study was mainly conducted in eastern New Mexico in Kiowa National Grassland (but we had one trip to Oklahoma, Black Kettle National Grassland), and we collected 10 species with one of them (*Eragrostis curvula*) collected in OK. For each species, we collected 8-10 paired individuals.  

data/specimens-locations.csv file stores Sample ID, date, area, latitude and longitude and memo (site description).  

### Plant traits   

#### Leaf trait and canopy architecture  

For each paired samples, we collected ~25g sub-sample for measuring field moisture content. Plant moisture content will also be measured right before burning trial on paired samples by taking leaf samples with minimizing damage to plant structure. Leaf area will be scanned,leaf thickness will be taken as well. Data is stored in data/specimens-leaf-traits.csv, but fuel moisture data is not included here.

For *Stipa* with needle leaves, leaf area was measured by taking two diameters of needle which perpendicular to each other, and needle length as well. Data is stored in ./leaf-measurements-for-stipa.csv

For paired samples, plant height and width, tiller number and diameter at plant base were taken and stored in ./canopy-sections.csv with one row per sample. Data related to fuel moisture content also stored along with sections canopy data.
 
#### Biomass density  

One of each paired samples was cut at every 5cm interval along 
its height. Fresh mass of each section was taken, as well as oven dry weight and corresponding height. Data is stored in data/biomass-density.csv with one row per sample.  

#### Burning  trial  

Burning  data contains three files:
  
* HOBO data which contains flame temperature(C) at 4 different heights: soil surface, 10cm, 20cm and 40cm. It is stored in data/hobo/.csv, named as trial data joined height measured
* Balance data which contains fuel biomass during burning for every second and it is stored in ./balance/.csv
* Burning trials measurements. For this, I recored trial date, trial reference number which based on order, sample ID, temperature(F), humidity and wind speed right before each trial, start time for each trial, initial weight(pot and plant) and weight after trial(pot only, fuel residual was removed), also weight before burn on burn table (pot, plant and any possible introduced pressure on scale) and weight after burn on burn table (pot, fuel residual and any possible introduced pressure) were taken.   
Flammability measurements are maximum flame height, time to ignition, flaming and glowing duration. The time point when cotton ball was dropped is recored also, fuel residual collected from soil surface was weighted. Memo is written to record any special events happen during burn, for instance, a gust of wind occurred during burn or some fuel fell off to the ground.  

