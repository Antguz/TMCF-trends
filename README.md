[![DOI](https://zenodo.org/badge/422720051.svg)](https://zenodo.org/badge/latestdoi/422720051)

# TMCF-trends

<br />

These codes are used to estimate trends of low-cloud fraction at the Tropical 
Mountain Cloud Forest. These are also used to estimate trends of other Essential 
Climatic Variables (ECVs).

The codes are the base of the manuscript 'Cloudiness has declined in 70% of the 
Tropical Montane Cloud Forests' by Guzmán, Hamann, and Sánchez-Azofeifa.

Trends of low-cloud fraction and other ECVs were optioned from [ERA5](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview) 
through [IBM PAIRS Geoscope](https://www.ibm.com/products/environmental-intelligence-suite/geospatial-analytics) 
using Python as an [API](https://pairs.res.ibm.com/tutorial/index.html) platform. Then,
statistics and figures were created locally using R or QGIS (Figure 1).

Here you will see three main folders: i) a 'python' folder, which contains the 
scripts for API access to IBM PAIRS Geoscope, ii) an 'R' folder, which contains 
scripts of the statistics applied in the manuscript, and iii) 'figures' folder that
contains the scripts for visualization. A fourth folder of 'data' could be create 
as path for easy access to data to reproduce these codes. If you use these codes, 
please cite them in addition to the supporting manuscript.

The data used in this manuscript that accompany these codes are available at the 
Tropi-Dry Dataverse (https://doi.org/10.7910/DVN/A25Q8V). Please follow the 
instructions described there for proper citation of the data.

<br />

#### Citing codes, manuscript, and data.

##### Codes

```
@codes{cloud_codes,
  author       = {Guzmán, J.A., Hamann, H. F., and Sánchez-Azofeifa, G.A.},
  title        = {Multi-decadal trends of low clouds at the Tropical Montane Cloud Forests},
  month        = abr,
  year         = 2023,
  publisher    = {Zenodo},
  version      = {v0.2},
  doi          = {10.5281/zenodo.6476817},
  url          = {https://doi.org/10.5281/zenodo.6476817}
}

```

##### Manuscript

```
@article{cloud_manuscript,
  author       = {Guzmán, J.A., Hamann, H. F., and Sánchez-Azofeifa, G.A.},
  title        = {Multi-decadal trends of low clouds at the Tropical Montane Cloud Forests},
  journal      = {pending},
  year         = {pending},
  volume       = {pending},
  pages        = {pending}
  doi          = {pending}
}

```

##### Data

Please follow the instructions from [Tropi-Dry Dataverse](https://doi.org/10.7910/DVN/A25Q8V)
