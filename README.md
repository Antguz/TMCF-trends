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
statistics and figures were created locally using R.

Here you will see four main folders: i) a 'python' folder, which contains the 
scripts for API access to IBM PAIRS Geoscope, ii) an 'R' folder, which contains 
scripts of the statistics applied in the manuscript, iii) 'figures' folder that
contains the scripts for visualization, and iv) a 'data' folder as path to 
data for easy access. If you use these codes or data, please cite them in addition
to the supporting manuscript.

The data used in this manuscript that accompany these codes are available at the 
Tropi-Dry Dataverse (https://doi.org/10.7910/DVN/A25Q8V). If your interest is to 
use the location or names of the TMCFs please cite Aldrich et al. (1997) following 
the instructions provided by [UN Environment Programme World Conservation Monitoring Centre](https://resources.unep-wcmc.org/products/84c3a142c4354cc1a75ac9e9ee8538e2).

<br />

#### Citing codes, data, and manuscript.

##### Codes

```
@article{cloud_codes,
  author       = {Guzmán, J.A., Hamann, H. F., and Sánchez-Azofeifa, G.A.},
  title        = {Cloudiness has declined in 70% of the Tropical Montane Cloud Forests},
  month        = abr,
  year         = 2022,
  publisher    = {Zenodo},
  version      = {v0.1},
  doi          = {10.5281/zenodo.pending},
  url          = {https://doi.org/10.5281/zenodo.pending}
}

```

##### Manuscript

```
@article{cloud_manuscript,
  author       = {Guzmán, J.A., Hamann, H. F., and Sánchez-Azofeifa, G.A.},
  title        = {Cloudiness has declined in 70% of the Tropical Montane Cloud Forests},
  journal      = {pending},
  year         = {pending},
  volume       = {pending},
  pages        = {pending}
  doi          = {pending}
}

```

##### Data

Please follow the instructions from [Tropi-Dry Dataverse](https://doi.org/10.7910/DVN/A25Q8V)
