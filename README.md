# Natural History of Song
This repo contains code for the Natural History of Song project (Mehr et al., 2019, Science). The Data and Materials Availability statement from the paper is copied below. To reproduce our analyses you will need some or all of these files.

>All Natural History of Song data and materials are publicly archived at http://osf.io/jmv3q, with the exception of the full audio recordings in the NHS Discography, which are available via the Harvard Dataverse, at https://doi.org/10.7910/DVN/SESAO1. All analysis scripts are available at http://github.com/themusiclab/nhs. Human Relations Area Files data and the eHRAF World Cultures database are available via licensing agreement at http://ehrafworldcultures.yale.edu; the document- and paragraph-wise word histograms from the Probability Sample File were provided by the Human Relations Area Files under a Data Use Agreement. The Global Summary of the Year corpus is maintained by the National Oceanic and Atmospheric Administration, United States Department of Commerce, and is publicly available at https://www.ncei.noaa.gov/data/gsoy/.

For those replicating analyses using the eHRAF Probability Sample File data, you will need to build `rds` files as per code in `script2_compare_psf_final.R`. If you run into issues, please contact us.

## Details
All analyses in the paper can be reproduced with the code posted here, in R and Python. The pipeline for visualizations takes `csv` output from R, processes it in Stata, and then produces visualizations in R. Some figure elements are augmented manually (e.g., adding some labels) and/or include illustrations, so your reproduced figures will not match those in the paper exactly.

[![DOI](https://zenodo.org/badge/206319619.svg)](https://zenodo.org/badge/latestdoi/206319619)
