# Florida Panther Den Site Selection
## data prep
RandomPts_FireCat: generate random points for each HR
Combine Fire Env: combine Den - fire category from previous analysis with new env extractions
Combine_gis_EE: Combine env extraction from GEE output and arcGIS output

## statistic analysis
Panther GLMM (and output) - first pass of applying GLMM. But encounter singularity issue - random effect factor has too many levels
Panther cLogit (and output) - applying conditional logistic regression. 

Try out different random point sizes (10, 50, and 97). The final model is consistent, the fire effect are relatively consistent but the 97 pts version better differentiate ABU and CD.

Last updated: April 20, 2020