# Replication Project
## Modeling the Non-linear Effect of Temperature on Economic Production
### University of Washington, DATA 598A Winter 2020


# Contributors: 
[Bianca Zlavog](https://github.com/zlavogb) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-6868-7265)

[Iacopo Garizio](https://github.com/igarizio) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-8431-516X)

[Advika Battini](https://github.com/advika18) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-1801-6484)

[James Lee](https://github.com/jameslee0920)

# Contents:
In this repository, our team aims to upload the code and data used to replicate some of the key figures and claims in the following paper:

Burke, M., Hsiang, S. & Miguel, E. Global non-linear effect of temperature on economic production. Nature 527, 235–239 (2015). [https://doi.org/10.1038/nature15725](https://doi.org/10.1038/nature15725)

This article seeks to model the relationship between gross domestic product per capita and climate change, discovering a quadratic relationship between climate variables such as temperature and precipitation with economic growth. The authors predict a 23\% decrease in global economic output by 2100 given “business-as-usual” emissions scenarios, relative to forecasts without climate change. Also of interest is the disproportionate negative economic impacts on lower-income countries which are typically warmer, compared to high-income countries which have lower temperatures; the authors predict slight economic gains for the wealthiest 20\% of countries, in contrast with decreases of 75\% for the poorest 40\% of countries.

We hope to replicate figures 2a and 3a from the paper. The first of these depicts the non-linear relationship between global temperatures and change in gross domestic product per capita, with incomes peaking around 13 degrees Celsius. This figure also points out where some of the major countries are located on this curve, and how population and economic output are distributed at various levels of temperature. The second figure we hope to replicate demonstrates how scenarios of large temperature increases with high economic growth will impact the projected GDP per capita of countries around the world, particularly resulting in a wider distribution than in scenarios without climate change, and disproportionately affecting lower-income countries.


# Data:
The data for this project are provided by the authors through the [Stanford Digital Repository](https://purl.stanford.edu/wb587wt4560).

# Dependencies:

Software: R, Stata

R packages used:

|  name      | version |
| ----------- | ----------- |
| maptools | 0.9-9 |
| fields | 10.0 |
| classInt | 0.4-2 |
| plotrix | 3.7-7 |
| dplyr | 0.8.3 |
| ncdf | x |
| maps | 3.3.0 |
| raster | x |
| data.table | x |
| wq | x |
| ggplot2 | x |
| xtable | x |
| reshape2 | x |
| rworldmap | x |
| spam | 2.5-1 |

R version 3.6.2 (2019-12-12)

Platform: x86_64-w64-mingw32/x64 (64-bit)

Running under: Windows Server x64 (build 14393)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
grid      
stats     
graphics  
grDevices utils     
datasets  
methods  
base     

other attached packages:
dplyr_0.8.3     
plotrix_3.7-7  
classInt_0.4-2  
fields_10.0    
maps_3.3.0      
spam_2.5-1      
dotCall64_1.0-0 
maptools_0.9-9 
sp_1.3-2    

STATA packages used:

|  name      |
| ----------- |
| parmest |
| vallist |
| estout |

STATA external scripts used:
|  name      |
| ----------- |
| cgmreg |

STATA version Stata/MP 14
