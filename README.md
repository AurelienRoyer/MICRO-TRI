# microTRI

Spatial Exploration of ArcHaeological Objects in R Shiny

[![DOI](https://zenodo.org/badge/811219296.svg)](https://zenodo.org/doi/10.5281/zenodo.11935939)
[![CRAN
Version](http://www.r-pkg.org/badges/version/microTRI)](https://cran.r-project.org/package=SEAHORS)


  - [**Presentation**](#presentation)
  - [**Installation**](#installation)
 
  - [**Major news**](#Major-news)

## Presentation

`microTRI` is a R Shiny free open-source script dedicated to the record of small vertebrate archaeological and palaeontological remains.
   It makes it possible to record easily small vertebrate remains with taxonomical and taphonomical information,
   ssociated with field data information. It makes also possible to visualise quick information recorded by levels, square or spit.
   This is an open and free software, 

`microTRI` has an easily accessible interface, allowing to record data in Excel
files (XLS respectively). The application includes functions to
visualize premiliminary data as Bioclim estimation, rarefaction curves, taphonomical proportions.


## Installation

  - Install [R](https://www.r-project.org) and optionally [Rstudio
    Desktop](https://posit.co/download/rstudio-desktop/) to have a more
    comfortable R environment.
  

  - Install the `microTRI` development version from github:

<!-- end list -->

``` r
# install.packages("devtools")
devtools::install_github("AurelienRoyer/MICRO-TRI")
```

Load the package and launch the `microTRI` application:

``` r
library(microTRI)
setwd("path_of_your_directory")
microTRI()
```


## a starting manual
-sidebarpanel : acquisition
1) Create a database 
![Figure 1-01](https://github.com/user-attachments/assets/da39f2f3-e13c-47df-bc11-e0cd0262a4d1)

2) Record the information
   -first : click on "new bag" to open a windows allowing to enter information of year, sector, ID, square, spit, level and US.
   -second : enter the information of the remain (species, anatomy, number of remain, completeness, observations, marks ...
   -third : click on "new line" to record it.
   possibility of modify the last recording by clicking on "previous", to developp a table to show the records,
   
![Figure 2-01](https://github.com/user-attachments/assets/ae4cc56a-a8a0-4c5e-88c0-b646e5dd620b)

the panel "table" allows to have a look of the table, to search information, to delete row and to modify a cell

![figure 3-01-01](https://github.com/user-attachments/assets/0f715e92-f271-4035-9b74-b25878d4df39)

-sidebarpanel : exploration
To realize quick exploration of the data. 
With the sidebarpanel, it is possible to select a variable and to select the modalities desired 
Different exploration are available : pivot table, digestion ratio, bioclim estimations ... 

![Figure 4-01](https://github.com/user-attachments/assets/b1bd1214-d107-424d-8adf-60072148eccc)

-sidebarpanel : note 
allow to record global notes of the observation and (not yet but soon) to export it

 ##  Major news 
 still in development
