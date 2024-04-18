# feedingMarkAnalyzeR  

This R package is a part of an image analysis pipeline for quantification of insect feeding parameters on leaf (cite-publication-here). It loads masked leaf images and `.xls` files that are produced by ImageJ macro [feeding_mark_analyzer]().  




## Installation

Installing packages from GitHub requires `devtools` package. 
```
# Only if you have not installed devtools before.
install.packages("devtools")
```

To load devtools and install `feedingMarkAnalyzeR`, run the following:  
```
library(devtools)
install_github("nsotta/feedingMarkAnalyzeR", dependencies = TRUE, build_vignettes = TRUE)
```

## Usage  

The usage of `feedingMarkAnalyzeR` is documented as an R vignette. 
Load `feedingMarkAnalyzeR` by 

```
library(`feedingMarkAnalyzeR`)
```
and run
```
browseVignettes("feedingMarkAnalyzeR")
```
to open the vignette.

