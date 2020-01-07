# wkfishdish-manuscript-analysis

This repository holds code to reproduce the analysis of changes in the 
center of gravity in fish distribution and changes in relative 
distribution between adjacent geographical areas, as presented in the 
paper:

Changing fish distributions challenge the effective
management of European fisheries, by Baudron et al.

The analysis can be rerun in the R statistical platform. To rerun the analysis first install the `icesTAF` package

```r
install.packages("icesTAF")
```

then to recreate the dataset used, run:
```r
icesTAF::taf.bootstrap()
```

and to rerun the analysis
```r
icesTAF::sourceAll()
```
