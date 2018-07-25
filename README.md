# Useful R code 

This document outlines some useful R code for plotting, cool functions, and other random tidbits.   


## Overview

This document outlines some useful R code for plotting, cool functions, and other random tidbits.   

### Install dependencies
```{r, load packages, include=F, cache=F, message=F}
packages <- c("rgdal","dplyr","zoo","RColorBrewer","viridis","plyr","digitize","jpeg","devtools","imager","dplyr","ggplot2","ggridges","ggjoy","ggthemes","svDialogs","data.table","tibble","extrafont","sp")   
if (require(packages)) {
    install.packages(packages,dependencies = T)
    require(packages)
}
lapply(packages,library,character.only=T)
```

### Classes
Convert character to factor to numeric without conversion error
```{r, classes1, results='hide',eval=F}
read.csv(f,header=F,sep=",",row.names=NULL,stringsAsFactors=FALSE, strip.white=TRUE)
f$V2<-as.numeric(f$V2)
```
