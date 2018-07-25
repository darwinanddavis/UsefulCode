# Useful R code 

Files:  
.R  
.Rmd  
.html  

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

See call options for class
```{r, classes2,eval=F}
methods(class="estUDm")
```

Set dynamic input for variable / assign variable to char vector 
```{r, classes3, results='hide',eval=F}
shadedens<-function(shadedens){ # set shade density to clumped (to match food) or sparse 
  if (shadedens == "Random"){
    NLCommand("set Shade-density \"Random\" ") 
    }else{
    NLCommand("set Shade-density \"Clumped\" ") 
    }
  }
shadedens("Clumped") # set clumped resources
```

### Dataframes  
Optimal empty data frame  
```{r, dataframes1}
df <- data.frame(Date=as.Date(character()),
                 X=numeric(), 
                 Y=integer(), 
                 stringsAsFactors=FALSE) 
```

#####
########################################

### `ggplot` functions  
Remove annoying stock gridlines from plot window  
```{r, ggplot1, results='hide',eval=F}
plot + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# alternative (after loading ggridges library)
theme_ridges(grid=F,center_axis_labels = T)
```

Setting global graphics theme for ggplot
```{r, ggplot2, results='hide',eval=F}
plot_it_gg <- function(bg,family){ # bg = colour to plot bg, family = font family
  theme_tufte(base_family = family) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = bg,
                                        colour = bg),
        plot.background = element_rect(fill=bg)
  ) +
    theme(axis.line = element_line(color = "white")) +
    theme(axis.ticks = element_line(color = "white")) +
    theme(plot.title = element_text(colour = "white")) +
    theme(axis.title.x = element_text(colour = "white"), 
          axis.title.y = element_text(colour = "white")) +
    theme(axis.text.x = element_text(color = "white"),
          axis.text.y = element_text(color = "white")) +
    theme(legend.key = element_rect(fill = bg)) + # fill bg of legend
    theme(legend.title = element_text(colour="white")) + # legend title
    theme(legend.text = element_text(colour="white")) # legend labels
} 
```

Put plot in function to take dynamic data inputs  
Ref: http://jcborras.net/carpet/visualizing-political-divergences-2012-local-elections-in-helsinki.html
```{r, ggplot3, results='hide',eval=F}
hr.mass.plot <- function(d) {
  p <- ggplot(d, aes(HR, Mass, color = colfunc)) + 
    geom_density_2d(data=d, aes(x = HR, y = Mass), 
                    stat = "density2d",position="identity", 
                    color=adjustcolor("orange",alpha=0.8), size=1.5, contour = T, lineend="square",linejoin="round") 
  p <- p + geom_point(data=d, aes(x = HR, y = Mass),
                      color=colfunc,
                      fill=colfunc) +
    scale_color_manual(values = magma(8))
  p <- p + scale_y_continuous(limits=c(-200,200), name="Mass lost (g)") 
  p <- p + scale_x_continuous(limits=c(0,0.35),name=expression("Home range area (km^2)")) 
  p <- p + theme_classic()
  print(p)
}
hr.mass.plot(d)
```
#####
########################################

### NAs 
Replace NAs with 0's
```{r, NAs1}
df[is.na(df)] <- 0
```

Replace X values less than given value (V) with 0  
```{r, NAs2, results='hide',eval=F}
df$X[df$X<V] <- 0 
```

Check for NAs
```{r, NAs3}
sapply(df, function(x) sum(is.na(x)))
```

Replace NaN and Inf values with NA 
```{r, NAs4}
df$col1[which(!is.finite(df$col1))] <-  NA
```

Fill in missing data values in sequence with NA
```{r, NAs5, results='hide',eval=F}
# /Users/malishev/Documents/Manuscripts/Chapter4/Sims/Chapter4_figs.R
library(zoo)
data <- data.frame(index = c(1:4, 6:10),
  data = c(1.5,4.3,5.6,6.7,7.1,12.5,14.5,16.8,3.4))
#you can create a series
z <- zoo(data$data, data$index)
#end extend it to the grid 1:10
z <- merge(zoo(,1:10), z)

#worked example
# fill in missing Tb values 
minTb.d <- zoo(minTb$Tick,minTb$Days)
minTb.d <- merge(zoo(NULL,1:days), minTb.d) # make the minTb series match the temp series (117 days)
minTb.d <- as.numeric(minTb.d) # = time individuals reached VTMIN in ticks
minTb <- minTb.d - temp$Tick # get diff between starting time and time to reach VTMIN
minTb <- minTb/2 # convert ticks to minutes
minTb <- minTb/60 #convert to hours
minTb <- data.frame("Days"=1:days,"Time"=minTb)

# then fill in missing values
approx(minTb$Time,method = "linear")
```

### Plotting
Plot one plot window above and two below
```{r, plotting1}
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
```

Bookend axis ticks for plot
#### e.g. at 0 and 100 when data is 1:99
```{r, plotting2, results='hide',eval=F}
axis(1,at=c(0,length(loco$X)),labels=c("",""))# bookending axis tick marks
```

Optimal legend formatting for base
```{r, plotting3, results='hide',eval=F}
legend("right",legend=c("Small","Intermediate","Large"),col=c(colfunc[colvec[1:3]]),
       bty="n",pch=20,pt.cex=1.5,cex=0.7,y.intersp = 0.5, xjust = 0.5,
       title="Size class",title.adj = 0.3,text.font=2,
       trace=T,inset=0.1)
```

Plot inset plot in current plot (https://stackoverflow.com/questions/17041246/how-to-add-an-inset-subplot-to-topright-of-an-r-plot)
```{r, plotting4, results='hide',eval=F}
# calculate position of inset
plotdim <- par("plt")# get plot window dims as fraction of current plot dims 
xleft    = plotdim[2] - (plotdim[2] - plotdim[1]) * 0.5
xright   = plotdim[2]  #
ybottom  = plotdim[4] - (plotdim[4] - plotdim[3]) * 0.5  #
ytop     = plotdim[4]  #

# set position for plot inset
par(fig = c(xleft, xright, ybottom, ytop),mar=c(0,0,0,0),new=TRUE)

boxplot(Eggs~Size,data=meso2,
                col=adjustcolor(colfunc[colvec[1:3]],alpha=0.5),
                notch = T,xlab="Week",ylab="Diameter (mm)",
                xaxs = "i", yaxs = "i"
                ) 
```
                
### Reading in files/data
Read in file manually
```{r, read1, results='hide',eval=F}
get.file.vol <- read.table(file.choose())#read file manually
v.file <- get.file.vol[1:100,1]#get the volume
```


Loop through files from dir and append to list  
``` {r, read2, results='hide',eval=F}
# reading in spdf (hrpath) files from drive
setwd("/Users/camel/Desktop/Matt2016/Manuscripts/MalishevBullKearney/Resubmission/2016/barcoo sims/barcooresults/hrpath_75")
file.list<-list.files()
hrs75<-as.list(rep(1,100)) # empty list
for (f in 1:100){
  load(file.list[f])
  hrs75[f]<-hrpath
}

# working version
#converting spdf into mcp(spdf,100,unout="m2)
ghr<-list()
for (i in hrs75[1:10]) {
  m<-mcp(i,100,unout='m2')
  ghr<-c(ghr,m)
};ghr
```

### Subsetting
Select specific rows 
E.g. select rows of sfeed_move not in foodh
```{r, subset1, results='hide',eval=F}
library(sqldf)
a1NotIna2_h  <- sqldf('SELECT * FROM sfeed_move EXCEPT SELECT * FROM foodh')
a1NotIna2_l  <- sqldf('SELECT * FROM sfeed_move EXCEPT SELECT * FROM foodl')
# select rows from sfeed_move that also appear in foodh
a1Ina2_h  <- sqldf('SELECT * FROM sfeed_move INTERSECT SELECT * FROM foodh')
a1Ina2_l  <- sqldf('SELECT * FROM sfeed_move INTERSECT SELECT * FROM foodl')
```

#####  

### R Markdown

Hide unwanted code output, such as inherent examples for functions  
```{r, cache = TRUE, tidy = TRUE, lazy = TRUE, results='markup'}

# ```{r, cache = TRUE, tidy = TRUE, lazy = TRUE, results='markup'}

```


### Web scraping
http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/XML/html/readHTMLTable.html[http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/XML/html/readHTMLTable.html]
```{r, web1, results='hide',eval=F}
library(XML)
readHTMLTable()
```  


