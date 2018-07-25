# Useful R code (28-6-18)

### Put plot in function to take dynamic data inputs
#### Ref: http://jcborras.net/carpet/visualizing-political-divergences-2012-local-elections-in-helsinki.html
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


### loop through files and append to list
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

### Optimal empty data frame
df <- data.frame(Date=as.Date(character()),
                 X=numeric(), 
                 Y=integer(), 
                 stringsAsFactors=FALSE) 

### Plot inset plot in current plot (https://stackoverflow.com/questions/17041246/how-to-add-an-inset-subplot-to-topright-of-an-r-plot)
#### calculate position of inset
plotdim <- par("plt")# get plot window dims as fraction of current plot dims 
xleft    = plotdim[2] - (plotdim[2] - plotdim[1]) * 0.5
xright   = plotdim[2]  #
ybottom  = plotdim[4] - (plotdim[4] - plotdim[3]) * 0.5  #
ytop     = plotdim[4]  #

#### set position for inset
par(fig = c(xleft, xright, ybottom, ytop),mar=c(0,0,0,0),new=TRUE)

boxplot(Eggs~Size,data=meso2,
                col=adjustcolor(colfunc[colvec[1:3]],alpha=0.5),
                notch = T,xlab="Week",ylab="Diameter (mm)",
                xaxs = "i", yaxs = "i"
                ) 

### optimal legend formatting for base
legend("right",legend=c("Small","Intermediate","Large"),col=c(colfunc[colvec[1:3]]),
       bty="n",pch=20,pt.cex=1.5,cex=0.7,y.intersp = 0.5, xjust = 0.5,
       title="Snail size class",title.adj = 0.3,text.font=2,
       trace=T,inset=0.1)

### bookend axis ticks for plot
#### e.g. at 0 and 100 when data is 1:99
axis(1,at=c(0,length(loco$X)),labels=c("",""))# bookending axis tick marks

### plot one plot window above and two below
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

### set dynamic input for variable / assign variable to char vector 
shadedens<-function(shadedens){ # set shade density to clumped (to match food) or sparse 
  if (shadedens == "Random"){
    NLCommand("set Shade-density \"Random\" ") 
    }else{
    NLCommand("set Shade-density \"Clumped\" ") 
    }
  }
shadedens("Clumped") # set clumped resources


### select rows of sfeed_move not in foodh
library(sqldf)
a1NotIna2_h  <- sqldf('SELECT * FROM sfeed_move EXCEPT SELECT * FROM foodh')
a1NotIna2_l  <- sqldf('SELECT * FROM sfeed_move EXCEPT SELECT * FROM foodl')
# select rows from sfeed_move that also appear in foodh
a1Ina2_h  <- sqldf('SELECT * FROM sfeed_move INTERSECT SELECT * FROM foodh')
a1Ina2_l  <- sqldf('SELECT * FROM sfeed_move INTERSECT SELECT * FROM foodl')

methods(class="estUDm")

### check for NAs
sapply(df, function(x) sum(is.na(x)))

#-------------------- 11-7-17
#replace NA's with 0's
df[is.na(df)] <- 0
#replace X values less than given value (V) with 0
df$X[df$X<V] <- 0 

#--------------------  7-7-17. /Users/malishev/Documents/Manuscripts/Chapter4/Sims/Chapter4_figs.R
# fill in missing data values in sequence with NA
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

#-------------------- 11-01-17
#convert character to factor to numeric without conversion error
read.csv(f,header=F,sep=",",row.names=NULL,stringsAsFactors=FALSE, strip.white=TRUE)
f$V2<-as.numeric(f$V2)

#-------------------- 29-4-16
## Reading in files/data
# read in file manually 
get.file.vol <- read.table(file.choose())#read file manually
v.file <- get.file.vol[1:100,1]#get the volume

########################################

## `ggplot` functions
### Remove gridlines
plot + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#### alternative (after loading ggridges library)
theme_ridges(grid=F,center_axis_labels = T)

### Setting global graphics theme for ggplot

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




                            
                            

