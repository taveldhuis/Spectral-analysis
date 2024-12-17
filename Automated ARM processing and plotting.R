#ARM Spectra loading and processing
# this clears all previous data in the R session
rm(list=ls());gc()
dev.off()
#load relevant packages
library(pavo)
library(tidyverse)
library(reshape2)
library(data.table)


#set your working directory. Make sure there are ONLY .dat files from the ARM in the directory
setwd("C:/Users/P309883/Downloads/today/20241111")

#here we set the variables determined by the ARM measurement protocol. The variables should match those specified in the Matlab script
anglim<- 60
dstep<-10
Species<- "Various"
wl<- c(300:1000)
#Set illumination angle for later plotting
Illum="0"
#set the wavelength at which you want to plot your angles
plotwl<- 800

#Spectra loading and mallnames#Spectra loading and manipulation. Collapse this part to skip to the next section
#####
#now that we have our variables, we can make some lists that we need later
# the first is a list that will form the column names of our dataset later on
angle_list<- c("wl",paste("M_Angle",seq(-anglim,anglim,dstep), sep="_"))

# import our spectra and make a list of their file names (illumangle)
files = list.files(pattern="*.dat", recursive= TRUE, include.dirs = FALSE)
fileslength<- length(files)

#Here we edit the files list in such a way that we remove the .dat . Very convoluted and probably inefficient, but it works.
X<- c(1:(length(files)))
Y<- as.data.frame(X)
Y$files<- files
#if you run into issues, change the numbers
Y$filesshort = substr(Y$files,1,nchar(Y$files)-4)
finalfiles<- as.vector(Y$filesshort)

dataset = do.call(cbind, lapply(files, fread))
# transform data to df
dataset <- as.data.frame(unclass(dataset))

#Now that we have a single dataframe with all spectra, we need to give each spectrum a unique name indicating measurement angle and illumination angle. for this we combine our angle_list and file_list. We then use a loop to make one giant list of all column names that should be the same length as our data
allnames<- vector()
for (i in 1:fileslength){name<- finalfiles[i]
cnames<- paste(name,angle_list, sep="_") 
allnames<- c(allnames,cnames)
}

#assign the list of names #assign the list of names #assign the list of names as our colnames
colnames(dataset)<- allnames

#remove all the wl columns that are duplicate and add one new one
specs<- dataset %>% select(!contains("wl"))
specs$wl<- wl

#we can now turn this into a pavo file and do our normal smoothing and processing
rspecdata<- as.rspec(specs)

#explore the data by plotting all measurements
plot(rspecdata)

##maybe skip the next step, as the ARM script seems to already smooth
#(Potentially skip) Now we transform the data. First step is to smooth out any noise. We want to remove as much noise while keeping the smoothing factor as low as possible to retain our data
#plotsmooth(rspecdata, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)

#Now we can see the effect of the different smoothing factors, we select and apply one. I typically use 0.3
#smoothspec <- procspec(rspecdata, opt = "smooth", span = 0.2) 
smoothspec<- rspecdata
#plot(smoothspec, ylim=c(0,1))

#If needed we can remove negative values using this line
pdat0<- procspec(smoothspec, fixneg = "addmin") 


#Remember to set your working directory to where you want to save the coming file. If you dont, the whole script stops working, as you are changing the number of files in your directory

#make an excel file of the created dataset
#write.csv(pdat0, paste(Species,"data_Casper.csv"))
#####


##### Creating a plotting environment
#Only use next line if plotting 2 spectra next to each other
#par(mfrow=c(1,2))

#Read the desired csv file, or replace with an object created in the last section
pdat0<- read.csv("ARM Casper 17-10-2024/Anthuriumlight 800nm ARM data.csv",header=T,sep=";")

#This selects data with the right illumination angle 
pdatR<- pdat0 %>% select(contains(paste("angle",Illum,sep="")))

#sometimes column names are read wrong when loaded from excel, where - is replaced with .
#These two lines switch them back
correctnames<- gsub("\\.", "-", colnames(pdatR))
colnames(pdatR)<- correctnames

#select the specific wavelength we want to plot, and transform the data so that we can compare measurement angle and reflectance
test<- pdatR %>% filter(wl==plotwl)
long <- t(test)
long2<- as.data.frame(long)
colnames(long2)<- c("reflectance")
long2$M_angle<- c(rownames(long2))
long2$Angle<- gsub(".*_","",long2$M_angle)
long2$Group<- sub("\\/.*", "", long2$M_angle)
long2$M_angle<- NULL

pdat<- reshape(long2, idvar = c("Angle"), timevar = "Group", direction = "wide")

#extract the maximum value for later use
pmax<- max(pdat[,c(2:ncol(pdat))])

#here we calculate and edit a cosine function to compare to measurements
angle<- c(-anglim:anglim)
Rangle<- angle*(pi/180)

#edit the value before cos(Rangle) to change line height
cosine<- pmax*cos(Rangle) 
Cosobj<-as.data.frame(cosine)
Cosobj$Angle<- angle


#Plotting individual measurements. Use next section for plotting more than one
#####
#First we create an empty space to plot in
plot(x=long2$Angle, y=long2$reflectance, ylim = c(0, 1.1),col=factor(long2$Group), type = "n", xlab = "Measurement Angle", ylab = "Reflectance (at 800 nm)", main = paste(Species,"800 nm Illumangle",Illum,sep=" "),bty= "l",las=1,pch=19)


#add points and line
#change y variable to change the plotted line, keep x (pdat$Angle)
#When plotting several measurements (so more than one line) use pdat$measurementname for x
lines(pdat$Angle, long2$reflectance, pch = 19, col = "blue", type = "b", lty = 1,lwd=2)
#we add the cosine line
lines(Cosobj$Angle, Cosobj$cosine, pch = 19, col = "black", type = "b", lty = 1)

#fix axis
axis(1, at = seq(-70, 100, by = 10), las=1)

#optional legend settings
#legend("topright", legend=c("Dark 1","Dark 2","Light 1","Light 2"), col=c("blue", "purple","green"), lty=1, cex=0.8, box.lty=0,)
######

#This section is for plotting several measurements at the same time
#First we create an empty space to plot in
plot(x=long2$Angle, y=long2$reflectance, ylim = c(0, 1.1),col=factor(long2$Group), type = "n", xlab = "Measurement Angle", ylab = "Reflectance (at 800 nm)", main = paste(Species,"800 nm Illumangle",Illum,sep=" "),bty= "l",las=1,pch=19)


#add points and line
#change y variable to change the plotted line, keep x (pdat$Angle)
#default y values are column numbers, column names would also work, but is not automated
#When plotting several measurements (so more than one line) use pdat$measurementname for y
lines(pdat$Angle, pdat[,2], pch = 19, col = "blue", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,3], pch = 19, col = "red", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,4], pch = 19, col = "green", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,5], pch = 19, col = "black", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,6], pch = 19, col = "purple", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,7], pch = 19, col = "firebrick", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,8], pch = 19, col = "yellow", type = "b", lty = 1,lwd=2)

#we add the cosine line
lines(Cosobj$Angle, Cosobj$cosine, pch = 19, col = "black", type = "b", lty = 1)

#fix axis
axis(1, at = seq(-70, 100, by = 10), las=1)

#optional legend
legend("topright", legend=c("Dark 1","Dark 2","Light 1","Light 2"), col=c("blue", "purple","green"), lty=1, cex=0.8, box.lty=0,)





