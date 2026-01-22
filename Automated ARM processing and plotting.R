#ARM Spectra loading and processing. Run till 79 for simple data file without plotting
# this clears all previous data in the R session
rm(list=ls());gc()
#dev.off()
#load relevant packages
library(pavo)
library(tidyverse)
library(reshape2)
library(data.table)


#set your working directory. Make sure there are ONLY .dat files from the ARM in the directory
setwd("C:/Users/P309883/OneDrive - University of Groningen/Desktop/Ophyrs 2025/ARM/20250224")

#here we set the variables determined by the ARM measurement protocol. The variables should match those specified in the Matlab script
anglim<- 60
dstep<-10
#Set the name you want to appear in your plots
Species<- "O_eos"
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
#Now you have a completed smoothed datafile with all your measurements. The next sections provide various plotting options with this type of dataset.


#Remember to set your working directory to where you want to save the coming file. If you dont, the whole script stops working, as you are changing the number of files in your directory

#make an excel file of the created dataset
#write.csv(pdat0, paste(Species,"data_Casper.csv"))
#####


##### Creating a plotting environment
#Only use next line if plotting 2 spectra next to each other
#par(mfrow=c(1,2))

#Read the desired csv file, or replace with an object created in the last section
#pdat0<- read.csv("ARM Casper 17-10-2024/Anthuriumlight 800nm ARM data.csv",header=T,sep=";")

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


#This section is for plotting several measurements at the same time
#First we reset our plotting environment and then create an empty space to plot in
dev.off()
plot(x=long2$Angle, y=long2$reflectance, ylim = c(0, 1.2),col=factor(long2$Group), type = "n", xlab = "Measurement Angle", ylab = "Reflectance (at 800 nm)", main = "Raw",bty= "l",las=1,pch=19)
#add points and line
#change y variable to change the plotted line, keep x (pdat$Angle)
#default y values are column numbers, column names would also work, but is not automated
#When plotting several measurements (so more than one line) use pdat$measurementname for y
lines(pdat$Angle, pdat[,2], pch = 19, col = "blue", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,3], pch = 19, col = "darkblue", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,4], pch = 19, col = "green", type = "b", lty = 1,lwd=2)
lines(pdat$Angle, pdat[,5], pch = 19, col = "darkgreen", type = "b", lty = 1,lwd=2)

#Now we add a nice cosine
#Change the number before cos(Rangle) to change the maximum of the cosine line.
cosine<- 1*cos(Rangle) 
Cosobj<-as.data.frame(cosine)
Cosobj$Angle<- angle
lines(Cosobj$Angle, Cosobj$cosine, pch = 19, col = "black", type = "b", lty = 1)


#fix axis
axis(1, at = seq(-70, 100, by = 10), las=1)

#This next section is for normalizing your spectra to 1. You may not need it. Remember to change the names to those in your own Npdat object
Npdat<- pdat
Npdat$reflectance.O_eos_1<- (Npdat$reflectance.O_eos_1/(max(Npdat$reflectance.O_eos_1)))
Npdat$reflectance.O_eos_1_b<- (Npdat$reflectance.O_eos_1_b/(max(Npdat$reflectance.O_eos_1_b)))
Npdat$reflectance.O_eos_2<- (Npdat$reflectance.O_eos_2/(max(Npdat$reflectance.O_eos_2)))
Npdat$reflectance.O_eos_2_b<- (Npdat$reflectance.O_eos_2_b/(max(Npdat$reflectance.O_eos_2_b)))

#This section is for plotting several normalized measurements at the same time
#First we create an empty space to plot in
plot(x=long2$Angle, y=long2$reflectance, ylim = c(0, 1.2),col=factor(long2$Group), type = "n", xlab = "Measurement Angle", ylab = "Reflectance (at 800 nm)", main = "Normalised",bty= "l",las=1,pch=19)

#add points and line
#change y variable to change the plotted line, keep x (pdat$Angle)
#default y values are column numbers, column names would also work, but is not automated
#When plotting several measurements (so more than one line) use pdat$measurementname for y
lines(Npdat$Angle, Npdat[,2], pch = 19, col = "blue", type = "b", lty = 1,lwd=2)
lines(Npdat$Angle, Npdat[,3], pch = 19, col = "darkblue", type = "b", lty = 1,lwd=2)
lines(Npdat$Angle, Npdat[,4], pch = 19, col = "green", type = "b", lty = 1,lwd=2)
lines(Npdat$Angle, Npdat[,5], pch = 19, col = "darkgreen", type = "b", lty = 1,lwd=2)

#we add the cosine line
cosine<- 1*cos(Rangle) 
Cosobj<-as.data.frame(cosine)
Cosobj$Angle<- angle
lines(Cosobj$Angle, Cosobj$cosine, pch = 19, col = "black", type = "b", lty = 1)

#fix axis
axis(1, at = seq(-70, 100, by = 10), las=1)


### AVERAGES
Apdat<- pdat[,2:5]
Apdat$reflectance.O_eos_average<- apply(Apdat,1,mean)
Apdat$Angle<- pdat$Angle

#First we create an empty space to plot in
plot(x=long2$Angle, y=long2$reflectance, ylim = c(0, 0.9),col=factor(long2$Group), type = "n", xlab = "Measurement Angle", ylab = "Reflectance (at 800 nm)", main = "Average",bty= "l",las=1,pch=19)

#add points and line
#change y variable to change the plotted line, keep x (pdat$Angle)
#default y values are column numbers, column names would also work, but is not automated
#When plotting several measurements (so more than one line) use pdat$measurementname for y
lines(Apdat$Angle, Apdat[,5], pch = 19, col = "purple", type = "b", lty = 1,lwd=2)

#we add the cosine line
cosine<- 0.58*cos(Rangle) 
Cosobj<-as.data.frame(cosine)
Cosobj$Angle<- angle
lines(Cosobj$Angle, Cosobj$cosine, pch = 19, col = "black", type = "b", lty = 1)

#fix axis
axis(1, at = seq(-70, 100, by = 10), las=1)

#optional legend
legend("topright", legend=c("Sample 1","Sample 1b","Sample 2","Sample 2b","Average"), col=c("blue", "darkblue","green","darkgreen","purple"), lty=1, cex=1.4, box.lty=0,)


#####

