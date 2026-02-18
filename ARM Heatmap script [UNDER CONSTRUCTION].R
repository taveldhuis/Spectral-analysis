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
#setwd("C:/Users/P309883/OneDrive - University of Groningen/Desktop/Ophyrs 2025/ARM/20250224")

#here we set the variables determined by the ARM measurement protocol. The variables should match those specified in the Matlab script
anglim<- 60
dstep<-10
#In your directory, which measurement folder do you want to analyze?
Sample<- "O_eos_1"
#What wl range are you using originally?
wl<- c(300:1000)
#Set illumination angle for later plotting
Illum="0"
#set the wavelength at which you want to plot your angles
lambda<- 800

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



##### Creating a plotting environment
#Only use next line if plotting 2 spectra next to each other
#par(mfrow=c(1,2))

#Read the desired csv file, or replace with an object created in the last section
#pdat0<- read.csv("ARM Casper 17-10-2024/Anthuriumlight 800nm ARM data.csv",header=T,sep=";")

#sometimes column names are read wrong when loaded from excel, where - is replaced with .
#These two lines switch them back
correctnames<- gsub("\\.", "-", colnames(pdat0))
colnames(pdat0)<- correctnames

#select the specific wavelength we want to plot, and transform the data so that we can compare measurement angle and reflectance
test<- pdat0 %>% filter(wl==lambda)
long <- t(test)
long2<- as.data.frame(long)
colnames(long2)<- c("Reflectance")
long2$M_angle<- c(rownames(long2))
long2$Observation_Angle<- gsub(".*_","",long2$M_angle)
long2$Group<- sub("\\/.*", "", long2$M_angle)
long2$M_angle<- NULL

long2<- long2 %>% filter(Group == Sample)

long2$Illumination_Angle <- sub("TE.*", "", rownames(long2))
long2$Illumination_Angle <- sub(".*Illumangle", "", long2$Illumination_Angle)
long2$Group<-NULL

# Interpolation: Create a dense grid
interp_data <- with(long2, akima::interp(
  x = Illumination_Angle,
  y = Observation_Angle,
  z = Reflectance,
  xo = seq(min(Illumination_Angle), max(Illumination_Angle), length.out = 200),
  yo = seq(min(Observation_Angle), max(Observation_Angle), length.out = 200)
))

