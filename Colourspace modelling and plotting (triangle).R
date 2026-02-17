###Vision modelling
#This script show how to do some basic visual modelling. We will be using a colour triangle. 
#While the colour hexagon is typically a better model, it is only behaviourally supported for bees
#The triangle model works for any trichromat

#General code 
# this clears all previous data in the R session
rm(list=ls());gc()

#Load/install some important libraries
library(pavo)
library(tidyverse)
library(readxl)

#All vision model methods require a dataframe that features a clearly indicated wavelength column and any number of spectral readings. For processing your spectral measurements into this format use "Raw spectra to smoothed excel.R" This file is also found on the github page, or in the git extension of R if you installed it.

#Set where your file is located. We will assume that the file is a excel file. You can also import the object directly from another script, but this would require you to run that script everytime you modify somthing here.
setwd("C:/YOUR LOCATION/YOUR SUBFOLDER/ETC")

#Load your data file
data<- read.csv("Processed spectral data.csv", sep=";") 

#Here, we specify some parameters of the model. Most importantly, What we are observing and what visual system we are using?

#What data are we observing? Can be anything, a species or a group name
Species<- "Speculum"
#What type of measurement are we processing? If like me you work with different setups, you may have a lot of different spectra in your file that you dont want to use here. Using the next line, we specify that we only want to process measurement that feater "probe" in the column name
Method<- "probe"

#What is the name of the pollinator system you are using
Pollinator<- "A.mellifera"

#Specify the spectral sensitivities you want to use you want to use. A lot of these can be found in "Van der Kooi et al., 2021" from Annual Reviews.
Visual<- c(346, 445, 529)

#Next we specify which wavelengths we want to use in our model
wlmin<- 300
wlmax<- 700
wl<- c(wlmin:wlmax)

#Everything below this should be automated, but feel free to tweak the code to your preferences.
#####

#First we make sure our data is a rspec object.
specdat<- as.rspec(data, whichwl="wl") 
#Lets make sure out specdat has the same wl as "data"
specdat$wl<- data$wl
#Inspect the spectra
plot(specdat)


#We limit our data to the wavelengths specified earlier
tdat<- specdat %>% filter(wl <(wlmax+1))
tdat<- tdat %>% filter(wl >(wlmin-1))

#Lets remove any negative values, as we cant have negatave photon counts. If your own script already did this nothing will happen
tdat<- procspec(tdat, fixneg = "addmin") 
#again overwrite the wl to make sure its right
tdat$wl<- c(wlmin:wlmax)

#Lets make our testframe, which we will use to feed data into the model
#We multiply be 100 because the model doesnt like being in a 0-1 range
testframe<- tdat*100
#since we also multiplied the wl column we need to specify it again. I could make it so that the wl column is ignored but since it might not always be in the same place that just invites trouble.
testframe$wl<- wl

#Now we get to do the fun stuff: actual modelling
#First we define the sensitivity peaks using the provided sensitivities. The pavo package features its own pre built models for bees and other animals, but I find that just doing it yourself is easy enough and less prone to error
r<- sensmodel(Visual,range=c(300,700))

#Now we combine our data and visual system into the model.
#Explanaition of parameters:
#data- Our dataframe, which we called tesframe before
#visual- our visual system, which we called r before
#vonkries- a transformation using the von Kries colour correction. Not applicable to some models, ask Casper. Or maybe I finally know how it works by the time someone uses this. 
#relative-do we want  to use relative quantum catches? Yes, always in colourspace model
#achromatic- which receptor is used as the achromatic (brightness) channel? Usually green for insects
# qcatch- Scary math metric. Qi for triangle, Ei for hexagon.
#bkg- Which background are we using in our model? Usually "green"
#illum- Under what light conditions are we looking at the spectral measurements? Usually "D65" which is natural daylight.
#For more parameters check write "?vismodel" in the console
vis.flower <- vismodel(testframe, visual = r, vonkries= TRUE, relative = TRUE, achromatic = r$L, qcatch= "Ei" , bkg = "green", illum = "D65")

#lets inspect the model (if you want). Ive put a hashtag so it doesnt throw you out of the script when you run this
#view(vis.flower)

#Now we turn this into a triangle colourspace. The math here is pretty cool, check it out using View(colspace). This will give a warning about column names, you can ignore it.
tri.flowers <- colspace(vis.flower, space = 'tri')

#Lets have a look
head(tri.flowers)

#Now we plot our colourspace Lots of parameters here are just graphical. For publications I reccomend plotting this yourself, as pavo is not great for modifying plot parameters beyond this
#bg- what colours are the points in the plot? spec2rgb makes them look like what they look like to us.
triplot(tri.flowers, pch = 22, cex= 1.4, bg = spec2rgb(testframe))

#We need to make a small adjustment to the achromatic column. Long story short, green excitation on a green background -0.5 (absolute difference) is the achromatic contrast.
tri.flowers$achromatic<- abs(tri.flowers$l-0.5)

#Lastly, we export all of this into one neat file, using the names we set earlier
Filename<-paste("Colourspace ",Species," ",Pollinator," ",".csv",sep="")
write.csv(tri.flowers,Filename)
#####
