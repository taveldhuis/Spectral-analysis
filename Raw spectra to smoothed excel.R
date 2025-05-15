#reading avasoft spectra through pavo

#General code 
# this clears all previous data in the R session
rm(list=ls());gc()


#load relevant packages
library(pavo)
library(tidyverse)

#Set directory to where you keep your spectra files
setwd("C:/Users/P309883/Downloads/Oenothera")

#Variables to modify
#smoothing factor (0.2 usually works best)
smoothing<- 0.2
#Set upper and lower wl limits, 300-800 for probe, 350-700 for MSP
wl_lower<- 300
wl_upper<- 800

#Load your spectra. They will not have the correct names yet (we will fix this later). Modify lim to change wavelength ranges. modify where= ("XXXX") to the desired folder
rspecdata <- getspec(where=("Oenothera"), ext = "TXT",decimal=",",lim=c(300,1000))
wl<- c(300:1000)

#First we remove useless characters from the start of the file names. The number represents the amount of characters we want to remove. This number (23 in my case) may change based on how you label your measurements. 
names(rspecdata)<- substring(names(rspecdata),12)

#Select the measurement type you want (in this case the species "Grandiflora"). We also add the wavelength again afterwards because the select function removes it
specdat<- rspecdata 
specdat$wl<- wl
specdat<- specdat[,-1]

#Now we inspect and transform the data. First step is to smooth out any noise. We want to remove as much noise while keeping the smoothing factor as low as possible to retain our data
plotsmooth(specdat, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)

#Now we can see the effect of the different smoothing factors, we select and apply one. I typically use 0.2, but lower is better, if it doesn't introduce a lot of noise
smoothspec <- procspec(specdat, opt = "smooth", span = smoothing) 
plot(smoothspec, ylim=c(-10,100))

#Here we cut the data into the wavelengths we want
shortspec<- smoothspec %>% filter(wl<(wl_upper+1)) %>% filter((wl>wl_lower-1))
#pdat
#If necessary we can modify any negative values. Use ?procspec to see options the current command adds the lowest value of the dataset to all spectra, removing measurement errors that cause negative reflectance values
pdat<- procspec(shortspec, fixneg = "addmin") 

#Lets plot the complete dataset to identify any issues
plot(pdat, ylim=c(-10,100))
pdat2<- pdat/100
pdat2$wl<- c(wl_lower:wl_upper)
pdat2<- as.rspec(pdat2)
plot(pdat2, ylim=c(0,1))

#If there are no issues, let's write a csv file with our transformed data
write.csv(pdat,"Smoothed data.csv")



#We do a little plotting. 
#Change plot labels based on measurement type and such
plot(pdat$wl, y=pdat$R_creticus_MSP_refl_gloss_1_0609021U1, type = "n", xlim = c(wl_lower, wl_upper), ylim = c(0, 1), 
     xlab = "Wavelength (nm)", ylab = "Measured unit", main = "MSP reflectance", las=1, bty="l")

#Now we add the spectra we want to see. you need to edit the legend based on the number of lines in the plot
lines(pdat$wl, pdat[,2], type = "l", col = "red", lwd=2)
lines(pdat$wl, pdat[,3], type = "l", col = "grey", lwd=2)
lines(pdat$wl, pdat[,4], type = "l", col = "blue", lwd=2)
lines(pdat$wl, pdat[,5], type = "l", col = "purple", lwd=2)
lines(pdat$wl, pdat[,6], type = "l", col = "yellow", lwd=2)
lines(pdat$wl, pdat[,7], type = "l", col = "cyan", lwd=2)
lines(pdat$wl, pdat[,8], type = "l", col = "green", lwd=2)

#optional legend
legend("topleft",inset=0.01, legend= c("Gloss 1","Gloss 2","Gloss 3","No gloss 1","No gloss 2","No gloss 3"),
col=c("red", "grey", "blue", "purple","yellow","cyan","green","pink"), lty=1, cex=1, box.lty=0)

