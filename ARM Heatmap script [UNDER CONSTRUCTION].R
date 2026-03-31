#ARM Spectra loading and processing. 
# this clears all previous data in the R session
rm(list=ls());gc()
#dev.off()
#load relevant packages
library(pavo)
library(tidyverse)
library(reshape2)
library(data.table)
library(ggfx)

#set your working directory. Make sure there are ONLY .dat files from the ARM in the directory
setwd("C:/Users/P309883/OneDrive - University of Groningen/Downloads/20260330/20260330")

#here we set the variables determined by the ARM measurement protocol. The variables should match those specified in the Matlab script
anglim<- 70
dstep<-10

#Set polarisation as set in matlab (NP,TE,TM)
pol<- "NP"
#In your directory, which measurement folder do you want to analyze?
Sample<- "Rasiaticus_2_along"
#What wl range are you using originally?
wl<- c(200:1100)
#Set illumination angle for later plotting
Illum="0"
#set the wavelength at which you want to plot your angles
lambda<- 500
# Manually define the range for reflectance values
manual_reflectance_max <- 0.3  # Set the maximum value for the color scale (outliers above this will be red)
#Snap 0.12 @ 411nm, Racris 1.05, Anthulight 0.7, glossy 5002 0.8, glossy 1021b 1.15, 

# cex-like parameter for scaling all text in plots
base_text_size <- 12  

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

pdat0<- rspecdata
#If needed we can remove negative values using this line
#pdat0<- procspec(pdat0, fixneg = "addmin") 

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

long2$Illumination_Angle <- sub(paste0(pol, ".*"), "", rownames(long2))
long2$Illumination_Angle <- sub(".*Illumangle", "", long2$Illumination_Angle)
long2$Group<-NULL
long2$Illumination_Angle<- as.numeric(long2$Illumination_Angle)
long2$Observation_Angle<- as.numeric(long2$Observation_Angle)


# Interpolation: Create a dense grid
interp_data <- with(long2, akima::interp(
  x = Illumination_Angle,
  y = Observation_Angle,
  z = Reflectance,
  xo = seq(min(Illumination_Angle), max(Illumination_Angle), length.out = 200),
  yo = seq(min(Observation_Angle), max(Observation_Angle), length.out = 200)
))


# Convert interpolated data to a data frame
interp_df <- expand.grid(
  Illumination_Angle = interp_data$x,
  Observation_Angle = interp_data$y
) %>%
  mutate(Reflectance = as.vector(interp_data$z))

# Handle any NA values (from interpolation edges)
interp_df <- interp_df %>%
  filter(!is.na(Reflectance))

# Define a function to adjust color limits based on quantiles
adjust_reflectance_limits <- function(data, quantile_trim = NULL) {
  if (!is.null(quantile_trim)) {
    # Compute lower and upper quantiles
    lower <- quantile(data$Reflectance, quantile_trim, na.rm = TRUE)
    upper <- quantile(data$Reflectance, 1 - quantile_trim, na.rm = TRUE)
    # Cap values at the computed quantile range
    data <- data %>%
      mutate(Reflectance = pmax(pmin(Reflectance, upper), lower))
  }
  return(data)
}


# Cap the reflectance values only for plotting purposes
interp_df_capped <- interp_df %>%
  mutate(Reflectance_Capped = pmin(Reflectance, manual_reflectance_max))

# Create the smoothed heatmap
smoothed_plot <- ggplot(interp_df_capped, aes(x = Illumination_Angle, y = Observation_Angle, fill = Reflectance_Capped)) +
  with_blur(geom_tile(), sigma = 10) +  # Adjust sigma to control smoothing
  scale_fill_gradientn(
    colors = c("blue", "green", "yellow", "red"),  # Define custom gradient
    values = scales::rescale(c(min(interp_df$Reflectance), manual_reflectance_max)),  # Map gradient to manual max
    limits = c(min(interp_df$Reflectance), manual_reflectance_max),  # Define color scale range
    oob = scales::squish  # Ensure values outside range are capped to the nearest color
  ) +
  scale_x_continuous(
    limits = c(-anglim, anglim),  # Force x-axis to range from -anglim to +anglim
    breaks = seq(-anglim-10, anglim-10, by = 20),  # Show ticks every 10 degrees
    expand = c(0, 0)  # Remove white space
  ) +
  scale_y_continuous(
    limits = c(-anglim, anglim),  # Force y-axis to range from -anglim to +anglim
    breaks = seq(-anglim-10, anglim-10, by = 20),  # Show ticks every 10 degrees
    expand = c(0, 0)  # Remove white space
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = base_text_size),    # Scale all text elements
    panel.grid = element_blank(),                 # Remove all gridlines
    axis.line = element_line(color = "black"),    # Add black x and y axes
    axis.text = element_text(color = "black", size = base_text_size),  # Axis tick labels
    axis.title = element_text(size = base_text_size),  # Axis titles
    legend.title = element_text(size = base_text_size),  # Legend title
    legend.text = element_text(size = base_text_size),   # Legend labels
    panel.border = element_blank()                # Remove border
  ) +
  labs(x = "Illumination Angle (°)", y = "Observation Angle (°)", fill = "Reflectance") +
  coord_fixed() +
  ggtitle(paste(Sample,lambda,"nm"))

# Display the smoothed heatmap
smoothed_plot

# Save the smoothed heatmap to a preferred folder
ggsave(paste0("C:/Users/P309883/OneDrive - University of Groningen/Desktop/Heatmaps/",Sample,lambda,".pdf"), smoothed_plot, width = 5, height = 5, units = "in")

