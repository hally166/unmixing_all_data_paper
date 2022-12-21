# R 4.1.1 (2022)
# Install the required packages.  You may also need to install flowCore, ggcyto, ect. Pay attention to the warning messages
install.packages("devtools")
devtools::install_github("hally166/flowUnmix")
biocManager::install("ggcyto")
library(flowUnmix)
library(ggcyto)

# Attune (Babraham) -- mouse (note: no SV780 detector)
files<-list.files("C:/Attune 07OCT22/Mouse", pattern = "fcs", full.names=TRUE)
files #check files
controlfilesFS<-read.flowSet(files[c(1:7,9:10)]) #select the controls
unstainedcontrol<-read.FCS(files[11]) #specify the unstained
filetounmix<-read.flowSet(files[8]) #specify the file(s) to unmix

# Fails to correctly select apc signature - gate manually
samp <- controlfilesFS[[3]]
colnames(samp)
p <- autoplot(samp, "RL1-A")
p + scale_x_logicle()
p + scale_x_logicle() + geom_vline(xintercept = c(10000,69000))
rectGate <- rectangleGate(filterId="apc","RL1-A"=c(10000,69000))
controlfilesFS[[3]]<-Subset(samp, rectGate)

# Use flowUnmix to unmix the data
flowUnmix(fs=filetounmix, cs=controlfilesFS, unstained =unstainedcontrol, guessPop = TRUE, popCheck = TRUE)
