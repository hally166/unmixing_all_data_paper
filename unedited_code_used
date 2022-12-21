# October 2022
# Christopher Hall, hallc@babraham.ac.uk
# Unmixing traditional machines project

setwd('C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Unmixed data (tradtional instruments)/October')
# Install packages. flowCore should be installed as part of the process, if not install from bioconductor
install.packages("devtools")
devtools::install_github("hally166/flowUnmix")
devtools::install_github("hally166/flowSpectrum")

library(flowUnmix)
library(ggcyto) #needed for the mis-ID'd spectra
library(flowSpectrum) #needed for the mis-ID'd spectra

# Fortessa (Babraham) -- human
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Fortessa 07OCT2022/Chris Hall Human Fortessa 07OCT22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:6)])
unstainedcontrol<-read.FCS(files[8])
filetounmix<-read.flowSet(files[7])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)
# Fails to find the APCef780 signature
samp <- controlfilesFS[[1]]
colnames(samp)
p <- autoplot(samp, "780/60 640nm-A")
p + scale_x_logicle() + geom_vline(xintercept = 600)
rectGate <- rectangleGate(filterId="CD19","780/60 640nm-A"=c(600,Inf))
controlfilesFS[[1]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fortessa (Babraham) -- mouse
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Fortessa 07OCT2022/Chris Hall Mouse Fortessa 07OCT22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:7,9:10)])
unstainedcontrol<-read.FCS(files[11])
filetounmix<-read.flowSet(files[8])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)
# Fails to find the ef506 signature
samp <- controlfilesFS[[6]]
colnames(samp)
p <- autoplot(samp, "525/50 405nm-A")
p + scale_x_logicle()
p + scale_x_logicle() + geom_vline(xintercept = c(2000,8000))
rectGate <- rectangleGate(filterId="ef506","525/50 405nm-A"=c(2000,8000), "SSC-A"=c(0,35000))
controlfilesFS[[6]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Attune (Babraham) -- human
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Attune 07OCT22/Human", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:6)])
unstainedcontrol<-read.FCS(files[8])
filetounmix<-read.flowSet(files[7])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the pe signature
samp <- controlfilesFS[[5]]
colnames(samp)
p <- autoplot(samp, "YL1-A")
p + scale_x_logicle()
p + scale_x_logicle() + geom_vline(xintercept = 20000)
rectGate <- rectangleGate(filterId="pe","YL1-A"=c(1500,20000))
controlfilesFS[[5]]<-Subset(samp, rectGate)

# Fails to find the apcef780 signature
samp <- controlfilesFS[[1]]
colnames(samp)
p <- autoplot(samp, "RL3-A")
p + scale_x_logicle()
p + scale_x_logicle() + geom_vline(xintercept = 30000)
rectGate <- rectangleGate(filterId="apc780","RL3-A"=c(2500,30000))
controlfilesFS[[1]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Attune (Babraham) -- mouse (no SV780 detector)
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Attune 07OCT22/Mouse", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:7,9:10)])
unstainedcontrol<-read.FCS(files[11])
filetounmix<-read.flowSet(files[8])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the apc signature
samp <- controlfilesFS[[3]]
colnames(samp)
p <- autoplot(samp, "RL1-A")
p + scale_x_logicle()
p + scale_x_logicle() + geom_vline(xintercept = 69000)
rectGate <- rectangleGate(filterId="apc","RL1-A"=c(10000,69000))
controlfilesFS[[3]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Penteon (Imperial) -- human
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Penteon 06OCT22/Chris Hall Human 06oct22/Chris Hall Human Penteon 06OCT22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(3:8)])
unstainedcontrol<-read.FCS(files[1])
filetounmix<-read.flowSet(files[2])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the apc780 signature
samp <- controlfilesFS[[3]]
colnames(samp)
p <- autoplot(samp, "R780-A")
p + scale_x_logicle()
p + scale_x_logicle() + geom_vline(xintercept = c(3000,30000))
rectGate <- rectangleGate(filterId="apc780","R780-A"=c(3000,30000))
controlfilesFS[[3]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Penteon (Imperial) -- mouse
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Penteon 06OCT22/Chris Hall Mouse Penteon 06OCT22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(2:4,6:11)])
unstainedcontrol<-read.FCS(files[1])
filetounmix<-read.flowSet(files[5])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

#remove high AF from controls
samp <- controlfilesFS[[9]]
colnames(samp)
p <- autoplot(samp, "FSC-A")
p + geom_vline(xintercept = 1400000)
rectGate <- rectangleGate(filterId="apc","FSC-A"=c(0,1500000))
p <- autoplot(samp, "SSC-A")
p + geom_vline(xintercept = 500000)

rectGate <- rectangleGate(filterId="Fluorescence Region",
                          "FSC-A"=c(0,1500000), "SSC-A"=c(0,1500000))
controlfilesFS<-Subset(controlfilesFS, rectGate)
unstainedcontrol<-Subset(unstainedcontrol, rectGate)

# Fails to find the apc signature
samp <- controlfilesFS[[9]]
colnames(samp)
p <- autoplot(samp, "R667-A")
p + scale_x_logicle()
p + scale_x_logicle() + geom_vline(xintercept = c(3000,13000))
rectGate <- rectangleGate(filterId="apc","R667-A"=c(2500,17000))
controlfilesFS[[9]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)


# Aurora (Babraham) -- human
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Aurora 07OCT2022/Chris Hall Human Aurora 07oct22/Raw/TubeRack_001/Reference Group", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(2:7)])
unstainedcontrol<-read.FCS(files[1])
filetounmix<-read.flowSet("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Aurora 07OCT2022/Chris Hall Human Aurora 07oct22/Raw/TubeRack_001/Group_001/A8 Fully Stained_2.fcs")

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)


# Fails to find the APC780 signiture
library(ggcyto)
samp <- controlfilesFS[[1]]
colnames(samp)
p <- autoplot(samp, "R7-A")
p + scale_x_logicle() + geom_vline(xintercept = c(10000,100000))
rectGate <- rectangleGate(filterId="CD19","R7-A"=c(10000,100000))
Subset(samp, rectGate)
controlfilesFS[[1]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Aurora (Babraham) -- mouse
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Aurora 07OCT2022/Chris Hall Mouse Aurora 07OCT22/Raw/TubeRack_001/Reference Group", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(2:10)])
unstainedcontrol<-read.FCS(files[1])
filetounmix<-read.flowSet("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Aurora 07OCT2022/Chris Hall Mouse Aurora 07OCT22/Raw/TubeRack_001/Group_001/B3 Fully stained.fcs")

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the APC signiture
library(ggcyto)
samp <- controlfilesFS[[3]]
colnames(samp)
p <- autoplot(samp, "R1-A")
p + scale_x_logicle() + geom_vline(xintercept = c(10000,65000))
rectGate <- rectangleGate(filterId="cd25","R1-A"=c(10000,65000))
Subset(samp, rectGate)
controlfilesFS[[3]]<-Subset(samp, rectGate)

# Fails to find the PERCPcy55 signiture
samp <- controlfilesFS[[4]]
colnames(samp)
p <- autoplot(samp, "B9-A")
p + scale_x_logicle() + geom_vline(xintercept = c(5000,40000))
rectGate <- rectangleGate(filterId="percp","B9-A"=c(5000,40000))
Subset(samp, rectGate)
controlfilesFS[[4]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)


# Symphony (Crick)-- human
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Symphony 06OCT2022/Chris Hall Symphony A5 Human 06OCT22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:6)], truncate_max_range = FALSE)
unstainedcontrol<-read.FCS(files[8], truncate_max_range = FALSE)
filetounmix<-read.flowSet(files[7], truncate_max_range = FALSE)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Failed to ID apc780
samp <- controlfilesFS[[1]]
colnames(samp)
p <- autoplot(samp, "640 - 780/60-A")
p + scale_x_logicle() + geom_vline(xintercept = c(800,3000))
rectGate <- rectangleGate(filterId="apc780","640 - 780/60-A"=c(800,3000))
Subset(samp, rectGate)
controlfilesFS[[1]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Symphony (Crick)-- mouse
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Symphony 06OCT2022/Chris Hall Symphony A5 Mouse 06OCT22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:7,9:10)], truncate_max_range = FALSE)
unstainedcontrol<-read.FCS(files[11], truncate_max_range = FALSE)
filetounmix<-read.flowSet(files[8], truncate_max_range = FALSE)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the APC signiture
samp <- controlfilesFS[[3]]
colnames(samp)
p <- autoplot(samp, "640 - 670/30-A")
p + scale_x_logicle() + geom_vline(xintercept = c(2200,10000))
rectGate <- rectangleGate(filterId="cd25","640 - 670/30-A"=c(2200,10000))
Subset(samp, rectGate)
controlfilesFS[[3]]<-Subset(samp, rectGate)

# Fails to find the PERCPcy55 signiture
samp <- controlfilesFS[[4]]
colnames(samp)
p <- autoplot(samp, "488 - 710/50-A")
p + scale_x_logicle() + geom_vline(xintercept = c(3000,7000))
rectGate <- rectangleGate(filterId="percp","488 - 710/50-A"=c(3000,10000))
Subset(samp, rectGate)
controlfilesFS[[4]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Cytoflex (Crick) -- human
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Cytoflex LS 06OCT2022/Human Cytoflex 06oct22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:6)], truncate_max_range = FALSE)
unstainedcontrol<-read.FCS(files[9], truncate_max_range = FALSE)
filetounmix<-read.flowSet(files[7], truncate_max_range = FALSE)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the SB702 signiture
samp <- controlfilesFS[[6]]
colnames(samp)
p <- autoplot(samp, "FL16-A")
p + scale_x_logicle() + geom_vline(xintercept = c(10000,100000))
rectGate <- rectangleGate(filterId="702","FL16-A"=c(10000,100000))
Subset(samp, rectGate)
controlfilesFS[[6]]<-Subset(samp, rectGate)

# Fails to find the APCef780 signiture
samp <- controlfilesFS[[1]]
colnames(samp)
p <- autoplot(samp, "FL11-A")
p + scale_x_logicle() + geom_vline(xintercept = c(2000,10000))
rectGate <- rectangleGate(filterId="apc780","FL11-A"=c(2000,10000))
Subset(samp, rectGate)
controlfilesFS[[1]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Cytoflex (Crick) -- mouse
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/Cytoflex LS 06OCT2022/Mouse Cytoflex 06oct22", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(1:7,9:10)], truncate_max_range = FALSE)
unstainedcontrol<-read.FCS(files[11], truncate_max_range = FALSE)
filetounmix<-read.flowSet(files[8], truncate_max_range = FALSE)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the SB506 signiture
samp <- controlfilesFS[[5]]
colnames(samp)
p <- autoplot(samp, "FL13-A")
p + scale_x_logicle() + geom_vline(xintercept = c(4000,20000))
rectGate <- rectangleGate(filterId="506","FL13-A"=c(5000,50000))
controlfilesFS[[5]]<-Subset(samp, rectGate)

# Fails to find the APC signiture
samp <- controlfilesFS[[3]]
colnames(samp)
p <- autoplot(samp, "FL9-A")
p + scale_x_logicle() + geom_vline(xintercept = c(7000,30000))
rectGate <- rectangleGate(filterId="apc","FL9-A"=c(7000,30000))
controlfilesFS[[3]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# ID7000 (Crick) -- human
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/ID7000 07OCT2022/20221007_CH_Sony_Human/20220610_CH_Sony_Human/24 Tube Rack (5mL) - 1/Single Positive Controls", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(2:7)])
unstainedcontrol<-read.FCS(files[1])
filetounmix<-read.flowSet(files[8])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# ID7000 (Crick) -- mouse
files<-list.files("C:/Users/hallc/OneDrive - BABRAHAM/Data Transfers/Instrument Comparison October 2022/ID7000 07OCT2022/20221006_CH_Sony_Mouse/20221006 CH Mouse/24 Tube Rack (5mL) - 1/Single Positive Controls", pattern = "fcs", full.names=TRUE)
files
controlfilesFS<-read.flowSet(files[c(2:10)])
unstainedcontrol<-read.FCS(files[1])
filetounmix<-read.flowSet(files[11])

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)

# Fails to find the APC signiture
samp <- controlfilesFS[[3]]
colnames(samp)
p <- autoplot(samp, "637CH17-A")
p + scale_x_logicle() + geom_vline(xintercept = c(2000,15000))
rectGate <- rectangleGate(filterId="apc","637CH17-A"=c(2000,15000))
Subset(samp, rectGate)
controlfilesFS[[3]]<-Subset(samp, rectGate)

# Fails to find the percp signiture
samp <- controlfilesFS[[4]]
colnames(samp)
p <- autoplot(samp, "488CH21-A")
p + scale_x_logicle() + geom_vline(xintercept = c(1000,6000))
rectGate <- rectangleGate(filterId="pp","488CH21-A"=c(1000,6000))
Subset(samp, rectGate)
controlfilesFS[[4]]<-Subset(samp, rectGate)

flowUnmix(fs=filetounmix,cs=controlfilesFS,unstained =unstainedcontrol,guessPop = TRUE, popCheck = TRUE)
