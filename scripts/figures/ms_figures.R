# Script to make manuscript figures
.libPaths(c("/home/lnd775/R/x86_64-pc-linux-gnu-library/4.2.2/",.libPaths()))
library(utils)
library(ggplot2)
library(scales)
library(tidyr)
library(lattice)
library(latticeExtra)
library(patchwork)
library(gtools)
library(data.table)
library(ggsci)
library(kableExtra)
library(ggpubr)
library(ggrepel)
args = commandArgs(trailingOnly=TRUE)

# Buffalo Freqs
freq_power<-read.table("rinder_pwr_freq.out",header=TRUE,sep="\t")
freq_power=subset(freq_power,freq_power$N!=1000)
freq_power$aimfA<-as.factor(as.character(freq_power$aimfA))
freq_power$N<-as.factor(as.character(freq_power$N))
freq_power$N<-factor(freq_power$N,levels=c("20","50","100","200","500"))
freq_power$Scenario<-as.factor(as.character(freq_power$Scenario))
freq_power$Scenario<-factor(freq_power$Scenario,levels=c("DA","BA","BP"))
freq_power$Scenario<-factor(freq_power$Scenario,levels=c("DA","BA","BP"),labels = c("Dead v Survivors","Before v After", "Before v Present"))
freq_power$Viability<-as.factor(as.character(freq_power$Viability))
freq_power$Viability<-factor(freq_power$Viability,levels = rev(levels(freq_power$Viability)))

# Prep for merge
buffalo_pwr_freq<-freq_power
buffalo_pwr_freq$Epi<-"Buffalo (Rinderpest)"
buffalo_pwr_freq=subset(buffalo_pwr_freq,buffalo_pwr_freq$Scenario!="BP")
#buffalo_pwr_freq=subset(buffalo_pwr_freq,select = -c(TrueDelta))

# Figure 3 Rinderpest
pp<-ggline(subset(freq_power,freq_power$Scenario!="Dead v Survivors"),"FST","aimfA",color="N",numeric.x.axis = TRUE,point.size=2,palette="aaas",facet.by=c("Viability","Scenario"), scales="free" ) + labs(color=expression(paste(italic('n'))),x=expression(paste("Mean Power (",italic('F'['ST']),")" )),y=expression(paste("Starting ",italic('f'['A'])))) + theme(axis.text.x=element_text(size = 10))
pp$layers[[1]]$aes_params$alpha <- 0.7
pp$layers[[2]]$aes_params$alpha <- 0.7
pp_fst<-pp

pp<-ggline(subset(freq_power,freq_power$Scenario!="Dead v Survivors"),"jSFS","aimfA",color="N",numeric.x.axis = TRUE,point.size=2,palette="aaas",facet.by=c("Viability","Scenario"), scales="free" ) + labs(color=expression(paste(italic('n'))),x=expression(paste("Mean Power (jSFS)")),y="") + theme(axis.text.x=element_text(size = 10))
pp$layers[[1]]$aes_params$alpha <- 0.7
pp$layers[[2]]$aes_params$alpha <- 0.7
pp_jsfs<-pp

ggarrange(pp_fst,pp_jsfs,labels = c("A", "B"), ncol = 2, nrow = 1,common.legend = TRUE) + plot_layout(guides = "collect")

# Swedish Freqs
freq_power<-read.table("plague_pwr_freq.out",header=TRUE,sep="\t")
freq_power=subset(freq_power,freq_power$N!=1000)
freq_power$aimfA<-as.factor(as.character(freq_power$aimfA))
freq_power$N<-as.factor(as.character(freq_power$N))
freq_power$N<-factor(freq_power$N,levels=c("20","50","100","200","500"))
freq_power$Scenario<-as.factor(as.character(freq_power$Scenario))
freq_power$Scenario<-factor(freq_power$Scenario,levels=c("DA","BA"))
freq_power$Scenario<-factor(freq_power$Scenario,levels=c("DA","BA"),labels = c("Dead v Survivors","Before v After"))
freq_power$Viability<-as.factor(as.character(freq_power$Viability))
freq_power$Viability<-factor(freq_power$Viability,levels = rev(levels(freq_power$Viability)))

# Figure 4 Plague
pp<-ggline(freq_power,"FST","aimfA",color="N",numeric.x.axis = TRUE,point.size=2,palette="aaas",facet.by=c("Viability","Scenario"), scales="free" ) + labs(color=expression(paste(italic('n'))),x=expression(paste("Mean Power (",italic('F'['ST']),")" )),y=expression(paste("Starting ",italic('f'['A'])))) + theme(axis.text.x=element_text(size = 10))
pp$layers[[1]]$aes_params$alpha <- 0.7
pp$layers[[2]]$aes_params$alpha <- 0.7
pp_fst<-pp

pp<-ggline(freq_power,"jSFS","aimfA",color="N",numeric.x.axis = TRUE,point.size=2,palette="aaas",facet.by=c("Viability","Scenario"), scales="free" ) + labs(color=expression(paste(italic('n'))),x=expression(paste("Mean Power (jSFS)" )),y="") + theme(axis.text.x=element_text(size = 10))
pp$layers[[1]]$aes_params$alpha <- 0.7
pp$layers[[2]]$aes_params$alpha <- 0.7
pp_jsfs<-pp

ggarrange(pp_fst,pp_jsfs,labels = c("A", "B"), ncol = 2, nrow = 1,common.legend = TRUE) + plot_layout(guides = "collect")


# Prep for merge
plague_frq_pwr<-freq_power
plague_frq_pwr$Epi<-"Medieval Swedes (Plague)"

# Merge Epidemics
both_freqs_add<-rbind(buffalo_pwr_freq,plague_frq_pwr)
pb_freqs<-subset(both_freqs_add,both_freqs_add$N=="20"|both_freqs_add$N=="50"|both_freqs_add$N=="100"|both_freqs_add$N=="200"|both_freqs_add$N=="500")

# Estimated Delta Plots
BA_fa=ggline(subset(pb_freqs,pb_freqs$Viability=="VAA=1" & pb_freqs$Scenario=="Before v After"),"FST","DeltaMean",color="Epi",shape="aimfA",numeric.x.axis = TRUE,point.size=2,alpha="Scenario",palette="aaas",facet.by=c("Scenario","N")) + labs(x="",y=expression(paste("Estimated ",Delta,italic(bar(fA)))),shape="Starting fA",color="Scenario") + scale_y_continuous(limits=c(0,0.5))
DA_fa=ggline(subset(pb_freqs,pb_freqs$Viability=="VAA=1" & pb_freqs$Scenario=="Dead v Survivors"),"FST","DeltaMean",color="Epi",shape="aimfA",numeric.x.axis = TRUE,point.size=2,alpha="Scenario",palette="aaas",facet.by=c("Scenario","N")) + labs(x=expression(paste("Mean Power (",italic('F'['ST']),")" )),y=expression(paste("Estimated ",Delta,italic(bar(fA)))),shape="Starting fA",color="Scenario") + scale_y_continuous(limits=c(0,0.5))
ggarrange(BA_fa, DA_fa,labels = c("A", "B"), ncol = 1, nrow = 2,common.legend = TRUE)

# True Delta Plots
BA_faTru=ggline(subset(pb_freqs,pb_freqs$Viability=="VAA=1" & pb_freqs$Scenario=="Before v After"),"FST","TrueDelta",color="Epi",shape="aimfA",numeric.x.axis = TRUE,point.size=2,alpha="Scenario",palette="aaas",facet.by=c("Scenario","N")) + labs(x="",y=expression(paste("True ",Delta,italic(bar(fA)))),shape="Starting fA",color="Scenario") + scale_y_continuous(limits=c(0,0.5))
DA_faTru=ggline(subset(pb_freqs,pb_freqs$Viability=="VAA=1" & pb_freqs$Scenario=="Dead v Survivors"),"FST","TrueDelta",color="Epi",shape="aimfA",numeric.x.axis = TRUE,point.size=2,alpha="Scenario",palette="aaas",facet.by=c("Scenario","N")) + labs(x=expression(paste("Mean Power (",italic('F'['ST']),")" )),y=expression(paste("True ",Delta,italic(bar(fA)))),shape="Starting fA",color="Scenario") + scale_y_continuous(limits=c(0,0.5))

# Create fA trajectories for both scenarios in two different plots
fA_traj<-read.table("plague_pop_fA.out",header=FALSE,sep="\t",col.names = c("Generation","Ne","aimfA","fA","sim","Epi"))
fA_traj$aimfA<-factor(as.character(fA_traj$aimfA))
fA_traj<-dcast(fA_traj,Generation+aimfA+Epi~.,value.var = "fA",fun.aggregate = mean)
colnames(fA_traj)=c("Generation","aimfA","Epi","fA")
plague_cols<-c("#EE0000","#BC0000","#8B0000")
fA_traj$Epi[fA_traj$Epi=="Rinderpest"] <-"Cape Buffalo (Rinderpest)"
fA_traj$Epi[fA_traj$Epi=="Plague"]<-"Medieval Swedes (Plague)"

rinder_fa=ggline(subset(fA_traj,fA_traj$Epi=="Cape Buffalo (Rinderpest)"),"Generation","fA",color="aimfA", palette = "aaas",facet.by = "Epi") + scale_x_continuous(n.breaks = 18) + labs(y=expression(paste("True ", italic(bar(fA))))) + theme(axis.text.x=element_text(angle=90))+ scale_y_continuous(breaks=seq(0,0.6,0.1),limits=c(0,0.6))
plague_fa=ggline(subset(fA_traj,fA_traj$Epi=="Medieval Swedes (Plague)"),"Generation","fA",color="aimfA", shape="aimfA",palette = rep(plague_cols[1],3),facet.by = "Epi") + scale_x_continuous(breaks = seq(58796,58818,1)) + labs(y=expression(paste("True ",italic(bar(fA))))) + theme(axis.text.x=element_text(angle=90)) + scale_y_continuous(limits=c(0,0.5))

ggarrange(BA_fa, DA_fa,rinder_fa,plague_fa,labels = c("A", "B","C","D"), ncol = 1, nrow = 4,common.legend = TRUE)

##(ALTERNATIVE WITH SIM INFORMATION)##
# Create fA trajectories for both scenarios in two different plots 
fA_traj<-read.table("~/data/plague/results/plots/pop_fA.out",header=FALSE,sep="\t",col.names = c("Generation","Ne","aimfA","fA","sim","Epi"))
fA_traj$aimfA<-factor(as.character(fA_traj$aimfA))
fA_traj$sim<-factor(as.character(fA_traj$sim))
fA_traj$Epi[fA_traj$Epi=="Rinderpest"] <-"Cape Buffalo (Rinderpest)"
fA_traj$Epi[fA_traj$Epi=="Plague"]<-"Medieval Swedes (Plague)"
fA_traj$simFA<-paste(fA_traj$sim,"_",fA_traj$aimfA)

# Create a mean True fA dataset
fA_traj_mean<-dcast(fA_traj,Generation+aimfA+Epi~.,value.var = "fA",fun.aggregate = mean)
colnames(fA_traj_mean)=c("Generation","aimfA","Epi","fA")
fA_traj_mean$Epi[fA_traj_mean$Epi=="Rinderpest"] <-"Cape Buffalo (Rinderpest)"
fA_traj_mean$Epi[fA_traj_mean$Epi=="Plague"]<-"Medieval Swedes (Plague)"

plague_cols<-c("#EE0000","#BC0000","#8B0000")

rinder_fa=ggline(subset(fA_traj,fA_traj$Epi=="Cape Buffalo (Rinderpest)"),"Generation","fA",color="sim", size=0.2, palette = rep("grey",10),facet.by = "Epi",shape="aimfA",legend="none") + scale_x_continuous(n.breaks = 18) + labs(color="",y=expression(paste("True ", italic(bar(fA))))) + theme(axis.text.x=element_text(angle=90))+ scale_y_continuous(breaks=seq(0,0.6,0.1),limits=c(0,0.6)) + geom_line(data=subset(fA_traj_mean,fA_traj_mean$Epi=="Cape Buffalo (Rinderpest)"), aes(x=Generation,y=fA), color="blue")
plague_fa=ggline(subset(fA_traj,fA_traj$Epi=="Medieval Swedes (Plague)"),"Generation","fA",color="simFA", size=0.2, shape="aimfA",palette = c(rep(plague_cols[1],3),rep("grey",30)),facet.by = "Epi",legend="none") + scale_x_continuous(breaks = seq(58796,58818,1)) + labs(y=expression(paste("True ",italic(bar(fA))))) + theme(axis.text.x=element_text(angle=90)) + scale_y_continuous(limits=c(0,0.5)) + geom_line(data=subset(fA_traj_mean,fA_traj_mean$Epi=="Medieval Swedes (Plague)"), aes(x=Generation,y=fA,color=aimfA))
ggarrange(BA_faTru, DA_faTru,rinder_fa,plague_fa,labels = c("A", "B","C","D"), ncol = 1, nrow = 4,common.legend = TRUE)
######MERGED PLAGUE AND RINDER PLOT#######
fA_traj$genMerge<-"None"
fA_traj[fA_traj$Generation=="40001" | fA_traj$Generation=="58796",]$genMerge<-"Before"
fA_traj[fA_traj$Generation=="40002" | fA_traj$Generation=="58797",]$genMerge<-"Outbreak"
fA_traj[fA_traj$Generation=="40003" | fA_traj$Generation=="58798",]$genMerge<-"After\nRinderpest"
fA_traj[fA_traj$Generation=="40004" | fA_traj$Generation=="58799",]$genMerge<-"Second Plague\nOutbreak"
fA_traj[fA_traj$Generation=="40005" | fA_traj$Generation=="58800",]$genMerge<-"After Plague"
for (i in seq(40006:40017)){
  plague_gens=c(58801:58818)
  rinder_gens=c(40006:40017)
  fA_traj[fA_traj$Generation==rinder_gens[i] | fA_traj$Generation==plague_gens[i],]$genMerge<-paste("After",i)
}
fA_traj$genCont=0
for (i in seq(40001:40017)){
  plague_gens=c(58796:58818)
  rinder_gens=c(40001:40017)
  fA_traj[fA_traj$Generation==rinder_gens[i] | fA_traj$Generation==plague_gens[i],]$genCont=i
}
fA_traj$simFAEpi<-paste(fA_traj$Epi,"_",fA_traj$simFA)

fA_traj_mean<-dcast(fA_traj,Generation+aimfA+Epi+genMerge+genCont~.,value.var = "fA",fun.aggregate = mean)
colnames(fA_traj_mean)=c("Generation","aimfA","Epi","genMerge","genCont","fA")
fA_traj_mean$FAEpi<-paste(fA_traj_mean$Epi,"_",fA_traj_mean$aimfA)

# Rinderpest and Plague time-points for labelling
rinderGens<-subset(fA_traj_mean,fA_traj_mean$genMerge!="None" & fA_traj_mean$aimfA=="0.1" & fA_traj_mean$Epi=="Medieval Swedes (Plague)")$genMerge
rinderGens[1]<-""
#rinderGens[2]<-"Outbreak                   \n"
rinderGens[2]<-"Outbreak"
rinderGens[3]<-"After Rinderpest"
rinderGens[4:17]<-""
rinderGens[17]<-"Present"
plagueGens<-subset(fA_traj_mean,fA_traj_mean$genMerge!="None" & fA_traj_mean$aimfA=="0.1" & fA_traj_mean$Epi=="Medieval Swedes (Plague)")$genMerge
plagueGens[2]<-"1st Outbreak"
plagueGens[3]<-""
plagueGens[4]<-"2nd\n Outbreak"
plagueGens[5]<-"After Plague"
plagueGens[6:17]<-""

#Vertical justification positioning of labels for each epidemic
vj=rep(c(1,0,-1), length.out=17)
vj[17]=-1
vj[2]<-1
vjR=vj
vj=rep(c(1,0,-1), length.out=17)
vj[1]<-1.5
vj[2]=-0.2
vj[4]=1.1
vj[5]=-0.5
vjP=vj

rinder_plague_fa=ggline(subset(fA_traj,fA_traj$genMerge!="None" & fA_traj$aimfA=="0.1"),"genMerge","fA",color="simFAEpi", size=0.2, shape="aimfA",palette = c(rep("lightblue3",10),rep("lightcoral",10)),legend="none") +
  labs(x="",y=expression(paste("True ",italic(fA)))) + rremove("x.axis") + rremove("x.text") + rremove("x.ticks") +
  scale_y_continuous(limits=c(0,0.6),breaks = seq(0,0.6,0.1)) +
  geom_line(data=subset(fA_traj_mean,fA_traj_mean$genMerge!="None" & fA_traj_mean$aimfA=="0.1" & fA_traj_mean$Epi=="Cape Buffalo (Rinderpest)"), aes(x=genCont,y=fA),color="#3B4992FF") +
  geom_line(data=subset(fA_traj_mean,fA_traj_mean$genMerge!="None" & fA_traj_mean$aimfA=="0.1" & fA_traj_mean$Epi=="Medieval Swedes (Plague)"), aes(x=genCont,y=fA),color="#EE0000FF") +
  geom_text_repel(data=subset(fA_traj_mean,fA_traj_mean$genMerge!="None" & fA_traj_mean$aimfA=="0.1" & fA_traj_mean$Epi=="Cape Buffalo (Rinderpest)"),label = rinderGens, max.overlaps = Inf,box.padding = 0.5,nudge_y = 0.05,min.segment.length = 0,arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text_repel(data=subset(fA_traj_mean,fA_traj_mean$genMerge!="None" & fA_traj_mean$aimfA=="0.1" & fA_traj_mean$Epi=="Medieval Swedes (Plague)"),label = plagueGens, max.overlaps = Inf,box.padding = 0.5,nudge_y = -0.05,min.segment.length = 0,arrow = arrow(length = unit(0.01, "npc")))

ggarrange(BA_faTru, DA_faTru,rinder_plague_fa,labels = c("A", "B","C"), ncol = 1, nrow = 3,common.legend = TRUE)