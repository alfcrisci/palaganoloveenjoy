
####################################################################################

library(openxlsx)
library(ggmatplot)
library(doBy)
library(ggplot2)
library(ggfortify)
library(scales)

####################################################################################

setwd("C:\\aaa_lavori\\lav_carvacrol")

grav_carv=read.xlsx("gravimetry_data.xlsx")

grav_carv$data=convertToDate(grav_carv$data)

grav_mat_df_mean=summaryBy(norm_carvarol_plate ~ treatment+level_dose+disposition+data, data=grav_carv,FUN=mean)
grav_mat_df_sd=summaryBy(norm_carvarol_plate ~ treatment+level_dose+disposition, data=grav_carv,FUN=sd)
grav_mat_df=merge(grav_mat_df_mean,grav_mat_df_sd)
grav_mat_df$data=convertToDate(grav_mat_df$data)

ls_data=split(grav_mat_df,grav_mat_df$data)

final_df_mean=data.frame(ls_data[[1]][,1:2],do.call("cbind",lapply(ls_data,function(x) x[,5])))
names(final_df_mean)<-gsub("X","days_",names(final_df_mean))
final_df_sd=data.frame(ls_data[[1]][,1:2],do.call("cbind",lapply(ls_data,function(x) x[,6])))
names(final_df_sd)<-gsub("X","days_",names(final_df_sd))
giorni <- unique(grav_carv$data)




grav_mat_df$trial=gsub(".$","",grav_mat_df$treatment)
grav_mat_df$trial=gsub("1","100 mg 1 pz uniform",grav_mat_df$trial)
grav_mat_df$trial=gsub("2","200 mg 2 pz uniform",grav_mat_df$trial)
grav_mat_df$trial=gsub("3","200 mg 1 pz uniform",grav_mat_df$trial)
grav_mat_df$trial=gsub("4","300 mg 3 pz uniform",grav_mat_df$trial)
grav_mat_df$trial=gsub("5","300 mg 1 pz uniform",grav_mat_df$trial)
grav_mat_df$trial=gsub("6","100 mg 1 pz random",grav_mat_df$trial)
grav_mat_df$trial=gsub("7","200 mg 1 pz random",grav_mat_df$trial)
grav_mat_df$trial=gsub("8","300 mg 1 pz random",grav_mat_df$trial)



p=ggplot(grav_mat_df, aes(x=data, y=norm_carvarol_plate.mean,fill=treatment))+
  geom_line(aes(linetype=disposition,color = trial), size = 1) +
  scale_x_date(labels = date_format("%d-%m"),breaks = unique(grav_mat_df$data))+
  geom_point(show_guide = FALSE)+ggtitle("Palagano's Love Normalized Carvacrol Trials")

p
ggsave("PLNCT.png")
######################################################################################################################

grav_carv_model=lm(norm_carvarol_plate ~ level_dose+disposition+days+plate,data=grav_carv)
sink("summary_models.txt")
summary(grav_carv_model)


######################################################################################################################
grav_carv$trial=paste0(gsub(".$","",grav_carv$treatment))

grav_carv$trial=gsub("1","100 mg 1 pz uniform",grav_carv$trial)
grav_carv$trial=gsub("2","200 mg 2 pz uniform",grav_carv$trial)
grav_carv$trial=gsub("3","200 mg 1 pz uniform",grav_carv$trial)
grav_carv$trial=gsub("4","300 mg 3 pz uniform",grav_carv$trial)
grav_carv$trial=gsub("5","300 mg 1 pz uniform",grav_carv$trial)
grav_carv$trial=gsub("6","100 mg 1 pz random",grav_carv$trial)
grav_carv$trial=gsub("7","200 mg 1 pz random",grav_carv$trial)
grav_carv$trial=gsub("8","300 mg 1 pz random",grav_carv$trial)

res_ls=lm_by(norm_carvarol_plate ~ days| trial,data=grav_carv)
lapply(res_ls,FUN=function(x) summary(x))


sink()

res_df=do.call("rbind",lapply(res_ls,FUN=function(x) data.frame(trend=x$coefficient[2],rsq=summary(x)$adj.r.squared)))

write.xlsx(list(trend=res_df,mat=grav_mat_df,mean=final_df_mean,sd=final_df_sd),"analisi_trials.xlsx",overwrite = T)



####################################################################################



#######################################################################################
# References
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#Calculating_survival_times
