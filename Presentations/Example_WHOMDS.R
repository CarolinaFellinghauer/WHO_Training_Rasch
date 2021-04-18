#rm(list = ls())
#Install whomds

devtools::install_github("lindsayevanslee/whomds", build_vignettes = TRUE)

#A)select option 1 for the first time you are prompted with
#Enter one or more numbers, or an empty line to skip updates:1
#if after a first complete update of R packages WHOMDS can still not be started
#and error comes that it "cannot remove prior installation of package "...
#B)select the 3 as option for the question.
#Enter one or more numbers, or an empty line to skip updates:3


#run
library(whomds)

#there is a small anonymized sample of data collected in MDS-chile to test the function

urlfile <- "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2021_Rasch/main/Data/WHO_MDS_course.csv"

MDS.data <- read.csv(url(urlfile), sep = ";")

dim(MDS.data)
colnames(MDS.data)

MDS.data[,"age_grp"] = cut(MDS.data$age, breaks=c(17.5, 39.5, 59.5,105 ))

colnames(MDS.data)
#let's test rasch_mds
#what does it do?

?rasch_mds

#the MDS - items are

MDS.items = c("Vision", "Hearing", "Walking", "Memory",  "Dressing",     
 "Communication", "Feeding",  "Toileting", "Bed",  "GoingOut",  "Shopping",     
"HandUse", "Sleeping", "Breathing",  "Household", "Caring", "Participating",
"Depressivity", "Anxiety", "GettingAlong", "Managing", "Pain")

path_output = "C:/Users/carol/Dropbox/MDS/09 national and regional surveys/Chile 2021/Workshop/Workshop_Part_II/WHOMDS_Outputs/"

#Note the MDS_items are not in the typical MDS coding scheme, this was done to 
#facility the interpretation


start <- rasch_mds(
  df = MDS.data, 
  vars_metric = MDS.items,
  vars_id = "ID", 
  vars_DIF = c("sex", "age_grp"),
  resp_opts = 1:5, 
  max_NA = 2,
  print_results = TRUE,
  path_parent = path_output,
  model_name = "Start",
  testlet_strategy = NULL,
  recode_strategy = NULL,
  drop_vars = NULL,
  split_strategy = NULL,
  comment = "Initial run"
)


# Example Investigate Loadings:
# PCA = read.csv(paste(path_output, "Start/", "Original_Data_PCA.csv", sep = ""), header = TRUE, sep = ";")
# 
# plot(PCA[,2], PCA[,3], cex = 0.2, xlab = "1st PC", ylab = "2nd PC")
# text(PCA[,2], PCA[,3], label = PCA[,1], cex = 0.8)

#Example Investigate Fit:
# Fit = read.csv(paste(path_output, "Start/", "item_fit.csv", sep = ""), header = TRUE, sep = ",")
# head(Fit)
# Fit[which(Fit$i.outfitMSQ > 1.2),"X"] 
# Fit[which(Fit$i.infitMSQ > 1.2),"X"]


##Creating Testlets

testlet = rasch_mds(
  df = MDS.data, 
  vars_metric = MDS.items,
  vars_id = "ID", 
  vars_DIF = c("sex", "age_grp"),
  resp_opts = 1:5, 
  max_NA = 2,
  print_results = TRUE,
  path_parent = path_output,
  model_name = "1.Adjust - Testlet",
 
   testlet_strategy = list(
    Care = c("Toileting", "Bed"),
    Outdoor =c("GoingOut", "Shopping"), Affect = c("Depressivity", "Anxiety")
    ),
  
    recode_strategy = NULL,
    drop_vars = NULL,
    split_strategy = NULL,
    comment = "Testlets for LID > 0.4")


##recode items


Recoding = rasch_mds(
  df = MDS.data, 
  vars_metric = MDS.items,
  vars_id = "ID", 
  vars_DIF = c("sex", "age_grp"),
  resp_opts = 1:5, 
  max_NA = 2,
  print_results = TRUE,
  path_parent = path_output,
  model_name = "2.Adjust - Recode",
  testlet_strategy = NULL,
  recode_strategy = list(
    "Vision" = c(0,1,1,2,2),
    "Hearing" = c(0,1,1,2,2),
    "Walking" = c(0,1,1,2,2),
    "Memory" = c(0,1,1,2,2),
    "Dressing" = c(0,1,1,2,2),
    "Communication" = c(0,1,1,2,2),
    "Feeding" = c(0,1,1,2,2),
    "Toileting" = c(0,1,1,2,2),
    "Bed" = c(0,1,1,2,2),
    "GoingOut" = c(0,1,1,2,2),
    "Shopping" = c(0,1,1,2,2),
    "HandUse" = c(0,1,1,2,2),
    "Sleeping" = c(0,1,1,2,2),
    "Breathing" = c(0,1,1,2,2),
    "Household" = c(0,1,1,2,2),
    "Caring" = c(0,1,1,2,2),
    "Participating" = c(0,1,1,2,2),
    "Depressivity" = c(0,1,1,2,2),
    "Anxiety" = c(0,1,1,2,2),
    "GettingAlong" = c(0,1,1,2,2),
    "Managing"= c(0,1,1,2,2),
    "Pain" = c(0,1,1,2,2)),
  drop_vars = NULL,
  split_strategy = NULL,
  comment = "Recode all items to have 3 options, 5 is too much 01122")

#fig_density

#fig_poppyramid
#for this plot the gender should be a factor vector with levels "Female" and "Male"
#the capitalization matters!


MDS.data[which(MDS.data[,"sex"]=="female"),"sex"] = "Female"
MDS.data[which(MDS.data[,"sex"]=="male"),"sex"] = "Male"
MDS.data[,"sex"] = as.factor(MDS.data[,"sex"])
#MDS.data = as_tibble(MDS.data)

class(MDS.data)

fig_poppyramid(
  df = MDS.data,
  var_age = "age",
  var_sex = "sex",
  x_axis = "n",
  age_by = 10
 )


#fig_density
path = "C:/Users/carol/Dropbox/MDS/09 national and regional surveys/Chile 2021/Workshop/Workshop_Part_II/WHOMDS_Outputs/Start/"
score.data = read.csv(file = paste(path, "DatawAbilities.csv", sep = ""), header = TRUE, sep = ",")
colnames(score.data)

#rescale the person parameter from 0 to 100
library(scales)

#Disability score from 0 to 100
score.data[,"disability_score"] = scales::rescale(score.data[,"person_pars"], to = c(0,100)) 

#merge the scores with the data containing the person factors (age and sex)
df_mds_score = merge(MDS.data, score.data, by = "ID", all.y = TRUE)
colnames(df_mds_score)

fig_density(df_mds_score, score = "disability_score", cutoffs = c(19.1, 34.4, 49.6), 
            x_lab = "Disability score")
fig_density(df_mds_score, score = "disability_score", var_color = "sex", 
            cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")
fig_density(df_mds_score, score = "disability_score", var_color = "sex", 
            var_facet = "age_grp",  cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")



#######

df_mds_score$disability_cat = NA
                                  
df_mds_score[which(df_mds_score[,"disability_score"]< 19.1), "disability_cat"] = "No"
df_mds_score[which(df_mds_score[,"disability_score"]>= 19.1 &
                   df_mds_score[,"disability_score"]< 34.4), "disability_cat"] = "Mild"
df_mds_score[which(df_mds_score[,"disability_score"]>= 34.4 &
                     df_mds_score[,"disability_score"]< 49.6), "disability_cat"] = "Moderate"
df_mds_score[which(df_mds_score[,"disability_score"]>= 49.6), "disability_cat"] = "Severe"

df_mds_score$disability_cat = as.ordered(df_mds_score$disability_cat)

fig_dist(df_mds_score, score = "disability_score", score_cat = "disability_cat", 
         cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")
fig_dist(df_mds_score, score = "disability_score", score_cat = "disability_cat", 
         cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score", y_max = 2000)
fig_dist(df_mds_score, score = "disability_score", score_cat = "disability_cat", 
         cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score", y_max = 0.2, pcent=TRUE)


###Analysis of children data


rasch_mds_children(df = df_children, 
                   vars_id = "HHID",
                   vars_group = "age_cat",
                   vars_metric_common = paste0("child", c(1:10)),
                   vars_metric_grouped = list(
                     "Age2to4" = paste0("child", c(12,15,16,19,20,24,25)),
                     "Age5to9" = paste0("child", c(11,13,14,17,18,20,21,22,23,24,25,27)),
                     "Age10to17" = paste0("child", c(11,13,14,17,18,20,21,22,23,25,26,27))),
                   TAM_model = "PCM2",
                   resp_opts = 1:5,
                   has_at_least_one = 4:5,
                   max_NA = 10,
                   print_results = TRUE,
                   path_parent = path_output,
                   model_name = "Start_children",
                   testlet_strategy = NULL,
                   recode_strategy = NULL,
                   drop_vars = NULL,
                   split_strategy = NULL,
                   comment = "Initial run")
