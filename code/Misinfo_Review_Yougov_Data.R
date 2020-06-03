

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv#
#
#         The consequences of fake news (Misinformation Review)
#                Katya Ognyanova, katya@ognyanova.net 
#
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv#
 

library(survey) 
library(lavaan.survey)  
library(poliscidata)

dat <- read.csv("../data/Misinfo_Review_Yougov_Data.csv", header=T, as.is=T)
dat.sv <- svydesign(ids=~1, weight=~weight, data=dat)


#=============================== ~~~~~~~~~~~~~ ================================== 
#===============================   VARIABLES    ==================================


# conf_wh   : Confidence in the White House; 1 (None) to 5 (A great deal)
# conf_cong : Confidence in Congress; 1 (None) to 5 (A great deal)
# conf_sc   : Confidence in the Supreme Court; 1 (None) to 5 (A great deal)
# conf_mil  : Confidence in the military; 1 (None) to 5 (A great deal)
# conf_crim : Confidence in the criminal justice system; 1 (None) to 5 (A great deal)
# conf_gov  : Average of the five political confidence variables
# 
# 
# conf_wh_w2   : Confidence in the White House, WAVE 2
# conf_cong_w2 : Confidence in Congress, WAVE 2
# conf_sc_w2   : Confidence in the Supreme Court, WAVE 2
# conf_mil_w2  : Confidence in the military, WAVE 2
# conf_crim_w2 : Confidence in the criminal justice system, WAVE 2
# conf_gov_w2  : Average of the five political confidence variables, WAVE 2
# 
# conf_med    : Condidence in mainstream media; 1 (None) to 5 (A great deal)
# conf_med_w2 : Confidence in mainstream media, WAVE 2
# 
# female      : Gender; 1 (female), 0 (male)
# race_white  : Race; White Non-Hispanic (1), other (0)
# race_black  : Race; Black (1), other (0)
# race_asian  : Race: Asian (1), other (0)
# race_hisp   : Ethnicity; Latino/Hispanic (1), other (0)
# age         : Age in years
# educ        : Education;  1 (Less than high school) to 6 (Graduate degree)
# income      : Income range;  1 (Under $10,000) to 16 (Over $500,000)
# dem         : Party affiliation; 1 (Democrat), 0 (other)
# rep         : Party affiliation; 1 (Republican), 0 (other)
# ideology    : Political ideology; 1 (Very liberal) to 5 (Very conservative)
# interest_pol: Interest in politics; 1 (Not at all interested) to 5 (Extremely interested)
# news_nat    : Following national news;  1 (Not closely at all) to 4 (Very closely)
# tot_news    : Number of news sites visited by respondents (log-transformed)
# 
# fake_news   : Exposure to fake news; 1 (Exposed), 0 (Not exposed)
# fox_news    : Exposure to Fox news; 1 (Exposed), 0 (Not exposed)
# breit_news  : Exposure to Breitbart; 1 (Exposed), 0 (Not exposed)



#=============================== ~~~~~~~~~~~~~ ================================== 
#===============================   ALPHA    ==================================



svycralpha(~ conf_wh + conf_cong + conf_sc + conf_mil + conf_crim, dat.sv)    
svycralpha(~ conf_wh_w2 + conf_cong_w2 + conf_sc_w2 + conf_mil_w2 + conf_crim_w2, dat.sv)    

 

#=============================== ~~~~~~~~~~~~~ ================================== 
#===============================   MAIN MODEL    ==================================
 

dat.sv <- svydesign(ids=~1, weight=~weight, data=dat)



mod.gov <- svyglm(conf_gov_w2 ~ conf_gov + female + race_black + race_hisp + race_asian
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + fox_news + breit_news, design=dat.sv)  
summary(mod.gov) 


mod.wh <- svyglm(conf_wh_w2  ~ conf_wh + female + race_black + race_hisp + race_asian
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + fox_news + breit_news, design=dat.sv)
summary(mod.wh)  


mod.cong <- svyglm(conf_cong_w2  ~ conf_cong + female + race_black + race_hisp + race_asian 
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + fox_news + breit_news, design=dat.sv)
summary(mod.cong) 


mod.sc <- svyglm(conf_sc_w2  ~ conf_sc + female + race_black + race_hisp + race_asian  
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + fox_news + breit_news, design=dat.sv)
summary(mod.sc)  


mod.mil <- svyglm(conf_mib_w2  ~  conf_mil + female + race_black + race_hisp + race_asian
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + fox_news + breit_news, design=dat.sv)
summary(mod.mil) 


mod.crim <- svyglm(conf_crim_w2  ~  conf_crim + female + race_black + race_hisp + race_asian
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + fox_news + breit_news, design=dat.sv)
summary(mod.crim) 



mod.med <- svyglm(conf_med_w2 ~ conf_med + female + race_black + race_hisp + race_asian
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + fox_news + breit_news, design=dat.sv)
summary(mod.med)  
 
  

#=============================== ~~~~~~~~~~~~~ ================================== 
#===============================  INTERACTIONS  ==================================  

  

mod.gov.i <- svyglm(conf_gov_w2 ~ conf_gov + female + race_black + race_hisp + race_asian
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + ideology*fake_news, design=dat.sv)
summary(mod.gov.i)
 

mod.med.i <- svyglm(conf_med_w2 ~ conf_med + female + race_black + race_hisp + race_asian
                + age + educ + income + dem + rep + ideology + interest_pol 
                + news_nat + tot_news + fake_news + ideology*fake_news, design=dat.sv)
summary(mod.med.i)
 

  
 
library(interactions)

interact_plot(mod.gov.i, pred="ideology", modx="fake_news", interval = T, int.width = 0.95)

interact_plot(mod.med.i, pred="ideology", modx="fake_news", interval = T, int.width = 0.95)

detach(package:interactions)



 

#=============================== ~~~~~~~~~~~~~ ================================== 
#===============================   The End   ==================================




