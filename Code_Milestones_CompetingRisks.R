
library(survival)
library(survminer)
library(lubridate)
library(cmprsk)
library(tidyverse)
library(utile.visuals)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(pROC)
library(caret)
library(mgcv)



# Global time-to-death ----------------------
GlobalTimeToDeath=read.csv("GlobalTimeToDeath.csv", sep=";")

Surv(GlobalTimeToDeath$time, GlobalTimeToDeath$status)

# [1] "60"  "96+" "60"  "96+" "96+" "96+" "96+" "96+" "96+"
# [10] "96+" "96+" "96+" "84"  "96+" "96+" "96+" "96+" "28" 
# [19] "96+" "75"  "96+" "96+" "96+" "60+" "96+" "96+" "96+"
# [28] "96+" "96+" "61"  "96+" "58"  "96+" "39"  "96+" "96+"
# [37] "96+" "96+" "96+" "96+" "96+" "96+" "96+" "96+" "95+"
# [46] "96+" "96+" "96+" "96+" "96+" "96+" "96+" "96+" "96+"
# [55] "57+" "96+" "67"  "96+" "96+" "96+" "88"  "96+" "76" 
# [64] "96+" "96+" "96+" "96+" "35+" "96+" "96+" "96+" "96+"
# [73] "96+" "45+" "96+" "96+" "96+" "96+" "96+" "88"  "96+"
# [82] "96+" "96+" "96+" "96+" "95+" "96+" " 5+" "96+" "96+"
# [91] "96+" "96+" "96+" "96+" "16"  "54"  "96+" "61+" "96+"
# [100] "96+" "96+" "96+" "96+" "96+" "96+" "86"  "42"  "96+"
# [109] "74" 

ggsurvplot(
  fit = survfit(Surv(time, status) ~ 1, data = GlobalTimeToDeath), 
  xlab = "Follow-up (Months)", 
  ylab = "Overall survival probability",
  legend = "none",
  break.x.by = 12,
  risk.table = TRUE,
  tables.col = "strata")

# Check Plot -> Export (...)

# Probability of surviving X's years
summary(survfit(Surv(time, status) ~ 1, data = GlobalTimeToDeath), times = 12)

# Call: survfit(formula = Surv(time, status) ~ 1, data = GlobalTimeToDeath)
# 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
# 12    108       0        1       0            1            1

summary(survfit(Surv(time, status) ~ 1, data = GlobalTimeToDeath), times = 24)

# Call: survfit(formula = Surv(time, status) ~ 1, data = GlobalTimeToDeath)
# 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
# 24    107       1    0.991 0.00922        0.973            1

summary(survfit(Surv(time, status) ~ 1, data = GlobalTimeToDeath), times = 48)
# 
# Call: survfit(formula = Surv(time, status) ~ 1, data = GlobalTimeToDeath)
# 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
# 48    102       4    0.963  0.0183        0.928        0.999

summary(survfit(Surv(time, status) ~ 1, data = GlobalTimeToDeath), times = 96)
# 
# Call: survfit(formula = Surv(time, status) ~ 1, data = GlobalTimeToDeath)
# 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
# 96     84      17    0.837  0.0362        0.769        0.911

# By gender 
ggsurvplot(
  fit = survfit(Surv(time, status) ~ sex, data = GlobalTimeToDeath), 
  xlab = "Follow-up (Months)", 
  ylab = "Overall survival probability",
  legend = "none",
  break.x.by = 12,
  risk.table = TRUE,
  tables.col = "strata",
  conf.int = TRUE)

# Check Plot -> Export (...)

coxph(Surv(time, status) ~ sex, data = GlobalTimeToDeath)

# Call:
#   coxph(formula = Surv(time, status) ~ sex, data = GlobalTimeToDeath)
# 
# coef exp(coef) se(coef)      z    p
# sex -0.01224   0.98783  0.48599 -0.025 0.98
# 
# Likelihood ratio test=0  on 1 df, p=0.9799
# n= 109, number of events= 17 






#  Cox proportional-hazards regression modeling to measure the association between baseline variables 
#  and survival of Parkinson's disease patients after Deep brain stimulation surgery 

GlobalDeath = read.csv("GlobalDeath.csv", sep = ";", na.strings=c("", " ","NA"))

# Measuring the contribution of each variable individually (i.e. non-adjusted) -------------

res.cox <- coxph(Surv(time, status) ~ AgeSurgery, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery, data = GlobalDeath)
# 
# n= 109, number of events= 17 
# 
# coef exp(coef) se(coef)     z Pr(>|z|)  
# AgeSurgery 0.1024    1.1079   0.0484 2.116   0.0343 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery     1.108     0.9026     1.008     1.218
# 
# Concordance= 0.67  (se = 0.062 )
# Likelihood ratio test= 5.84  on 1 df,   p=0.02
# Wald test            = 4.48  on 1 df,   p=0.03
# Score (logrank) test = 4.53  on 1 df,   p=0.03

res.cox <- coxph(Surv(time, status) ~ Sex, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ Sex, data = GlobalDeath)
# 
# n= 109, number of events= 17 
# 
# coef exp(coef) se(coef)      z Pr(>|z|)
# SexM -0.01224   0.98783  0.48599 -0.025     0.98
# 
# exp(coef) exp(-coef) lower .95 upper .95
# SexM    0.9878      1.012    0.3811     2.561
# 
# Concordance= 0.5  (se = 0.061 )
# Likelihood ratio test= 0  on 1 df,   p=1
# Wald test            = 0  on 1 df,   p=1
# Score (logrank) test = 0  on 1 df,   p=1


res.cox <- coxph(Surv(time, status) ~ DiseaseDurationSurgery, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ DiseaseDurationSurgery, 
#         data = GlobalDeath)
# 
# n= 105, number of events= 17 
# (4 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)
# DiseaseDurationSurgery 0.04324   1.04419  0.03484 1.241    0.215
# 
# exp(coef) exp(-coef) lower .95 upper .95
# DiseaseDurationSurgery     1.044     0.9577    0.9753     1.118
# 
# Concordance= 0.523  (se = 0.086 )
# Likelihood ratio test= 1.31  on 1 df,   p=0.3
# Wald test            = 1.54  on 1 df,   p=0.2
# Score (logrank) test = 1.54  on 1 df,   p=0.2

res.cox <- coxph(Surv(time, status) ~ LEDDpreop, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ LEDDpreop, data = GlobalDeath)
# 
# n= 109, number of events= 17 
# 
# coef  exp(coef)   se(coef)      z Pr(>|z|)
# LEDDpreop -0.0004030  0.9995971  0.0005077 -0.794    0.427
# 
# exp(coef) exp(-coef) lower .95 upper .95
# LEDDpreop    0.9996          1    0.9986     1.001
# 
# Concordance= 0.561  (se = 0.074 )
# Likelihood ratio test= 0.66  on 1 df,   p=0.4
# Wald test            = 0.63  on 1 df,   p=0.4
# Score (logrank) test = 0.63  on 1 df,   p=0.4

res.cox <- coxph(Surv(time, status) ~ LCTresp, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ LCTresp, data = GlobalDeath)
# 
# n= 109, number of events= 17 
# 
# coef exp(coef) se(coef)    z Pr(>|z|)
# LCTresp 0.01366   1.01375  0.01898 0.72    0.472
# 
# exp(coef) exp(-coef) lower .95 upper .95
# LCTresp     1.014     0.9864    0.9767     1.052
# 
# Concordance= 0.553  (se = 0.07 )
# Likelihood ratio test= 0.52  on 1 df,   p=0.5
# Wald test            = 0.52  on 1 df,   p=0.5
# Score (logrank) test = 0.52  on 1 df,   p=0.5

res.cox <- coxph(Surv(time, status) ~ AgeSurgeryMORE, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgeryMORE, data = GlobalDeath)
# 
# n= 109, number of events= 17 
# 
# coef exp(coef) se(coef)     z Pr(>|z|)  
# AgeSurgeryMOREMore61 1.2239    3.4004   0.6363 1.923   0.0544 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgeryMOREMore61       3.4     0.2941     0.977     11.84
# 
# Concordance= 0.625  (se = 0.048 )
# Likelihood ratio test= 4.66  on 1 df,   p=0.03
# Wald test            = 3.7  on 1 df,   p=0.05
# Score (logrank) test = 4.18  on 1 df,   p=0.04

res.cox <- coxph(Surv(time, status) ~ DiseaseDurationSurgeryMORE, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ DiseaseDurationSurgeryMORE, 
#         data = GlobalDeath)
# 
# n= 104, number of events= 16 
# (5 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)
# DiseaseDurationSurgeryMOREMore14 0.2645    1.3028   0.5040 0.525      0.6
# 
# exp(coef) exp(-coef) lower .95 upper .95
# DiseaseDurationSurgeryMOREMore14     1.303     0.7676    0.4852     3.499
# 
# Concordance= 0.533  (se = 0.062 )
# Likelihood ratio test= 0.27  on 1 df,   p=0.6
# Wald test            = 0.28  on 1 df,   p=0.6
# Score (logrank) test = 0.28  on 1 df,   p=0.6

res.cox <- coxph(Surv(time, status) ~ LCTrespMORE, data = GlobalDeath)
summary(res.cox)
# 
# Call:
#   coxph(formula = Surv(time, status) ~ LCTrespMORE, data = GlobalDeath)
# 
# n= 106, number of events= 17 
# (3 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)
# LCTrespMOREMore50 0.09488   1.09953  0.53235 0.178    0.859
# 
# exp(coef) exp(-coef) lower .95 upper .95
# LCTrespMOREMore50       1.1     0.9095    0.3873     3.121
# 
# Concordance= 0.513  (se = 0.055 )
# Likelihood ratio test= 0.03  on 1 df,   p=0.9
# Wald test            = 0.03  on 1 df,   p=0.9
# Score (logrank) test = 0.03  on 1 df,   p=0.9

res.cox <- coxph(Surv(time, status) ~ GAITOFFmore, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ GAITOFFmore, data = GlobalDeath)
# 
# n= 105, number of events= 17 
# (4 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)    z Pr(>|z|)
# GAITOFFmoreZeroOROne 0.0885    1.0925   0.4929 0.18    0.857
# 
# exp(coef) exp(-coef) lower .95 upper .95
# GAITOFFmoreZeroOROne     1.093     0.9153    0.4158     2.871
# 
# Concordance= 0.501  (se = 0.058 )
# Likelihood ratio test= 0.03  on 1 df,   p=0.9
# Wald test            = 0.03  on 1 df,   p=0.9
# Score (logrank) test = 0.03  on 1 df,   p=0.9

res.cox <- coxph(Surv(time, status) ~ PostInstOFFmore, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ PostInstOFFmore, data = GlobalDeath)
# 
# n= 104, number of events= 15 
# (5 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)
# PostInstOFFmoreZeroOne 0.2723    1.3130   0.5478 0.497    0.619
# 
# exp(coef) exp(-coef) lower .95 upper .95
# PostInstOFFmoreZeroOne     1.313     0.7616    0.4488     3.842
# 
# Concordance= 0.53  (se = 0.062 )
# Likelihood ratio test= 0.25  on 1 df,   p=0.6
# Wald test            = 0.25  on 1 df,   p=0.6
# Score (logrank) test = 0.25  on 1 df,   p=0.6

res.cox <- coxph(Surv(time, status) ~ HYOFFmore, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ HYOFFmore, data = GlobalDeath)
# 
# n= 106, number of events= 17 
# (3 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)      z Pr(>|z|)
# HYOFFmoreMore2 -0.03386   0.96671  0.49290 -0.069    0.945
# 
# exp(coef) exp(-coef) lower .95 upper .95
# HYOFFmoreMore2    0.9667      1.034    0.3679      2.54
# 
# Concordance= 0.492  (se = 0.061 )
# Likelihood ratio test= 0  on 1 df,   p=0.9
# Wald test            = 0  on 1 df,   p=0.9
# Score (logrank) test = 0  on 1 df,   p=0.9

res.cox <- coxph(Surv(time, status) ~ factor(Neuropsychologic, levels = c("Normal", "MCI")), data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ factor(Neuropsychologic, 
#                                               levels = c("Normal", "MCI")), data = GlobalDeath)
# 
# n= 90, number of events= 15 
# (19 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)  
# factor(Neuropsychologic, levels = c("Normal", "MCI"))MCI 1.0315    2.8053   0.5277 1.955   0.0506 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# factor(Neuropsychologic, levels = c("Normal", "MCI"))MCI     2.805     0.3565    0.9972     7.892
# 
# Concordance= 0.606  (se = 0.062 )
# Likelihood ratio test= 3.44  on 1 df,   p=0.06
# Wald test            = 3.82  on 1 df,   p=0.05
# Score (logrank) test = 4.17  on 1 df,   p=0.04

res.cox <- coxph(Surv(time, status) ~ UPDRS3off, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ UPDRS3off, data = GlobalDeath)
# 
# n= 105, number of events= 17 
# (4 observations deleted due to missingness)
# 
# coef exp(coef)  se(coef)      z Pr(>|z|)
# UPDRS3off -0.007503  0.992525  0.019265 -0.389    0.697
# 
# exp(coef) exp(-coef) lower .95 upper .95
# UPDRS3off    0.9925      1.008    0.9557     1.031
# 
# Concordance= 0.567  (se = 0.081 )
# Likelihood ratio test= 0.15  on 1 df,   p=0.7
# Wald test            = 0.15  on 1 df,   p=0.7
# Score (logrank) test = 0.15  on 1 df,   p=0.7

res.cox <- coxph(Surv(time, status) ~ UPDRS3on, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ UPDRS3on, data = GlobalDeath)
# 
# n= 106, number of events= 17 
# (3 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)
# UPDRS3on 0.02697   1.02734  0.03168 0.851    0.395
# 
# exp(coef) exp(-coef) lower .95 upper .95
# UPDRS3on     1.027     0.9734    0.9655     1.093
# 
# Concordance= 0.536  (se = 0.079 )
# Likelihood ratio test= 0.69  on 1 df,   p=0.4
# Wald test            = 0.72  on 1 df,   p=0.4
# Score (logrank) test = 0.73  on 1 df,   p=0.4


res.cox <- coxph(Surv(time, status) ~ UPDRS2off, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ UPDRS2off, data = GlobalDeath)
# 
# n= 98, number of events= 15 
# (11 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)
# UPDRS2off 0.03832   1.03907  0.03508 1.093    0.275
# 
# exp(coef) exp(-coef) lower .95 upper .95
# UPDRS2off     1.039     0.9624      0.97     1.113
# 
# Concordance= 0.56  (se = 0.071 )
# Likelihood ratio test= 1.18  on 1 df,   p=0.3
# Wald test            = 1.19  on 1 df,   p=0.3
# Score (logrank) test = 1.2  on 1 df,   p=0.3

res.cox <- coxph(Surv(time, status) ~ UPDRS2on, data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ UPDRS2on, data = GlobalDeath)
# 
# n= 98, number of events= 17 
# (11 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)
# UPDRS2on 0.03738   1.03808  0.03693 1.012    0.312
# 
# exp(coef) exp(-coef) lower .95 upper .95
# UPDRS2on     1.038     0.9633    0.9656     1.116
# 
# Concordance= 0.58  (se = 0.073 )
# Likelihood ratio test= 0.95  on 1 df,   p=0.3
# Wald test            = 1.02  on 1 df,   p=0.3
# Score (logrank) test = 1.03  on 1 df,   p=0.3

res.cox <- coxph(Surv(time, status) ~ factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.cox)

# Call:
#   coxph(formula = Surv(time, status) ~ factor(Phenotype, levels = c("Tremor", 
#                                                                     "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 96, number of events= 16 
# (13 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD          0.9607    2.6134   0.6010
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate 1.1345    3.1096   0.7638
# z Pr(>|z|)
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD          1.598    0.110
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate 1.485    0.137
# 
# exp(coef) exp(-coef)
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD              2.613     0.3826
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate     3.110     0.3216
# lower .95 upper .95
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             0.8047     8.488
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    0.6959    13.896
# 
# Concordance= 0.628  (se = 0.061 )
# Likelihood ratio test= 3.51  on 2 df,   p=0.2
# Wald test            = 3.09  on 2 df,   p=0.2
# Score (logrank) test = 3.36  on 2 df,   p=0.2

# Adjusting for different variables, Multiple models compared --------------

res.coxGLOBAL <- coxph(Surv(time, status) ~ AgeSurgery + factor(Sex, levels = c("F", "M")) + 
                   DiseaseDurationSurgery +  LEDDpreop + 
                   factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2")) +
                   factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) +
                   factor(Neuropsychologic, levels = c("Normal", "MCI")) + 
                   UPDRS3off +  UPDRS3on +  UPDRS2off +  UPDRS2on + 
                   factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL)

# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + factor(Sex, 
#                                                            levels = c("F", "M")) + DiseaseDurationSurgery + LEDDpreop + 
#           factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2")) + 
#           factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) + 
#           factor(Neuropsychologic, levels = c("Normal", "MCI")) + UPDRS3off + 
#           UPDRS3on + UPDRS2off + UPDRS2on + factor(Phenotype, levels = c("Tremor", 
#                                                                          "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 73, number of events= 11 
# (36 observations deleted due to missingness)
# 
# coef  exp(coef)   se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.1915746  1.2111552  0.1052675  1.820   0.0688 .
# factor(Sex, levels = c("F", "M"))M                                             0.6103711  1.8411144  0.8522117  0.716   0.4739  
# DiseaseDurationSurgery                                                         0.0565598  1.0581899  0.0629302  0.899   0.3688  
# LEDDpreop                                                                     -0.0006150  0.9993852  0.0007182 -0.856   0.3918  
# factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2"))EqualORMore2      -0.2355050  0.7901717  0.8215139 -0.287   0.7744  
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2        -1.3742662  0.2530252  1.0943083 -1.256   0.2092  
# factor(Neuropsychologic, levels = c("Normal", "MCI"))MCI                       0.0776550  1.0807497  1.0516408  0.074   0.9411  
# UPDRS3off                                                                     -0.0454664  0.9555517  0.0330123 -1.377   0.1684  
# UPDRS3on                                                                       0.0816657  1.0850930  0.0554545  1.473   0.1408  
# UPDRS2off                                                                      0.0983207  1.1033165  0.0703415  1.398   0.1622  
# UPDRS2on                                                                      -0.1253948  0.8821485  0.0811308 -1.546   0.1222  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.2983418  3.6632173  0.8629053  1.505   0.1324  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.3595275  1.4326524  1.2551020  0.286   0.7745  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.2112     0.8257   0.98536     1.489
# factor(Sex, levels = c("F", "M"))M                                               1.8411     0.5431   0.34648     9.783
# DiseaseDurationSurgery                                                           1.0582     0.9450   0.93540     1.197
# LEDDpreop                                                                        0.9994     1.0006   0.99798     1.001
# factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2"))EqualORMore2         0.7902     1.2655   0.15792     3.954
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2           0.2530     3.9522   0.02963     2.161
# factor(Neuropsychologic, levels = c("Normal", "MCI"))MCI                         1.0807     0.9253   0.13758     8.490
# UPDRS3off                                                                        0.9556     1.0465   0.89568     1.019
# UPDRS3on                                                                         1.0851     0.9216   0.97334     1.210
# UPDRS2off                                                                        1.1033     0.9064   0.96123     1.266
# UPDRS2on                                                                         0.8821     1.1336   0.75246     1.034
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             3.6632     0.2730   0.67508    19.878
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.4327     0.6980   0.12240    16.768
# 
# Concordance= 0.803  (se = 0.058 )
# Likelihood ratio test= 13.91  on 13 df,   p=0.4
# Wald test            = 10.43  on 13 df,   p=0.7
# Score (logrank) test = 13.11  on 13 df,   p=0.4

res.coxGLOBAL2 <- coxph(Surv(time, status) ~ AgeSurgery +  factor(Sex, levels = c("F", "M")) + 
                         DiseaseDurationSurgery +  LEDDpreop + 
                         factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2")) +
                         factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) +
                         UPDRS3off +  UPDRS3on +  UPDRS2off +  UPDRS2on + 
                         factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL2)

# 
# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + factor(Sex, 
#                                                            levels = c("F", "M")) + DiseaseDurationSurgery + LEDDpreop + 
#           factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2")) + 
#           factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) + 
#           UPDRS3off + UPDRS3on + UPDRS2off + UPDRS2on + factor(Phenotype, 
#                                                                levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 87, number of events= 12 
# (22 observations deleted due to missingness)
# 
# coef  exp(coef)   se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.2085287  1.2318643  0.0926687  2.250   0.0244 *
#   factor(Sex, levels = c("F", "M"))M                                             0.4775387  1.6121017  0.7052464  0.677   0.4983  
# DiseaseDurationSurgery                                                         0.0449609  1.0459869  0.0613367  0.733   0.4635  
# LEDDpreop                                                                     -0.0008411  0.9991593  0.0006961 -1.208   0.2270  
# factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2"))EqualORMore2      -0.4457803  0.6403244  0.7714492 -0.578   0.5634  
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2        -1.4043020  0.2455384  0.9738066 -1.442   0.1493  
# UPDRS3off                                                                     -0.0494704  0.9517333  0.0311519 -1.588   0.1123  
# UPDRS3on                                                                       0.0828404  1.0863685  0.0494948  1.674   0.0942 .
# UPDRS2off                                                                      0.1012527  1.1065563  0.0666491  1.519   0.1287  
# UPDRS2on                                                                      -0.0921636  0.9119560  0.0754159 -1.222   0.2217  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.4671618  4.3369085  0.7794918  1.882   0.0598 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.2984962  1.3478305  1.1847677  0.252   0.8011  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.2319     0.8118   1.02727     1.477
# factor(Sex, levels = c("F", "M"))M                                               1.6121     0.6203   0.40466     6.422
# DiseaseDurationSurgery                                                           1.0460     0.9560   0.92751     1.180
# LEDDpreop                                                                        0.9992     1.0008   0.99780     1.001
# factor(GAITOFFmore, levels = c("ZeroOROne", "EqualORMore2"))EqualORMore2         0.6403     1.5617   0.14117     2.904
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2           0.2455     4.0727   0.03641     1.656
# UPDRS3off                                                                        0.9517     1.0507   0.89536     1.012
# UPDRS3on                                                                         1.0864     0.9205   0.98593     1.197
# UPDRS2off                                                                        1.1066     0.9037   0.97105     1.261
# UPDRS2on                                                                         0.9120     1.0965   0.78665     1.057
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             4.3369     0.2306   0.94119    19.984
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.3478     0.7419   0.13218    13.744
# 
# Concordance= 0.826  (se = 0.054 )
# Likelihood ratio test= 16.48  on 12 df,   p=0.2
# Wald test            = 11.59  on 12 df,   p=0.5
# Score (logrank) test = 13.48  on 12 df,   p=0.3


res.coxGLOBAL3 <- coxph(Surv(time, status) ~ AgeSurgery +factor(Sex, levels = c("F", "M")) + 
                          DiseaseDurationSurgery +  LEDDpreop +   factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) +
                          UPDRS3off +   UPDRS3on +  UPDRS2off +  UPDRS2on + 
                          factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL3)

# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + factor(Sex, 
#                                                            levels = c("F", "M")) + DiseaseDurationSurgery + LEDDpreop + 
#           factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) + 
#           UPDRS3off + UPDRS3on + UPDRS2off + UPDRS2on + factor(Phenotype, 
#                                                                levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 87, number of events= 12 
# (22 observations deleted due to missingness)
# 
# coef  exp(coef)   se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.2028846  1.2249311  0.0916235  2.214   0.0268 *
#   factor(Sex, levels = c("F", "M"))M                                             0.5447206  1.7241267  0.6900456  0.789   0.4299  
# DiseaseDurationSurgery                                                         0.0536488  1.0551140  0.0600216  0.894   0.3714  
# LEDDpreop                                                                     -0.0008464  0.9991540  0.0006779 -1.249   0.2118  
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2        -1.3997213  0.2466657  0.9615748 -1.456   0.1455  
# UPDRS3off                                                                     -0.0537532  0.9476659  0.0304939 -1.763   0.0779 .
# UPDRS3on                                                                       0.0891841  1.0932819  0.0471101  1.893   0.0583 .
# UPDRS2off                                                                      0.0871202  1.0910278  0.0604855  1.440   0.1498  
# UPDRS2on                                                                      -0.0936379  0.9106125  0.0737171 -1.270   0.2040  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.4081782  4.0885000  0.7758962  1.815   0.0695 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.2715498  1.3119962  1.1669087  0.233   0.8160  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.2249     0.8164   1.02358     1.466
# factor(Sex, levels = c("F", "M"))M                                               1.7241     0.5800   0.44586     6.667
# DiseaseDurationSurgery                                                           1.0551     0.9478   0.93801     1.187
# LEDDpreop                                                                        0.9992     1.0008   0.99783     1.000
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2           0.2467     4.0541   0.03746     1.624
# UPDRS3off                                                                        0.9477     1.0552   0.89269     1.006
# UPDRS3on                                                                         1.0933     0.9147   0.99685     1.199
# UPDRS2off                                                                        1.0910     0.9166   0.96906     1.228
# UPDRS2on                                                                         0.9106     1.0982   0.78811     1.052
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             4.0885     0.2446   0.89355    18.707
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.3120     0.7622   0.13325    12.918
# 
# Concordance= 0.814  (se = 0.054 )
# Likelihood ratio test= 16.16  on 11 df,   p=0.1
# Wald test            = 11.66  on 11 df,   p=0.4
# Score (logrank) test = 13.16  on 11 df,   p=0.3


res.coxGLOBAL4 <- coxph(Surv(time, status) ~ AgeSurgery + DiseaseDurationSurgery + LEDDpreop + 
                          factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) +
                          UPDRS3off + UPDRS3on +  UPDRS2off +  UPDRS2on + 
                          factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL4)


# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + DiseaseDurationSurgery + 
#           LEDDpreop + factor(PostInstOFFmore, levels = c("ZeroOne", 
#                                                          "EqualMore2")) + UPDRS3off + UPDRS3on + UPDRS2off + UPDRS2on + 
#           factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), 
#         data = GlobalDeath)
# 
# n= 87, number of events= 12 
# (22 observations deleted due to missingness)
# 
# coef  exp(coef)   se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.1976215  1.2185011  0.0911047  2.169   0.0301 *
#   DiseaseDurationSurgery                                                         0.0402473  1.0410682  0.0560973  0.717   0.4731  
# LEDDpreop                                                                     -0.0006326  0.9993676  0.0006126 -1.033   0.3017  
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2        -1.3160961  0.2681802  0.9559887 -1.377   0.1686  
# UPDRS3off                                                                     -0.0523227  0.9490225  0.0298942 -1.750   0.0801 .
# UPDRS3on                                                                       0.0922400  1.0966279  0.0484960  1.902   0.0572 .
# UPDRS2off                                                                      0.0762900  1.0792756  0.0564337  1.352   0.1764  
# UPDRS2on                                                                      -0.0846290  0.9188531  0.0705472 -1.200   0.2303  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.3142601  3.7219962  0.7596799  1.730   0.0836 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.3098765  1.3632568  1.1547874  0.268   0.7884  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.2185     0.8207   1.01924     1.457
# DiseaseDurationSurgery                                                           1.0411     0.9606   0.93267     1.162
# LEDDpreop                                                                        0.9994     1.0006   0.99817     1.001
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2           0.2682     3.7288   0.04118     1.746
# UPDRS3off                                                                        0.9490     1.0537   0.89502     1.006
# UPDRS3on                                                                         1.0966     0.9119   0.99719     1.206
# UPDRS2off                                                                        1.0793     0.9265   0.96626     1.206
# UPDRS2on                                                                         0.9189     1.0883   0.80020     1.055
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             3.7220     0.2687   0.83972    16.497
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.3633     0.7335   0.14178    13.108
# 
# Concordance= 0.815  (se = 0.048 )
# Likelihood ratio test= 15.53  on 10 df,   p=0.1
# Wald test            = 11.35  on 10 df,   p=0.3
# Score (logrank) test = 12.28  on 10 df,   p=0.3


res.coxGLOBAL5 <- coxph(Surv(time, status) ~ AgeSurgery + LEDDpreop + 
                          factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) + UPDRS3off + 
                          UPDRS3on +  UPDRS2off +  UPDRS2on + 
                          factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL5)


# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + LEDDpreop + 
#           factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) + 
#           UPDRS3off + UPDRS3on + UPDRS2off + UPDRS2on + factor(Phenotype, 
#                                                                levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 89, number of events= 12 
# (20 observations deleted due to missingness)
# 
# coef  exp(coef)   se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.1747938  1.1910006  0.0820526  2.130   0.0331 *
#   LEDDpreop                                                                     -0.0005338  0.9994664  0.0005977 -0.893   0.3718  
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2        -0.9486992  0.3872444  0.8283616 -1.145   0.2521  
# UPDRS3off                                                                     -0.0451395  0.9558641  0.0288319 -1.566   0.1174  
# UPDRS3on                                                                       0.0970260  1.1018891  0.0457841  2.119   0.0341 *
#   UPDRS2off                                                                      0.0516171  1.0529725  0.0484176  1.066   0.2864  
# UPDRS2on                                                                      -0.0624677  0.9394434  0.0656372 -0.952   0.3412  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.3415555  3.8249885  0.7582750  1.769   0.0769 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.2713344  1.3117137  1.0282778  0.264   0.7919  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.1910     0.8396   1.01407     1.399
# LEDDpreop                                                                        0.9995     1.0005   0.99830     1.001
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2           0.3872     2.5823   0.07636     1.964
# UPDRS3off                                                                        0.9559     1.0462   0.90335     1.011
# UPDRS3on                                                                         1.1019     0.9075   1.00732     1.205
# UPDRS2off                                                                        1.0530     0.9497   0.95764     1.158
# UPDRS2on                                                                         0.9394     1.0645   0.82604     1.068
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             3.8250     0.2614   0.86534    16.907
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.3117     0.7624   0.17481     9.843
# 
# Concordance= 0.803  (se = 0.057 )
# Likelihood ratio test= 14.39  on 9 df,   p=0.1
# Wald test            = 11.23  on 9 df,   p=0.3
# Score (logrank) test = 11.87  on 9 df,   p=0.2
# 


res.coxGLOBAL6 <- coxph(Surv(time, status) ~ AgeSurgery +factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) +
                          UPDRS3off + UPDRS3on +  UPDRS2off +   UPDRS2on + 
                          factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL6)



# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + factor(PostInstOFFmore, 
#                                                            levels = c("ZeroOne", "EqualMore2")) + UPDRS3off + UPDRS3on + 
#           UPDRS2off + UPDRS2on + factor(Phenotype, levels = c("Tremor", 
#                                                               "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 89, number of events= 12 
# (20 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.17259   1.18838  0.08544  2.020   0.0434 *
#   factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2        -0.82353   0.43888  0.77524 -1.062   0.2881  
# UPDRS3off                                                                     -0.04882   0.95235  0.02859 -1.708   0.0877 .
# UPDRS3on                                                                       0.09412   1.09869  0.04456  2.112   0.0346 *
#   UPDRS2off                                                                      0.04809   1.04927  0.05003  0.961   0.3365  
# UPDRS2on                                                                      -0.06069   0.94111  0.06698 -0.906   0.3649  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.18742   3.27862  0.73879  1.607   0.1080  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.31681   1.37275  1.02226  0.310   0.7566  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.1884     0.8415   1.00514     1.405
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2           0.4389     2.2785   0.09604     2.006
# UPDRS3off                                                                        0.9524     1.0500   0.90046     1.007
# UPDRS3on                                                                         1.0987     0.9102   1.00682     1.199
# UPDRS2off                                                                        1.0493     0.9530   0.95125     1.157
# UPDRS2on                                                                         0.9411     1.0626   0.82533     1.073
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             3.2786     0.3050   0.77060    13.949
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.3727     0.7285   0.18511    10.180
# 
# Concordance= 0.793  (se = 0.056 )
# Likelihood ratio test= 13.53  on 8 df,   p=0.09
# Wald test            = 10.17  on 8 df,   p=0.3
# Score (logrank) test = 11.03  on 8 df,   p=0.2


res.coxGLOBAL7 <- coxph(Surv(time, status) ~ AgeSurgery + factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2")) +
                          UPDRS3off + UPDRS3on +  UPDRS2off + factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL7)


# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + factor(PostInstOFFmore, 
#                                                         levels = c("ZeroOne", "EqualMore2")) + UPDRS3off + UPDRS3on + 
#        UPDRS2off + factor(Phenotype, levels = c("Tremor", "PIGD", 
#                                                 "Indeterminate")), data = GlobalDeath)
# 
# n= 90, number of events= 12 
# (19 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.15689   1.16986  0.08081  1.942   0.0522 .
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2        -0.49177   0.61154  0.66744 -0.737   0.4612  
# UPDRS3off                                                                     -0.04325   0.95768  0.02897 -1.493   0.1355  
# UPDRS3on                                                                       0.07524   1.07814  0.03951  1.904   0.0568 .
# UPDRS2off                                                                      0.03709   1.03779  0.04957  0.748   0.4543  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.23734   3.44642  0.73362  1.687   0.0917 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.16967   1.18492  0.98243  0.173   0.8629  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.1699     0.8548    0.9985     1.371
# factor(PostInstOFFmore, levels = c("ZeroOne", "EqualMore2"))EqualMore2           0.6115     1.6352    0.1653     2.262
# UPDRS3off                                                                        0.9577     1.0442    0.9048     1.014
# UPDRS3on                                                                         1.0781     0.9275    0.9978     1.165
# UPDRS2off                                                                        1.0378     0.9636    0.9417     1.144
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             3.4464     0.2902    0.8183    14.515
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.1849     0.8439    0.1728     8.127
# 
# Concordance= 0.777  (se = 0.058 )
# Likelihood ratio test= 12.97  on 7 df,   p=0.07
# Wald test            = 9.94  on 7 df,   p=0.2
# Score (logrank) test = 10.91  on 7 df,   p=0.1


res.coxGLOBAL8 <- coxph(Surv(time, status) ~ AgeSurgery + UPDRS3off + UPDRS3on +   UPDRS2off + 
                          factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL8)


# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + UPDRS3off + 
#           UPDRS3on + UPDRS2off + factor(Phenotype, levels = c("Tremor", 
#                                                               "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 94, number of events= 14 
# (15 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.20763   1.23075  0.08826  2.352   0.0187 *
#   UPDRS3off                                                                     -0.06017   0.94160  0.02877 -2.091   0.0365 *
#   UPDRS3on                                                                       0.05742   1.05910  0.03340  1.719   0.0855 .
# UPDRS2off                                                                      0.05999   1.06182  0.04694  1.278   0.2013  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           0.94176   2.56449  0.64244  1.466   0.1427  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate -0.19868   0.81982  0.92825 -0.214   0.8305  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.2308     0.8125    1.0352    1.4632
# UPDRS3off                                                                        0.9416     1.0620    0.8900    0.9962
# UPDRS3on                                                                         1.0591     0.9442    0.9920    1.1307
# UPDRS2off                                                                        1.0618     0.9418    0.9685    1.1642
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             2.5645     0.3899    0.7280    9.0333
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    0.8198     1.2198    0.1329    5.0564
# 
# Concordance= 0.784  (se = 0.057 )
# Likelihood ratio test= 16.17  on 6 df,   p=0.01
# Wald test            = 10.87  on 6 df,   p=0.09
# Score (logrank) test = 12.2  on 6 df,   p=0.06



res.coxGLOBAL9 <- coxph(Surv(time, status) ~ AgeSurgery + UPDRS3off + UPDRS3on + 
                          factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL9)


# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + UPDRS3off + 
#           UPDRS3on + factor(Phenotype, levels = c("Tremor", "PIGD", 
#                                                   "Indeterminate")), data = GlobalDeath)
# 
# n= 96, number of events= 16 
# (13 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)      z Pr(>|z|)  
# AgeSurgery                                                                     0.08646   1.09031  0.05185  1.668   0.0954 .
# UPDRS3off                                                                     -0.02165   0.97858  0.02271 -0.953   0.3405  
# UPDRS3on                                                                       0.05697   1.05862  0.03460  1.647   0.0997 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD           1.15920   3.18737  0.61647  1.880   0.0601 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate  0.63097   1.87944  0.80267  0.786   0.4318  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                       1.0903     0.9172    0.9850     1.207
# UPDRS3off                                                                        0.9786     1.0219    0.9360     1.023
# UPDRS3on                                                                         1.0586     0.9446    0.9892     1.133
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD             3.1874     0.3137    0.9521    10.670
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate    1.8794     0.5321    0.3898     9.063
# 
# Concordance= 0.719  (se = 0.059 )
# Likelihood ratio test= 9.66  on 5 df,   p=0.09
# Wald test            = 8.32  on 5 df,   p=0.1
# Score (logrank) test = 8.91  on 5 df,   p=0.1


res.coxGLOBAL10 <- coxph(Surv(time, status) ~ AgeSurgery + UPDRS3on + 
                          factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL10)


# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + UPDRS3on + 
#           factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), 
#         data = GlobalDeath)
# 
# n= 96, number of events= 16 
# (13 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)  
# AgeSurgery                                                                    0.08333   1.08690  0.04968 1.677   0.0935 .
# UPDRS3on                                                                      0.04093   1.04178  0.03370 1.215   0.2245  
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD          1.16226   3.19716  0.60831 1.911   0.0561 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate 0.58531   1.79555  0.81063 0.722   0.4703  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                        1.087     0.9200    0.9861     1.198
# UPDRS3on                                                                          1.042     0.9599    0.9752     1.113
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD              3.197     0.3128    0.9704    10.533
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate     1.796     0.5569    0.3666     8.794
# 
# Concordance= 0.725  (se = 0.055 )
# Likelihood ratio test= 8.76  on 4 df,   p=0.07
# Wald test            = 7.87  on 4 df,   p=0.1
# Score (logrank) test = 8.36  on 4 df,   p=0.08


res.coxGLOBAL11 <- coxph(Surv(time, status) ~ AgeSurgery + 
                           factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
summary(res.coxGLOBAL11)


# Call:
#   coxph(formula = Surv(time, status) ~ AgeSurgery + factor(Phenotype, 
#                                                            levels = c("Tremor", "PIGD", "Indeterminate")), data = GlobalDeath)
# 
# n= 96, number of events= 16 
# (13 observations deleted due to missingness)
# 
# coef exp(coef) se(coef)     z Pr(>|z|)  
# AgeSurgery                                                                    0.08613   1.08995  0.05036 1.710   0.0872 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD          1.07651   2.93441  0.60253 1.787   0.0740 .
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate 0.77788   2.17686  0.78758 0.988   0.3233  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# AgeSurgery                                                                        1.090     0.9175    0.9875     1.203
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))PIGD              2.934     0.3408    0.9008     9.559
# factor(Phenotype, levels = c("Tremor", "PIGD", "Indeterminate"))Indeterminate     2.177     0.4594    0.4650    10.191
# 
# Concordance= 0.689  (se = 0.061 )
# Likelihood ratio test= 7.29  on 3 df,   p=0.06
# Wald test            = 6.13  on 3 df,   p=0.1
# Score (logrank) test = 6.59  on 3 df,   p=0.09


# Cumulative Incidence
# After Scrucca L., Santucci A., Aversa F. (2007) Competing risks analysis using R: 
# an easy guide for clinicians. Bone Marrow Transplantation, 40, 381--387.

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



# Falls -----------------
falls=read.csv("falls.csv", sep=";")
str(falls)
attach(falls)
disfalls=factor(disfalls, levels=c(0), labels= c("PD"))
table(disfalls, statusfalls)
fitfalls=CumIncidence (ftimefalls, statusfalls, cencode = 0, xlab="Months", t=c(0, 3, 6, 9, 12, 24, 36, 48,72,96,97), level = 0.95)

# Estimates at time points:
#   0        3       6       9      12      24      36      48      72      96 97
# 1 1 0 0.000000 0.00000 0.00000 0.00000 0.00926 0.01852 0.01852 0.02809 0.02809 NA
# 1 2 0 0.009174 0.02769 0.04621 0.06473 0.25920 0.39824 0.46459 0.60817 0.73261 NA
# 
# Standard errors:
#   0        3       6       9      12       24      36      48      72      96 97
# 1 1 0 0.000000 0.00000 0.00000 0.00000 0.009268 0.01305 0.01305 0.01611 0.01611 NA
# 1 2 0 0.009174 0.01584 0.02028 0.02378 0.042389 0.04740 0.04845 0.04779 0.04360 NA


# Freezing of Gait -------------
freezing=read.csv("freezing.csv", sep=";")
str(freezing)
attach(freezing)
disfreezing=factor(disfreezing, levels=c(0), labels= c("PD"))
table(disfreezing, statusfreezing)
fitfreezing=CumIncidence (ftimefreezing, statusfreezing, cencode = 0, xlab="Months", t=c(0, 3, 6, 9, 12, 24, 36, 48,72,96,97), level = 0.95)


# Estimates at time points:
#   0 3 6      9      12       24      36      48      72     96 97
# 1 1 0 0 0 0.0000 0.00000 0.009259 0.01852 0.02778 0.06533 0.1123 NA
# 1 2 0 0 0 0.0463 0.09259 0.222222 0.26852 0.29630 0.39017 0.4747 NA
# 
# Standard errors:
#   0 3 6       9      12       24      36      48      72      96 97
# 1 1 0 0 0 0.00000 0.00000 0.009265 0.01304 0.01591 0.02403 0.03075 NA
# 1 2 0 0 0 0.02032 0.02803 0.040210 0.04287 0.04418 0.04733 0.04855 NA


# Hallucinations ----------
hallucinations=read.csv("hallucinations.csv", sep=";")
str(hallucinations)
attach(hallucinations)
disfreezing=factor(dishallucinations, levels=c(0), labels= c("PD"))
table(dishallucinations, statushallucinations)
fithallucinations=CumIncidence (ftimehallucinations, statushallucinations, cencode = 0, xlab="Months", t=c(0, 3, 6, 9, 12, 24, 36, 48,72,96,97), level = 0.95)

# Estimates at time points:
#   0 3 6 9 12       24      36      48      72      96 97
# 1 1 0 0 0 0  0 0.009259 0.01852 0.02787 0.05644 0.07629 NA
# 1 2 0 0 0 0  0 0.009259 0.05565 0.10270 0.21902 0.31828 NA
# 
# Standard errors:
#   0 3 6 9 12       24      36      48      72      96 97
# 1 1 0 0 0 0  0 0.009259 0.01303 0.01594 0.02251 0.02610 NA
# 1 2 0 0 0 0  0 0.009260 0.02219 0.02949 0.04066 0.04616 NA

# Dementia ----------------
dementia=read.csv("dementia.csv", sep=";")
str(dementia)
attach(dementia)
disdementia=factor(disdementia, levels=c(0), labels= c("PD"))
table(disdementia, statusdementia)
fitdementia=CumIncidence (ftimedementia, statusdementia, cencode = 0, xlab="Months", t=c(0, 3, 6, 9, 12, 24, 36, 48,72,96,97), level = 0.95)

# Estimates at time points:
#   0 3 6        9      12       24      36      48      72      96 97
# 1 1 0 0 0 0.000000 0.00000 0.009259 0.01852 0.02787 0.05647 0.08592 NA
# 1 2 0 0 0 0.009259 0.01852 0.018519 0.06481 0.12114 0.25617 0.34454 NA
# 
# Standard errors:
#   0 3 6        9      12      24      36      48      72      96 97
# 1 1 0 0 0 0.000000 0.00000 0.00926 0.01303 0.01594 0.02252 0.02757 NA
# 1 2 0 0 0 0.009259 0.01303 0.01303 0.02380 0.03166 0.04284 0.04690 NA


# Institutionalization ----------------
nursinghome=read.csv("nursinghome.csv", sep=";")
str(nursinghome)
attach(nursinghome)
nursinghome=factor(disnursinghome, levels=c(0), labels= c("PD"))
table(disnursinghome, statusnursinghome)
fitnursinghome=CumIncidence (ftimenursinghome, statusnursinghome, cencode = 0, xlab="Months", t=c(0, 3, 6, 9, 12, 24, 36, 48,72,96,97), level = 0.95)

# Estimates at time points:
#   0 3 6 9 12       24      36      48      72     96 97
# 1 1 0 0 0 0  0 0.009259 0.01852 0.03721 0.09462 0.1628 NA
# 1 2 0 0 0 0  0 0.000000 0.00000 0.00000 0.01947 0.0589 NA
# 
# Standard errors:
#   0 3 6 9 12       24      36      48      72      96 97
# 1 1 0 0 0 0  0 0.009259 0.01303 0.01835 0.02863 0.03635 NA
# 1 2 0 0 0 0  0 0.000000 0.00000 0.00000 0.01371 0.02347 NA

# Subdistribution Hazards

if(!require(cmprsk))
{ stop("the package 'cmprsk' is required, please install it. \nSee help(install.packages).") }

factor2ind <- function(x, baseline)
{
  #### dummy variable encoding ####
  xname <- deparse(substitute(x))
  n <- length(x)
  x <- as.factor(x)
  if(!missing(baseline)) x <- relevel(x, baseline)
  X <- matrix(0, n, length(levels(x)))
  X[(1:n) + n*(unclass(x)-1)] <- 1
  X[is.na(x),] <- NA
  dimnames(X) <- list(names(x), paste(xname, levels(x), sep = ":"))
  return(X[,-1,drop=FALSE])
}

modsel.crr <- function (object, ..., d = log(object$n)) 
{
  if(class(object) != "crr") 
    stop("object is not of class 'crr'")
  objects <- list(object, ...)
  nmodels <- length(objects)
  modnames <- paste("Model ", format(1:nmodels), ": ", 
                    lapply(objects, function(x) x$call), 
                    sep = "", collapse = "\n")
  
  mod0 <- object
  mod0$loglik <- mod0$loglik.null
  mod0$coef <- mod0$call$cov1 <- mod0$call$cov2 <- NULL
  objects <- c(list(mod0), objects)
  nmodels <- nmodels + 1
  
  modnames <- c("Model 0: Null model", modnames)
  ns <- sapply(objects, function(x) x$n) 
  dfs <- sapply(objects, function(x) length(x$coef)) 
  if(any(ns != ns[1]))
    stop("models were not all fitted to the same dataset")
  out <- matrix(rep(NA, 5 * nmodels), ncol = 5)
  loglik <- sapply(objects, function(x) x$loglik)
  crit <- sapply(objects, function(x) -2*x$loglik + d*length(x$coef))
  out[,1] <- ns
  out[,2] <- loglik
  out[,3] <- dfs
  out[,4] <- crit
  out[,5] <- crit - min(crit)
  if(d==log(object$n)) critname <- "BIC"
  else if(d == 2) critname <- "AIC"
  else critname <- "Criterion"
  colnames(out) <- c("Num.obs", "logLik", "Df.fit", critname, paste(critname, "diff"))
  rownames(out) <- 0:(nmodels-1)
  title <- "Model selection table\n"
  topnote <- modnames
  structure(as.data.frame(out), heading = c(title, topnote), 
            class = c("anova", "data.frame"))
}








# Measuring the contribution of each variable onto the incidence of each milestone event
# in the presence of a competing risk -> Death (event 0 on the column " *variable*status ")


# Falls Regression -----------------
falls_predictions = read.csv("falls_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
falls_predictions$Sex <- as.factor(falls_predictions$Sex)
falls_predictions$PostInstOFFmore <- as.factor(falls_predictions$PostInstOFFmore)
falls_predictions$GAITOFFmore <- as.factor(falls_predictions$GAITOFFmore)
falls_predictions$LCTrespMORE <- as.factor(falls_predictions$LCTrespMORE)
newfalls = cbind(falls_predictions$AgeSurgery, 
                 falls_predictions$DiseaseDurationSurgery, 
                 factor2ind(falls_predictions$Sex, "F"), 
                 factor2ind(falls_predictions$PostInstOFFmore, "ZeroOne"),
                 falls_predictions$UPDRS3off, 
                 falls_predictions$UPDRS2off,
                 factor2ind(falls_predictions$LCTrespMORE, "Less50"))
mod1 = crr(falls_predictions$ftimefalls, falls_predictions$statusfalls, newfalls)
summary(mod1)


# Competing Risks Regression
# 
# Call:
#   crr(ftime = falls_predictions$ftimefalls, fstatus = falls_predictions$statusfalls, 
#       cov1 = newfalls)
# 
# coef exp(coef) se(coef)       z p-value
# newfalls1                                     0.30349     1.355  0.09443  3.2139  0.0013
# newfalls2                                     0.03109     1.032  0.09687  0.3210  0.7500
# falls_predictions$Sex:M                      -0.90819     0.403  1.38692 -0.6548  0.5100
# falls_predictions$PostInstOFFmore:EqualMore2  1.15432     3.172  2.05656  0.5613  0.5700
# newfalls5                                    -0.06926     0.933  0.04646 -1.4909  0.1400
# newfalls6                                     0.00775     1.008  0.00666  1.1638  0.2400
# falls_predictions$LCTrespMORE:More50         -0.10939     0.896  1.58871 -0.0689  0.9500
# 
# exp(coef) exp(-coef)   2.5%  97.5%
#   newfalls1                                        1.355      0.738 1.1257   1.63
# newfalls2                                        1.032      0.969 0.8532   1.25
# falls_predictions$Sex:M                          0.403      2.480 0.0266   6.11
# falls_predictions$PostInstOFFmore:EqualMore2     3.172      0.315 0.0563 178.59
# newfalls5                                        0.933      1.072 0.8519   1.02
# newfalls6                                        1.008      0.992 0.9947   1.02
# falls_predictions$LCTrespMORE:More50             0.896      1.116 0.0398  20.17
# 
# Num. cases = 91 (18 cases omitted due to missing values)
# Pseudo Log-likelihood = -10.4 
# Pseudo likelihood ratio test = 6.13  on 7 df,



# Freezing Regression ------------------
freezing_predictions = read.csv("freezing_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
freezing_predictions$Sex <- as.factor(freezing_predictions$Sex)
freezing_predictions$PostInstOFFmore <- as.factor(freezing_predictions$PostInstOFFmore)
freezing_predictions$GAITOFFmore <- as.factor(freezing_predictions$GAITOFFmore)
freezing_predictions$LCTrespMORE <- as.factor(freezing_predictions$LCTrespMORE)
newfreezing = cbind(freezing_predictions$AgeSurgery, 
                    freezing_predictions$DiseaseDurationSurgery, 
                    factor2ind(freezing_predictions$Sex, "F"), 
                    factor2ind(freezing_predictions$PostInstOFFmore, "ZeroOne"),
                    factor2ind(freezing_predictions$GAITOFFmore, "ZeroOROne"),
                    freezing_predictions$UPDRS3off, 
                    freezing_predictions$UPDRS2off,
                    factor2ind(freezing_predictions$LCTrespMORE, "Less50"))
modfreezing = crr(freezing_predictions$ftimefreezing, freezing_predictions$statusfreezing, newfreezing)
summary(modfreezing)


# Competing Risks Regression
# 
# Call:
#   crr(ftime = freezing_predictions$ftimefreezing, fstatus = freezing_predictions$statusfreezing, 
#       cov1 = newfreezing)
# 
# coef exp(coef) se(coef)      z p-value
# newfreezing1                                     0.1339     1.143  0.05818  2.302 0.02100
# newfreezing2                                     0.0251     1.025  0.03111  0.805 0.42000
# freezing_predictions$Sex:M                      -0.5181     0.596  0.67155 -0.771 0.44000
# freezing_predictions$PostInstOFFmore:EqualMore2 -0.1279     0.880  1.22600 -0.104 0.92000
# freezing_predictions$GAITOFFmore:EqualORMore2   -1.5830     0.205  0.99129 -1.597 0.11000
# newfreezing6                                     0.0197     1.020  0.06394  0.307 0.76000
# newfreezing7                                     0.0198     1.020  0.00525  3.774 0.00016
# freezing_predictions$LCTrespMORE:More50          0.3078     1.360  0.67598  0.455 0.65000
# 
# exp(coef) exp(-coef)   2.5% 97.5%
#   newfreezing1                                        1.143      0.875 1.0201  1.28
# newfreezing2                                        1.025      0.975 0.9647  1.09
# freezing_predictions$Sex:M                          0.596      1.679 0.1597  2.22
# freezing_predictions$PostInstOFFmore:EqualMore2     0.880      1.136 0.0796  9.73
# freezing_predictions$GAITOFFmore:EqualORMore2       0.205      4.870 0.0294  1.43
# newfreezing6                                        1.020      0.981 0.8997  1.16
# newfreezing7                                        1.020      0.980 1.0096  1.03
# freezing_predictions$LCTrespMORE:More50             1.360      0.735 0.3616  5.12
# 
# Num. cases = 91 (18 cases omitted due to missing values)
# Pseudo Log-likelihood = -32.3 
# Pseudo likelihood ratio test = 15.4  on 8 df,



# Hallucinations Regression ---------------
hallucinations_predictions = read.csv("hallucinations_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
hallucinations_predictions$Sex <- as.factor(hallucinations_predictions$Sex)
hallucinations_predictions$PostInstOFFmore <- as.factor(hallucinations_predictions$PostInstOFFmore)
hallucinations_predictions$GAITOFFmore <- as.factor(hallucinations_predictions$GAITOFFmore)
hallucinations_predictions$LCTrespMORE <- as.factor(hallucinations_predictions$LCTrespMORE)
hallucinations_predictions$Neuropsychologic <- as.factor(hallucinations_predictions$Neuropsychologic)
newhallucinations = cbind(hallucinations_predictions$AgeSurgery, 
                          hallucinations_predictions$DiseaseDurationSurgery, 
                          factor2ind(hallucinations_predictions$Sex, "F"), 
                          hallucinations_predictions$UPDRS3off, 
                          hallucinations_predictions$UPDRS2off,
                          hallucinations_predictions$UPDRS1preop,
                          factor2ind(hallucinations_predictions$LCTrespMORE, "Less50"),
                          factor2ind(hallucinations_predictions$Neuropsychologic, "Normal"),
                          hallucinations_predictions$LEDDpreop)
modnewhallucinations = crr(hallucinations_predictions$ftimehallucinations, hallucinations_predictions$statushallucinations, newhallucinations)
summary(modnewhallucinations)



# Competing Risks Regression
# 
# Call:
#   crr(ftime = hallucinations_predictions$ftimehallucinations, fstatus = hallucinations_predictions$statushallucinations, 
#       cov1 = newhallucinations)
# 
# coef exp(coef) se(coef)      z p-value
# newhallucinations1                               0.152616     1.165   0.1645  0.928    0.35
# newhallucinations2                               0.060240     1.062   0.0817  0.737    0.46
# hallucinations_predictions$Sex:M                -0.886417     0.412   1.3693 -0.647    0.52
# newhallucinations4                              -0.006649     0.993   0.0506 -0.131    0.90
# newhallucinations5                               0.078264     1.081   0.1747  0.448    0.65
# newhallucinations6                               0.232930     1.262   0.4298  0.542    0.59
# hallucinations_predictions$LCTrespMORE:More50    1.154772     3.173   1.7851  0.647    0.52
# hallucinations_predictions$Neuropsychologic:MCI  0.191918     1.212   1.0327  0.186    0.85
# newhallucinations9                               0.000327     1.000   0.0014  0.234    0.81
# 
# exp(coef) exp(-coef)   2.5%  97.5%
#   newhallucinations1                                  1.165      0.858 0.8439   1.61
# newhallucinations2                                  1.062      0.942 0.9049   1.25
# hallucinations_predictions$Sex:M                    0.412      2.426 0.0282   6.03
# newhallucinations4                                  0.993      1.007 0.8996   1.10
# newhallucinations5                                  1.081      0.925 0.7679   1.52
# newhallucinations6                                  1.262      0.792 0.5436   2.93
# hallucinations_predictions$LCTrespMORE:More50       3.173      0.315 0.0959 104.96
# hallucinations_predictions$Neuropsychologic:MCI     1.212      0.825 0.1601   9.17
# newhallucinations9                                  1.000      1.000 0.9976   1.00
# 
# Num. cases = 75 (34 cases omitted due to missing values)
# Pseudo Log-likelihood = -14.4 
# Pseudo likelihood ratio test = 5.19  on 9 df,




# Dementia Regression ------------------
dementia_predictions = read.csv("dementia_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
dementia_predictions$Sex <- as.factor(dementia_predictions$Sex)
dementia_predictions$PostInstOFFmore <- as.factor(dementia_predictions$PostInstOFFmore)
dementia_predictions$GAITOFFmore <- as.factor(dementia_predictions$GAITOFFmore)
dementia_predictions$LCTrespMORE <- as.factor(dementia_predictions$LCTrespMORE)
dementia_predictions$Neuropsychologic <- as.factor(dementia_predictions$Neuropsychologic)
newdementia = cbind(dementia_predictions$AgeSurgery, 
                    dementia_predictions$DiseaseDurationSurgery, 
                    factor2ind(dementia_predictions$Sex, "F"), 
                    dementia_predictions$UPDRS3off, 
                    dementia_predictions$UPDRS2off,
                    dementia_predictions$UPDRS1preop,
                    factor2ind(dementia_predictions$LCTrespMORE, "Less50"),
                    factor2ind(dementia_predictions$Neuropsychologic, "Normal"))
modnewdementia = crr(dementia_predictions$ftimedementia, dementia_predictions$statusdementia, newdementia)
summary(modnewdementia)


# Competing Risks Regression
# 
# Call:
#   crr(ftime = dementia_predictions$ftimedementia, fstatus = dementia_predictions$statusdementia, 
#       cov1 = newdementia)
# 
# coef exp(coef) se(coef)      z p-value
# newdementia1                               0.1806     1.198   0.1089  1.660   0.097
# newdementia2                               0.0423     1.043   0.1080  0.392   0.700
# dementia_predictions$Sex:M                 0.1481     1.160   0.8185  0.181   0.860
# newdementia4                              -0.0210     0.979   0.0338 -0.622   0.530
# newdementia5                               0.0866     1.090   0.1275  0.679   0.500
# newdementia6                               0.2546     1.290   0.2762  0.922   0.360
# dementia_predictions$LCTrespMORE:More50    1.4633     4.320   1.4180  1.032   0.300
# dementia_predictions$Neuropsychologic:MCI -0.2863     0.751   0.8985 -0.319   0.750
# 
# exp(coef) exp(-coef)  2.5% 97.5%
#   newdementia1                                  1.198      0.835 0.968  1.48
# newdementia2                                  1.043      0.959 0.844  1.29
# dementia_predictions$Sex:M                    1.160      0.862 0.233  5.77
# newdementia4                                  0.979      1.021 0.916  1.05
# newdementia5                                  1.090      0.917 0.849  1.40
# newdementia6                                  1.290      0.775 0.751  2.22
# dementia_predictions$LCTrespMORE:More50       4.320      0.231 0.268 69.59
# dementia_predictions$Neuropsychologic:MCI     0.751      1.332 0.129  4.37
# 
# Num. cases = 75 (34 cases omitted due to missing values)
# Pseudo Log-likelihood = -22.3 
# Pseudo likelihood ratio test = 6.01  on 8 df,
# 


# Institutionalization Regression ------------------
nursing_predictions = read.csv("nursing_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
nursing_predictions$Sex <- as.factor(nursing_predictions$Sex)
nursing_predictions$PostInstOFFmore <- as.factor(nursing_predictions$PostInstOFFmore)
nursing_predictions$GAITOFFmore <- as.factor(nursing_predictions$GAITOFFmore)
nursing_predictions$LCTrespMORE <- as.factor(nursing_predictions$LCTrespMORE)
nursing_predictions$Neuropsychologic <- as.factor(nursing_predictions$Neuropsychologic)
newnursing = cbind(nursing_predictions$AgeSurgery, 
                   nursing_predictions$DiseaseDurationSurgery, 
                   factor2ind(nursing_predictions$Sex, "F"), 
                   nursing_predictions$UPDRS3off, 
                   nursing_predictions$UPDRS2off,
                   nursing_predictions$UPDRS1preop,
                   factor2ind(nursing_predictions$LCTrespMORE, "Less50"),
                   factor2ind(nursing_predictions$Neuropsychologic, "Normal"))

modnewnursing= crr(nursing_predictions$ftimenursing, nursing_predictions$statusnursing, newnursing)
summary(modnewnursing)


# 
# Competing Risks Regression
# 
# Call:
#   crr(ftime = nursing_predictions$ftimenursing, fstatus = nursing_predictions$statusnursing, 
#       cov1 = newnursing)
# 
# coef exp(coef) se(coef)      z p-value
# newnursing1                               0.1455     1.157   0.0623  2.337   0.019
# newnursing2                               0.0502     1.052   0.0457  1.099   0.270
# nursing_predictions$Sex:M                -0.2481     0.780   0.6479 -0.383   0.700
# newnursing4                              -0.0126     0.988   0.0267 -0.471   0.640
# newnursing5                               0.0449     1.046   0.0601  0.748   0.450
# newnursing6                               0.4545     1.575   0.2732  1.664   0.096
# nursing_predictions$LCTrespMORE:More50    1.5088     4.521   0.9971  1.513   0.130
# nursing_predictions$Neuropsychologic:MCI  1.2281     3.415   0.6880  1.785   0.074
# 
# exp(coef) exp(-coef)  2.5% 97.5%
#   newnursing1                                  1.157      0.865 1.024  1.31
# newnursing2                                  1.052      0.951 0.961  1.15
# nursing_predictions$Sex:M                    0.780      1.282 0.219  2.78
# newnursing4                                  0.988      1.013 0.937  1.04
# newnursing5                                  1.046      0.956 0.930  1.18
# newnursing6                                  1.575      0.635 0.922  2.69
# nursing_predictions$LCTrespMORE:More50       4.521      0.221 0.641 31.91
# nursing_predictions$Neuropsychologic:MCI     3.415      0.293 0.887 13.15
# 
# Num. cases = 75 (34 cases omitted due to missing values)
# Pseudo Log-likelihood = -39.2 
# Pseudo likelihood ratio test = 13.7  on 8 df,




# BACKWARDS STEEPING MODEL , FINAL PREICTORS (P <= 0.2)

# Falls Regression  Backwards Stepping -----------------
falls_predictions = read.csv("falls_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
falls_predictions$Sex <- as.factor(falls_predictions$Sex)
falls_predictions$PostInstOFFmore <- as.factor(falls_predictions$PostInstOFFmore)
falls_predictions$GAITOFFmore <- as.factor(falls_predictions$GAITOFFmore)
falls_predictions$LCTrespMORE <- as.factor(falls_predictions$LCTrespMORE)

newfallsback = cbind(falls_predictions$AgeSurgery, 
                                          falls_predictions$UPDRS2off,
                                         falls_predictions$LEDDpreop)

modnewfallsback = crr(falls_predictions$ftimefalls, falls_predictions$statusfalls, newfallsback)

summary(modnewfallsback)

# Competing Risks Regression
# 
# Call:
#   crr(ftime = falls_predictions$ftimefalls, fstatus = falls_predictions$statusfalls, 
#       cov1 = newfallsback)
# 
# coef exp(coef) se(coef)     z p-value
# newfallsback1  0.24977     1.284  0.08549  2.92  0.0035
# newfallsback2  0.00730     1.007  0.00424  1.72  0.0860
# newfallsback3 -0.00242     0.998  0.00162 -1.49  0.1400
# 
# exp(coef) exp(-coef)  2.5% 97.5%
#   newfallsback1     1.284      0.779 1.086  1.52
# newfallsback2     1.007      0.993 0.999  1.02
# newfallsback3     0.998      1.002 0.994  1.00
# 
# Num. cases = 98 (11 cases omitted due to missing values)
# Pseudo Log-likelihood = -10.9 
# Pseudo likelihood ratio test = 5.53  on 3 df,




# Freezing Regression  Backwards Stepping ------------------
freezing_predictions = read.csv("freezing_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
freezing_predictions$Sex <- as.factor(freezing_predictions$Sex)
freezing_predictions$PostInstOFFmore <- as.factor(freezing_predictions$PostInstOFFmore)
freezing_predictions$GAITOFFmore <- as.factor(freezing_predictions$GAITOFFmore)
freezing_predictions$LCTrespMORE <- as.factor(freezing_predictions$LCTrespMORE)

newfreezingback = cbind(freezing_predictions$AgeSurgery, 
                                            factor2ind(freezing_predictions$GAITOFFmore, "ZeroOROne"),
                                            freezing_predictions$UPDRS3on,
                                            freezing_predictions$UPDRS2off,
                                           freezing_predictions$UPDRS2on)


modnewfreezingback = crr(freezing_predictions$ftimefreezing, freezing_predictions$statusfreezing, newfreezingback)


summary(modnewfreezingback)



# Competing Risks Regression
# 
# Call:
#   crr(ftime = freezing_predictions$ftimefreezing, fstatus = freezing_predictions$statusfreezing, 
#       cov1 = newfreezingback)
# 
# coef exp(coef) se(coef)     z
# newfreezingback1                               0.1705     1.186  0.06191  2.75
# freezing_predictions$GAITOFFmore:EqualORMore2 -1.4098     0.244  0.74660 -1.89
# newfreezingback3                               0.0791     1.082  0.04198  1.88
# newfreezingback4                               0.0235     1.024  0.00569  4.13
# newfreezingback5                              -0.1188     0.888  0.06972 -1.70
# p-value
# newfreezingback1                              5.9e-03
# freezing_predictions$GAITOFFmore:EqualORMore2 5.9e-02
# newfreezingback3                              5.9e-02
# newfreezingback4                              3.6e-05
# newfreezingback5                              8.8e-02
# 
# exp(coef) exp(-coef)   2.5% 97.5%
#   newfreezingback1                                  1.186      0.843 1.0504  1.34
# freezing_predictions$GAITOFFmore:EqualORMore2     0.244      4.095 0.0565  1.05
# newfreezingback3                                  1.082      0.924 0.9968  1.18
# newfreezingback4                                  1.024      0.977 1.0124  1.04
# newfreezingback5                                  0.888      1.126 0.7745  1.02
# 
# Num. cases = 96 (13 cases omitted due to missing values)
# Pseudo Log-likelihood = -34.7 
# Pseudo likelihood ratio test = 20.3  on 5 df,



# Hallucinations Regression Backwards Stepping ---------------
hallucinations_predictions = read.csv("hallucinations_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
hallucinations_predictions$Sex <- as.factor(hallucinations_predictions$Sex)
hallucinations_predictions$PostInstOFFmore <- as.factor(hallucinations_predictions$PostInstOFFmore)
hallucinations_predictions$GAITOFFmore <- as.factor(hallucinations_predictions$GAITOFFmore)
hallucinations_predictions$LCTrespMORE <- as.factor(hallucinations_predictions$LCTrespMORE)
hallucinations_predictions$Neuropsychologic <- as.factor(hallucinations_predictions$Neuropsychologic)

newhallucinationsback = cbind( factor2ind(hallucinations_predictions$Phenotype, "Tremor"))

modnewhallucinationsback = crr(hallucinations_predictions$ftimehallucinations, hallucinations_predictions$statushallucinations, newhallucinationsback)

summary(modnewhallucinationsback)


# Competing Risks Regression
# 
# Call:
#   crr(ftime = hallucinations_predictions$ftimehallucinations, fstatus = hallucinations_predictions$statushallucinations, 
#       cov1 = newhallucinationsback)
# 
# coef exp(coef) se(coef)    z
# hallucinations_predictions$Phenotype:Indeterminate 2.12      8.31     1.22 1.73
# hallucinations_predictions$Phenotype:PIGD          1.51      4.51     1.10 1.37
# p-value
# hallucinations_predictions$Phenotype:Indeterminate   0.083
# hallucinations_predictions$Phenotype:PIGD            0.170
# 
# exp(coef) exp(-coef)  2.5% 97.5%
#   hallucinations_predictions$Phenotype:Indeterminate      8.31      0.120 0.759  91.0
# hallucinations_predictions$Phenotype:PIGD               4.51      0.222 0.526  38.7
# 
# Num. cases = 96 (13 cases omitted due to missing values)
# Pseudo Log-likelihood = -29.5 
# Pseudo likelihood ratio test = 3.85  on 2 df,




# Dementia Regression Backwards Stepping ------------------
dementia_predictions = read.csv("dementia_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
dementia_predictions$Sex <- as.factor(dementia_predictions$Sex)
dementia_predictions$PostInstOFFmore <- as.factor(dementia_predictions$PostInstOFFmore)
dementia_predictions$GAITOFFmore <- as.factor(dementia_predictions$GAITOFFmore)
dementia_predictions$LCTrespMORE <- as.factor(dementia_predictions$LCTrespMORE)
dementia_predictions$Neuropsychologic <- as.factor(dementia_predictions$Neuropsychologic)


newdementiaback = cbind(dementia_predictions$AgeSurgery, 
                                             dementia_predictions$DiseaseDurationSurgery, 
                                             dementia_predictions$UPDRS2off,
                                             dementia_predictions$UPDRS2on,
                                             factor2ind(dementia_predictions$PostInstOFFmore, "ZeroOne"))
modnewdementiaback = crr(dementia_predictions$ftimedementia, dementia_predictions$statusdementia, newdementiaback)

summary(modnewdementiaback)



# Competing Risks Regression
# 
# Call:
#   crr(ftime = dementia_predictions$ftimedementia, fstatus = dementia_predictions$statusdementia, 
#       cov1 = newdementiaback)
# 
# coef exp(coef) se(coef)     z
# newdementiaback1                                 0.1590     1.172  0.08100  1.96
# newdementiaback2                                 0.0856     1.089  0.06617  1.29
# newdementiaback3                                 0.0252     1.026  0.00564  4.47
# newdementiaback4                                -0.1904     0.827  0.07715 -2.47
# dementia_predictions$PostInstOFFmore:EqualMore2 -1.3591     0.257  0.87338 -1.56
# p-value
# newdementiaback1                                5.0e-02
# newdementiaback2                                2.0e-01
# newdementiaback3                                7.8e-06
# newdementiaback4                                1.4e-02
# dementia_predictions$PostInstOFFmore:EqualMore2 1.2e-01
# 
# exp(coef) exp(-coef)   2.5% 97.5%
#   newdementiaback1                                    1.172      0.853 1.0002 1.374
# newdementiaback2                                    1.089      0.918 0.9569 1.240
# newdementiaback3                                    1.026      0.975 1.0143 1.037
# newdementiaback4                                    0.827      1.210 0.7106 0.962
# dementia_predictions$PostInstOFFmore:EqualMore2     0.257      3.893 0.0464 1.423
# 
# Num. cases = 90 (19 cases omitted due to missing values)
# Pseudo Log-likelihood = -24.2 
# Pseudo likelihood ratio test = 13.3  on 5 df,



# Hospitalization Regression Backwards Stepping ---------
nursing_predictions = read.csv("nursing_predictions.csv", sep = ";", na.strings=c("", " ","NA"))
nursing_predictions$Sex <- as.factor(nursing_predictions$Sex)
nursing_predictions$PostInstOFFmore <- as.factor(nursing_predictions$PostInstOFFmore)
nursing_predictions$GAITOFFmore <- as.factor(nursing_predictions$GAITOFFmore)
nursing_predictions$LCTrespMORE <- as.factor(nursing_predictions$LCTrespMORE)
nursing_predictions$Neuropsychologic <- as.factor(nursing_predictions$Neuropsychologic)

newnursingback = cbind(nursing_predictions$AgeSurgery, 
                                           nursing_predictions$UPDRS2off)
 

modnewnursingback = crr(nursing_predictions$ftimenursing, nursing_predictions$statusnursing, newnursingback)

summary(modnewnursingback)

# Competing Risks Regression
# 
# Call:
#   crr(ftime = nursing_predictions$ftimenursing, fstatus = nursing_predictions$statusnursing, 
#       cov1 = newnursingback)
# 
# coef exp(coef) se(coef)    z p-value
# newnursingback1 0.1736      1.19  0.05389 3.22 1.3e-03
# newnursingback2 0.0136      1.01  0.00207 6.56 5.2e-11
# 
# exp(coef) exp(-coef) 2.5% 97.5%
#   newnursingback1      1.19      0.841 1.07  1.32
# newnursingback2      1.01      0.987 1.01  1.02
# 
# Num. cases = 98 (11 cases omitted due to missing values)
# Pseudo Log-likelihood = -59.3 
# Pseudo likelihood ratio test = 15.2  on 2 df,












#To compare the performance of different models (i.e. backwards stepping models for each event--------
install.packages("aod")
library(aod)

## checking whether the explanatory variables in a given model are significant or not 
wald.test(mod17$var, mod17$coef, Terms = 1:2)

# Creating different models
# ftime: time to event, 
# status: event happened or competing risk (death) occurred
# [ x,   "all_rows" : "which_columns" ]

mod1 = crr(filename$ftime, filename$Status, x[,c(4,7)]) # vars 4,7
mod2 = crr(filename$ftime, filename$Status, x[,c(4,2,3)]) # vars 4,2,3
mod3 = crr(filename$ftime, filename$Status, x[,c(4,5,2)]) # vars 4,5,2

modsel.crr(mod1, mod2, mod3)

# The aforementioned model comparisons will only work in the absence of missing values 
# or if NAs occur in a systematic way
# "n" needs to be the same for all models 
# if different patients have different NAs on different variables, modesel.crr won't work









# Correlation Matrix -------------
Correlation_Matrix_Input_Data <- read.csv("Correlation_Matrix_Input_Data.csv", sep = ";", header = T)

cor_data = rcorr(as.matrix(Correlation_Matrix_Input_Data),  type=c("spearman"))

corr_values <- as.data.frame(cor_data$r)
p_values <- as.data.frame(cor_data$P)

ggcorrplot(corr_values, method=c("circle"),  colors = c("yellow", "white", "darkslateblue"), outline.color = "white", hc.order = TRUE, type = "lower", lab = TRUE)
ggcorrplot(p_values, method=c("circle"),  colors = c("white", "white", "cadetblue2"), outline.color = "white", hc.order = TRUE, type = "lower", lab = TRUE, show.legend = F)













# Logistic Regression Falls ---------------------

# Falls
Multiple_Logistic_Regression_Falls <- read.csv("Multiple_Logistic_Regression_Falls.csv", sep = ";", header = T)
Multiple_Logistic_Regression_Falls <-Multiple_Logistic_Regression_Falls %>% drop_na()

Falls_fit_allVars <- glm(Falls ~ ageatsurgery + diseaseduration + LEDD + UPDRS2ON + UPDRS2OFF + UPDRS3OFF + UPDRS3ON + PostInstOFF + GaitOFF+ LD.response + MMSE, data = Multiple_Logistic_Regression_Falls, family = binomial, na.action = na.pass)

summary(Falls_fit_allVars)

# glm(formula = Falls ~ ageatsurgery + diseaseduration + LEDD + 
#       UPDRS2ON + UPDRS2OFF + UPDRS3OFF + UPDRS3ON + PostInstOFF + 
#       GaitOFF + LD.response + MMSE, family = binomial, data = Multiple_Logistic_Regression_Falls, 
#     na.action = na.pass)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2700  -0.8860   0.5471   0.8096   1.3538  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)     -2.9681293  6.0609507  -0.490   0.6243  
# ageatsurgery     0.0348754  0.0402021   0.868   0.3857  
# diseaseduration -0.0120960  0.0546799  -0.221   0.8249  
# LEDD            -0.0004512  0.0005190  -0.869   0.3846  
# UPDRS2ON        -0.0393040  0.0571693  -0.688   0.4918  
# UPDRS2OFF        0.1343156  0.0631992   2.125   0.0336 *
# UPDRS3OFF       -0.0495731  0.0692055  -0.716   0.4738  
# UPDRS3ON         0.0198961  0.1536661   0.129   0.8970  
# PostInstOFF      0.5475360  0.3209960   1.706   0.0881 .
# GaitOFF         -0.1350553  0.3595334  -0.376   0.7072  
# LD.response      0.0115517  0.0681200   0.170   0.8653  
# MMSE             0.0170376  0.1465024   0.116   0.9074  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 94.607  on 77  degrees of freedom
# Residual deviance: 82.804  on 66  degrees of freedom
# AIC: 106.8




roc(Multiple_Logistic_Regression_Falls$Falls, as.vector(fitted.values(Falls_fit_allVars)), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE,  col="deepskyblue4", main = paste("ROC curve for Falls \n ~All Variables \n") )

Falls_fit_UPDRS2OFF_PostInstVars <- glm(Falls ~ UPDRS2OFF + PostInstOFF, data = Multiple_Logistic_Regression_Falls, family = binomial, na.action = na.pass)

summary(Falls_fit_UPDRS2OFF_PostInstVars)




# Call:
#   roc.default(response = Multiple_Logistic_Regression_Freezing$Freezing,     predictor = as.vector(fitted.values(Freezing_fit_allVars)))
# 
# Data: as.vector(fitted.values(Freezing_fit_allVars)) in 35 controls (Multiple_Logistic_Regression_Freezing$Freezing 0) < 43 cases (Multiple_Logistic_Regression_Freezing$Freezing 1).
# Area under the curve: 0.7435
# > summary(Falls_fit_UPDRS2OFF_PostInstVars)
# 
# Call:
#   glm(formula = Falls ~ UPDRS2OFF + PostInstOFF, family = binomial, 
#       data = Multiple_Logistic_Regression_Falls, na.action = na.pass)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4270  -1.1536   0.6368   0.8758   1.4752  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept) -1.12706    0.93676  -1.203    0.229
# UPDRS2OFF    0.06426    0.04259   1.509    0.131
# PostInstOFF  0.44231    0.28923   1.529    0.126
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 94.607  on 77  degrees of freedom
# Residual deviance: 87.404  on 75  degrees of freedom
# AIC: 93.404


# roc(Multiple_Logistic_Regression_Falls$Falls, as.vector(fitted.values(Falls_fit_UPDRS2OFF_PostInstVars)))
# Area under the curve: 0.698

roc(Multiple_Logistic_Regression_Falls$Falls, as.vector(fitted.values(Falls_fit_UPDRS2OFF_PostInstVars)), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE,  col="deeppink4", main = paste("ROC curve for Falls \n ~UPDRS 2 OFF + Postural Instability \n") )




# Partition data: 80% vs 20% split for FALLS -------------

training.samples.falls <- Multiple_Logistic_Regression_Falls$Falls %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data.falls  <- Multiple_Logistic_Regression_Falls[training.samples.falls, ]
test.data.falls <- Multiple_Logistic_Regression_Falls[-training.samples.falls, ]

model <- glm( Falls ~., data = train.data.falls, family = binomial)

summary(model)

# Call:
#   glm(formula = Falls ~ ., family = binomial, data = train.data.falls)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.3137  -0.7331   0.4449   0.6931   1.4575  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)     -9.5458479  7.2479408  -1.317   0.1878  
# ageatsurgery     0.0725620  0.0463207   1.567   0.1172  
# diseaseduration -0.1006120  0.0985466  -1.021   0.3073  
# LEDD            -0.0002502  0.0006167  -0.406   0.6850  
# UPDRS2ON        -0.0549591  0.0651157  -0.844   0.3987  
# UPDRS2OFF        0.1685947  0.0757809   2.225   0.0261 *
#   UPDRS3OFF        0.0216884  0.0872985   0.248   0.8038  
# UPDRS3ON        -0.1647492  0.2048824  -0.804   0.4213  
# PostInstOFF      0.8549213  0.4669180   1.831   0.0671 .
# GaitOFF         -0.0974620  0.4182793  -0.233   0.8158  
# LD.response     -0.0353385  0.0843283  -0.419   0.6752  
# MMSE             0.2726250  0.1794458   1.519   0.1287  
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 75.382  on 62  degrees of freedom
# Residual deviance: 58.729  on 51  degrees of freedom
# AIC: 82.729


probabilities <- model %>% predict(test.data.falls, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == test.data.falls$Falls) # 0.5333333
 

model <- glm(Falls ~ ageatsurgery + diseaseduration + LEDD + UPDRS2ON + UPDRS2OFF + UPDRS3OFF + UPDRS3ON + PostInstOFF + GaitOFF+ LD.response + MMSE, data = train.data.falls, family = binomial, na.action = na.pass)

summary(model)$coef

# Estimate   Std. Error    z value   Pr(>|z|)
# (Intercept)     -9.5458478630 7.2479408025 -1.3170427 0.18782430
# ageatsurgery     0.0725619708 0.0463206987  1.5665129 0.11722860
# diseaseduration -0.1006120417 0.0985465538 -1.0209595 0.30727362
# LEDD            -0.0002501705 0.0006166564 -0.4056886 0.68497142
# UPDRS2ON        -0.0549590829 0.0651156850 -0.8440222 0.39865701
# UPDRS2OFF        0.1685947425 0.0757809476  2.2247642 0.02609707
# UPDRS3OFF        0.0216884186 0.0872984687  0.2484399 0.80379410
# UPDRS3ON        -0.1647491657 0.2048823748 -0.8041159 0.42133007
# PostInstOFF      0.8549212985 0.4669180413  1.8309879 0.06710234
# GaitOFF         -0.0974619621 0.4182792558 -0.2330069 0.81575603
# LD.response     -0.0353384616 0.0843283317 -0.4190580 0.67517375
# MMSE             0.2726250073 0.1794458263  1.5192608 0.12869687

probabilities <- model %>% predict(test.data.falls, type = "response")

contrasts(as.factor(test.data.falls$Falls))

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == test.data.falls$Falls) # 0.5333333








# Logistic Regression Freezing ------------------------------

# Freezing
Multiple_Logistic_Regression_Freezing <- read.csv("Multiple_Logistic_Regression_Freezing.csv", sep = ";", header = T)
Multiple_Logistic_Regression_Freezing <- Multiple_Logistic_Regression_Freezing %>% drop_na()

Freezing_fit_allVars <- glm(Freezing ~ ageatsurgery + diseaseduration + LEDD + UPDRS2ON + UPDRS2OFF + UPDRS3OFF + UPDRS3ON + PostInstOFF + GaitOFF+ LD.response + MMSE, data = Multiple_Logistic_Regression_Freezing, family = binomial, na.action = na.pass)

summary(Freezing_fit_allVars)

# Call:
#   glm(formula = Freezing ~ ageatsurgery + diseaseduration + LEDD + 
#         UPDRS2ON + UPDRS2OFF + UPDRS3OFF + UPDRS3ON + PostInstOFF + 
#         GaitOFF + LD.response + MMSE, family = binomial, data = Multiple_Logistic_Regression_Freezing, 
#       na.action = na.pass)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.9297  -1.0448   0.6390   0.8404   1.9232  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)     -3.2002222  6.1416666  -0.521  0.60232   
# ageatsurgery    -0.0054560  0.0384321  -0.142  0.88711   
# diseaseduration -0.0155190  0.0513319  -0.302  0.76240   
# LEDD            -0.0001454  0.0005010  -0.290  0.77159   
# UPDRS2ON        -0.0258960  0.0501416  -0.516  0.60553   
# UPDRS2OFF       -0.0398989  0.0519478  -0.768  0.44245   
# UPDRS3OFF       -0.0618607  0.0612636  -1.010  0.31262   
# UPDRS3ON         0.1250735  0.1398418   0.894  0.37111   
# PostInstOFF     -0.4462795  0.2962257  -1.507  0.13193   
# GaitOFF          1.0105080  0.3536841   2.857  0.00428 **
#   LD.response      0.0343811  0.0626704   0.549  0.58328   
# MMSE             0.0939472  0.1451411   0.647  0.51745   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 107.31  on 77  degrees of freedom
# Residual deviance:  93.77  on 66  degrees of freedom
# AIC: 117.77
 
# roc(Multiple_Logistic_Regression_Freezing$Freezing, as.vector(fitted.values(Freezing_fit_allVars)))
# Area under the curve: 0.7435

roc(Multiple_Logistic_Regression_Freezing$Freezing, as.vector(fitted.values(Freezing_fit_allVars)), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE,  col="deepskyblue4", main = paste("ROC curve for Freezing \n ~All Variables \n") )

Freezing_fit_GAITOFF <- glm(Freezing ~ GaitOFF, data = Multiple_Logistic_Regression_Freezing, family = binomial, na.action = na.pass)

summary(Freezing_fit_GAITOFF)

# Call:
#   glm(formula = Freezing ~ GaitOFF, family = binomial, data = Multiple_Logistic_Regression_Freezing, 
#       na.action = na.pass)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7321  -1.1093   0.8696   1.0499   1.4550  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  -0.6324     0.4589  -1.378   0.1682  
# GaitOFF       0.4700     0.2235   2.103   0.0355 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 107.31  on 77  degrees of freedom
# Residual deviance: 102.59  on 76  degrees of freedom
# AIC: 106.59

# roc(Multiple_Logistic_Regression_Freezing$Freezing, as.vector(fitted.values(Freezing_fit_GAITOFF)))
# Area under the curve: 0.6375

roc(Multiple_Logistic_Regression_Freezing$Freezing, as.vector(fitted.values(Freezing_fit_GAITOFF)), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE,  col="deeppink4", main = paste("ROC curve for Freezing \n ~GAIT OFF \n") )




# Partition data: 80% vs 20% split for FREEZING -------------

training.samples.freezing <- Multiple_Logistic_Regression_Freezing$Freezing %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data.freezing  <- Multiple_Logistic_Regression_Freezing[training.samples.freezing, ]
test.data.freezing <- Multiple_Logistic_Regression_Freezing[-training.samples.freezing, ]

model <- glm( Freezing ~., data = train.data.freezing, family = binomial)

summary(model)

# Call:
#   glm(formula = Freezing ~ ., family = binomial, data = train.data.freezing)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2014  -0.9674   0.6062   0.8812   1.8359  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)     -1.091e+01  8.004e+00  -1.363   0.1728  
# ageatsurgery    -4.027e-03  4.160e-02  -0.097   0.9229  
# diseaseduration -1.189e-02  5.671e-02  -0.210   0.8339  
# LEDD            -1.331e-04  5.645e-04  -0.236   0.8136  
# UPDRS2ON        -2.626e-02  5.384e-02  -0.488   0.6257  
# UPDRS2OFF       -1.956e-02  5.751e-02  -0.340   0.7337  
# UPDRS3OFF       -1.307e-01  8.977e-02  -1.456   0.1455  
# UPDRS3ON         2.623e-01  2.069e-01   1.268   0.2048  
# PostInstOFF     -2.199e-01  3.388e-01  -0.649   0.5163  
# GaitOFF          9.987e-01  4.115e-01   2.427   0.0152 *
#   LD.response      1.071e-01  9.250e-02   1.158   0.2468  
# MMSE             2.053e-01  1.763e-01   1.164   0.2443  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 86.046  on 62  degrees of freedom
# Residual deviance: 73.733  on 51  degrees of freedom
# AIC: 97.733

probabilities <- model %>% predict(test.data.freezing, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == test.data.freezing$Freezing) # 0.6666667


model <- glm(Freezing ~ ageatsurgery + diseaseduration + LEDD + UPDRS2ON + UPDRS2OFF + UPDRS3OFF + UPDRS3ON + PostInstOFF + GaitOFF+ LD.response + MMSE, data = train.data.freezing, family = binomial, na.action = na.pass)

summary(model)$coef


# Estimate   Std. Error     z value  Pr(>|z|)
# (Intercept)     -1.091180e+01 8.0042284412 -1.36325482 0.1728022
# ageatsurgery    -4.026782e-03 0.0415987536 -0.09680054 0.9228848
# diseaseduration -1.189403e-02 0.0567060867 -0.20974881 0.8338637
# LEDD            -1.331048e-04 0.0005645432 -0.23577434 0.8136078
# UPDRS2ON        -2.626353e-02 0.0538352393 -0.48785015 0.6256560
# UPDRS2OFF       -1.956336e-02 0.0575148192 -0.34014476 0.7337475
# UPDRS3OFF       -1.306886e-01 0.0897719930 -1.45578414 0.1454523
# UPDRS3ON         2.623030e-01 0.2068782235  1.26791027 0.2048300
# PostInstOFF     -2.198918e-01 0.3388152310 -0.64900221 0.5163369
# GaitOFF          9.986916e-01 0.4115182467  2.42684651 0.0152307
# LD.response      1.071255e-01 0.0925022733  1.15808451 0.2468295
# MMSE             2.053158e-01 0.1763373871  1.16433517 0.2442882



probabilities <- model %>% predict(test.data.freezing, type = "response")

contrasts(as.factor(test.data.freezing$Freezing))

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == test.data.freezing$Freezing) # 0.6666667





# Plotting Figures -----------------------------------------------

# Cumulative Incidence for each Event --------------------------------
Milestones_Summary <- read.csv("Milestones_Summary.csv", sep = ";", header = T)
event_order <- c("Falls", "Freezing", "Dementia", "Hallucinations", "Institutionalization")

Milestones_Summary <- Milestones_Summary %>% mutate(Event = factor(Event, levels = event_order))

Milestones_Summary %>% 
  ggplot(mapping = aes(x = Follow_up_months, y = Proportion)) +
  geom_step(aes(color = Event), show.legend = FALSE) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Event), alpha = 0.6) +
  ylim(0,1)+
  labs(x = "\n Follow-up (Months)", y = "Cumulative Incidence \n") +
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_minimal()+
  facet_wrap(~Event, scales = "free")


# Summary With All Events Cumulative Incidence  ----------------------------
Milestones_Each <- read.csv("Milestones_Each.csv", sep = ";", header = T)
Milestones_Each <- Milestones_Each %>% mutate(Event = factor(Event, levels = event_order))

Milestones_Each %>% 
  mutate(Follow_up_months = log10(Follow_up_months)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Proportion)) +
  geom_line(aes(color = Event), show.legend = FALSE, size=2) +
  ylim(0,1)+
  labs(x = NULL, y = NULL) +
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_minimal()


# Events Incidence Paired Groups (with vs without accompanying event) ---------------
Milestones_Grouped <- read.csv("Milestones_Grouped.csv", sep = ";", header = T)

Milestones_Grouped[,3] <- as.factor(Milestones_Grouped[,3])
Milestones_Grouped[,4] <- as.factor(Milestones_Grouped[,4])

Milestones_Grouped %>% 
  ggplot(mapping = aes(x = Follow_up_months, y = Proportion)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=1.5) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.4) +
  ylim(0,1)+
  labs(x = "\n Follow-up (Months)", y = "Cumulative Incidence \n") +
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_minimal()+
  facet_wrap(~Event, scales = "free")


# Survival ~ Milestone event development  ---------------
Milestones_Mortality_per_Group <- read.csv("Milestones_Mortality_per_Group.csv", sep = ";", header = T)

Milestones_Mortality_per_Group[,3] <- as.factor(Milestones_Mortality_per_Group[,3])
Milestones_Mortality_per_Group[,4] <- as.factor(Milestones_Mortality_per_Group[,4])

event_order_2 <- c("Overall_Survival", "Survival_Freezing", "Survival_Falls", "Survival_Hallucinations", "Survival_Dementia" , "Survival_Institutionalization")

Milestones_Mortality_per_Group <- Milestones_Mortality_per_Group %>% mutate(Event = factor(Event, levels = event_order_2))

Milestones_Mortality_per_Group %>% 
  group_by(Event, Group) %>%
  fill(Proportion) %>%
  fill(conf.high) %>% fill(conf.high, .direction = c("up")) %>%
  fill(conf.low) %>% fill(conf.low, .direction = c("up")) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Proportion)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=2) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.2) +
  ylim(0,100)+xlim(0,110)+
  labs(x = "\n Follow-up (Months)", y = "Survival (%) \n") +
  scale_fill_viridis_d(option = "D")+
  scale_colour_viridis_d(option = "D")+
  theme_minimal()+
  facet_wrap(~Event, scales = "free")




# Check for drug usage  -------------

MilestonesDrugUsage <- read.csv("MilestonesDrugUsage.csv", sep = ";", header = T)

for(x in 4:17) { print(sum(MilestonesDrugUsage[,x] != 0)) }



# Stimulation settings per group  -------------

Stimul_Settings <- read.csv("Stimul_Settings.csv", sep = ";", header = T)

Stimul_Settings <- Stimul_Settings[,3:10]

names(Stimul_Settings)

# Right Side
Stimul_Settings %>% group_by(modeSTNrightEND) %>% count()
Stimul_Settings %>% group_by(modeSTNrightEND) %>% summarise(n = mean(voltageSTNrightEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNrightEND) %>% summarise(n = sd(voltageSTNrightEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNrightEND) %>% summarise(n = mean(frequencySTNrightEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNrightEND) %>% summarise(n = sd(frequencySTNrightEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNrightEND) %>% summarise(n = mean(pulsewidthSTNrightEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNrightEND) %>% summarise(n = sd(pulsewidthSTNrightEND, na.rm=T))

# Left Side
Stimul_Settings %>% group_by(modeSTNLeftEND) %>% count()
Stimul_Settings %>% group_by(modeSTNLeftEND) %>% summarise(n = mean(voltageSTNLeftEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNLeftEND) %>% summarise(n = sd(voltageSTNLeftEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNLeftEND) %>% summarise(n = mean(frequencySTNLeftEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNLeftEND) %>% summarise(n = sd(frequencySTNLeftEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNLeftEND) %>% summarise(n = mean(pulsewidthSTNLeftEND, na.rm=T))
Stimul_Settings %>% group_by(modeSTNLeftEND) %>% summarise(n = sd(pulsewidthSTNLeftEND, na.rm=T))

