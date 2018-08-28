# MDR-TB_TMLE
This repository contains the simulation codes for the paper "Estimating treatment importance in multidrug-resistant tuberculosis using Targeted Learning: an observational individual patient data meta-analysis"



sim_seed.txt is file contains the seeds we generated to be used.

s1_Generate_30.R is the R code for generating the data for the first scenerio.

s1_T.R is the R code for generating the true value for the first scenerio.

s1_trans.R is the R code to estimate the parameter of interest and the associated variance etc. for the first scenario.

s1_notrans.R is the R code to estimate the parameter of interest and the associated variance etc. for the first scenario without considering transportability.



s2_Generate_30.R is the R code for generating the data for the second scenerio.

s2_T.R is the R code for generating the true value for the second scenerio.

s2_trans.R is the R code to estimate the parameter of interest and the associated variance etc. for the second scenario.

s2_notrans.R is the R code to estimate the parameter of interest and the associated variance etc. without considering transportability for the second scenario.



s3_Generate_30.R is the R code for generating the data for the third scenerio.

s3_T.R is the R code for generating the true value for the third scenerio.

s3_trans.R is the R code to estimate the parameter of interest and the associated variance etc. for the third scenario.

s3_notrans.R is the R code to estimate the parameter of interest and the associated variance etc. without considering transportability for the third scenario.



coverage.R is the R code for estimating the coverage with/without considering the transportability.
