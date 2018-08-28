# MDR-TB_TMLE
This repository contains the main simulation codes for the paper "Estimating treatment importance in multidrug-resistant tuberculosis using Targeted Learning: an observational individual patient data meta-analysis".

In thess simulation studies, we used TMLE algorithm combined with transortability, reisistance information, missing outcomes in fused studies to estimate our parameter of interest--treatment importance, in different situations.

This simulation study aims to: 1) demonstrate the consistency and double robustness of the estimator under increasingly complex  settings, 2) investigate the appropriateness of the variance estimation and the coverage of the Wald-type confidence intervals based on the empirical influence curves, and 3) illustrate the potential importance of considering treatment availability (transportability) in our setting. 

Please find more details in our paper.

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
