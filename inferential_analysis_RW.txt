# Representative code for inferential analysis. All examples provided are shown for A. lumbricoides. 
#Examples include:
#1) Univariate multilevel mixed-effects logistic regression analysis (Appendix C Table C.2.1)
#2) Univariate multilevel mixed-effects logistic regression analysis for quadratic forms of continuous environmental variables (Appendix C Table C.2.2)
#3) Multivariable multilevel mixed-effects logistic regression model, for final Ascaris model (Table 4.2.1)
#4) Semivariogram analysis (Figure 4.2.1)

#### Preamble material ####

#Set working directory
setwd("E:\\Project")

#Install packages

require(lme4) #Required fr multilevel modelling
library("lme4")
require(compiler)
library("compiler")
require(parallel)
library("parallel")
require(boot)
library("boot")
require(MASS)
library("MASS")
install.packages("reporttools")
library (reporttools)
install.packages('tidyr') #To tidy up dataframe
library(tidyr)
install.packages("geoR") #Required for semivariogram analysis
library(geoR)

#Read in dataset
dataset = read.csv("tables_for_analysis_individual_level\\environmental_variables_point_and_buff.csv", as.is=TRUE)
attach(dataset)

#--------------------------------------------------------------------------------------------------
#### Univariate multilevel mixed-effects logistic regression analysis (Appendix C Table C.2.1) ####
#--------------------------------------------------------------------------------------------------

#1) Create function for get OR from beta coeffs.
  #95% CI's calculated using WALD 
  Get_OR_pval_CI<-function(fit){
  cc <- lme4::confint.merMod(fit, method= "Wald", which="beta_")  ## slow (~ 11 seconds)
  ctab<- cbind(OR=fixef(fit),cc)
  #Exponentiate to get odds ratios:
  rtab <- exp(ctab)
  #Get p values
  p_val<-summary(fit)$coefficients[,4]
  results<- cbind(rtab, p_val)
  }

#2) For continuous environmental variables, run for loop

  #Define continuous variables
  continuous_variables<-colnames(dataset)[c(95, 69, 72, 73, 70, 71, 74, 75,76, 63, 64:66, 68, 67, 96, 97, 9)]
  
  #Reset counters 
  i=1
  j=1

  #Creates an output file called 'continuous_var_uni_var_analysis_ASCARIS_09.10.csv'
  cat("Variable","OR", "Lower 95CI", "Upper 95CI", "p value", "AIC\n", 
    file="output_files\\continuous_var_uni_var_analysis_ASCARIS_09.10.csv",sep=",")


  for (i in 1:length(continuous_variables)){
    #part1: Make multilevel model 
      model<-glmer(ascaris_ye ~ get(continuous_variables[i])+ (1| village/household),
               family = binomial, control=glmerControl(optCtrl=list(maxfun=100000)))
      var_name= continuous_variables[i] #save the name of the output var
  
    #part2: calculate OR
      odds_ratio = exp(unname(fixef(model))[2])
  
    #part3: compute CIs
    cc <- lme4::confint.merMod(model, method= "Wald", which="beta_")  ## slow (~ 11 seconds)
    lower_CI = exp(cc[2])
    upper_CI = exp(cc[4])
  
    #part4: get p value
    p_val<-formatPval(summary(model)$coefficients[2,4], eps=0.0001, digits=3, scientific=F, includeEquality=T)
  
    #part5: get AIC
    aic_mod<-unname(summary(model)$AICtab[1])
    
    #part 6: output to the file that was just created
    cat(var_name,',', odds_ratio, ',', lower_CI, ',', upper_CI, ',', p_val,',', aic_mod,"\n",
      file="output_files\\continuous_var_uni_var_analysis_ASCARIS_09.10.csv",append=TRUE,sep="")
    }

#3) Categorical variables. For each categorical variable, a seperate output file will be produced
  
    #Define which variables are non continuous, and put into list
    non_cont_var<-colnames(dataset[,c(17, 34, 55, 80, 56, 81, 57, 82, 58, 83, 59, 84)])
    
    #Reset counter
    i=1
  
    #For loop- for each categorical variable, create a multilevel model (fit1) and calculate 95% CI, p value and AIC
    for (i in 1:length(non_cont_var)) {
      
      #Create model    
      fit1<-glmer(ascaris_ye ~  I(get(non_cont_var[i]))+ (1| village/household), data=dataset,
                family = binomial, control=glmerControl(optCtrl=list(maxfun=100000)))
      
      #Calculate CI and OR
      cc11 <- lme4::confint.merMod(fit1, method= "Wald", which="beta_")  ## slow (~ 11 seconds)
      ctab1 <- cbind(OR=fixef(fit1),cc11)
      rtab1 <- exp(ctab1)
      
      #Get p value
      get_p1<-summary(fit1)$coefficients[,4]
      
      #Get AIC
      get_aic1<-round(unname(summary(fit1)$AICtab[1]), 2)
      
      #Create table with all of this information
      output1<-data.frame(cbind(rtab1, get_p1, get_aic1))
      
      #Output to file that includes variable name
      write.csv(output1, file= paste("output_files\\univariate9.10_round_ascaris_non_cont_var_", 
                                   non_cont_var[i], ".csv", sep=""))
    
    }

#----------------------------------------------------------------------------------------------------------------------------------------------------------
####Univariate multilevel mixed-effects logistic regression analysis for quadratic forms of continuous environmental variables (Appendix C Table C.2.2)####
#----------------------------------------------------------------------------------------------------------------------------------------------------------
    
    #1) Function which creates multilevel model with square quadratic term included for each continuous environmental variable, and outcome as probability of
    #ascaris infection. Output is beta coefficient and 95% CI 
    
    univariate_poly_betaAIC_ASCARIS<-function(variable){
      
      #Creates model, poly,2 refers to square polynomial term
      fit<-glmer(ascaris_ye ~  poly(variable,2)+ (1| village/household), data=dataset,
                 family = binomial, control=glmerControl(optCtrl=list(maxfun=100000)))
      
      #Calculate beta and 95% CI
      cc1 <- lme4::confint.merMod(fit, method= "Wald", which="beta_") 
      ctab <- cbind(beta=fixef(fit),cc1)
      
      #Get p value
      get_p<-summary(fit)$coefficients[,4]
      
      #get AIC
      get_aic<-round(unname(summary(fit)$AICtab[1]),2)
      
      #Output to table
      output<-data.frame(cbind(ctab, get_p, get_aic))
      output<-output[c(2,3), ]
      return(output)
    }
    
    #2) Call function: 
    
    #Apply function to continuous environmental variables
    out_b_asc<-apply(dataset[,c(95, 69, 72, 73, 70, 71, 74, 75,76, 63, 64:66, 68, 67, 96, 97, 9)],2, univariate_poly_betaAIC_ASCARIS)
    out_b_asc_tidy<-data.frame(out_b_asc)
    
    #Change the output table format
    out_b_asc_long <- gather(out_b_asc_tidy, factor, value, b1km_ele_div_100.OR:age.get_aic)
    
    #Write to file. Each variable will have 10 rows (for B1, B2) with 5 outputs. 
    write.csv(out_b_asc_long, file= "output_files\\BETA_continuous_env_var_uni_var_analysis_POLY_ASCARIS.09.10.csv")
    
#--------------------------------------------------------------------------------------------------------------
####Multivariable multilevel mixed-effects logistic regression model, for final Ascaris model (Table 4.2.1)####
#--------------------------------------------------------------------------------------------------------------    
    
    #Final multivariable multilevel mixed-effects logistic regression model for Ascaris
    fit_asc_chosen_mod.3<-glmer(ascaris_ye ~ b1km_ele_div_100 + b1km_ast_slope+ I(ph_3_grp_pt) +
                                  I(loamY_pt) + I(woodY_1km)+ I(sex_female)+ I(grp_age_by)+
                                  (1| village/household), data=dataset,
                                family = binomial, control=glmerControl(optCtrl=list(maxfun=100000)))
    
    #Creates OR and 95% CI
    asc_ch.3<-Get_OR_pval_CI(fit_asc_chosen_mod.3)
    
    #Puts information into dataframe
    asc_ch.3<-data.frame(asc_ch.3)
    
    #Outputs to file
    write.csv(asc_ch.3, "output_files\\fit_asc_chosen_mod.3.3.10.csv" )
    
    #Saves summary (for use below in semivariogram)
    sum_asc_fin_model<-summary(fit_asc_chosen_mod.3)
    
#---------------------------------------------
####Semivariogram analysis (Figure 4.2.1) ####
#---------------------------------------------
    
  #1) Get residuals into useable format
    
    #Get residuals from final ascaris model
    dataset_w_residuals<-cbind(dataset, sum_asc_fin_model$residuals)
    
    #Rename residual column as 'asc_mod_redisuals'
    names(dataset_w_residuals)[names(dataset_w_residuals)=="sum_asc_fin_model$residuals"] <- "asc_mod_resid"
    
    #save residuals with the rest of dataset as a new file
    write.csv(dataset_w_residuals, file="output_files\\environmental_variables_point_and_buff_w_residuals_310.csv")
    
    #read in saved file
    data_w_resid<-read.csv("output_files\\environmental_variables_point_and_buff_w_residuals_310.csv", as.is=TRUE)
    
    #Create new table summarising the mean residuals for each unique lat and lon.
    library(plyr)
    ascaris_residual_table<-ddply(data_w_resid, .(lat, lon), summarize, mean_asc_residual=mean(asc_mod_resid))

  #2) Inspect latitude and longitude ranges
    
    #Look at lat, min is -9.143, max is -8.889, range =0.253394
    summary(ascaris_residual_table$lat)
    
    #Look at lon, min is 125.6, max is 125.9, range = 0.30
    summary(ascaris_residual_table$lon)
    
  #3) Based on this, calculate lag distance. Range(lon)=0.3/2, width =0.15. Lag =0.15/10 =0.015
    
    #This says: give me a sequence from 0 to 0.15, and divide by 10. So the lag distance is 0.015
    breaks=seq(0, 0.15, l = 11)
    
  #4) Calculate semivariance for ascaris
    v1 <- variog(coords = ascaris_residual_table[,1:2], data = ascaris_residual_table[,3], breaks = breaks)
    v1.summary <- cbind(c(1:10), v1$v, v1$n)
    colnames(v1.summary) <- c("lag", "semi-variance", "# of pairs")

  #5) Plots this and save as a pdf
    pdf("write_up\\figures\\semivariograms_10breaks_3.10.pdf", width=5,height=6)
    par(mfrow = c(2, 1))
    par(oma=c(0,0,2,0),mar = c(5,5,1,2))
    
    #Reset counter
    j=1
    
    #Plots ascaris semivariogram
    plot(v1, 
         type = "b", 
         #    main = "Variogram: ascaris residuals",
         ylim= c(0,0.3),
         xlim = c(0, 0.15),
         cex.axis=0.8,
         #     xaxt="n", 
         yaxt="n",
         xlab= "Lag Distance", 
         ylab= "Semivariance", 
         cex.lab=0.8,
         pch=20)
    #axis(1, at = seq(0, 0.15, by = 0.03), cex.axis= 0.8) 
    axis(2, las=1, cex.axis= 0.8)
    
    #Add a letter to the plot (A)
    mtext(paste0(toupper(letters[j])), side = 3, adj = 0.02, 
          line = -1.2)
    #TUrn pdf off
    dev.off()