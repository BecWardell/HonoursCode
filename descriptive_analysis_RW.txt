# Representative code for descriptive analysis. All examples provided are shown for A. lumbricoides. 
#Examples include:
#1) Age Standardisation to create ASP for each village (Table 4.1.2)
#2) Plots of ASP vs. continuous environmental variables (Figure 4.1.3 and Appendix C Figure C.1.1)
#3) Box and whisker plots and tests used for ASP vs. categorical environmental variables (Figure 4.1.4)

#### Preamble material ####

#Set working directory
setwd("E:\\Project")

#Install packages

#Epitools is for age standardisation
install.packages("epitools")
library (epitools)

#Reporttools can be used for reporting data in a scientific manner
install.packages("reporttools")
library (reporttools)

#Statistics package
library(car)

#Install science package
install.packages('userfriendlyscience')
library(userfriendlyscience)

#------------------------------------------------------
#### Direct Age Standardisation as per Table 4.1.2 ####
#------------------------------------------------------

#1) Read in cleaned infection dataset (from W4W study)
dataset = read.csv("input_modified\\STH_baseline_data_with_GPS_criteria_applied_and_new_var.csv", as.is=TRUE)
  attach(dataset)
  
  #Tell R that each village is unique, and save the list of unique villages
  villages=as.character(unique(villagehpr))

#2) Read in census information - from: http://www.statistics.gov.tl/
  #wp-content/uploads/2013/12/Publication_202_20FINAL_20_20English_20Fina_Website.pdf
census_data = read.csv("input_original\\census_data\\census_population_manufahi_2010_all_ages.csv", as.is=TRUE)

  #Merge rows in census data, so that we have the age groups 1-<5, 5-<20 and 20+ years
  census_data <- c(census_data[1,2], sum(census_data[2:4,2]),sum(census_data[5:18,2]))

#3) For the infection dataset, we need to create a file which groups infection information by the three 
  #age categories (1-<5, 5-<20 and 20+ years, denoted as 1-4, 5-19 and 20 + years respectively)

  #Define unique ages
  three_ages_census<- c("1-4 years", "5-19 years", "20+ years")

  #Save n as the number of villages
  n=length(villages)

  #Reset counters 
  i=1
  j=1

  #Creates a file called 'illage_level_info_for_age_stand_three_groups.csv'and assigns the following headings
  cat("Village","Village_number", "Village_population", "intervention_village", "Mean_latitude", "Mean_longitude", "Median_elevation", 
    "Median_age", "Total_number_ascaris", "Total_number_necator", "Age_group", "Age_group_population", "Age_group_with_ascaris", 
    "Age_group_with_necator\n", file="output_original\\village_level_info_for_age_stand_three_groups.csv",sep=",")


  #Finds each village, and for each village finds the THREE age groups - 1-4, 5-19 and 20+, and prints out the
  #village name, village number, total village population, mean latitude, mean long, median altitude, total number
  #with ascaris and necator, age group, number in age group, number in age group with ascaris and necator.
  
for(i in 1:n) 
{
  dataset_specific_village<-subset(dataset, villagehpr==villages[i])
  
    for (j in 1:length(three_ages_census))
    { 
    
    specific_age<-subset(dataset_specific_village, dataset_specific_village$age_3_groups_census == three_ages_census[j])
    
    out1 = villages[i]
    out2 = dataset_specific_village$village[1]
    out3 = length(dataset_specific_village$villagehpr)
    out3b = dataset_specific_village$intervention_grp[1]
    out4 = mean(dataset_specific_village$lat, na.rm=T)
    out5 = mean(dataset_specific_village$lon, na.rm=T)
    out6 = median(dataset_specific_village$ns1ele, na.rm=T)
    out6.2= median(dataset_specific_village$age)
    out7 = sum(dataset_specific_village$ascaris_yes)
    out8 = sum(dataset_specific_village$necator_yes)
    out9 = three_ages_census[j]
    out10 = length(specific_age$villagehpr)
    out11= sum(specific_age$ascaris_yes) 
    out12= sum(specific_age$necator_yes)
    
    
    cat(out1,',', out2, ',', out3, ',', out3b, ',', out4,',', out5, ',', out6, ',', out6.2, ',', out7, ',' , out8, ',' , out9, ',' ,
        out10, ',' , out11, ',' , out12, "\n",file="output_original\\village_level_info_for_age_stand_three_groups.csv",append=TRUE,sep="")
  }
  
}

#4) Now that infection information from the participants in the study is stratified by the THREE age groups, age standardisation can occur.
  
  #Read in csv for three age groups per village (created above)
  data_three_agegrp = read.csv("output_original\\village_level_info_for_age_stand_three_groups.csv", as.is=TRUE)
  
  #Reset counters
  i=1
  j=1
  
  #Creates the dataset called 'village_level_age_stand_ascaris_three_groups.csv'and saves the file. 
  cat("Village","Village_number", "Sample_population", "Intervention_village", "Mean_latitude", "Mean_longitude", "Median_elevation", "Median_age" , 
      "Crd_pw_Ascaris", "Adj_pw_Ascaris", "Ascaris_Lower_95CI", "Ascaris_Upper_95CI\n", 
      file="output_original\\village_level_age_stand_ascaris_three_groups.csv",sep=",")
  
  #For loop with loops through each of the villages to calculate the ASP. For each village, it prints out the village name, village number, number of people 
  #in the village, mean latitude and longitude of village, median elevation, median age and the ASP of Ascaris (out25). This calculation is conducted using a 
  #function from the R epitools package. The information for each village is then output to the file created above (village_level_age_stand_ascaris_three_groups.csv)
  for (i in 1:n)
  {
    
    data_three_agegrp_specific_village<-subset(data_three_agegrp, Village==villages[i]) #Subset to specific village
    
    out20<- villages[i]
    out21<- data_three_agegrp_specific_village$Village_number[1]
    outN<- data_three_agegrp_specific_village$Village_population[1]
    out21b<- data_three_agegrp_specific_village$intervention_village[1]
    out22<- data_three_agegrp_specific_village$Mean_latitude[1]
    out23<-data_three_agegrp_specific_village$Mean_longitude[1]
    out24<-data_three_agegrp_specific_village$Median_elevation[1]
    out24.1<-data_three_agegrp_specific_village$Median_age[1]
    out25<-ageadjust.direct(data_three_agegrp_specific_village$Age_group_with_ascaris,
                            data_three_agegrp_specific_village$Age_group_population, rate = NULL, 
                            census_data, conf.level = 0.95) #Age adjust using epitools command
    
    
    cat(out20,',', out21, ',' , outN, ',' , out21b, ',' ,out22,',', out23, ',' , out24, ',' , out24.1, ',' ,(out25[1]*100), ',', (out25[2]*100), ',', 
        (out25[3]*100),',', (out25[4]*100), "\n",
        file="output_original\\village_level_age_stand_ascaris_three_groups.csv",append=TRUE,sep="")
    
  }
  

#----------------------------------------------------------------------------------------------------------  
#### Scatter plots of ASP versus continuous environmental variables, example provided for Figure 4.1.3 ####
#----------------------------------------------------------------------------------------------------------
  
#1) Write a function to get p value and Pearson's r from univariable linear regression between each continuous environmental variable and ASP
  
  get_rp <- function(fit, outvar, indepvar){
    
    #Determine results of regression line (fit)
    result<- summary(fit)
    b0 <- result$coef[1]
    b1 <- result$coef[2]
    my.p<- result$coef[2,4] #gives you p value
    my.p<- formatPval(my.p, eps=0.001, digits=3, scientific=F, includeEquality=T) #formats p value to 3 digits
    r <- cor(outvar, indepvar) #calculates r
    
    
    #Create expression so that r and p will be displayed in legend of figures
    rp = vector('expression',2)
    rp[1] = substitute(expression(paste(italic(rletter), " = ", rvalue)), 
                       list(rletter=noquote(letters[18]), rvalue=format(r,dig=2)))[2]
    
    rp[2] =substitute(expression(paste(italic(pval)," ",my.pvalue)), 
                      list(pval=noquote(letters[16]), my.pvalue=my.p))[2]
    #   rp[2] = noquote(paste('p', my.p, sep=" ")))
    return(rp)
  }
  
#2) Read in merged dataset containing ASP for each STH and environmental variables
  merged_data= read.csv("vil_tables_for_analysis\\environmental_variables_1kmb_with_ASP.csv", as.is=TRUE)
  merged_data = merged_data[,c(1:2,6:32)] #Specify which variables I want
  
  #Rename a variable: village_nu to village_number 
  names(merged_data)[names(merged_data) == 'Village_nu'] <- 'village_number'
  
  #Ensure that you only have to use the names of the columns
  attach(merged_data)

#3) Define variables of interest
  
  #Define outcome variables
  outcome_var<-c("Adj_pw_Ascaris", "Adj_pw_Necator")
  
  #Define outcome variable names
  outcome_var_names<-c(substitute(paste("ASP ", italic(Ascaris), sep="")), substitute(paste("ASP ", italic(Necator), sep="")))
  
  #Define environmental variables
  env_var<- colnames(merged_data)[c(4,8,  15, 18, 19, 16, 17, 20, 21, 22, 9:12, 14, 13, 23:24)]
  
  #Define envionmental variable names
  env_var_names<-c( "Median village age (years)",
                    "Median elevation (m)", 
                    "Annual mean temperature (°C)",
                    "Annual maximum temperature (°C)",
                    "Annual minimum temperature (°C)", 
                    "Mean temperature in hottest quarter (°C)",
                    "Mean temperature in coldest quarter (°C)",
                    "Maximum temperature in hottest month (°C)",
                    "Minimum temperature in coldest month (°C)",
                    "Temperature range (°C)",
                    "Median slope (°)",
                    "Annual mean precipitation (cm)", 
                    "Mean precipitation in driest quarter (cm)", 
                    "Mean precipitation in wettest quarter (cm)", 
                    "Precipitation in driest month (cm)",
                    "Precipitation in wettest month (cm)",
                    "NDVI average",
                    "EVI average")
  
#4) Create scatterplots for each continuous environmental variable vs. ASP (as seen in results)
  
  #Select variables to display in results (one of each var type)- Figure 4.1.3
  env_var_cut<-env_var[c(2, 11, 18, 3, 12, 1)]
  env_var_names_cut<-env_var_names[c(2, 11, 18, 3, 12, 1)]
  
  #Reset counters
  i=1
  j=1
  
  #Create multipanel pdf, which plots each environmental variable vs. ASP, draws a linear regression line through the data, 
  #and adds a Pearson's R and p-value to the figure
  pdf(paste("write_up\\figures\\" , outcome_var[i], "_correlation_for_results_env_vars.pdf", by = ""), width=6,height=8)
  par(mfrow = c(3, 2)) #Specify columns and rows in pdf, 3 rows, 2 columns
  par(oma=c(0,0,2,0),mar = c(6,5,1,2))# Specify margins of each plot

  #For loop runs through each of the variables listed in env_var_cut (6 variables displayed in figure 4.1.3)
  for(j in 1:length(env_var_cut)) {
    
    #Conduct linear regression
    fit<- lm(get(outcome_var[i])~get(env_var_cut[j])) 
    
    #Call function to get p value and r values
    rp<- get_rp(fit, get(outcome_var[i]), get(env_var_cut[j]))
    
    #Create scatte plot
    plot(get(env_var_cut[j]),   
         get(outcome_var[i]), 
         ylim= c(0,100),
         xaxt="n", 
         yaxt="n",
         xlab= env_var_names_cut[j], 
         ylab= outcome_var_names[i], 
         cex.lab=1,
         pch=20)
    axis(1, cex.axis= 0.8) 
    axis(2, las=1, cex.axis= 0.8)
    abline(fit, col="red")   #Add regression line
    
    #Add legend, and put a letter in the corner of the plot
    legend('topright', legend = rp, bty = 'n', cex= 0.7)
    mtext(paste0(toupper(letters[j])), side = 3, adj = 0.02, 
          line = -1.5)
  }
  dev.off()

#------------------------------------------------------------------------------------------------------------------------------
#### Box and whisker plots with tests of significance for categorical environmental variables vs. ASP, as per Figure 4.1.4 ####
#------------------------------------------------------------------------------------------------------------------------------  
  #1) Read in data
  merged_data<-read.csv("vil_tables_for_analysis\\environmental_variables_1kmb_with_ASP.csv")
  
  #2) Perform hypothesis tests
  
  #SOIL TEXTURE
  #------------
  
  #a) Test for heteroscesdasticity
  leveneTest(Adj_pw_Ascaris~texture_1km_4grp, merged_data)
  # OUTPUT: Levene's Test for Homogeneity of Variance (center = median)
  #       Df F value  Pr(>F)  
  # group  3  4.2985 0.01705 *
  # Conclusion: Variances not homogeneous
  
  #b) One way ANOVA: Welch correction for nonhomogeneity automatically applied, F test used. 
  aT<-oneway.test(Adj_pw_Ascaris~texture_1km_4grp, data= merged_data, na.action=na.omit, var.equal=FALSE)
  
  # OUTPUT: One-way analysis of means (not assuming equal variances)
  # data:  Adj_pw_Ascaris and texture_1km_5grp
  # F = 16.695, num df = 3.0000, denom df = 9.2498, p-value = 0.0004518

  #c) Post-hoc Games-Howell test
  output_soiltex_ascaris<-oneway(y=merged_data$Adj_pw_Ascaris, x=merged_data$texture_1km_4grp, posthoc= 'games-howell', 
                                 means=FALSE, fullDescribe=T, levene=T,
                                 plot=T, digits=2, pvalueDigits=5, t=FALSE,conf.level=0.95)
  # OUTPUT- Post hoc test: games-howell
  # 
  #                            t   df    p
  # Sandy Clay:Sandy Loam    0.26 7.46 .993
  # Sandy Clay:Clay          3.79 4.43 .054
  # Sandy Clay:Clay Loam/Loam 4.33 4.00 .041
  # Sandy Loam:Clay          4.99 7.41 .006
  # Sandy Loam:Clay Loam/Loam 5.92 6.01 .004
  # Clay:Clay Loam/Loam       1.88 8.06 .307
  
  #Soil pH
  #-------
  
  #a) Test for heteroscesdasticity
  library(car)
  leveneTest(Adj_pw_Ascaris~ordered_ph, merged_data)
  
  #OUTPUT: Levene's Test for Homogeneity of Variance (center = median)
  #       Df F value  Pr(>F)  
  # group  2   3.015 0.07062 .
  #Conclusion: Variances homogeneous
  
  #b) One-way ANOVA F test used, equal variance assumed 
  apH<-oneway.test(Adj_pw_Ascaris~ordered_ph, data= merged_data, na.action=na.omit, var.equal=T)
  
  #OUTPUT: One-way analysis of means
  # 
  # data:  Adj_pw_Ascaris and ph_3_1km_grp
  # F = 14.494, num df = 2, denom df = 21, p-value = 0.000111
  #Conclusion: statistically significant difference between groups

  #c) Post-hoc test: as assumption of homogeneity of variances met, Tukey test used instead of games howell.
  output_soilph_ascaris<-oneway(y=merged_data$Adj_pw_Ascaris, x=merged_data$ordered_ph, posthoc= "tukey", 
                                means=FALSE, fullDescribe=T, levene=T,
                                plot=T, digits=2, pvalueDigits=5, t=FALSE,conf.level=0.95)
  
  #OUTPUT- Post hoc Tukey test.
  # 
  # diff   lwr    upr    p adj
  # Neutral-Acidic   -35.26 -57.77 -12.76 .002 
  # Alkaline-Acidic  -48.14 -72.64 -23.64 <.001
  # Alkaline-Neutral -12.88 -38.5  12.74  .429 
  
  #Landcover
  #-------------
  
  #a) Test for heteroscesdasticity
  leveneTest(Adj_pw_Ascaris~landcover_assign_1km_pres, merged_data)
  
  # OUTPUT: Levene's Test for Homogeneity of Variance (center = median)
  # Df F value Pr(>F)
  # group  2  0.5441 0.5883
  # CONCLUSION: Variances homogeneous
  
  #b) One-way ANOVA: F test used, variance homogeneous 
  aL<-oneway.test(Adj_pw_Ascaris~landcover_assign_1km_pres, data= merged_data, na.action=na.omit, var.equal=T)
  
  # OUTPUT: One-way analysis of means 
  # data:  Adj_pw_Ascaris and landcover_assign_1km_pres
  # F = 1.517, num df = 2, denom df = 21, p-value = 0.2425
  #CONCLUSION: NO difference between groups, no need for post hoc test.
  

  #3) Create boxplots for Ascaris ASP vs. each categorical variable. As per Figure 4.1.4. 
  pdf("write_up\\figures\\ascaris_ASPvsLandvar_boxplots_with_significance_1810.pdf", width=6,height=9)
  par(mfrow = c(3, 1)) #For the pdf, create 3 rows and 1 column, to allow a series of boxplots on the one page
  par(oma=c(2,2,2,2),mar = c(7,8.5,2.3,8.5), cex=0.7) # Set margin of boxplot
  
  #Create 1st boxplot: Ascaris ASP for each soil texture category
  boxplot(Adj_pw_Ascaris ~ texture_1km_4grp, 
          data= merged_data, 
          yaxt="n",
          las=2,
          cex.axis=1,
          axis=F,
          xaxt = "n",  
          xlab = "",
          col="lightsteelblue") 
  
  #Add letter to boxplot (A)
  mtext(paste0(toupper(letters[1])), side = 3, adj = 0.02, 
        line = -1.5)
  #Add Y axis
  axis(2, las=1, cex.axis= 1)
  
  #Add x axis
  ## Set up x axis with tick marks alone
  axis(1, labels = FALSE)
  
  #Create some text labels
  lablist.xT<-as.vector(c("Sandy clay", "Sandy loam", "Clay", "Clay loam/ loam"))
  
  ## Plot x axis labels at default tick marks
  text(1:4, par("usr")[3] - 8, srt = 25, adj = 1,
       labels = lablist.xT, xpd = TRUE, cex=1)
  
  #Add overall x axis label
  mtext("Soil texture", side=1, line=4.5, cex=1)
  
  #Add asterisk marks
  a=0
  par(xpd=T)
  yrange<-par("usr")[3:4]
  ypos<-yrange[2]+diff(yrange)/40
  
  #For bar between sandy loam to clay
  segments((2+a),ypos,(3-a),ypos)
  text(2.5,ypos+diff(yrange)/40,"**",cex=1.8)
  
  b=5
  
  #For bar between sandy loam to clay loam/loam
  segments((2+a),(ypos+b),(4-a),(ypos+b))
  text(3,(ypos+b)+diff(yrange)/40,"**",cex=1.8)
  
  #For bar between Sandy clay to clay loam/loam
  segments((1+a),(ypos+2*b),(4-a),(ypos+2*b))
  text(2,(ypos+2*b)+diff(yrange)/40,"*",cex=1.8)
  
  par(xpd=FALSE)
  
  #Boxplot for ASP Ascaris for each of the pH categories
  boxplot(Adj_pw_Ascaris ~ ordered_ph, 
          data= merged_data, 
          yaxt="n",
          las=2,
          cex.axis=0.8,
          axis=F,
          xaxt = "n",  
          xlab = "",
          col="lightsteelblue") 
  
  #Add letter
  mtext(paste0(toupper(letters[2])), side = 3, adj = 0.02, 
        line = -1.5)
  
  #Y axis
  axis(2, las=1, cex.axis= 1)
  
  #x axis
  #Set up x axis with tick marks alone
  where=c(1,2,3)
  axis(1, at=where, labels = FALSE)
  
  # Create some text labels
  lablist.xpH<-as.vector(c("Acidic", "Neutral", "Alkaline"))
  
  # Plot x axis labels at default tick marks
  text(1:3, par("usr")[3] - 8, srt = 25, adj = 1,
       labels = lablist.xpH, xpd = TRUE, cex=1)
  
  mtext("Soil pH", side=1, line=4.5, cex=1)
  
  #Add asterisk marks
  a=0
  par(xpd=T)
  yrange<-par("usr")[3:4]
  ypos<-yrange[2]+diff(yrange)/40
  
  #For bar between Acidic to Alkaline
  segments((1+a),ypos,(2-a),ypos)
  text(1.5,ypos+diff(yrange)/40,"**",cex=1.8)
  
  b=5
  
  #For bar between Acidic to Neutral
  segments((1+a),(ypos+b),(3-a),(ypos+b))
  text(2,(ypos+b)+diff(yrange)/40,"***",cex=1.8)
  
  par(xpd=FALSE)
  
  #For landcover categories
  boxplot(Adj_pw_Ascaris ~ landcover_assign_1km_pres, 
          data= merged_data, 
          yaxt="n",
          las=2,
          cex.axis=0.8,
          axis=F,
          xaxt = "n",  
          xlab = "",
          col="lightsteelblue")
  
  mtext(paste0(toupper(letters[3])), side = 3, adj = 0.02, 
        line = -1.5)
  
  #Y axis
  axis(2, las=1, cex.axis= 0.8)
  
  #x axis
  # Set up x axis with tick marks alone
  where=c(1,2,3)
  axis(1, at=where, labels = FALSE)
  
  #Create some text labels
  lablist.xL<-as.vector(c("Evergreen forest", "Woody savanna", "Vegetation mosaic"))
  
  ## Plot x axis labels at default tick marks
  text(1:3, par("usr")[3] - 8, srt = 25, adj = 1,
       labels = lablist.xL, xpd = TRUE, cex=0.8)
  
  mtext("Landcover", side=1, line=5, cex=1)
  
  #Add y axis to document
  mtext(substitute(paste("ASP ", italic(Ascaris), sep="")), side=2, outer= T, line=-3, cex=1)
  
  #Close pdf
  dev.off()
  
  