"
Author: Arran J. Davis
Email: arran.davis@anthro.ox.ac.uk | davis.arran@gmail.com
Affiliation: Social Body Lab, Institute of Cognitive and Evolutionary Anthropology, University of Oxford
Date: 24/09/2020
"

#these functions are adapted from the R script provided by Snijders & Bosker (2012); https://www.stats.ox.ac.uk/~snijders/ch10.r

library(lubridate)
library(lme4)
library(nlme)
library(lmerTest)
library(RColorBrewer)
library(influence.ME)
library(mediation)

################################################################################################################################################

### LEVEL-ONE HOMOSCEDASTICITY ###

#a function for estimating level-one heteroscedasticity
level_one_heteroscedasticity = function(predictor, predictor_name, outcome, level_two_variable, mydata) {
  
  #regressions at each level-two unit
  form = as.formula(paste(outcome, "~ 1 +", predictor,"|",  level_two_variable))
  olselist = lme4::lmList(form, data = mydata)
  
  #run the same model on all data
  form1 = as.formula(paste(outcome, "~ 1 +", predictor))
  form2 = as.formula(paste("~", predictor, "|", level_two_variable))
  mod1 = nlme::lme(form1, random = form2, control = lmeControl(opt = 'optim'), data = mydata)
  
  #compare model fits
  olse_coefs = coef(olselist)
  mlm_coefs = coef(mod1)
  comp.models = compareFits(olse_coefs, mlm_coefs)
  
  #intercept coefficients from olselist
  frame1_int_olselist = as.data.frame(comp.models[, 1, 1])
  colnames(frame1_int_olselist)[1] = "Estimate"
  frame1_int_olselist$coefficient = "Intercept"
  frame1_int_olselist$Model = "OLS regression"
  frame1_int_olselist$level_two_unit = rownames(frame1_int_olselist)
  
  #intercept coefficients from lmer
  frame1_int_lme = as.data.frame(comp.models[, 2, 1])
  colnames(frame1_int_lme)[1] = "Estimate" 
  frame1_int_lme$coefficient = "Intercept"
  frame1_int_lme$Model = "Multilevel model"
  frame1_int_lme$level_two_unit = rownames(frame1_int_lme)
  
  #predictor coefficients from olselist
  frame2_pred_olselist = as.data.frame(comp.models[, 1, 2])
  colnames(frame2_pred_olselist)[1] = "Estimate"
  frame2_pred_olselist$coefficient = "Predictor"
  frame2_pred_olselist$Model = "OLS regression"
  frame2_pred_olselist$level_two_unit = rownames(frame2_pred_olselist)
  
  #intercept coefficients from lmer
  frame2_pred_lme = as.data.frame(comp.models[, 2, 2])
  colnames(frame2_pred_lme)[1] = "Estimate" 
  frame2_pred_lme$coefficient = "Predictor"
  frame2_pred_lme$Model = "Multilevel model"
  frame2_pred_lme$level_two_unit = rownames(frame2_pred_lme)
  
  #combine and create row name variable
  new_frame = do.call("rbind", list(frame1_int_olselist, frame1_int_lme, frame2_pred_olselist, frame2_pred_lme)) 
  
  #create list and function from renaming predictor variable
  variable_names <- list(
    'Intercept'="Intercept",
    'Predictor'= predictor_name
  ) 
  
  variable_labeller <- function(variable,value){
    return(variable_names[value])
  }
  
  #make the graph
  graph = ggplot(new_frame, aes(x=Estimate, y=level_two_unit, group=Model)) + 
    geom_point(aes(colour=Model), na.rm = TRUE) + 
    suppressWarnings(facet_grid(. ~ coefficient, scales = 'free', labeller=variable_labeller)) +
    ylab("Level-two unit") +
    theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
          axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(family="Arial", colour="black", size=rel(1)),
          legend.title = element_text(family="Arial", face="bold", colour="black", size=rel(1)),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(), 
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(family="Arial", face="bold", colour="black", size=rel(1)))
 
  #make the plots
  jpeg('visualisation_for_level_one_heteroscedasticity.jpg', width = 15, height = 10, units = "in",res = 300, pointsize = 12, quality = 100)
  plot(graph)
  comp.models
  dev.off()
  
  #return the 'comp.models' object
  return(comp.models)
}

### ### ###

#function for getting the model comparisons object used to check level-one homoscedasticity
get_comparisons = function(predictor, predictor_name, outcome, level_two_variable, mydata) {
  
  #regressions at each level-two unit
  form = as.formula(paste(outcome, "~ 1 +", predictor,"|",  level_two_variable))
  olselist = lme4::lmList(form, data = mydata)
  
  #run the same model on all data
  form1 = as.formula(paste(outcome, "~ 1 +", predictor))
  form2 = as.formula(paste("~", predictor, "|", level_two_variable))
  mod1 = nlme::lme(form1, random = form2, control = lmeControl(opt = 'optim'), data = mydata)
  
  #compare model fits
  comp.models = compareFits(coef(olselist), coef(mod1))
  
  return(comp.models)
}

### ### ###

#a function for getting descriptives about level-one heteroscedasticity outliers
get_outliers = function(comp.models, predictor, pred_mean, pred_cutoff, level_two_variable, mydata){

  #get cutoffs for outliers
  cutoffs =  c(pred_mean - pred_cutoff, pred_mean + pred_cutoff)
  
  #get outlying 
  strong_pred_low = which(comp.models[,1,predictor] < cutoffs[1]) 
  strong_pred_high = which(comp.models[,1,predictor] > cutoffs[2]) 
  strongs = c(strong_pred_low, strong_pred_high)
  
  #get the mean response per participant
  mean_responses_total = round(mean(table(mydata[ , c(level_two_variable)]), na.rm = TRUE), digits = 3)
  
  #get the mean responses of the outliers
  outliers = subset(mydata, mydata$Athlete_ID %in% names(strongs))
  outliers$Athlete_ID = droplevels(outliers$Athlete_ID)
  mean_responses_outliers = round(mean(table(outliers[ , c(level_two_variable)])), digits = 3)
  
  #this will give the percentage of these participants who have a total number of survey responses smaller than the average
  counter = 0
  for (i in table(outliers[ , c(level_two_variable)])) {
    if (mean_responses_total > i)
      counter = counter + 1
  }
  
  #percentage of these participants who have fewer than the average amount of survey responses
  percent_fewer_than_mean  = round(counter/length(strongs) * 100, digits = 3)
  
  #print results
  cat(paste("Mean observations per participant:", mean_responses_total), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Mean observations per outlying participant:", mean_responses_outliers), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Percentage of outlying coefficient estimates that came from participants with fewer observations than the overall average:", percent_fewer_than_mean), file = 'assumption_tests.txt', append=TRUE)
}

### ### ###

#a function for testing level-one homoscedasticity
test_homoscedasticity = function(predictor, outcome, level_two_variable, mydata){
  
  #run the test on level-two units with sufficient observations
  form = as.formula(paste(outcome, "~ 1 +", predictor,"|",  level_two_variable))
  olselist = lme4::lmList(form, data = mydata)
  df_res = sapply(olselist, function(x){x$df.residual})
  
  #make a selection of the participants with a d.f. that is relatively large (Snijders & Bosker, 2012)
  mean = mean(df_res)
  sd = sd(df_res)
  cutoff = 10
  use = df_res >= cutoff

  #get the within-group residual standard deviations
  summary(olselist[[1]])$sigma
  
  #make a plot of residual variance against residual d.f.:
  sigma2_res = suppressWarnings(sapply(olselist, function(x){(summary(x)$sigma)^2}))

  #test heteroscedasticity (first drop any infinite or NA values)
  aths = (df_res*log(sigma2_res))[use]
  aths = aths[!is.na(aths) & !is.infinite(aths)]
  
  ls_tot = sum(aths)/sum(df_res[use])
  d = (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
  d = d[!is.na(d) & !is.infinite(d)]

  #convert d and use to a dataframe
  d_dat = as.data.frame(d)
  d_dat$id = rownames(d_dat)
  
  use_dat = as.data.frame(use)
  use_dat$id = rownames(use_dat)
  
  #get all d scores from participants with a d.f. that is relatively large (Snijders & Bosker, 2012)
  d_list = c()
  counter = 1
  for (i in d_dat$id){
    id_dat = use_dat[ which(use_dat$id == i), ]
    if (id_dat$use == TRUE){
      d = as.numeric(d_dat[ which(d_dat$id == i), ]['d'])
      d_list[[counter]] = d
    counter = counter + 1
    }
  }
  
  #test score
  H = round(sum(d_list^2), digits = 3)
  df = sum(use)-1
  
  #the associated p-value is:
  p_val = round(1-pchisq(H, sum(use)-1), digits = 7)
  cat("\n", "LEVEL-ONE HOMOSCEDASTICITY", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Homoscedasticity test statistic (H):", H), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Homoscedasticity test degrees of freedom:", df), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Homoscedasticity test p-value (significance indicates heteroscedasticity):", p_val), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  
  #this will plot d, which is Gausian if there is level-one homoscedasticity
  jpeg('d_normality_for_level_one_homoscedasticity.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
  qqnorm(d_list, main = "")
  qqline(d_list)
  dev.off()

}

################################################################################################################################################

### LEVEL-ONE LINEARITY ###

#a function for creating a plot to examine linearity in level-one explanatory variables
level_one_linearity = function(predictor, outcome, level_one_variable, level_two_variable, mydata){
  
  #run the test on level-two units with sufficient observations
  form = as.formula(paste(outcome, "~ 1 +", predictor, "+", level_one_variable, "|",  level_two_variable))
  olselist = lme4::lmList(form, data = mydata)
  df_res = sapply(olselist, function(x){x$df.residual})
  
  #get within-group OLS residuals
  resi = residuals(olselist) 
  
  #for the logged times variable
  cols = brewer.pal(n=4,name="Set1")
  
  #make the plot
  jpeg('level_one_linearity.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
  plot(mydata[, level_one_variable], resi, xlab = expression('Level-one explanatory variable'), ylab = "Residual")
  lines(lowess(mydata[, level_one_variable], resi))
  dev.off()

}

################################################################################################################################################

### LEVEL-ONE RESIDUAL NORMALITY ###

#a function to check normality of standardised OLS residuals
ols_normality = function(predictor, outcome, level_one_covariate, level_two_variable, mydata){
  
  #compute within-participant studentized OLS residuals
  if(!is.null(level_one_covariate)) {
    form = as.formula(paste(outcome, "~ 1 +", predictor, "+", level_one_covariate))
    resi_st = by(mydata, mydata[,level_two_variable], function(mydata) rstudent(lm(form, data=mydata)))
  } else {
    form = as.formula(paste(outcome, "~ 1 +", predictor))
    resi_st = by(mydata, mydata[,level_two_variable], function(mydata) rstudent(lm(form, data=mydata)))
  }
  
  #unlist
  rs = unlist(resi_st)
  
  #run the test on level-two units with sufficient observations (d.f. for each level-two unit)
  form = as.formula(paste(deparse(form),  paste("|",  level_two_variable), sep = " "))
  olselist = lme4::lmList(form, data = mydata)
  df_res = sapply(olselist, function(x){x$df.residual})
  
  #remove participants with residuals of 0 or NA
  rs.no_zero = subset(rs, (rs[!is.na(rs)]==0))

  #make a QQ plot
  jpeg('ols_residual_normality.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
  qqnorm(rs.no_zero, main = "")
  qqline(rs.no_zero)
  dev.off()
  
}

################################################################################################################################################

### LEVEL-TWO RESIDUAL NORMALITY ###

#a function for checking the normality of level-two residuals (intercepts and slopes for each level-two unit)
#produces a qq plot for the random intercept and slope (if present) residuals
level_two_residual_normality = function(lme_model, level_two_variable, random_effect_variable = NULL){
  
  #get the random effects for the model
  mod_re = lme4::ranef(lme_model, condVar=TRUE)
  
  #get the posterior means for random intercepts for each level-two unit
  postmean_int =  as.data.frame(mod_re[level_two_variable])[, 1] #first column is the intercept
  
  #this will get the posterior variances for the random intercepts
  postmeanvar_int = attr(mod_re[[level_two_variable]],'postVar')[1,1,]
  
  #calculate diagnostic variances and posterior means for the random intercept
  diagmeanvar_int = VarCorr(lme_model)[[level_two_variable]][1,1] - postmeanvar_int
  postmean_stand_int = postmean_int/sqrt(diagmeanvar_int)
  
  #add analyses of random slopes (if the argument exists)
  if(!is.null(random_effect_variable)) {
    
    #get the posterior means for random slopes for each level-two unit
    postmean_slope = as.data.frame(mod_re[level_two_variable])[, 2] #second column is the first random slope
    
    #this will get the posterior variances for the random slopes
    postmeanvar_slope <-  attr(mod_re[[level_two_variable]],'postVar')[2,2,] 
    
    #calculate diagnostic variances and posterior means for the random slopes
    diagmeanvar_slope = VarCorr(lme_model)[[level_two_variable]][2,2] - postmeanvar_slope 
    postmean_stand_slope = postmeanvar_slope/sqrt(diagmeanvar_slope)
    
    #make plots for the intercepts and slopes
    jpeg('level_two_residual_normality.jpeg', width = 10, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
    
    par(mfrow=c(1,2))
    
    qqnorm(postmean_stand_int, main = "Random intercepts")
    qqline(postmean_stand_int)
    
    qqnorm(postmean_stand_slope, main = "Random slopes")
    qqline(postmean_stand_slope)
    
    dev.off()
    
  } else {
    
    #make plots for the intercepts
    jpeg('level_two_residual_normality.jpeg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
    qqnorm(postmean_stand_int, main = "Random intercepts")
    qqline(postmean_stand_int)
    dev.off()
    
  }
}

### ### ###

#a function for getting descriptives about outlying level-two units
level_two_outliers = function(model_comparison, predictor, outcome, level_two_variable, mydata){
  
  #rename the predictor variable if its a binary factor
  if (length(levels(mydata[, predictor])) == 2){
    predictor_binary = paste0(predictor, "1")
  } else {predictor_binary = predictor}
  
  #to study the outliers for the effect of the predictor variable, find what is +/- 2 SD from the mean
  mean = mean(model_comparison[,1,predictor_binary], na.rm = TRUE)
  sd = sd(model_comparison[,1,predictor_binary], na.rm = TRUE)
  
  #get all outliers
  outliers = which(abs(model_comparison[,1,predictor_binary]) > (mean + 2 *sd))
  outlier_names = c(names(outliers))
  
  #the average number of responses per participant and per outlier
  mean_observations = round(mean(table(mydata[, level_two_variable])), digits = 2)
  
  form = as.formula(paste(outcome, "~ 1 +", predictor,"|",  level_two_variable))
  olselist = lme4::lmList(form, data = mydata)
  df_res = sapply(olselist, function(x){x$df.residual})
  df_res_dat = as.data.frame(df_res[outliers])
  
  #vary print output based on number of outliers
  cat("LEVEL-TWO OUTLIERS", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  
  if (length(outliers) == 0){
    
    cat(paste("Total level-two outliers:",  length(outliers)), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    cat(paste("Mean total observations per level-two unit:", mean_observations), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    
  }
  
  if (length(outliers) == 1){
    
    outlier_subset = nrow(subset(mydata, mydata$Athlete_ID %in% outlier_names))
    mean_outlier_observations = outlier_subset / length(outliers)
    cat(paste("Total level-two outliers:",  length(outliers)), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    cat(paste("Mean total observations per level-two unit:", mean_observations), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    cat(paste("Observations for outlying level-two unit:", mean_outlier_observations), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    
    #this will give the percentage of these parkrunners who have a total number of runs larger than the average
    counter = 0
    for (i in outlier_names) {
      if (mean_observations < nrow(subset(mydata, mydata$Athlete_ID == i)))
        counter = counter + 1
    }
    
    #percentage of outliers with fewer than the average amount of observations
    percentage_with_few_obs = (1 - (counter / length(df_res[outliers]))) * 100
    cat(paste("Percentage of outliers with fewer observations than the overall average:", percentage_with_few_obs), file = 'assumption_tests.txt', append=TRUE)
    
  } else{
    
    #get mean outlier observations
    outlier_subset = nrow(subset(mydata, mydata$Athlete_ID %in% outlier_names))
    mean_outlier_observations = outlier_subset / length(outliers)
    
    #this will give the percentage of these parkrunners who have a total number of runs larger than the average
    counter = 0
    for (i in outlier_names) {
      if (mean_observations < nrow(subset(mydata, mydata$Athlete_ID == i)))
        counter = counter + 1
    }
    
    #percentage of outliers with fewer than the average amount of observations
    percentage_with_few_obs = (1 - (counter / length(df_res[outliers]))) * 100
    
    cat(paste("Total level-two outliers:",  length(outliers)), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    cat(paste("Mean total observations per level-two unit:", mean_observations), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    cat(paste("Mean observations for outlying level-two units:", mean_outlier_observations), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
    cat(paste("Percentage of outliers with fewer observations than the overall average:", percentage_with_few_obs), file = 'assumption_tests.txt', append=TRUE)
    
  }
}

################################################################################################################################################

### LEVEL-TWO UNIT INFLUENCE ###

#a function for checking the influence of level-two units using Cook's distances
check_influence = function(lme_model, level_two_variable){
  
  #check the influence level-two units
  alt.est = influence(lme_model, group= level_two_variable)
  
  #Cook's distances
  cooks_distance = cooks.distance.estex(alt.est)
  cooks_distance_mean = mean(cooks.distance(alt.est))
  cooks_distance_sd = sd(cooks.distance(alt.est))
  cooks_distance_range = range(cooks.distance.estex(alt.est))
  
  #percentage of participants with problematic Cook's distances
  influencers = cooks.distance.estex(alt.est)
  obvs = length(influencers)
  nieuwenhuis = 4/obvs
  
  #this will give the percentage of participants with potentially problematic Cook's distances
  count = 0
  count_nieuwenhuis = 0
  
  for (i in influencers) {
    
    #threshold for potentially problematic Cook's distances: 1 (Field et al., 2014)
    if (i > 1){
      count = count + 1
    }
    #threshold for potentially problematic Cook's distances: 4 / n, where n in the number of level-two units (Nieuwenhuis et al., 2012)
    if (i > nieuwenhuis){
      count_nieuwenhuis = count_nieuwenhuis + 1
    }
  }
  
  cat("\n", "LEVEL-TWO UNIT INFLUENCE", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Mean Cook's distances:", cooks_distance_mean), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Standard deviation of Cook's distances:", cooks_distance_sd), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Minimum Cook's distance:", cooks_distance_range[1]), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Maximum Cook's distance:", cooks_distance_range[2]), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  
  cat(paste("Percentage of level-two units with potentially problematic Cook's distances (greater than 1; Field et al., 2014):", round(((count/obvs)*100), digits = 3)),
      file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  cat(paste("Percentage of level-two units with potentially problematic Cook's distances (greater than 4/n; Nieuwenhuis et al., 2012):", round(((count_nieuwenhuis/obvs)*100), digits = 3)),
      file = 'assumption_tests.txt', append=TRUE, sep = "\n")
  
}

### ### ###

#a function for checking the influence of level-two units using change in predictor significance with level-two unit exclusion
check_influence_with_exclusion = function(lme_model, predictor, level_two_variable, mydata){
  
  #rename the predictor variable if its a binary factor
  if (length(levels(mydata[, predictor])) == 2){
    predictor_binary = paste0(predictor, "1")
  } else {predictor_binary = predictor}
  
  #check the influence level-two units
  alt.est = influence(lme_model, group = level_two_variable)
  
  #the sigtest function tests whether excluding a particular level-two unit changes the statistical significance (at 1.96) of a model’s predictor variables (Nieuwenhuis et al. 2012)
  exclusions = sigtest(alt.est)
  sig.test.dataframe = as.data.frame(exclusions)
  
  #use only columns for predictor variables (first three are about the intercept) that are about changes in significance
  subset = sig.test.dataframe[, c(4:ncol(sig.test.dataframe))]
  
  #get whether the predictor variable(s) changed significance
  changed_sig = subset[, grepl(paste(predictor_binary,".Changed.Sig", sep = ""), names(subset))]

  trues = table(changed_sig)["TRUE"]
  trues = if (is.na(trues) == TRUE) 0 else trues
  
  falses = table(changed_sig)["FALSE"]
  falses = if (is.na(falses) == TRUE) 0 else falses
  
  percent_true = round(as.integer((trues / (trues + falses)) * 100), digits = 2)
  
  cat(paste("Percentage of level-two units that would change predictor significance if removed from model:", percent_true), file = 'assumption_tests.txt', append=TRUE)

}

################################################################################################################################################

### SEQUENTIAL IGNORABILITY FOR MEDIATION ANALYSES ###

#a function for testing the sequential ignorability hypothesis
sequential_ignorability = function(main_predictor, covariate, mediator, outcome, level_two_variable, mydata){
  
  #formula for the model
  med.fit.form = as.formula(paste(mediator, "~", main_predictor, "+", covariate, "+", level_two_variable))
  out.fit.form = as.formula(paste(outcome, "~", mediator, "+", main_predictor, "+", covariate, "+", level_two_variable))
  
  #mediation analyses rely upon the sequential ignorability assumption (which cannot be tested with lmer models)
  med.fit.lm = lm(med.fit.form, data = mydata)
  out.fit.lm = lm(out.fit.form, data = mydata)
  
  #run the mediation analysis on the linear models
  med.out.lm = mediation::mediate(med.fit.lm, out.fit.lm, treat = main_predictor, mediator = mediator, covariates = c(covariate, level_two_variable), sims = 1000, boot.ci.type = "bca")

  #sensitivity analysis for possible existence of unobserved pre-treatment covariates (Imai et al., 2010)
  sens.out = medsens(med.out.lm, rho.by = 0.1, effect.type = "indirect", sims = 100)

  #make the sensitivity analysis plots
  jpeg('sequential_ignorability_plots.jpg', width = 10, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
  par(mfrow=c(1,2))
  plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Positive confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.5))
  plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "negative", main = "Negative confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.5))
  dev.off()
  
}  

################################################################################################################################################

### CHECK ALL MAIN MULTILEVEL MODEL ASSUMPTIONS ###

#a function for checking all assumptions for the main multilevel models
check_assumptions = function(code_dir, mlms, mlm_dir_names, predictors, predictors_integers, predictor_names, outcome, level_two_variable, level_one_variable = NULL, mydata){
  
  #track the model being checked
  model_count = 1
  
  #go through all the models in the list
  for (m in mlms){
    
    #set directory back to the one containing analysis code
    setwd(code_dir)
    
    #get the current model
    cur_model = mlm_dir_names[model_count]
    
    #print current model
    cat("\n",paste("CURRENT MODEL:", cur_model, '\n'))
    
    #create a directory for the results, set the directory to that 
    cur_dir = code_dir
    new_dir = paste0('../outputs/', cur_model)
    dir.create(file.path(cur_dir, new_dir))
    setwd(new_dir)
    
    #model variables
    predictor = predictors[model_count]
    predictor_int = predictors_integers[model_count]
    predictor_name = predictor_names[model_count]
    
    #level-two residual normality
    if (length(m@cnms[level_two_variable]) == 1) {
      #level_two_residual_normality(m, level_two_variable)
    } else {
      #level_two_residual_normality(m, level_two_variable, predictor)
    }
    
    #level-two outliers
    comparisons = get_comparisons(predictor, predictor_name, outcome, level_two_variable, mydata)
    level_two_outliers(comparisons, predictor, outcome, level_two_variable, mydata)
    
    #level-two unit influence
    check_influence(m, level_two_variable)
    check_influence_with_exclusion(m, predictor, level_two_variable, mydata)
    
    #observe level-one homoscedasticity
    comparisons = level_one_heteroscedasticity(predictor, predictor_name, outcome, level_two_variable, mydata)
    
    #test level-one homoscedasticity
    test_homoscedasticity(predictor_int, outcome, level_two_variable, mydata)
    
    #level-one linearity
    level_one_linearity(predictor, outcome, level_one_variable, level_two_variable, mydata)
    
    #level-one residual normality
    ols_normality(predictor_int, outcome, level_one_variable, level_two_variable, mydata)
    
    #move to next model
    model_count = model_count + 1
    cat("\n", "### ### ### ### ### ### ### ### ###", "\n")
  }
}

################################################################################################################################################

### CHECK ALL MULTILEVEL MEDIATION MODEL ASSUMPTIONS ###

#a function for checking all assumptions for the multilevel mediation models (same as for the main multilevel mediation models, but with different file paths)
check_mediation_assumptions = function(code_dir, mlms, mlm_dir_names, predictors, predictors_integers, predictor_names, outcome,level_two_variable, level_one_variable = NULL, mydata){
  
  #track the model being checked
  model_count = 1
  
  #go through all the models in the list
  for (m in mlms){
    
    #set directory back to the one containing analysis code
    setwd(code_dir)
    
    #get the current model
    cur_model = mlm_dir_names[model_count]
    
    #print current model
    cat("\n",paste("CURRENT MODEL:", cur_model, '\n'))
    
    #create a directory for the results, set the directory to that 
    cur_dir = code_dir
    new_dir = file.path(cur_dir, cur_model)
    dir.create(new_dir)
    setwd(new_dir)
    
    #model variables
    predictor = predictors[model_count]
    predictor_int = predictors_integers[model_count]
    predictor_name = predictor_names[model_count]
    
    #level-two residual normality
    if (length(m@cnms[level_two_variable]) == 1) {
      level_two_residual_normality(m, level_two_variable)
    } else {
      level_two_residual_normality(m, level_two_variable, predictor)
    }
    
    #level-two outliers
    comparisons = get_comparisons(predictor, predictor_name, outcome, level_two_variable, mydata)
    level_two_outliers(comparisons, predictor, outcome, level_two_variable, mydata)
    
    #level-two unit influence
    check_influence(m, level_two_variable)
    check_influence_with_exclusion(m, predictor_integer, level_two_variable, mydata)
    
    #observe level-one homoscedasticity
    comparisons = level_one_heteroscedasticity(predictor, predictor_name, outcome, level_two_variable, mydata)
    
    #test level-one homoscedasticity
    test_homoscedasticity(predictor_int, outcome, level_two_variable, mydata)
    
    #level-one linearity
    level_one_linearity(predictor, outcome, level_one_variable, level_two_variable, mydata)
    
    #level-one residual normality
    ols_normality(predictor_int, outcome, level_one_variable, level_two_variable, mydata)
    
    #move to next model
    model_count = model_count + 1
    cat("\n", "### ### ### ### ### ### ### ### ###", "\n")
  }
}
