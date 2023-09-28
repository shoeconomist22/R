
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(tidyr)
  library(tidyverse)
  library(MASS)
  library(reshape)
  library(reshape2)
  options(scipen = 999)

## TIMEFRAME "2023-01-01" to "2023-06-30"
## INCLUDES NA, APLA, EMEA and GC
## INCLUDES "Unknown" values in franchise, category, and merch_class
## INCLUDES "NULL" values as "Unknown" in the store concept
## NOTE some store square feet are captured in square meters, converted them to square feet (10.764 * square meters)
## SQUR_FT_FKT_BMOL - 48 stores (of 1137) in FKT are missing square footage, 32 of those have square footage available in bmol
## this var populates 32 of 48 missing in FKT from BMOL
  ## UPDATE DATA PULL IN SNOWFLAKE OPP COST TABLE FOLDER
  

# PULL IN DATA
  sum_perf  <- read.csv("/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/Data/OPP_COST_SUMMER_PERFORMANCE.csv")
  sum_life <- read.csv("/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/Data/OPP_COST_SUMMER_LIFESTYLE.csv")
  spr_life <- read.csv("/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/Data/OPP_COST_SPRING_LIFESTYLE.csv")
  spr_perf <- read.csv("/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/Data/OPP_COST_SPRING_PERFORMANCE.csv")

  all <- rbind(sum_perf, sum_life, spr_life, spr_perf)
  names(all) <- tolower(names(all))
#  nrow(all); nrow(sum_perf) + nrow(sum_life) + nrow(spr_life) + nrow(spr_perf)

# DROP GC
  all <- filter(all, geo!="GREATER CHINA")

# MAKE SQUR_FT_FKT_BMOL A NUMERIC FIELD
  all$squr_ft_fkt_bmol <- gsub(pattern = ' SqFT', replace='', all$squr_ft_fkt_bmol, fixed=TRUE)
  all$squr_ft_fkt_bmol <- as.numeric(all$squr_ft_fkt_bmol)

# REVENUE TO NUMERIC
  all$gross_revenue <- as.numeric(all$gross_revenue)
  all$net_revenue   <- as.numeric(all$net_revenue)  
  all <- filter(all, gross_revenue >0)

# EXAMINE MISSING SQUARE FOOTAGE DATA
  missing <- summarise(group_by(all, store_id),
                     squr_ft_fkt_bmol = mean(squr_ft_fkt_bmol, na.rm=TRUE))

# REMOVE STORES MISSING SQUARE FOOT
  all$drop <- ifelse(is.na(all$squr_ft_fkt_bmol)==TRUE, 1, 0)
  all <- filter(all, drop!=1)

  all.ag <- summarise(group_by(all, store_id, squr_ft_fkt_bmol, concept, month_nm, division, consumer_construct, intended_use),
                     cc_count      = sum(cc_count,      na.rm=TRUE),
                     gross_units   = sum(gross_units,   na.rm=TRUE),
                     gross_revenue = sum(gross_revenue, na.rm=TRUE))
  all.ag$id <- paste(all.ag$concept, all.ag$division, all.ag$consumer_construct, all.ag$intended_use, sep=".")
  
# DATA FILTERS FOR MODELING 
  concept.ag <- filter(all.ag, concept=="NIKE RISE" ) #  | concept=="NIKE UNITE" | concept=="NIKE LIVE" | concept=="NIKE RISE" 
  concept.ag <- filter(concept.ag, division!="Equipment")
  concept.ag <- filter(concept.ag, division!="Apparel")
  concept.ag <- filter(concept.ag, !(consumer_construct=="JORDAN" & intended_use=="PERFORMANCE"))

# MODELING SUBSETS OF DATA  
  c.ag.less60 <- filter(concept.ag, cc_count<60 & cc_count>5)
  c.ag.60_120 <- filter(concept.ag, cc_count>=60 & cc_count<120)
  c.ag.less120 <- filter(concept.ag, cc_count<120)
  c.ag.less180 <- filter(concept.ag, cc_count<180)

#  concept.ag <- filter(concept.ag, !(consumer_construct=="JORDAN" & intended_use=="LIFESTYLE"))
#  c.ag.fq  <- filter(concept.ag, division=="Footwear")
  
  # can run regression or other models across all groups automatically 
  # Spline 1 less than 60 ccs
  fitted_models = c.ag.less60 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  
#  fitted_models = c.ag.less60 %>% 
#    group_by(id) %>% 
#    do(model = lm(gross_revenue~cc_count + squr_ft_fkt_bmol, data = .))
  
  # how to create a table that automatically expands/contracts with the number of models run (i.e. if I try different granularities)
  # run for loop for each grouping of cc counts (0-10, 11-20... 190-200 etc.)
  # group the coefficients in each by their product heading and add the appropriate cc level marker as a column
  # ultimately don't want to group the coefficients - want to group the forecasted values for revenue and SOB

  # create the table values for opportunity cost table 
  # log cc variable (predictive independent var)
#  pred.cc <- data.frame(c(log(10), log(20), log(30), log(40), log(50), log(60))). # huge problems if you use this with predict function!
  pred.cc <- data.frame(c(10, 20, 30, 40, 50, 60))
  names(pred.cc) <- "cc_count"
   
  
  # log square fot (predictive independent var)
#  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc$squr_ft_fkt_bmol <- mean(c.ag.less60$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc) <- c("10_CCs","20_CCs","30_CCs","40_CCs","50_CCs","60_CCs")
  
  predicts <- list()
  for(i in 1:length(fitted_models[[2]])) {
    predicts <- c(predicts, exp(predict(fitted_models[[2]][[i]], pred.cc)))
  }  # vector of predictions
  
# # # # # # # # # # # # # # PROBLEM 
  # # # # # predicted values are incorrect
#  predict(fitted_models[[2]][[5]], pred.cc) # Mens performance predictions
  # genarates 8.028958 8.755354 9.180269 9.481750 9.715598 9.906665 
  # but looping predicts function
  # [1]  3068.544  6344.569  9703.761 13118.128 16574.119 20063.646
  # # # # # # # # # # # # # # SOLVED
  # predict automatically recognizes if variables are log within lm object
  # must feed new data to predict with UNLOGGED vars - predict will automatically transform them
  
  
  # Spline model 1 fit for 0-60 ccs
  # put moodel revenue predictions into a table for each product group (7 total)
  opp.cost <- pred.cc
  opp.cost$col1 <- c(predicts[[1]],predicts[[2]],predicts[[3]],predicts[[4]],predicts[[5]],predicts[[6]])
  opp.cost$col2 <- c(predicts[[7]],predicts[[8]],predicts[[9]],predicts[[10]],predicts[[11]],predicts[[12]])
  opp.cost$col3 <- c(predicts[[13]],predicts[[14]],predicts[[15]],predicts[[16]],predicts[[17]],predicts[[18]])
  opp.cost$col4 <- c(predicts[[19]],predicts[[20]],predicts[[21]],predicts[[22]],predicts[[23]],predicts[[24]])
  opp.cost$col5 <- c(predicts[[25]],predicts[[26]],predicts[[27]],predicts[[28]],predicts[[29]],predicts[[30]])
  opp.cost$col6 <- c(predicts[[31]],predicts[[32]],predicts[[33]],predicts[[34]],predicts[[35]],predicts[[36]])
  opp.cost$col7 <- c(predicts[[37]],predicts[[38]],predicts[[39]],predicts[[40]],predicts[[41]],predicts[[42]])
  
 opp.cost <- opp.cost %>%
    set_names("cc_count", "squr_fkt_bmol", fitted_models$id[1],
              fitted_models$id[2],
              fitted_models$id[3],
              fitted_models$id[4],
              fitted_models$id[5],
              fitted_models$id[6],
              fitted_models$id[7])
  
  # Spline 2 60-120 ccs
  # less 120 more statistically significant, average together?   # c.ag.60_120. # c.ag.less120
  fitted_models2 = c.ag.less120 %>%
    group_by(id) %>%
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  # create the table values for opportunity cost table 
  # log cc variable (predictive independent var)
#  pred.cc2 <- data.frame(c(log(70), log(80), log(90), log(100), log(110), log(120))). # huge problems if use this with predict function!
  pred.cc2 <- data.frame(c(70, 80, 90, 100, 110, 120))
  names(pred.cc2) <- "cc_count"
  
  # log square fot (predictive independent var)
#  pred.cc2$squr_ft_fkt_bmol <- log(mean(c.ag.less120$squr_ft_fkt_bmol))
  pred.cc2$squr_ft_fkt_bmol <- mean(c.ag.less120$squr_ft_fkt_bmol)
  
   
  # name rows
  rownames(pred.cc2) <- c("70_CCs","80_CCs","90_CCs","100_CCs","110_CCs","120_CCs")
  
  predicts2 <- list()
  for(i in 1:length(fitted_models2[[2]])) {
    predicts2 <- c(predicts2, exp(predict(fitted_models2[[2]][[i]], pred.cc2)))
  }  # vector of predictions
  
  # Spline model 2 fit for 70-120 ccs
  # put model revenue predictions into a table for each product group (7 total)
  opp.cost2 <- pred.cc2
  opp.cost2$col1 <- c(predicts2[[1]],predicts2[[2]],predicts2[[3]],predicts2[[4]],predicts2[[5]],predicts2[[6]])
  opp.cost2$col2 <- c(predicts2[[7]],predicts2[[8]],predicts2[[9]],predicts2[[10]],predicts2[[11]],predicts2[[12]])
  opp.cost2$col3 <- c(predicts2[[13]],predicts2[[14]],predicts2[[15]],predicts2[[16]],predicts2[[17]],predicts2[[18]])
  opp.cost2$col4 <- c(predicts2[[19]],predicts2[[20]],predicts2[[21]],predicts2[[22]],predicts2[[23]],predicts2[[24]])
  opp.cost2$col5 <- c(predicts2[[25]],predicts2[[26]],predicts2[[27]],predicts2[[28]],predicts2[[29]],predicts2[[30]])
  opp.cost2$col6 <- c(predicts2[[31]],predicts2[[32]],predicts2[[33]],predicts2[[34]],predicts2[[35]],predicts2[[36]])
  opp.cost2$col7 <- c(predicts2[[37]],predicts2[[38]],predicts2[[39]],predicts2[[40]],predicts2[[41]],predicts2[[42]])
  
  opp.cost2 <- opp.cost2 %>%
    set_names("cc_count", "squr_fkt_bmol", fitted_models2$id[1],
              fitted_models2$id[2],
              fitted_models2$id[3],
              fitted_models2$id[4],
              fitted_models2$id[5],
              fitted_models2$id[6],
              fitted_models2$id[7])
  
  
  # Spline 3 120-180 ccs
  fitted_models3 = c.ag.less180 %>%
    group_by(id) %>%
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  # create the table values for opportunity cost table 
  # log cc variable (predictive independent var)
#  pred.cc3 <- data.frame(c(log(130), log(140), log(150), log(160), log(170), log(180))). # huge problems if use this with predict function
  pred.cc3 <- data.frame(c(130, 140, 150, 160, 170, 180))
  
  names(pred.cc3) <- "cc_count"
  
  # log square foot (predictive independent var)
#  pred.cc3$squr_ft_fkt_bmol <- log(mean(c.ag.less180$squr_ft_fkt_bmol))
  pred.cc3$squr_ft_fkt_bmol <- mean(c.ag.less180$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc3) <- c("130_CCs","140_CCs","150_CCs","160_CCs","170_CCs","180_CCs")
  
  predicts3 <- list()
  for(i in 1:length(fitted_models3[[2]])) {
    predicts3 <- c(predicts3, exp(predict(fitted_models3[[2]][[i]], pred.cc3)))
  }  # vector of predictions
  
  # Spline model 3 fit for 130-180 ccs
  # put moodel revenue predictions into a table for each product group (7 total)
  opp.cost3 <- pred.cc3
  opp.cost3$col1 <- c(predicts3[[1]],predicts3[[2]],predicts3[[3]],predicts3[[4]],predicts3[[5]],predicts3[[6]])
  opp.cost3$col2 <- c(predicts3[[7]],predicts3[[8]],predicts3[[9]],predicts3[[10]],predicts3[[11]],predicts3[[12]])
  opp.cost3$col3 <- c(predicts3[[13]],predicts3[[14]],predicts3[[15]],predicts3[[16]],predicts3[[17]],predicts3[[18]])
  opp.cost3$col4 <- c(predicts3[[19]],predicts3[[20]],predicts3[[21]],predicts3[[22]],predicts3[[23]],predicts3[[24]])
  opp.cost3$col5 <- c(predicts3[[25]],predicts3[[26]],predicts3[[27]],predicts3[[28]],predicts3[[29]],predicts3[[30]])
  opp.cost3$col6 <- c(predicts3[[31]],predicts3[[32]],predicts3[[33]],predicts3[[34]],predicts3[[35]],predicts3[[36]])
  opp.cost3$col7 <- c(predicts3[[37]],predicts3[[38]],predicts3[[39]],predicts3[[40]],predicts3[[41]],predicts3[[42]])
  
  opp.cost3 <- opp.cost3 %>%
    set_names("cc_count", "squr_fkt_bmol", fitted_models2$id[1],
              fitted_models3$id[2],
              fitted_models3$id[3],
              fitted_models3$id[4],
              fitted_models3$id[5],
              fitted_models3$id[6],
              fitted_models3$id[7])
  
  
  opp.cost.all <- rbind(opp.cost, opp.cost2, opp.cost3)

  # round all numbers in the data frame  
  opp.cost.all <- opp.cost.all %>%
    mutate_if(is.numeric, round)
  
  # To do
  # moving scale of regression (0-60, 10-70, 20-80, 30-90 etc...)
  # create more granular forecasting for rev
  # how to improve display?
  # why not working for UNITE? need different hardcoding?
  

# OUTPUT SCENARIO PLANNING TOOL TO BOX  
  library("writexl")
  write_xlsx(opp.cost.all, "/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/cc_scenario_tables/test.xlsx")
# how to input to Shiny with adjustable knobs  
  
  
  
  
# test <- exp(predict(fitted_models[[2]][[3]], pred.cc))
  
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
  
  # pull out coefficients only from lm models
  coeffs <- c()
  # put coefficient of interest into a table and label it appropriately
  for (i in 1:length(fitted_models[[2]])) {
    coeffs <- c(coeffs, fitted_models[[2]][[i]]$coefficients[[2]])
  }. # vector
  names(coeffs) <- data.frame(t((fitted_models$id))) # labelled vector?
  
  
  
 concept.ag.split <- split(concept.ag, f = concept.ag$id)
 predicts <- c()
 for(i in 1:length(fitted_models[[2]])) {
   predicts <- c(predicts, predict(fitted_models[[2]][[i]], concept.ag.split[[i]]))
 }
 
 
   # not the right approach - need prediction set to be the desired cc count
  # create a prediction data set
  pred.ag <- summarise(group_by(all, concept, division, consumer_construct, intended_use),
                      squr_ft_fkt_bmol = mean(squr_ft_fkt_bmol, na.rm=TRUE),
                      cc_count      = sum(cc_count,      na.rm=TRUE),
                      gross_units   = sum(gross_units,   na.rm=TRUE),
                      gross_revenue = sum(gross_revenue, na.rm=TRUE))
  pred.ag$id <- paste(pred.ag$concept, pred.ag$division, pred.ag$consumer_construct, pred.ag$intended_use, sep=".")
  pred.ag <- filter(pred.ag, concept=="NIKE RISE" ) #  | concept=="NIKE UNITE" | concept=="NIKE LIVE"
  pred.ag <- filter(pred.ag, division!="Equipment")
  
  # puts each modeling prediction output into a separate list 
  pred.ag.split <- split(pred.ag, f = pred.ag$id)
  
  predicts <- list(1:1:length(fitted_models[[2]]))
  
  for(i in 1:length(fitted_models[[2]])) {
    predicts[[i]] <- predict(fitted_models[[2]][[i]], pred.ag.split[[i]])
  }
  
  
# winner   
 # puts each modeling prediction output into a separate list 
  concept.ag.split <- split(concept.ag, f = concept.ag$id)
  
  predicts <- list(1:1:length(fitted_models[[2]]))
  
  for(i in 1:length(fitted_models[[2]])) {
    predicts[[i]] <- predict(fitted_models[[2]][[i]], concept.ag.split[[i]])
  }
  
  
  # this doesn't work because the concept.ag.split[[i]] have different lengths - corresponding to # of store months observed 
  # could store in list instead
  prediction <- data.frame(mod = 1:nrow(concept.ag.split[[1]]))
  predicts <- data.frame(mod = 1:nrow(concept.ag.split[[1]]))
  for(i in 1:length(fitted_models[[2]])) {
    prediction <- data.frame(predict(fitted_models[[2]][[i]], concept.ag.split[[i]]))
    predicts[ , ncol(predicts) +1 ] <- prediction
  }
  
  
  predicts <- c()
  for(i in 1:length(fitted_models[[2]])) {
    predicts <- c(predicts, predict(fitted_models[[2]][[i]], concept.ag.split[[i]]))
    predicts$id <- as.character(fitted_models[[i]])
  }
  

  
  
    look <- predict(fitted_models[[2]][[1]], concept.ag)
  
  
  
  
  
  # create a new table where each row is the +/-CC granularity you want
  # then merge many to one with the coefficients
  # repeat for each range of cc's desired and rbind the results together
  
  
  
  
  
  
  
  fitted_models$id # NEVER HARD CODE IDs
  fitted_models
  fitted_models$model
  
  test <- list(fitted_models[[2]][[1]]$coefficients[[2]], fitted_models[[2]][[2]]$coefficients[[2]]) 
  
  col1 <- as.data.frame(c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10))
  names(col1) <- "cc"
  test <- cbind(col1, coeffs)
  
  ## # # # # # # TROUBLE SHOOTING O-60 CC'S REG MODELS FOR MENS AND WOMENS PERFORMANCE ## # # # # # # #
  13.3305 + 1.048*log(10) + -0.8387*9.191021
  
  6.4308 + 1.0939*log(10) + -0.1181*9.191021
  
  predict(fitted_models[[2]][[5]], pred.cc)
  exp(predict(fitted_models[[2]][[7]], pred.cc))
  
  