# TEST ACCURACY OF FORECASTS (FORECAST SU FROM SP)
# V2.5
# NEW AND IMPROVED FEATURES
# INCREASE OVERLAP OF PREDICTED MODELS
# PREDICTIVE GRANULARITY TO EVERY CC
# PREDICT BY PRODUCT CATEGORY
# TEST FOR APPAREL


  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(tidyr)
  library(tidyverse)
  library(MASS)
  library(reshape)
  library(reshape2)
  library(writexl)
  options(scipen = 999)

## TIMEFRAME "2023-01-01" to "2023-06-30"
## INCLUDES NA, APLA, EMEA (EXCLUDES GC)
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

  
  all.ag <- summarise(group_by(all, store_id, squr_ft_fkt_bmol, concept, month_nm, division, consumer_construct, intended_use, category),
                    cc_count      = sum(cc_count,      na.rm=TRUE),
                    gross_units   = sum(gross_units,   na.rm=TRUE),
                    gross_revenue = sum(gross_revenue, na.rm=TRUE))
  all.ag$id <- paste(all.ag$concept, all.ag$division, all.ag$consumer_construct, all.ag$intended_use, all.ag$category, sep=".")

# DATA FILTERS FOR MODELING 
  concept.ag <- filter(all.ag, concept=="NIKE RISE" ) #  | concept=="NIKE UNITE" | concept=="NIKE LIVE" | concept=="NIKE RISE" 
  concept.ag <- filter(concept.ag, division!="Equipment")
  concept.ag <- filter(concept.ag, division!="Apparel")
  concept.ag <- filter(concept.ag, !(consumer_construct=="JORDAN" & intended_use=="PERFORMANCE"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="KIDS" & category=="Baseball"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="MENS" & category=="American Football"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="MENS" & category=="Baseball"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="MENS" & category=="Converse"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="WOMENS" & category=="Basketball"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="WOMENS" & category=="NikeSB"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="WOMENS" & category=="Collegiate"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="WOMENS" & category=="Global Football"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="WOMENS" & category=="American Football"))
  concept.ag <- filter(concept.ag, !(consumer_construct=="WOMENS" & category=="Golf"))
  concept.ag <- filter(concept.ag, category!="Golf")
  concept.ag <- filter(concept.ag, category!="Tennis")
  concept.ag <- filter(concept.ag, category!="Collegiate")
  concept.ag <- filter(concept.ag, category!="NikeSB")

# CREATE LAG GROSS REVENUE VARIABLE (SHOULD I BE MODELING UNITS INSTEAD OF REVENUE AND INCLUDE DISCOUNT FACTOR - MAY SUBSTANTIALLY IMPROVE ACCURACY)
  revs <- concept.ag[, c("store_id", "id", "month_nm", "gross_revenue", "cc_count")]
  lags <- filter(revs, month_nm==1 | month_nm==2 | month_nm==3)
  lags$month_nm <- ifelse(lags$month_nm==1, 4,
                   ifelse(lags$month_nm==2, 5,
                   ifelse(lags$month_nm==3, 6, NA)))
  names(lags) <- c("store_id", "id", "month_nm", "lag_rev", "lag_cc")
  concept.ag2 <- merge(concept.ag, lags, by = c("store_id", "id", "month_nm"), all.x=TRUE)
  concept.ag2$cc_chg <- (concept.ag2$cc_count-concept.ag2$lag_cc)/concept.ag2$lag_cc
  
# MODELING SUBSETS OF DATA  
  c.ag.less10 <- filter(concept.ag2, cc_count<50 & cc_count>0)
  c.ag.10_20 <- filter(concept.ag2, cc_count>=5 & cc_count<55)
  c.ag.20_30 <- filter(concept.ag2, cc_count>=10 & cc_count<60)
  c.ag.30_40 <- filter(concept.ag2, cc_count>=15 & cc_count<65)
  c.ag.40_50 <- filter(concept.ag2, cc_count>=15 & cc_count<70)
  c.ag.50_60 <- filter(concept.ag2, cc_count>=30 & cc_count<80)
  c.ag.60_70 <- filter(concept.ag2, cc_count>=40 & cc_count<90)
  c.ag.70_80 <- filter(concept.ag2, cc_count>=40 & cc_count<100)
  c.ag.80_90 <- filter(concept.ag2, cc_count>=40 & cc_count<110)
  c.ag.90_100 <- filter(concept.ag2, cc_count>=60 & cc_count<120)

# c.ag.less120 <- filter(concept.ag, cc_count<120)
#  c.ag.less180 <- filter(concept.ag, cc_count<180)

# can run regression or other models across all groups automatically 
# Spline 1 less than 10 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6}  
  all.sp <- filter(c.ag.less10, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.less10, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models = c.ag.less10 %>%  # c.ag.less10  all.sp  all.su
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol) + log(lag_rev), data = .))

  su.byid <- split(x = all.su, f = all.su$id)

  predicts <- list()
  for(i in 1:length(fitted_models[[2]])) {
    predicts <- c(predicts, exp(predict(fitted_models[[2]][[i]], su.byid[[i]]))) # pred.cc
  }  # 
  
  su.pred <- as.data.frame(do.call("rbind", su.byid))
  su.pred$pred <- as.numeric(predicts)
  su.pred$resid <- su.pred$pred - su.pred$gross_revenue
  su.pred$mape <- su.pred$resid/su.pred$gross_revenue
  mean(su.pred$mape, na.rm=TRUE)  # save this calc for the end
  
  # quick and dirty model, mape, compare
#  look <- exp(predict(fitted_models[[2]][[1]], byid[[1]]))
#  byid[[1]]$preds<-look
#  byid[[1]]$resid <- byid[[1]]$preds - byid[[1]]$gross_revenue
#  byid[[1]]$mape <- byid[[1]]$resid/byid[[1]]$gross_revenue
#  mean(byid[[1]]$mape)
  
 # REGRESSION TREES INSTEAD OF LOG LOG REGRESSION IN FITTED MODELS 
#  fitted_models = c.ag.less10 %>%  # c.ag.less10  all.sp  all.su
#    group_by(id) %>% 
#    do(model = rpart(
#      formula = log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol),
#      data = c.ag.less10,
#      method = "anova"
#    ))
  
  #define function that divides values by 2
#  divide_by_two <- function(x) {
#    return(x/2)
#  }
  
  #define function that multiplies values by 3
#  multiply_by_three <- function(x) {
#    return(x*3)
#  }  
  
# Source ing
#  source("some_functions.R")
  
  #create data frame
#  df <- data.frame(team=c('A', 'B', 'C', 'D', 'E', 'F'),
#                   points=c(14, 19, 22, 15, 30, 40))
  
  #view data frame
#  df
  
#  team points
#  1    A     14
#  2    B     19
#  3    C     22
#  4    D     15
#  5    E     30
#  6    F     40
  
  #create new columns using functions from some_functions.R
#  df$half_points <- divide_by_two(df$points)
  
#  df$triple_points <- multiply_by_three(df$points)
  
  
 
# # # # # # # # # # # # # #               
# Spline 2, 11 to 20 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6}  
  all.sp <- filter(c.ag.10_20, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.10_20, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models2 = c.ag.10_20 %>% # c.ag.10_20 all.sp all.su
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)

  predicts2 <- list()
  for(i in 1:length(fitted_models2[[2]])) {
    predicts2 <- c(predicts2, exp(predict(fitted_models2[[2]][[i]], su.byid[[i]]))) # pred.cc
}  # 

  su.pred2 <- as.data.frame(do.call("rbind", su.byid))
  su.pred2$pred  <- as.numeric(predicts2)
  su.pred2$resid <- su.pred2$pred - su.pred2$gross_revenue
  su.pred2$mape  <- su.pred2$resid/su.pred2$gross_revenue
  mean(su.pred2$mape)
  

# # # # # # # # # # # # # #        
# Spline 3, 21 to 30 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6}  
  all.sp <- filter(c.ag.20_30, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.20_30, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models3 = c.ag.20_30 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)
  
  predicts3 <- list()
  for(i in 1:length(fitted_models3[[2]])) {
    predicts3 <- c(predicts3, exp(predict(fitted_models3[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred3 <- as.data.frame(do.call("rbind", su.byid))
  su.pred3$pred  <- as.numeric(predicts3)
  su.pred3$resid <- su.pred3$pred - su.pred3$gross_revenue
  su.pred3$mape  <- su.pred3$resid/su.pred3$gross_revenue
  mean(su.pred3$mape)
  
 
# # # # # # # # # # # # # #        
# Spline 4, 31 to 40 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6}  
  c.ag.30_40.p <- filter(c.ag.30_40, category!="NikeSB")
  all.sp <- filter(c.ag.30_40.p, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.30_40.p, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models4 = c.ag.30_40.p %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)

  predicts4 <- list()
  for(i in 1:length(fitted_models4[[2]])) {
    predicts4 <- c(predicts4, exp(predict(fitted_models4[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred4 <- as.data.frame(do.call("rbind", su.byid))
  su.pred4$pred  <- as.numeric(predicts4)
  su.pred4$resid <- su.pred4$pred - su.pred4$gross_revenue
  su.pred4$mape  <- su.pred4$resid/su.pred4$gross_revenue
  mean(su.pred4$mape)
  
 
# # # # # # # # # # # # # #        
# Spline 5, 41 to 50 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6}  
  all.sp <- filter(c.ag.40_50, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.40_50, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models5 = c.ag.40_50 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)
  
  predicts5 <- list()
  for(i in 1:length(fitted_models5[[2]])) {
    predicts5 <- c(predicts5, exp(predict(fitted_models5[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred5 <- as.data.frame(do.call("rbind", su.byid))
  su.pred5$pred  <- as.numeric(predicts5)
  su.pred5$resid <- su.pred5$pred - su.pred5$gross_revenue
  su.pred5$mape  <- su.pred5$resid/su.pred5$gross_revenue
  mean(su.pred5$mape)
  
    
# # # # # # # # # # # # # #        
# Spline 6, 51 to 60 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6} 
  c.ag.50_60.p <- filter(c.ag.50_60, category!="Athletic Training")
 # c.ag.50_60.p <- filter(c.ag.50_60.p, !(category=="Young Athlete" & intended_use=="PERFORMANCE"))
  all.sp <- filter(c.ag.50_60.p, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.50_60.p, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models6 = c.ag.50_60.p %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)
  
  predicts6 <- list()
  for(i in 1:length(fitted_models6[[2]])) {
    predicts6 <- c(predicts6, exp(predict(fitted_models6[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred6 <- as.data.frame(do.call("rbind", su.byid))
  su.pred6$pred  <- as.numeric(predicts6)
  su.pred6$resid <- su.pred6$pred - su.pred6$gross_revenue
  su.pred6$mape  <- su.pred6$resid/su.pred6$gross_revenue
  mean(su.pred6$mape)
 
 
# # # # # # # # # # # # # #        
# Spline 7, 61 to 70 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6} 
  c.ag.60_70.p <- filter(c.ag.60_70, category!="Athletic Training")
  c.ag.60_70.p <- filter(c.ag.60_70.p, !(category=="Young Athlete" & intended_use=="PERFORMANCE"))
  c.ag.60_70.p <- filter(c.ag.60_70.p, !(category=="Sportswear" & consumer_construct=="MENS"))
  c.ag.60_70.p <- filter(c.ag.60_70.p, category!="Womens Training")
  all.sp <- filter(c.ag.60_70.p, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.60_70.p, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models7 = c.ag.60_70.p %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  su.byid <- split(x = all.su, f = all.su$id)

  predicts7 <- list()
  for(i in 1:length(fitted_models7[[2]])) {
    predicts7 <- c(predicts7, exp(predict(fitted_models7[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred7 <- as.data.frame(do.call("rbind", su.byid))
  su.pred7$pred  <- as.numeric(predicts7)
  su.pred7$resid <- su.pred7$pred - su.pred7$gross_revenue
  su.pred7$mape  <- su.pred7$resid/su.pred7$gross_revenue
  mean(su.pred7$mape)
  
  su.pred.all <- rbind(su.pred, su.pred2, su.pred3, su.pred4, su.pred5, su.pred6, su.pred7)
  mean(su.pred.all$mape)


# # # # # # # # # # # # # #        
# Spline 8, 71 to 80 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6} 
  c.ag.70_80.p <- filter(c.ag.70_80, category!="Athletic Training")
  c.ag.70_80.p <- filter(c.ag.70_80.p, !(category=="Young Athlete" & intended_use=="PERFORMANCE"))
  c.ag.70_80.p <- filter(c.ag.70_80.p, !(category=="Sportswear" & consumer_construct=="MENS"))
  c.ag.70_80.p <- filter(c.ag.70_80.p, category!="Womens Training")
  all.sp <- filter(c.ag.60_70.p, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.60_70.p, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models8 = c.ag.60_70.p %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)
  
  predicts8 <- list()
  for(i in 1:length(fitted_models8[[2]])) {
    predicts8 <- c(predicts8, exp(predict(fitted_models8[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred8 <- as.data.frame(do.call("rbind", su.byid))
  su.pred8$pred  <- as.numeric(predicts8)
  su.pred8$resid <- su.pred8$pred - su.pred8$gross_revenue
  su.pred8$mape  <- su.pred8$resid/su.pred8$gross_revenue
  mean(su.pred8$mape)
  
  su.pred.all <- rbind(su.pred, su.pred2, su.pred3, su.pred4, su.pred5, su.pred6, su.pred7, su.pred8)
  mean(su.pred.all$mape)

# # # # # # # # # # # # # #        
# Spline 9, 81 to 90 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6} 
  c.ag.80_90.p <- filter(c.ag.80_90, category!="Athletic Training")
  c.ag.80_90.p <- filter(c.ag.80_90.p, !(category=="Young Athlete" & intended_use=="PERFORMANCE"))
  c.ag.80_90.p <- filter(c.ag.80_90.p, !(category=="Sportswear" & consumer_construct=="MENS"))
  c.ag.80_90.p <- filter(c.ag.80_90.p, category!="Womens Training")
  all.sp <- filter(c.ag.80_90.p, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.80_90.p, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models9 = c.ag.80_90.p %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)
  
  predicts9 <- list()
  for(i in 1:length(fitted_models9[[2]])) {
    predicts9 <- c(predicts9, exp(predict(fitted_models9[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred9 <- as.data.frame(do.call("rbind", su.byid))
  su.pred9$pred  <- as.numeric(predicts9)
  su.pred9$resid <- su.pred9$pred - su.pred9$gross_revenue
  su.pred9$mape  <- su.pred9$resid/su.pred9$gross_revenue
  mean(su.pred9$mape)
  
  su.pred.all <- rbind(su.pred, su.pred2, su.pred3, su.pred4, su.pred5, su.pred6, su.pred7, su.pred8, su.pred9)
  mean(su.pred.all$mape)


# # # # # # # # # # # # # #        
# Spline 10, 91 to 100 ccs
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6} 
  c.ag.90_100.p <- filter(c.ag.90_100, category!="Athletic Training")
  c.ag.90_100.p <- filter(c.ag.90_100.p, !(category=="Young Athlete" & intended_use=="PERFORMANCE"))
  c.ag.90_100.p <- filter(c.ag.90_100.p, !(category=="Sportswear" & consumer_construct=="MENS"))
  c.ag.90_100.p <- filter(c.ag.90_100.p, category!="Basketball")
  c.ag.90_100.p <- filter(c.ag.90_100.p, category!="Global Football")
  c.ag.90_100.p <- filter(c.ag.90_100.p, category!="Jordan")
  all.sp <- filter(c.ag.90_100.p, month_nm==1 | month_nm==2 | month_nm==3)
  all.su <- filter(c.ag.90_100.p, month_nm==4 | month_nm==5 | month_nm==6)
  
  fitted_models10 = c.ag.90_100.p %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))

  su.byid <- split(x = all.su, f = all.su$id)
  
  predicts10 <- list()
  for(i in 1:length(fitted_models10[[2]])) {
    predicts10 <- c(predicts10, exp(predict(fitted_models10[[2]][[i]], su.byid[[i]])))
}  # vector of predictions

  su.pred10 <- as.data.frame(do.call("rbind", su.byid))
  su.pred10$pred  <- as.numeric(predicts10)
  su.pred10$resid <- su.pred10$pred - su.pred10$gross_revenue
  su.pred10$mape  <- su.pred10$resid/su.pred10$gross_revenue
  mean(su.pred10$mape)
  
  su.pred.all <- rbind(su.pred, su.pred2, su.pred3, su.pred4, su.pred5, su.pred6, su.pred7, su.pred8, su.pred9, su.pred10)
  mean(su.pred.all$mape)

  # To dos
  # improce accuracy
  # add last year sales or last season sales to the model
  # copy paste with regression trees (allows more non-linear effects)
  # experiment with BSTS
  # experiment with neural net
  # find additional vars (inventory on hand, sell through, full price or discount %)
  # 
