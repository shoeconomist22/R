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

# EXAMINE MISSING SQUARE FOOTAGE DATA
  missing <- summarise(group_by(all, store_id),
                     squr_ft_fkt_bmol = mean(squr_ft_fkt_bmol, na.rm=TRUE))

# REMOVE STORES MISSING SQUARE FOOT
  all$drop <- ifelse(is.na(all$squr_ft_fkt_bmol)==TRUE, 1, 0)
  all <- filter(all, drop!=1)

  # CALCULATE INSIGHTS
  # WHO HAS THE HIGHEST MARGINAL BENEFIT FROM AVERAGE OF ADDING MORE
  # WHO HAS THE HIGHEST MARGINAL COST OF REDUCTION FROM AVERAGE OF TAKING AWAY
  # WHO HAS THE FASTEST DIMINISHING MARGINAL RETURNS
  # WHAT ARE THE MOST REVENUE EFFICIENT WAYS TO MEET A GIVEN FINANCIAL TARGET
  
  # prep data to estimate splines of marginal cost/return by concept-division-consumer_construct-intended_use
  splines <- summarise(group_by(all, store_id, squr_ft_fkt_bmol, concept, month_nm, division, consumer_construct, intended_use),
                       cc_count      = sum(cc_count,      na.rm=TRUE),
                       gross_units   = sum(gross_units,   na.rm=TRUE),
                       gross_revenue = sum(gross_revenue, na.rm=TRUE))
  
  # CREATE CC BKT
  splines$cc_bkt <- ifelse(splines$cc_count<=5, 1,
                           ifelse(splines$cc_count>5 & splines$cc_count<=10, 2,
                           ifelse(splines$cc_count>10 & splines$cc_count<=15, 3,
                           ifelse(splines$cc_count>15 & splines$cc_count<=20, 4,
                           ifelse(splines$cc_count>20 & splines$cc_count<=25, 5, 
                           ifelse(splines$cc_count>25 & splines$cc_count<=30, 6,
                           ifelse(splines$cc_count>30 & splines$cc_count<=35, 7,
                           ifelse(splines$cc_count>35 & splines$cc_count<=40, 8,
                           ifelse(splines$cc_count>40 & splines$cc_count<=45, 9,
                           ifelse(splines$cc_count>45 & splines$cc_count<=50, 10,
                           ifelse(splines$cc_count>50 & splines$cc_count<=55, 11,
                           ifelse(splines$cc_count>55 & splines$cc_count<=60, 12,
                           ifelse(splines$cc_count>60 & splines$cc_count<=65, 13, 
                           ifelse(splines$cc_count>65 & splines$cc_count<=70, 14,
                           ifelse(splines$cc_count>70 & splines$cc_count<=75, 15,
                           ifelse(splines$cc_count>75 & splines$cc_count<=80, 16,
                           ifelse(splines$cc_count>80 & splines$cc_count<=85, 17,       
                           ifelse(splines$cc_count>85 & splines$cc_count<=90, 18,
                           ifelse(splines$cc_count>90 & splines$cc_count<=95, 19,
                           ifelse(splines$cc_count>95 & splines$cc_count<=100, 20,
                           ifelse(splines$cc_count>100 & splines$cc_count<=105, 21, 
                           ifelse(splines$cc_count>105 & splines$cc_count<=110, 22,
                           ifelse(splines$cc_count>110 & splines$cc_count<=115, 23,
                           ifelse(splines$cc_count>115 & splines$cc_count<=120, 24,
                           ifelse(splines$cc_count>120 & splines$cc_count<=125, 25,
                           ifelse(splines$cc_count>125 & splines$cc_count<=130, 26,
                           ifelse(splines$cc_count>130 & splines$cc_count<=135, 27, 
                           ifelse(splines$cc_count>135 & splines$cc_count<=140, 28,
                           ifelse(splines$cc_count>140 & splines$cc_count<=145, 29,
                           ifelse(splines$cc_count>145 & splines$cc_count<=150, 30,
                           ifelse(splines$cc_count>150 & splines$cc_count<=155, 31,
                           ifelse(splines$cc_count>155 & splines$cc_count<=160, 32, 
                           ifelse(splines$cc_count>160 & splines$cc_count<=165, 33,
                           ifelse(splines$cc_count>165 & splines$cc_count<=170, 34,
                           ifelse(splines$cc_count>170 & splines$cc_count<=175, 35,
                           ifelse(splines$cc_count>175 & splines$cc_count<=180, 36,
                           ifelse(splines$cc_count>180 & splines$cc_count<=185, 37,
                           ifelse(splines$cc_count>185 & splines$cc_count<=190, 38,       
                           ifelse(splines$cc_count>190 & splines$cc_count<=195, 39,
                           ifelse(splines$cc_count>195 & splines$cc_count<=200, 40,
                           ifelse(splines$cc_count>200 & splines$cc_count<=205, 41,
                           ifelse(splines$cc_count>205 & splines$cc_count<=210, 41,
                           ifelse(splines$cc_count>210 & splines$cc_count<=215, 43,
                           ifelse(splines$cc_count>215 & splines$cc_count<=220, 44,
                           ifelse(splines$cc_count>220 & splines$cc_count<=225, 45,
                           ifelse(splines$cc_count>225 & splines$cc_count<=230, 46,       
                           ifelse(splines$cc_count>230 & splines$cc_count<=235, 47,
                           ifelse(splines$cc_count>235 & splines$cc_count<=240, 48,
                           ifelse(splines$cc_count>240 & splines$cc_count<=245, 49,
                           ifelse(splines$cc_count>245 & splines$cc_count<=250, 50, 51))))))))))))))))))))))))))))))))))))))))))))))))))
  
  # CREATE CC BUCKET #2
  splines$cc_bkt2 <- ifelse(splines$cc_count<=10, 1,
                     ifelse(splines$cc_count>10 & splines$cc_count<=20, 2,
                     ifelse(splines$cc_count>20 & splines$cc_count<=30, 3,
                     ifelse(splines$cc_count>30 & splines$cc_count<=40, 4,
                     ifelse(splines$cc_count>40 & splines$cc_count<=50, 5, 
                     ifelse(splines$cc_count>50 & splines$cc_count<=60, 6,
                     ifelse(splines$cc_count>60 & splines$cc_count<=70, 7,
                     ifelse(splines$cc_count>70 & splines$cc_count<=80, 8,
                     ifelse(splines$cc_count>80 & splines$cc_count<=90, 9,
                     ifelse(splines$cc_count>90 & splines$cc_count<=100, 10,
                     ifelse(splines$cc_count>100 & splines$cc_count<=110, 11,
                     ifelse(splines$cc_count>110 & splines$cc_count<=120, 12,
                     ifelse(splines$cc_count>120 & splines$cc_count<=130, 12,                                                                                    
                     ifelse(splines$cc_count>130 & splines$cc_count<=140, 13, 
                     ifelse(splines$cc_count>140 & splines$cc_count<=150, 14,
                     ifelse(splines$cc_count>150 & splines$cc_count<=160, 15,
                     ifelse(splines$cc_count>160 & splines$cc_count<=170, 16,
                     ifelse(splines$cc_count>170 & splines$cc_count<=180, 17,       
                     ifelse(splines$cc_count>180 & splines$cc_count<=190, 18,
                     ifelse(splines$cc_count>190 & splines$cc_count<=200, 19,
                     ifelse(splines$cc_count>200 & splines$cc_count<=210, 20, 21)))))))))))))))))))))                                                                                                                                    
  
  rise.spline <- filter(splines, concept=="NIKE RISE")
  rise.spline$id <- paste(rise.spline$division, rise.spline$consumer_construct, rise.spline$intended_use, sep=".")
  length(unique(rise.spline$store_id)) # 25 stores
  sum(rise.spline$gross_revenue) # $38M
  rise.spline.fw <- filter(rise.spline, division=="Footwear")
  # drop low cc count data
#  rise.spline.fw <- filter(rise.spline.fw, cc_count>7)
  
  
  rsf.ag <- summarise(group_by(rise.spline.fw, id, division, consumer_construct, intended_use, cc_bkt2),  #cc_bkt2
                      rev_squr_ft    = mean(gross_revenue/squr_ft_fkt_bmol, na.rm=TRUE),
                      avg_squr_ft_fkt_bmol = mean(squr_ft_fkt_bmol, na.rm=TRUE),
                      avg_cc_count    = mean(cc_count,    na.rm=TRUE),
                      avg_gross_units = mean(gross_units, na.rm=TRUE),
                      avg_gross_rev   = mean(gross_revenue, na.rm=TRUE),
                      store_count    = length(unique(store_id)))
  
 # barplots
  # avg store units
  bar.bkt <- ggplot(rsf.ag, aes(y = avg_gross_units, x = as.factor(cc_bkt2), fill = as.factor(cc_bkt2))) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    ggtitle("Rise FW
Units by CC bucket") +
    ylab("Units") +
    xlab("CC Bucket") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) +
    facet_wrap(~id)
  bar.bkt
  
  # removal of prod groupings for easier viewing
  vizb <- filter(rsf.ag, id!="Footwear.JORDAN.PERFORMANCE" & id!="Footwear.JORDAN.LIFESTYLE" & id!="Footwear.KIDS.LIFESTYLE" & id!="Footwear.KIDS.PERFORMANCE")
  # removal of outliers
  vizb2 <- filter(vizb, !(id=="Footwear.MENS.LIFESTYLE" & cc_bkt2==16 | cc_bkt2==17 | cc_bkt2==20))
  vizb2 <- filter(vizb2, !(id=="Footwear.WOMENS.LIFESTYLE" & cc_bkt2==15 | cc_bkt2==16 | cc_bkt2==18 | cc_bkt2==19))
  vizb2 <- filter(vizb2, cc_bkt2!=21) 
   # avg revenue
  bar.bkt <- ggplot(vizb2, aes(y = avg_gross_rev, x = as.factor(cc_bkt2), fill = as.factor(cc_bkt2))) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    ggtitle("Rise FW
Avg Revenue of Store-Month
by CC bucket") +
    ylab("Revenue") +
    xlab("CC Bucket") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) +
    facet_wrap(~id)
  bar.bkt
  
  
  # calculate marginal benefit of adding more styles
  # AVG CCs by prod group 
  avgcc <- summarise(group_by(rise.spline.fw, division, consumer_construct, intended_use),
                     avg_cc_count    = mean(cc_count,    na.rm=TRUE),
                     q75.            = quantile(cc_count, probs = seq(0.25, 0.75), na.rm=TRUE))
  
  
  # graph on averages but regress on store-month values 
  # MENS LIFESTYLE B = 0.80
  fml <- filter(rise.spline.fw, consumer_construct=="MENS" & intended_use=="LIFESTYLE")
  lmfml = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = fml)
  summary(lmfml)
  # spine 1
  s1 <- filter(fml, cc_count<130)
  lms1 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s1)
  summary(lms1) # 2.14 less than 67, 1.6 less than 108
  # spline 2
  s2 <- filter(fml, cc_count>=67 & cc_count<108)
  lms2 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s2)
  summary(lms2) # -2.95
  # spline 3
  s3 <- filter(fml, cc_count>=108 & cc_count<130)
  lms3 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s3)
  summary(lms3) # 0.54
  # spline 4
  s4 <- filter(fml, cc_count>=130)
  lms4 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s4)
  summary(lms4) # -2.05 not SS
  # less 67, 2.14, less 108 1.6, less 130 1.3, less 250 0.8
   
  # MENS PERFORMANCE B = 1.01
  fmp <- filter(rise.spline.fw, consumer_construct=="MENS" & intended_use=="PERFORMANCE")
  lmfmp = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = fmp)
  summary(lmfmp)
  # spine 1
  s1 <- filter(fmp, cc_count<49)
  lms1 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s1)
  summary(lms1) # 0.77 less 49, 0.98 less 90, 0.95 less 98, 1.03 less 250
  # spline 2
  s2 <- filter(fmp, cc_count>=49 & cc_count<90)
  lms2 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s2)
  summary(lms2) # 1.42
  # spline 3
  s3 <- filter(fmp, cc_count>=90 & cc_count<98)
  lms3 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s3)
  summary(lms3) # -5.5 not SS
  # spline 4
  s4 <- filter(fmp, cc_count>=98)
  lms4 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s4)
  summary(lms4) # 0.89
  # less 67, 2.14, less 108 1.6, less 130 1.3, less 250 0.8
  
  
  # WOMENS LIFESTYLE B = 0.28
  fwl <- filter(rise.spline.fw, consumer_construct=="WOMENS" & intended_use=="LIFESTYLE")
  lmfwl = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = fwl)
  summary(lmfwl)
  # spine 1
  s1 <- filter(fwl, cc_count<60)
  lms1 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s1)
  summary(lms1) # 1.24 less 60, -0.2 less 90 not SS, -0.12 not ss less 102, 0.22 less 250
  # spline 2
  s2 <- filter(fwl, cc_count>=60 & cc_count<250)
  lms2 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s2)
  summary(lms2) # -3.1 #60-250 = 0.22
  # spline 3
 # s3 <- filter(fwl, cc_count>=90 & cc_count<102)
#  lms3 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s3)
#  summary(lms3) # -5.5 not SS
  # spline 4
#  s4 <- filter(fwl, cc_count>=102)
#  lms4 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s4)
#  summary(lms4) # 0.89
  # less 67, 2.14, less 108 1.6, less 130 1.3, less 250 0.8
  
  
  # WOMENS PERFORMANCE B = 1.24
  fwp <- filter(rise.spline.fw, consumer_construct=="WOMENS" & intended_use=="PERFORMANCE")
  lmfwp = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = fwp)
  summary(lmfwp)
  # spine 1
  s1 <- filter(fwp, cc_count<33)
  lms1 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s1)
  summary(lms1) # 1.05 less 33, 1.02 less 53, 1.13 less 64, 1.24 less 250
  # spline 2
  s2 <- filter(fwp, cc_count>=33 & cc_count<53)
  lms2 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s2)
  summary(lms2) # 1.15
  # spline 3
   s3 <- filter(fwp, cc_count>=53 & cc_count<64)
    lms3 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s3)
    summary(lms3) # 2.75 not SS
  # spline 4
    s4 <- filter(fwp, cc_count>=64)
    lms4 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s4)
    summary(lms4) # 0.85
  # less 67, 2.14, less 108 1.6, less 130 1.3, less 250 0.8
  
  
  
  # KIDS LIFESTYLE B = 0.31
  fkl <- filter(rise.spline.fw, consumer_construct=="KIDS" & intended_use=="LIFESTYLE")
  lmfkl = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = fkl)
  summary(lmfkl)
  # spine 1
  s1 <- filter(fkl, cc_count<49)
  lms1 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s1)
  summary(lms1) # 2.4 less 49, 0.26 less 68 (use not ss .83), -0.05 less 75 (use the not ss .8), 0.31 less 250
  # spline 2
  s2 <- filter(fkl, cc_count>=49 & cc_count<68)
  lms2 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s2)
  summary(lms2) # .83 not SS
  # spline 3
  s3 <- filter(fkl, cc_count>=68 & cc_count<90)
  lms3 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s3)
  summary(lms3) # -2.4 not SS
  # spline 4
  s4 <- filter(fkl, cc_count>=90)
  lms4 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s4)
  summary(lms4) # 0.8 not SS
 
  
  
  # KIDS PERFORMANCE B = 0.97
  fkp <- filter(rise.spline.fw, consumer_construct=="KIDS" & intended_use=="PERFORMANCE")
  lmfkp = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = fkp)
  summary(lmfkp)
  # spine 1
  s1 <- filter(fkp, cc_count<11)
  lms1 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s1)
  summary(lms1) # 1 less 11, 0.79 less 22, 0.97 less 33, 0.97 less 250
  # spline 2
  s2 <- filter(fkp, cc_count>=11 & cc_count<33)
  lms2 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s2)
  summary(lms2) # .43 not SS, btw 11 and 33 1.4 (SS)
  # spline 3
  s3 <- filter(fkp, cc_count>=22 & cc_count<33)
  lms3 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s3)
  summary(lms3) # 1.94
  # spline 4
  s4 <- filter(fkp, cc_count>=33)
  lms4 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s4)
  summary(lms4) # -0.57 not SS
  
  
  # JORDAN LIFESTYLE B = 0.61
  fjl <- filter(rise.spline.fw, consumer_construct=="JORDAN" & intended_use=="LIFESTYLE")
  lmfjl = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = fjl)
  summary(lmfjl)
  # spine 1
  s1 <- filter(fjl, cc_count<41)
  lms1 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s1)
  summary(lms1) # 1.31 less 41, 1.02 less 72, 0.92 less 90, 0.61 less 250
  # spline 2
  s2 <- filter(fjl, cc_count>=41 & cc_count<72)
  lms2 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s2)
  summary(lms2) # -.68 not SS
  # spline 3
  s3 <- filter(fjl, cc_count>=72 & cc_count<90)
  lms3 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s3)
  summary(lms3) # -2.5 not SS
  # spline 4
  s4 <- filter(fjl, cc_count>=90)
  lms4 = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = s4)
  summary(lms4) # 1.17 not SS
  # less 67, 2.14, less 108 1.6, less 130 1.3, less 250 0.8
  

# INITIAL TAKEAWAYS
  # at average cc's MENS LIFE and KIDS PERF have highest ROI to add't CC
  # highest marginal returns observed are MENS LIFE, KIDS LIFE, and JORDAN LIFE when store cc's are in the lowest quartile
  # highest opp cost of reduction at avg ccs, Men's life -1.6, Men's perf -1.4, Kid's perf -1.4
  # ALL marginal returns of add'l cc's in highest quartile are less than 1
  # lowest marginal returns of add'l cc's in highest quartile are WMNS LIFE 0.22, KIDS perf -0.5, JORDAN LIFE 0.6 (all others bt .8-.89)
  # WMNS LIFE (.22) and KID LIFE (0.8) experience the fastest Diminishing marginal returns, less than 1 above the lowest quartile
  
  
    
  scatterp <- ggplot(fwp) + 
    geom_point(aes(y = log(gross_revenue), x = log(cc_count)), show.legend = FALSE) +
    geom_smooth(aes(y = log(gross_revenue), x = log(cc_count)), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("Revenue
") +
    ylab("Log(Gross Revenue)") +
    xlab("Log(CC count)") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp
  
  
  scatterp <- ggplot(fmp) + 
    geom_point(aes(y = gross_revenue, x = cc_count), show.legend = FALSE) +
    geom_smooth(aes(y = gross_revenue, x = cc_count), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("Revenue
") +
    ylab("Gross REvenue") +
    xlab("CC count") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp  
  
  
  
 scatterp <- ggplot(s1) + 
    geom_point(aes(y = log(gross_units), x = log(cc_count)), show.legend = FALSE) +
    geom_smooth(aes(y = log(gross_units), x = log(cc_count)), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("
") +
    ylab("Log(Gross Units)") +
    xlab("Log(CC count)") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp
  
  
  scatterp <- ggplot(s1) + 
    geom_point(aes(y = gross_units, x = cc_count), show.legend = FALSE) +
    geom_smooth(aes(y = gross_units, x = cc_count), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("
") +
    ylab("Gross Units") +
    xlab("CC count") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp  
 
  
### DATA VISUALS
  ### SCATTER PLOT OF SINGLE RISE STORE CC VS UNITS OVER TIME (OR REVENUE) COLOR CODE BY GROUPING
  ### FW ONLY
  
  #find biggest store---also consider average
  big <- summarise(group_by(rise.spline.fw, store_id),
                   rev = sum(gross_revenue, na.rm=TRUE))
  big <- arrange(big, desc(rev))
  
#    store_id     rev
#  1      478 2812921
#  2     6104 1798071
#  3     2793 1693937
#  4      806 1621965
#  5     2444  944059
#  6     2478  822042
  
  
#  viz1 <- filter(rise.spline.fw, store_id==478)
  viz1 <- filter(rise.spline.fw, store_id==2489)
  scatterp <- ggplot(viz1) + 
    geom_point(aes(y = log(gross_revenue), x = log(cc_count), color=id)) +
    geom_smooth(aes(y = log(gross_revenue), x = log(cc_count)), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("Rise Door
") +
    ylab("Log(Gross Revenue)") +
    xlab("Log(CC count)") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp  
  
  # LOG UNITS
#  viz1 <- filter(rise.spline.fw, store_id==478)
  viz1 <- filter(rise.spline.fw, store_id==2489)
  scatterp <- ggplot(viz1) + 
    geom_point(aes(y = log(gross_units), x = log(cc_count), color=id)) +
    geom_smooth(aes(y = log(gross_units), x = log(cc_count), color=id), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("Rise Door
") +
    ylab("Gross Units") +
    xlab("CC count") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp   
  
  # UNITS NO LOG
#  viz1 <- filter(rise.spline.fw, store_id==478)
  viz1 <- filter(rise.spline.fw, store_id==2489)
#  viz1 <- rise.spline.fw
  scatterp <- ggplot(viz1) + 
    geom_point(aes(y = gross_units, x = cc_count, color=id)) +
#    geom_smooth(aes(y = gross_units, x = cc_count, color=id), 
#               se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("FW Units by CC 
Average Rise Door (Store #2489) 
Jan 1 - June 30 2023") +
    ylab("Gross Units") +
    xlab("CC count") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp   
  
# UNITS NO LOG ALL STORES
  viz1 <- rise.spline.fw
  scatterp <- ggplot(viz1) + 
    geom_point(aes(y = gross_units, x = cc_count, color=id)) +
        geom_smooth(aes(y = gross_units, x = cc_count, color=id), 
                   se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("FW Units by CC 
All Rise Doors 
Jan 1 - June 30 2023") +
    ylab("Gross Units") +
    xlab("CC count") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  scatterp
     
  # what is average revenue of womens FW Rise
  look <- summarise(group_by(rise.spline.fw, consumer_construct, intended_use),
                    avg_cc = mean(cc_count, na.rm=TRUE),
                    med_cc = median(cc_count, na.rm=TRUE),
                    q1_cc = quantile(cc_count, probs =(0.25)),
                    q3_cc = quantile(cc_count, probs= (0.75)),
                    tot_rev = sum(gross_revenue, na.rm=TRUE))
  tot <- sum(look$tot_rev)
  tot_cc <- sum(look$avg_cc)
  look$sob <- look$tot_rev/tot
  look$socc <- look$avg_cc/tot_cc
  
  0.01*tot
  
 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# # # # # # # # # # # EXPERIMENTAL # # # # # # # # # # # # # # #     
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
  # in order to automate and scale opp. cost table creation I need to...
  # automate running models across groups and output coefficients into a table
  # automate selection of model coefficients, filtering out non-statistically significant and replacing with average estimates
  # automate the calculation of values in the table (can potentially be done in excel but slower)
  
 
  # can run regression or other models across all groups automatically 
  test <- filter(rise.spline.fw, id=="Footwear.MENS.LIFESTYLE" | id=="Footwear.WOMENS.LIFESTYLE")
  fitted_models = test %>% group_by(id) %>% do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
 
  # can put coefficients from as many models as you want into list or data frame with manual code 
#  fitted_models[[2]][[1]]$coefficients[[2]]. # how to pull out coefficients with multiple model results in object
  see <- list(fitted_models[[2]][[1]]$coefficients[[2]], fitted_models[[2]][[2]]$coefficients[[2]]) # put coeff from dif models in a list
  see <- c(fitted_models[[2]][[1]]$coefficients[[2]], fitted_models[[2]][[2]]$coefficients[[2]]) # put coeff from dif models in a vector
  see <- as.data.frame(c(fitted_models[[2]][[1]]$coefficients[[2]], fitted_models[[2]][[2]]$coefficients[[2]])) # put coeff from dif models in a dataframe
  
  # how to input coefficient into a table next to correct cc allocation row
  # maybe can't do quartiles since this will vary by product....
  # need obs by week or bi-weekly to look at smaller ranges of cc values (e.g. +/-10ccs)

  # may need to run all models across multiple ranges of cc values to improve data quality
  # (i.e. run 2nd set of models over +/-20ccs)  
  # then need a selection process to pick the best coefficient
  # Could fill a dataframe with desired coefficients then hard code their placement into opp. cost table
  # if each coefficient for further out cc counts includes all data up to that point it will be easier to calculate opp. table
  # for example, instead of spline from cc=0 to cc=20, and separate spline from cc=21 to cc=40, second model is cc=0 to cc=40
  # this should still be accurate - models should improve with accuracy further from 0 as they include more data... - 
  # this also allows for more granular adjustments since small cc ranges are just adding data to the previous model and not
  # calculated on a small range of ccs only
  # adjust cc range of models to the smallest requirement for all prod groups
  # perform fitted models for each of these small incremental prod groups...
  # issue! likely to positively bias estimates where cc count is high since plateau areas will now incorporate low cc periods... ()
  # have a set of models that incrementally add small cc groups starting from both ends...
  # need a criteria for determining when to switch from one to the other for predictions (accuracy? average/geom avg coeffs?, geom avg forecasts)
  # need to input coeffs into a table with a column for their predictive accuracy too perhaps
  
  
  
  
  
  
  
  
  # # if you downloaded llvm manually above, replace with your chosen NEW_PATH/clang
  # LLVM_LOC = /usr/local/opt/llvm
  # CC=$(LLVM_LOC)/bin/clang -fopenmp
  # CXX=$(LLVM_LOC)/bin/clang++ -fopenmp
  # # -O3 should be faster than -O2 (default) level optimisation ..
  # CFLAGS=-g -O3 -Wall -pedantic -std=gnu99 -mtune=native -pipe
  # CXXFLAGS=-g -O3 -Wall -pedantic -std=c++11 -mtune=native -pipe
  # LDFLAGS=-L/usr/local/opt/gettext/lib -L$(LLVM_LOC)/lib -Wl,-rpath,$(LLVM_LOC)/lib
  # CPPFLAGS=-I/usr/local/opt/gettext/include -I$(LLVM_LOC)/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include
  
  # GCC (Official GNU fortran) ver
  
  LOC = /usr/local/gfortran
  CC=$(LOC)/bin/gcc -fopenmp
  CXX=$(LOC)/bin/g++ -fopenmp
  CXX11 = $(LOC)/bin/g++ -fopenmp # for fst package
  
  CFLAGS=-g -O3 -Wall -pedantic -std=gnu99 -mtune=native -pipe
  CXXFLAGS=-g -O3 -Wall -pedantic -std=c++11 -mtune=native -pipe
  LDFLAGS=-L$(LOC)/lib -Wl,-rpath,-I$(LOC)/lib
  CPPFLAGS=-I$(LOC)/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include
  
  