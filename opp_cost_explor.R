 
#  library(data.table)
  library(readxl)
#  library(xlsx)
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
  
  # CALCULATE CC COUNT PER SQUARE FOOT
  all$cc_per <- all$cc_count/all$squr_ft_fkt_bmol
  
# plot ccs by unit sales of stores
  stores <- summarise(group_by(all, store_id, squr_ft_fkt_bmol, concept, month_nm),
                      cc_count    = sum(cc_count,    na.rm=TRUE),
                      gross_units = sum(gross_units, na.rm=TRUE))
  
  scatall <- ggplot(stores) + 
    geom_point(aes(y = gross_units, x = cc_count), show.legend = FALSE) +
    geom_smooth(aes(y = gross_units, x = cc_count), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("Each dot is a store in a given month
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
  
  scatall
  
 # Test linearity assumption with logs 
  scatall.log <- ggplot(stores) + 
    geom_point(aes(y = log(gross_units), x = log(cc_count)), show.legend = FALSE) +
    geom_smooth(aes(y = log(gross_units), x = log(cc_count)), 
                se = FALSE, method = "lm", formula = y ~ x, size = 1) +
    ggtitle("Each dot is a wellness collective store in a given month
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

  scatall.log
  
# inspect cc variation over time and across stores (by concept)
  # line plot
  # NIKE LIVE (WELLNESS COLLECTIVE)
  well <- filter(stores, concept=="NIKE LIVE")
  well$store_id <- as.factor(well$store_id)
  
 # in LIVE more variation across stores than within
 # appear to be seasonal trends i.e. fewer ccs in month 2 than 1
 # good amount of variation across stores in a similar range  
  
  line.well <- ggplot(well, aes(y = cc_count, x = month_nm, group=store_id)) + 
    geom_line(aes(color=store_id), show.legend = FALSE) +
    ggtitle("Each line is a Wellness Collective store over 6 months
") +
    ylab("CC count") +
    xlab("Months") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  
  line.well
  
  # NIKE RISE
  rise <- filter(stores, concept=="NIKE RISE")
  rise$store_id <- as.factor(rise$store_id)
  
  
  line.rise <- ggplot(rise, aes(y = cc_count, x = month_nm, group=store_id)) + 
    geom_line(aes(color=store_id)) +
    ggtitle("Each line is a Rise store over 6 months
") +
    ylab("CC count") +
    xlab("Months") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  
  line.rise
  
  # NIKE UNITE
  # NON OUTLET
  # SHOW HIGHER WITHIN STORE VARIATION (QUANTIFY?)
  unite <- filter(stores, concept=="NIKE UNITE" | concept=="UNITE NON-OUTLET")
  unite$store_id <- as.factor(unite$store_id)
  
  line.unite <- ggplot(unite, aes(y = cc_count, x = month_nm, group=store_id)) + 
    geom_line(aes(color=store_id)) +
    ggtitle("Each line is a Unite Non-outlet store over 6 months
") +
    ylab("CC count") +
    xlab("Months") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  
  line.unite
  
  
  # NIKE UNITE
  # OUTLET
  
  unite.out <- filter(stores, concept=="UNITE NON-OUTLET")
  unite.out$store_id <- as.factor(unite.out$store_id)
  
  line.unite.out <- ggplot(unite.out, aes(y = cc_count, x = month_nm, group=store_id)) + 
    geom_line(aes(color=store_id)) +
    ggtitle("Each line is a Unite Outlet store over 6 months
") +
    ylab("CC count") +
    xlab("Months") +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(),             
          panel.grid.minor.y = element_blank(),             
          panel.border = element_blank(),
          panel.background = element_blank(),              
          legend.key = element_rect(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  
  line.unite.out
  
# PERFORM THE ABOVE WITH CC/SQUARE FOOT
  # WHAT IS THE DIFFERENCE? WHICH SHOULD WE USE?
  
  # HOW DO UNITS INCREASE WITH CCS
  # PREP DATA FOR HISTOGRAMS
  # LIVE/WELLNESS COLLECTIVE  
  summary(well$cc_count)
  well$cc_bkt <- ifelse(well$cc_count<=130, 1,
                 ifelse(well$cc_count>130 & well$cc_count<=260, 2,
                 ifelse(well$cc_count>260 & well$cc_count<=390, 3,
                 ifelse(well$cc_count>390 & well$cc_count<=520, 4,
                 ifelse(well$cc_count>520 & well$cc_count<=650, 5, 
                 ifelse(well$cc_count>650 & well$cc_count<=780, 6,
                 ifelse(well$cc_count>780 & well$cc_count<=910, 7,
                 ifelse(well$cc_count>1040 & well$cc_count<=1170, 8,
                 ifelse(well$cc_count>1300 & well$cc_count<=1430, 9,
                 ifelse(well$cc_count>1430 & well$cc_count<=1560, 10,
                 ifelse(well$cc_count>1560 & well$cc_count<=1690, 11,
                 ifelse(well$cc_count>1690 & well$cc_count<=1820, 12,
                 ifelse(well$cc_count>1820 & well$cc_count<=1950, 13, 
                 ifelse(well$cc_count>1950 & well$cc_count<=2080, 14,
                 ifelse(well$cc_count>2080 & well$cc_count<=2210, 15,
                 ifelse(well$cc_count>2210 & well$cc_count<=2340, 16,
                 ifelse(well$cc_count>2340 & well$cc_count<=2470, 17,       
                 ifelse(well$cc_count>2470 & well$cc_count<=2600, 18,
                 ifelse(well$cc_count>2600 & well$cc_count<=2730, 19,
                 ifelse(well$cc_count>2730 & well$cc_count<=2860, 20,
                 ifelse(well$cc_count>2860 & well$cc_count<=2990, 21, 
                 ifelse(well$cc_count>2990 & well$cc_count<=3120, 22,
                 ifelse(well$cc_count>3120 & well$cc_count<=3250, 23,
                 ifelse(well$cc_count>3250 & well$cc_count<=3380, 24,
                 ifelse(well$cc_count>3380 & well$cc_count<=3510, 25,
                 ifelse(well$cc_count>3510 & well$cc_count<=3640, 26,
                 ifelse(well$cc_count>3640 & well$cc_count<=3770, 27, 28)))))))))))))))))))))))))))      
                        
  histprep <- summarise(group_by(well, cc_bkt),
                        cc_count    = mean(cc_count,    na.rm=TRUE),
                        gross_units = mean(gross_units, na.rm=TRUE))
  
  hist.well <- ggplot(well, aes(y = gross_units, x = cc_count, color = store_id)) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    ggtitle("Wellness Collective
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
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  hist.well
  
  tab.well <- summarise(group_by(well, cc_bkt),
                        avg_cc_count    = mean(cc_count,    na.rm=TRUE),
                        avg_gross_units = mean(gross_units, na.rm=TRUE),
                        store_count    = length(unique(store_id)))
  tab.well
  
  hist.well2 <- ggplot(tab.well, aes(y = avg_gross_units, x = cc_bkt, fill = as.factor(cc_bkt))) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    ggtitle("Wellness Collective
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
          axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
  hist.well2 
  
  
  lmprod = lm(log(avg_gross_units)~log(avg_cc_count) + store_count, data = tab.well)
  summary(lmprod)
  
  lmprod = lm(log(gross_units)~log(cc_count), data=well)
  summary(lmprod)
  
  lmprod2 = lm(log(gross_units)~log(cc_count) + log(squr_ft_fkt_bmol), data=well)
  summary(lmprod2)
  
  lmprod3 = lm(log(gross_units)~log(cc_count) + store_id, data=well)
  summary(lmprod3)
  
  # ESTIMATE SPLINES
  w1 <- filter(well, cc_bkt<=6)
  w2 <- filter(well, cc_bkt>=7 & cc_bkt<=14)
  w3 <- filter(well, cc_bkt>14 & cc_bkt<=26)
  
  lmprod2 = lm(log(gross_units)~log(cc_count) + log(squr_ft_fkt_bmol), data=w3)
  summary(lmprod2)
  
 # lmprod3 = lm(log(gross_units)~log(cc_count) + store_id, data=w1)
#  summary(lmprod3)
  
  # CREATE WELLNESS COLLECTIVE OPP COST BAR PLOTS
  
  stores.tab <- summarise(group_by(all, concept, store_id, squr_ft_fkt_bmol, month_nm, division, consumer_construct, intended_use),
                      cc_count      = sum(cc_count,    na.rm=TRUE),
                      gross_units   = sum(gross_units, na.rm=TRUE),
                      gross_revenue = sum(gross_revenue, na.rm=TRUE))
  
  # REMOVE EQUIPMENT
  stores.tab <- filter(stores.tab, division!="Equipment")
  # REMOVE JORDAN PERFORMANCE
  stores.tab <- filter(stores.tab, !(consumer_construct=="JORDAN" & intended_use=="PERFORMANCE"))
  
  # AVERAGE CC COUNT BY DIV-CC-INTENDED
  avgcc <- summarise(group_by(stores.tab, concept, division, consumer_construct, intended_use),
                    avg_cc = round(mean(cc_count, na.rm=TRUE), digits=0))
#  well.bkt <- filter(avgcc, concept=="NIKE LIVE")
  avgcc$minus10 <- round(avgcc$avg_cc*0.9, digits=0)
  avgcc$minus20 <- round(avgcc$avg_cc*0.8, digits=0)
  avgcc$minus30 <- round(avgcc$avg_cc*0.7, digits=0)
  avgcc$plus10 <- round(avgcc$avg_cc*1.1, digits=0)
  avgcc$plus20 <- round(avgcc$avg_cc*1.2, digits=0)
  avgcc$plus30 <- round(avgcc$avg_cc*1.3, digits=0)

  # MELT INTO LONG FORM  
#  test <- melt(well.bkt, na.rm=FALSE, value.name ="change in cc count", id = c('concept', 'division', 'consumer_construct', 'intended_use'))
  
  # MERGE BUCKET WITH STORES.TAB 
  stores.bkt <- merge(stores.tab, avgcc, by = c("concept", "division", "consumer_construct", "intended_use"), all.x=TRUE)
  
  stores.bkt$flag_minus30   <- ifelse(stores.bkt$cc_count <= stores.bkt$minus30, 1, 0) 
  stores.bkt$flag_minus20   <- ifelse(stores.bkt$cc_count >  stores.bkt$minus30 & stores.bkt$cc_count <= stores.bkt$minus20, 1, 0) 
  stores.bkt$flag_minus10   <- ifelse(stores.bkt$cc_count >  stores.bkt$minus20 & stores.bkt$cc_count <= stores.bkt$minus10, 1, 0) 
  stores.bkt$flag_within10 <- ifelse(stores.bkt$cc_count > stores.bkt$minus10 & stores.bkt$cc_count < stores.bkt$plus10, 1, 0)
  stores.bkt$flag_plus10   <- ifelse(stores.bkt$cc_count >= stores.bkt$plus10 & stores.bkt$cc_count <= stores.bkt$plus20, 1, 0) 
  stores.bkt$flag_plus20   <- ifelse(stores.bkt$cc_count >= stores.bkt$plus20 & stores.bkt$cc_count <= stores.bkt$plus30, 1, 0) 
  stores.bkt$flag_plus30   <- ifelse(stores.bkt$cc_count >= stores.bkt$plus30, 1, 0) 
  
# MELT INTO LONG FORM WITH FLAGS ROLLED INTO ONE COLUMN
  stores.bkt.abb <- stores.bkt[, c("concept", "division", "consumer_construct", "intended_use", "store_id","squr_ft_fkt_bmol", "month_nm", "cc_count", "gross_units", "gross_revenue", "avg_cc",
                                   "flag_minus30", "flag_minus20", "flag_minus10", "flag_within10", "flag_plus10", "flag_plus20", "flag_plus30")]
  
  stores.bkt.long <- melt(stores.bkt.abb, na.rm=FALSE, value.name = "cc_bucket", id = c('concept', 'division', 'consumer_construct', 'intended_use', 'store_id', 'squr_ft_fkt_bmol', 'month_nm', 'cc_count', 'gross_units', 'gross_revenue', 'avg_cc'))
  
  stores.bkt.long.ag <- summarise(group_by(stores.bkt.long, concept, division, consumer_construct, intended_use, variable, cc_bucket),
                                  avg.cc_count      = mean(cc_count, na.rm=TRUE),
                                  avg.gross_units   = mean(gross_units, na.rm=TRUE),
                                  avg.gross_revenue = mean(gross_revenue, na.rm=TRUE))
  
  stores.bkt.long.ag$id <- paste(stores.bkt.long.ag$division, stores.bkt.long.ag$consumer_construct, stores.bkt.long.ag$intended_use, sep=".")
  well.bkt.long <- filter(stores.bkt.long.ag, concept=="NIKE LIVE")
  well.bkt.long <- filter(well.bkt.long, cc_bucket==1)
  
  bar.bkt <- ggplot(well.bkt.long, aes(y = avg.gross_units, x = variable, fill = variable)) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    ggtitle("Wellness Collective
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
  
  
   
  # CREATE OPP COST TABLES
  stores.opp.tab <- filter(stores.bkt.long.ag, cc_bucket==1)
  stores.opp.trunc <- stores.opp.tab[,c ("concept","id","variable","avg.gross_units")]
 
  # CALCULATE PERCENT COLUMNS
  perc_cols <- dcast(stores.opp.trunc, concept + id ~ variable)
  perc_cols$pchg_minus_30_20 <- (perc_cols$flag_minus20-perc_cols$flag_minus30)/((perc_cols$flag_minus20+perc_cols$flag_minus30)/2)
  
  test <- melt(perc_cols, na.rm=FALSE, value.name = "cc_change", id = c("concept", "id", "pchg_minus_30_20"))
  stores.bkt.long <- melt(stores.bkt.abb, na.rm=FALSE, value.name = "cc_bucket", id = c('concept', 'division', 'consumer_construct', 'intended_use', 'store_id', 'squr_ft_fkt_bmol', 'month_nm', 'cc_count', 'gross_units', 'gross_revenue', 'avg_cc'))
  
  # OPP COST TABLE
   opp.tab <- dcast(stores.opp.trunc, concept + variable ~ id)
# CALCULATE % CHANGE IN EXCEL
  
   
   
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
  
   
   rsf.ag <- summarise(group_by(rise.spline.fw, division, consumer_construct, intended_use, cc_bkt2),
                     avg_squr_ft_fkt_bmol = mean(squr_ft_fkt_bmol, na.rm=TRUE),
                     avg_cc_count    = mean(cc_count,    na.rm=TRUE),
                     avg_gross_units = mean(gross_units, na.rm=TRUE),
                     store_count    = length(unique(store_id)))
   
  
   bar.bkt <- ggplot(rsf.ag, aes(y = avg_gross_units, x = cc_bkt2, fill = cc_bkt2)) + 
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
           axis.text.x=element_text(angle = 90, vjust = 0, hjust=0)) #+
   #  facet_wrap(~id)
   bar.bkt
   
   
   # test regressions
   fjl <- filter(rise.spline.fw, consumer_construct=="JORDAN" & intended_use=="LIFESTYLE")
   fjl <- filter(fjl, cc_bkt2>8 & cc_bkt2<23)
   lmfjl = lm(log(gross_units)~log(cc_count) + log(squr_ft_fkt_bmol), data = fjl)
   summary(lmfjl) 
   
   fjl.ag <- summarise(group_by(fjl, cc_bkt2),
                       avg_cc_count    = mean(cc_count,    na.rm=TRUE),
                       avg_gross_units = mean(gross_units, na.rm=TRUE),
                       store_count    = length(unique(store_id)))
   
   lmfjl = lm(log(avg_gross_units)~log(avg_cc_count), data = fjl.ag)
   summary(lmfjl) 
   
   # must use avg across stores to create sensical bar plots
   # regressions are similar on averages or store-month level data
   # probably want to model on store-month level 
   # blow out visuals and regression (no splines) for Rise FW and APP
   #copy paste and simplify code to achieve just desired results
   
   
   
   
   
#                  ifelse(splines$cc_count>250 & splines$cc_count<=255, 51,       
#                  ifelse(splines$cc_count>255 & splines$cc_count<=260, 52,
#                  ifelse(splines$cc_count>260 & splines$cc_count<=265, 53,
#                  ifelse(splines$cc_count>265 & splines$cc_count<=270, 54,
#                  ifelse(splines$cc_count>270 & splines$cc_count<=275, 55,
#                  ifelse(splines$cc_count>275 & splines$cc_count<=280, 56,
#                  ifelse(splines$cc_count>280 & splines$cc_count<=285, 57,
#                  ifelse(splines$cc_count>285 & splines$cc_count<=290, 58,       
#                  ifelse(splines$cc_count>290 & splines$cc_count<=295, 59,
#                  ifelse(splines$cc_count>295 & splines$cc_count<=300, 60,
#                  ifelse(splines$cc_count>300 & splines$cc_count<=305, 61, 62)))))))))))))))))))))))))))))))))))))))))))))))))))))))
   
   # partition, spline and estimate (control for square footage)
   # RISE
   
   
   
  
  well.tab <- filter(stores.tab, concept=="NIKE LIVE")
 table(well.tab$cc_count, well.tab$gross_units)
  
 
 
 
  