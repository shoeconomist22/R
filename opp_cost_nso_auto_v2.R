# V2
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
  
  # SEASONAL FILTER SPRING = MONTH {1, 2, 3}, SUMMER = MONTH {4, 5, 6}  
#  all.s <- filter(all, month_nm==1 | month_nm==2 | month_nm==3)
  
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
  
  # MODELING SUBSETS OF DATA  
  c.ag.less10 <- filter(concept.ag, cc_count<50 & cc_count>0)
  c.ag.10_20 <- filter(concept.ag, cc_count>=5 & cc_count<55)
  c.ag.20_30 <- filter(concept.ag, cc_count>=10 & cc_count<60)
  c.ag.30_40 <- filter(concept.ag, cc_count>=15 & cc_count<65)
  c.ag.40_50 <- filter(concept.ag, cc_count>=15 & cc_count<70)
  c.ag.50_60 <- filter(concept.ag, cc_count>=30 & cc_count<80)
  c.ag.60_70 <- filter(concept.ag, cc_count>=40 & cc_count<90)
  c.ag.70_80 <- filter(concept.ag, cc_count>=40 & cc_count<100)
  c.ag.80_90 <- filter(concept.ag, cc_count>=40 & cc_count<110)
  c.ag.90_100 <- filter(concept.ag, cc_count>=60 & cc_count<120)
  
# c.ag.less120 <- filter(concept.ag, cc_count<120)
#  c.ag.less180 <- filter(concept.ag, cc_count<180)
  
  # can run regression or other models across all groups automatically 
  # Spline 1 less than 10 ccs
  fitted_models = c.ag.less10 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
 
#   
  pred.cc <- data.frame(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  names(pred.cc) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc$squr_ft_fkt_bmol <- mean(c.ag.less10$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc) <- c("1_CCs","2_CCs","3_CCs","4_CCs","5_CCs","6_CCs","7_CCs","8_CCs","9_CCs","10_CCs")
  
  predicts <- list()
  for(i in 1:length(fitted_models[[2]])) {
    predicts <- c(predicts, exp(predict(fitted_models[[2]][[i]], pred.cc)))
  }  # vector of predictions
  
  # Spline model 1 fit for 0-10 ccs
  # model generates 10 predicts (1cc to 10ccs) for each ID (prod) group
  # put moodel revenue predictions into a table for each product group (29 total)
  opp.cost <- pred.cc
  opp.cost$col1 <- c(predicts[[1]],predicts[[2]],predicts[[3]],predicts[[4]],predicts[[5]],predicts[[6]],predicts[[7]],predicts[[8]],predicts[[9]],predicts[[10]])
  opp.cost$col2 <- c(predicts[[11]],predicts[[12]],predicts[[13]],predicts[[14]],predicts[[15]],predicts[[16]],predicts[[17]],predicts[[18]],predicts[[19]],predicts[[20]])
  opp.cost$col3 <- c(predicts[[21]],predicts[[22]],predicts[[23]],predicts[[24]],predicts[[25]],predicts[[26]],predicts[[27]],predicts[[28]],predicts[[29]],predicts[[30]])
  opp.cost$col4 <- c(predicts[[31]],predicts[[32]],predicts[[33]],predicts[[34]],predicts[[35]],predicts[[36]],predicts[[37]],predicts[[38]],predicts[[39]],predicts[[40]])
  opp.cost$col5 <- c(predicts[[41]],predicts[[42]],predicts[[43]],predicts[[44]],predicts[[45]],predicts[[46]],predicts[[47]],predicts[[48]],predicts[[49]],predicts[[50]])
  opp.cost$col6 <- c(predicts[[51]],predicts[[52]],predicts[[53]],predicts[[54]],predicts[[55]],predicts[[56]],predicts[[57]],predicts[[58]],predicts[[59]],predicts[[60]])
  opp.cost$col7 <- c(predicts[[61]],predicts[[62]],predicts[[63]],predicts[[64]],predicts[[65]],predicts[[66]],predicts[[63]],predicts[[64]],predicts[[65]],predicts[[66]])
  opp.cost$col8 <- c(predicts[[71]],predicts[[72]],predicts[[73]],predicts[[74]],predicts[[75]],predicts[[76]],predicts[[77]],predicts[[78]],predicts[[79]],predicts[[80]])
  opp.cost$col9 <- c(predicts[[81]],predicts[[82]],predicts[[83]],predicts[[84]],predicts[[85]],predicts[[86]],predicts[[87]],predicts[[88]],predicts[[89]],predicts[[90]])
  opp.cost$col10 <- c(predicts[[91]],predicts[[92]],predicts[[93]],predicts[[94]],predicts[[95]],predicts[[96]],predicts[[97]],predicts[[98]],predicts[[99]],predicts[[100]])
  opp.cost$col11 <- c(predicts[[101]],predicts[[102]],predicts[[103]],predicts[[104]],predicts[[105]],predicts[[106]],predicts[[107]],predicts[[108]],predicts[[109]],predicts[[110]])
  opp.cost$col12 <- c(predicts[[111]],predicts[[112]],predicts[[113]],predicts[[114]],predicts[[115]],predicts[[116]],predicts[[117]],predicts[[118]],predicts[[119]],predicts[[120]])
  opp.cost$col13 <- c(predicts[[121]],predicts[[122]],predicts[[123]],predicts[[124]],predicts[[125]],predicts[[126]],predicts[[127]],predicts[[128]],predicts[[129]],predicts[[130]])
#  opp.cost$col14 <- c(predicts[[131]],predicts[[132]],predicts[[133]],predicts[[134]],predicts[[135]],predicts[[136]],predicts[[137]],predicts[[138]],predicts[[139]],predicts[[140]])
#  opp.cost$col15 <- c(predicts[[141]],predicts[[142]],predicts[[143]],predicts[[144]],predicts[[145]],predicts[[146]],predicts[[147]],predicts[[148]],predicts[[149]],predicts[[150]])
#  opp.cost$col16 <- c(predicts[[151]],predicts[[152]],predicts[[153]],predicts[[154]],predicts[[155]],predicts[[156]],predicts[[157]],predicts[[158]],predicts[[159]],predicts[[160]])
#  opp.cost$col17 <- c(predicts[[161]],predicts[[162]],predicts[[163]],predicts[[164]],predicts[[165]],predicts[[166]],predicts[[167]],predicts[[168]],predicts[[169]],predicts[[170]])
#  opp.cost$col18 <- c(predicts[[171]],predicts[[172]],predicts[[173]],predicts[[174]],predicts[[175]],predicts[[176]],predicts[[177]],predicts[[178]],predicts[[179]],predicts[[180]])
#  opp.cost$col19 <- c(predicts[[181]],predicts[[182]],predicts[[183]],predicts[[184]],predicts[[185]],predicts[[186]],predicts[[187]],predicts[[188]],predicts[[189]],predicts[[190]])
#  opp.cost$col20 <- c(predicts[[191]],predicts[[192]],predicts[[193]],predicts[[194]],predicts[[195]],predicts[[196]],predicts[[197]],predicts[[198]],predicts[[199]],predicts[[200]])
#  opp.cost$col21 <- c(predicts[[201]],predicts[[202]],predicts[[203]],predicts[[204]],predicts[[205]],predicts[[206]],predicts[[207]],predicts[[208]],predicts[[209]],predicts[[210]])
#  opp.cost$col22 <- c(predicts[[211]],predicts[[212]],predicts[[213]],predicts[[214]],predicts[[215]],predicts[[216]],predicts[[217]],predicts[[218]],predicts[[219]],predicts[[220]])
#  opp.cost$col23 <- c(predicts[[221]],predicts[[222]],predicts[[223]],predicts[[224]],predicts[[225]],predicts[[226]],predicts[[227]],predicts[[228]],predicts[[229]],predicts[[230]])
#  opp.cost$col24 <- c(predicts[[231]],predicts[[232]],predicts[[233]],predicts[[234]],predicts[[235]],predicts[[236]],predicts[[237]],predicts[[238]],predicts[[239]],predicts[[240]])
#  opp.cost$col25 <- c(predicts[[241]],predicts[[242]],predicts[[243]],predicts[[244]],predicts[[245]],predicts[[246]],predicts[[247]],predicts[[248]],predicts[[249]],predicts[[250]])
#  opp.cost$col26 <- c(predicts[[251]],predicts[[252]],predicts[[253]],predicts[[254]],predicts[[255]],predicts[[256]],predicts[[257]],predicts[[258]],predicts[[259]],predicts[[260]])
#  opp.cost$col27 <- c(predicts[[261]],predicts[[262]],predicts[[263]],predicts[[264]],predicts[[265]],predicts[[266]],predicts[[267]],predicts[[268]],predicts[[269]],predicts[[270]])
#  opp.cost$col28 <- c(predicts[[271]],predicts[[272]],predicts[[273]],predicts[[274]],predicts[[275]],predicts[[276]],predicts[[277]],predicts[[278]],predicts[[279]],predicts[[280]])
# opp.cost$col29 <- c(predicts[[281]],predicts[[282]],predicts[[283]],predicts[[284]],predicts[[285]],predicts[[286]],predicts[[287]],predicts[[288]],predicts[[289]],predicts[[290]])
  
  
  opp.cost <- opp.cost %>%
    set_names("cc_count", "squr_fkt_bmol", fitted_models$id[1],
              fitted_models$id[2],
              fitted_models$id[3],
              fitted_models$id[4],
              fitted_models$id[5],
              fitted_models$id[6],
              fitted_models$id[7],
              fitted_models$id[8],
              fitted_models$id[9],
              fitted_models$id[10],
              fitted_models$id[11],
              fitted_models$id[12],
              fitted_models$id[13])
          #    fitted_models$id[14])
          #    fitted_models$id[15],
          #    fitted_models$id[16],
          #    fitted_models$id[17],
          #    fitted_models$id[18],
          #    fitted_models$id[19],
          #    fitted_models$id[20],
          #    fitted_models$id[21],
          #    fitted_models$id[22],
          #    fitted_models$id[23],
          #    fitted_models$id[24],
          #    fitted_models$id[25],
          #    fitted_models$id[26],
          #    fitted_models$id[27],
          #    fitted_models$id[28],
          #    fitted_models$id[29])
              
# # # # # # # # # # # # # #               
  # Spline 2, 11 to 20 ccs
  fitted_models2 = c.ag.10_20 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc2 <- data.frame(c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
  names(pred.cc2) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc2$squr_ft_fkt_bmol <- mean(c.ag.10_20$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc2) <- c("11_CCs","12_CCs","13_CCs","14_CCs","15_CCs","16_CCs","17_CCs","18_CCs","19_CCs","20_CCs")
  
  predicts2 <- list()
  for(i in 1:length(fitted_models2[[2]])) {
    predicts2 <- c(predicts2, exp(predict(fitted_models2[[2]][[i]], pred.cc2)))
  }  # vector of predictions
  
  
  # Spline model 2 fit for 11-20 ccs
  # model generates 10 predicts (1cc to 10ccs) for each ID (prod) group
  # put moodel revenue predictions into a table for each product group (29 total)
  opp.cost2 <- pred.cc2
  opp.cost2$col1 <- c(predicts2[[1]],predicts2[[2]],predicts2[[3]],predicts2[[4]],predicts2[[5]],predicts2[[6]],predicts2[[7]],predicts2[[8]],predicts2[[9]],predicts2[[10]])
  opp.cost2$col2 <- c(predicts2[[11]],predicts2[[12]],predicts2[[13]],predicts2[[14]],predicts2[[15]],predicts2[[16]],predicts2[[17]],predicts2[[18]],predicts2[[19]],predicts2[[20]])
  opp.cost2$col3 <- c(predicts2[[21]],predicts2[[22]],predicts2[[23]],predicts2[[24]],predicts2[[25]],predicts2[[26]],predicts2[[27]],predicts2[[28]],predicts2[[29]],predicts2[[30]])
  opp.cost2$col4 <- c(predicts2[[31]],predicts2[[32]],predicts2[[33]],predicts2[[34]],predicts2[[35]],predicts2[[36]],predicts2[[37]],predicts2[[38]],predicts2[[39]],predicts2[[40]])
  opp.cost2$col5 <- c(predicts2[[41]],predicts2[[42]],predicts2[[43]],predicts2[[44]],predicts2[[45]],predicts2[[46]],predicts2[[47]],predicts2[[48]],predicts2[[49]],predicts2[[50]])
  opp.cost2$col6 <- c(predicts2[[51]],predicts2[[52]],predicts2[[53]],predicts2[[54]],predicts2[[55]],predicts2[[56]],predicts2[[57]],predicts2[[58]],predicts2[[59]],predicts2[[60]])
  opp.cost2$col7 <- c(predicts2[[61]],predicts2[[62]],predicts2[[63]],predicts2[[64]],predicts2[[65]],predicts2[[66]],predicts2[[63]],predicts2[[64]],predicts2[[65]],predicts2[[66]])
  opp.cost2$col8 <- c(predicts2[[71]],predicts2[[72]],predicts2[[73]],predicts2[[74]],predicts2[[75]],predicts2[[76]],predicts2[[77]],predicts2[[78]],predicts2[[79]],predicts2[[80]])
  opp.cost2$col9 <- c(predicts2[[81]],predicts2[[82]],predicts2[[83]],predicts2[[84]],predicts2[[85]],predicts2[[86]],predicts2[[87]],predicts2[[88]],predicts2[[89]],predicts2[[90]])
  opp.cost2$col10 <- c(predicts2[[91]],predicts2[[92]],predicts2[[93]],predicts2[[94]],predicts2[[95]],predicts2[[96]],predicts2[[97]],predicts2[[98]],predicts2[[99]],predicts2[[100]])
  opp.cost2$col11 <- c(predicts2[[101]],predicts2[[102]],predicts2[[103]],predicts2[[104]],predicts2[[105]],predicts2[[106]],predicts2[[107]],predicts2[[108]],predicts2[[109]],predicts2[[110]])
  opp.cost2$col12 <- c(predicts2[[111]],predicts2[[112]],predicts2[[113]],predicts2[[114]],predicts2[[115]],predicts2[[116]],predicts2[[117]],predicts2[[118]],predicts2[[119]],predicts2[[120]])
  opp.cost2$col13 <- c(predicts2[[121]],predicts2[[122]],predicts2[[123]],predicts2[[124]],predicts2[[125]],predicts2[[126]],predicts2[[127]],predicts2[[128]],predicts2[[129]],predicts2[[130]])

  
  opp.cost2 <- opp.cost2 %>%
    set_names("cc_count", "squr_fkt_bmol", fitted_models2$id[1],
              fitted_models2$id[2],
              fitted_models2$id[3],
              fitted_models2$id[4],
              fitted_models2$id[5],
              fitted_models2$id[6],
              fitted_models2$id[7],
              fitted_models2$id[8],
              fitted_models2$id[9],
              fitted_models2$id[10],
              fitted_models2$id[11],
              fitted_models2$id[12],
              fitted_models2$id[13])
              

# # # # # # # # # # # # # #        
  # Spline 3, 21 to 30 ccs
  fitted_models3 = c.ag.20_30 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc3 <- data.frame(c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30))
  names(pred.cc3) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc3$squr_ft_fkt_bmol <- mean(c.ag.20_30$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc3) <- c("21_CCs","22_CCs","23_CCs","24_CCs","25_CCs","26_CCs","27_CCs","28_CCs","29_CCs","30_CCs")
  
  predicts3 <- list()
  for(i in 1:length(fitted_models3[[2]])) {
    predicts3 <- c(predicts3, exp(predict(fitted_models3[[2]][[i]], pred.cc3)))
  }  # vector of predictions
  
  
  # Spline model 3 fit for 21-30 ccs
  # model generates 10 predictions (11cc to 30ccs) for each ID (prod) group
  # put moodel revenue predictions into a table for each product group (14 total)
  opp.cost3 <- pred.cc3
  opp.cost3$col1 <- c(predicts3[[1]],predicts3[[2]],predicts3[[3]],predicts3[[4]],predicts3[[5]],predicts3[[6]],predicts3[[7]],predicts3[[8]],predicts3[[9]],predicts3[[10]])
  opp.cost3$col2 <- c(predicts3[[11]],predicts3[[12]],predicts3[[13]],predicts3[[14]],predicts3[[15]],predicts3[[16]],predicts3[[17]],predicts3[[18]],predicts3[[19]],predicts3[[20]])
  opp.cost3$col3 <- c(predicts3[[21]],predicts3[[22]],predicts3[[23]],predicts3[[24]],predicts3[[25]],predicts3[[26]],predicts3[[27]],predicts3[[28]],predicts3[[29]],predicts3[[30]])
  opp.cost3$col4 <- c(predicts3[[31]],predicts3[[32]],predicts3[[33]],predicts3[[34]],predicts3[[35]],predicts3[[36]],predicts3[[37]],predicts3[[38]],predicts3[[39]],predicts3[[40]])
  opp.cost3$col5 <- c(predicts3[[41]],predicts3[[42]],predicts3[[43]],predicts3[[44]],predicts3[[45]],predicts3[[46]],predicts3[[47]],predicts3[[48]],predicts3[[49]],predicts3[[50]])
  opp.cost3$col6 <- c(predicts3[[51]],predicts3[[52]],predicts3[[53]],predicts3[[54]],predicts3[[55]],predicts3[[56]],predicts3[[57]],predicts3[[58]],predicts3[[59]],predicts3[[60]])
  opp.cost3$col7 <- c(predicts3[[61]],predicts3[[62]],predicts3[[63]],predicts3[[64]],predicts3[[65]],predicts3[[66]],predicts3[[63]],predicts3[[64]],predicts3[[65]],predicts3[[66]])
  opp.cost3$col8 <- c(predicts3[[71]],predicts3[[72]],predicts3[[73]],predicts3[[74]],predicts3[[75]],predicts3[[76]],predicts3[[77]],predicts3[[78]],predicts3[[79]],predicts3[[80]])
  opp.cost3$col9 <- c(predicts3[[81]],predicts3[[82]],predicts3[[83]],predicts3[[84]],predicts3[[85]],predicts3[[86]],predicts3[[87]],predicts3[[88]],predicts3[[89]],predicts3[[90]])
  opp.cost3$col10 <- c(predicts3[[91]],predicts3[[92]],predicts3[[93]],predicts3[[94]],predicts3[[95]],predicts3[[96]],predicts3[[97]],predicts3[[98]],predicts3[[99]],predicts3[[100]])
  opp.cost3$col11 <- c(predicts3[[101]],predicts3[[102]],predicts3[[103]],predicts3[[104]],predicts3[[105]],predicts3[[106]],predicts3[[107]],predicts3[[108]],predicts3[[109]],predicts3[[110]])
  opp.cost3$col12 <- c(predicts3[[111]],predicts3[[112]],predicts3[[113]],predicts3[[114]],predicts3[[115]],predicts3[[116]],predicts3[[117]],predicts3[[118]],predicts3[[119]],predicts3[[120]])
  opp.cost3$col13 <- c(predicts3[[121]],predicts3[[122]],predicts3[[123]],predicts3[[124]],predicts3[[125]],predicts3[[126]],predicts3[[127]],predicts3[[128]],predicts3[[129]],predicts3[[130]])
  
  
  opp.cost3 <- opp.cost3 %>%
    set_names("cc_count", "squr_fkt_bmol", fitted_models3$id[1],
              fitted_models3$id[2],
              fitted_models3$id[3],
              fitted_models3$id[4],
              fitted_models3$id[5],
              fitted_models3$id[6],
              fitted_models3$id[7],
              fitted_models3$id[8],
              fitted_models3$id[9],
              fitted_models3$id[10],
              fitted_models3$id[11],
              fitted_models3$id[12],
              fitted_models3$id[13])

  
  # # # # # # # # # # # # # #        
  # Spline 4, 31 to 40 ccs
  fitted_models4 = c.ag.30_40 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc4 <- data.frame(c(31, 32, 33, 34, 35, 36, 37, 38, 39, 40))
  names(pred.cc4) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc4$squr_ft_fkt_bmol <- mean(c.ag.30_40$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc4) <- c("31_CCs","32_CCs","33_CCs","34_CCs","35_CCs","36_CCs","37_CCs","38_CCs","39_CCs","40_CCs")
  
  predicts4 <- list()
  for(i in 1:length(fitted_models4[[2]])) {
    predicts4 <- c(predicts4, exp(predict(fitted_models4[[2]][[i]], pred.cc4)))
  }  # vector of predictions
  
  
  # Spline model 4 fit for 31-40 ccs
  # model generates 10 predictions (11cc to 30ccs) for each ID (prod) group
  # put moodel revenue predictions into a table for each product group (14 total)
  opp.cost4 <- pred.cc4
  opp.cost4$col1 <- c(predicts4[[1]],predicts4[[2]],predicts4[[3]],predicts4[[4]],predicts4[[5]],predicts4[[6]],predicts4[[7]],predicts4[[8]],predicts4[[9]],predicts4[[10]])
  opp.cost4$col2 <- c(predicts4[[11]],predicts4[[12]],predicts4[[13]],predicts4[[14]],predicts4[[15]],predicts4[[16]],predicts4[[17]],predicts4[[18]],predicts4[[19]],predicts4[[20]])
  opp.cost4$col3 <- c(predicts4[[21]],predicts4[[22]],predicts4[[23]],predicts4[[24]],predicts4[[25]],predicts4[[26]],predicts4[[27]],predicts4[[28]],predicts4[[29]],predicts4[[30]])
  opp.cost4$col4 <- c(predicts4[[31]],predicts4[[32]],predicts4[[33]],predicts4[[34]],predicts4[[35]],predicts4[[36]],predicts4[[37]],predicts4[[38]],predicts4[[39]],predicts4[[40]])
  opp.cost4$col5 <- c(predicts4[[41]],predicts4[[42]],predicts4[[43]],predicts4[[44]],predicts4[[45]],predicts4[[46]],predicts4[[47]],predicts4[[48]],predicts4[[49]],predicts4[[50]])
  opp.cost4$col6 <- c(predicts4[[51]],predicts4[[52]],predicts4[[53]],predicts4[[54]],predicts4[[55]],predicts4[[56]],predicts4[[57]],predicts4[[58]],predicts4[[59]],predicts4[[60]])
  opp.cost4$col7 <- c(predicts4[[61]],predicts4[[62]],predicts4[[63]],predicts4[[64]],predicts4[[65]],predicts4[[66]],predicts4[[63]],predicts4[[64]],predicts4[[65]],predicts4[[66]])
  opp.cost4$col8 <- c(predicts4[[71]],predicts4[[72]],predicts4[[73]],predicts4[[74]],predicts4[[75]],predicts4[[76]],predicts4[[77]],predicts4[[78]],predicts4[[79]],predicts4[[80]])
  opp.cost4$col9 <- c(predicts4[[81]],predicts4[[82]],predicts4[[83]],predicts4[[84]],predicts4[[85]],predicts4[[86]],predicts4[[87]],predicts4[[88]],predicts4[[89]],predicts4[[90]])
  opp.cost4$col10 <- c(predicts4[[91]],predicts4[[92]],predicts4[[93]],predicts4[[94]],predicts4[[95]],predicts4[[96]],predicts4[[97]],predicts4[[98]],predicts4[[99]],predicts4[[100]])
  opp.cost4$col11 <- c(predicts4[[101]],predicts4[[102]],predicts4[[103]],predicts4[[104]],predicts4[[105]],predicts4[[106]],predicts4[[107]],predicts4[[108]],predicts4[[109]],predicts4[[110]])
  opp.cost4$col12 <- c(predicts4[[111]],predicts4[[112]],predicts4[[113]],predicts4[[114]],predicts4[[115]],predicts4[[116]],predicts4[[117]],predicts4[[118]],predicts4[[119]],predicts4[[120]])
  opp.cost4$col13 <- c(predicts4[[121]],predicts4[[122]],predicts4[[123]],predicts4[[124]],predicts4[[125]],predicts4[[126]],predicts4[[127]],predicts4[[128]],predicts4[[129]],predicts4[[130]])
  
  
  opp.cost4 <- opp.cost4 %>%
    set_names("cc_count", "squr_fkt_bmol", fitted_models4$id[1],
              fitted_models4$id[2],
              fitted_models4$id[3],
              fitted_models4$id[4],
              fitted_models4$id[5],
              fitted_models4$id[6],
              fitted_models4$id[7],
              fitted_models4$id[8],
              fitted_models4$id[9],
              fitted_models4$id[10],
              fitted_models4$id[11],
              fitted_models4$id[12],
              fitted_models4$id[13])
  
  
  # # # # # # # # # # # # # #        
  # Spline 5, 41 to 50 ccs
  fitted_models5 = c.ag.40_50 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc5 <- data.frame(c(41, 42, 43, 44, 45, 46, 47, 48, 49, 50))
  names(pred.cc5) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc5$squr_ft_fkt_bmol <- mean(c.ag.40_50$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc5) <- c("41_CCs","42_CCs","43_CCs","44_CCs","45_CCs","46_CCs","47_CCs","48_CCs","49_CCs","50_CCs")
  
  predicts5 <- list()
  for(i in 1:length(fitted_models5[[2]])) {
    predicts5 <- c(predicts5, exp(predict(fitted_models5[[2]][[i]], pred.cc5)))
  }  # vector of predictions
  
  
  # Spline model 5 fit for 41-50 ccs
  # model generates 10 predictions (41cc to 50ccs) for each ID (prod) group
  # put model revenue predictions into a table for each product group (13 total)
  opp.cost5 <- pred.cc5
  opp.cost5$col1 <- c(predicts5[[1]],predicts5[[2]],predicts5[[3]],predicts5[[4]],predicts5[[5]],predicts5[[6]],predicts5[[7]],predicts5[[8]],predicts5[[9]],predicts5[[10]])
  opp.cost5$col2 <- c(predicts5[[11]],predicts5[[12]],predicts5[[13]],predicts5[[14]],predicts5[[15]],predicts5[[16]],predicts5[[17]],predicts5[[18]],predicts5[[19]],predicts5[[20]])
  opp.cost5$col3 <- c(predicts5[[21]],predicts5[[22]],predicts5[[23]],predicts5[[24]],predicts5[[25]],predicts5[[26]],predicts5[[27]],predicts5[[28]],predicts5[[29]],predicts5[[30]])
  opp.cost5$col4 <- c(predicts5[[31]],predicts5[[32]],predicts5[[33]],predicts5[[34]],predicts5[[35]],predicts5[[36]],predicts5[[37]],predicts5[[38]],predicts5[[39]],predicts5[[40]])
  opp.cost5$col5 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost5$col6 <- c(predicts5[[51]],predicts5[[52]],predicts5[[53]],predicts5[[54]],predicts5[[55]],predicts5[[56]],predicts5[[57]],predicts5[[58]],predicts5[[59]],predicts5[[60]])
  opp.cost5$col7 <- c(predicts5[[61]],predicts5[[62]],predicts5[[63]],predicts5[[64]],predicts5[[65]],predicts5[[66]],predicts5[[63]],predicts5[[64]],predicts5[[65]],predicts5[[66]])
  opp.cost5$col8 <- c(predicts5[[71]],predicts5[[72]],predicts5[[73]],predicts5[[74]],predicts5[[75]],predicts5[[76]],predicts5[[77]],predicts5[[78]],predicts5[[79]],predicts5[[80]])
  opp.cost5$col9 <- c(predicts5[[81]],predicts5[[82]],predicts5[[83]],predicts5[[84]],predicts5[[85]],predicts5[[86]],predicts5[[87]],predicts5[[88]],predicts5[[89]],predicts5[[90]])
  opp.cost5$col10 <- c(predicts5[[91]],predicts5[[92]],predicts5[[93]],predicts5[[94]],predicts5[[95]],predicts5[[96]],predicts5[[97]],predicts5[[98]],predicts5[[99]],predicts5[[100]])
  opp.cost5$col11 <- c(predicts5[[101]],predicts5[[102]],predicts5[[103]],predicts5[[104]],predicts5[[105]],predicts5[[106]],predicts5[[107]],predicts5[[108]],predicts5[[109]],predicts5[[110]])
  opp.cost5$col12 <- c(predicts5[[111]],predicts5[[112]],predicts5[[113]],predicts5[[114]],predicts5[[115]],predicts5[[116]],predicts5[[117]],predicts5[[118]],predicts5[[119]],predicts5[[120]])
  opp.cost5$col13 <- c(predicts5[[121]],predicts5[[122]],predicts5[[123]],predicts5[[124]],predicts5[[125]],predicts5[[126]],predicts5[[127]],predicts5[[128]],predicts5[[129]],predicts5[[130]])
  
  
  opp.cost5 <- opp.cost5 %>%
    set_names("cc_count", "squr_fkt_bmol", 
              fitted_models5$id[1],
              fitted_models5$id[2],
              fitted_models5$id[3],
              fitted_models5$id[4],
              fitted_models5$id[5],
              fitted_models5$id[6],
              fitted_models5$id[7],
              fitted_models5$id[8],
              fitted_models5$id[9],
              fitted_models5$id[10],
              fitted_models5$id[11],
              fitted_models5$id[12],
              fitted_models5$id[13])
  
#  check <- summarise(group_by(c.ag.40_50, consumer_construct, category),
#                     gross_units = sum(gross_units, na.rm=TRUE),
#                     store_count = length(unique(store_id)))

#  check2 <- summarise(group_by(c.ag.30_40, consumer_construct, category),
#                     gross_units = sum(gross_units, na.rm=TRUE),
#                     store_count = length(unique(store_id)))
  
#  check3 <- summarise(group_by(c.ag.50_60, consumer_construct, category),
#                     gross_units = sum(gross_units, na.rm=TRUE),
#                     store_count = length(unique(store_id)))
  
      
  # MENS ATHLETIC TRAINING (MOD#8) DECREASES WITH CC'S IN THIS RANGE...?
  
   
  # # # # # # # # # # # # # #        
  # Spline 6, 51 to 60 ccs
  fitted_models6 = c.ag.50_60 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc6 <- data.frame(c(51, 52, 53, 54, 55, 56, 57, 58, 59, 60))
  names(pred.cc6) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc6$squr_ft_fkt_bmol <- mean(c.ag.50_60$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc6) <- c("51_CCs","52_CCs","53_CCs","54_CCs","55_CCs","56_CCs","57_CCs","58_CCs","59_CCs","60_CCs")
  
  predicts6 <- list()
  for(i in 1:length(fitted_models6[[2]])) {
    predicts6 <- c(predicts6, exp(predict(fitted_models6[[2]][[i]], pred.cc6)))
  }  # vector of predictions
  
  
  # Spline model 6 fit for 51-60 ccs
  # model generates 10 predictions (51cc to 60ccs) for each ID (prod) group
  # put model revenue predictions into a table for each product group (13 total)
  opp.cost6 <- pred.cc6
  opp.cost6$col1 <- c(predicts6[[1]],predicts6[[2]],predicts6[[3]],predicts6[[4]],predicts6[[5]],predicts6[[6]],predicts6[[7]],predicts6[[8]],predicts6[[9]],predicts6[[10]])
  opp.cost6$col2 <- c(predicts6[[11]],predicts6[[12]],predicts6[[13]],predicts6[[14]],predicts6[[15]],predicts6[[16]],predicts6[[17]],predicts6[[18]],predicts6[[19]],predicts6[[20]])
  opp.cost6$col3 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost6$col4 <- c(predicts5[[21]],predicts5[[22]],predicts5[[23]],predicts5[[24]],predicts5[[25]],predicts5[[26]],predicts5[[27]],predicts5[[28]],predicts5[[29]],predicts5[[30]])
  opp.cost6$col5 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost6$col6 <- c(predicts6[[31]],predicts6[[32]],predicts6[[33]],predicts6[[34]],predicts6[[35]],predicts6[[36]],predicts6[[37]],predicts6[[38]],predicts6[[39]],predicts6[[40]])
  opp.cost6$col7 <- c(predicts6[[41]],predicts6[[42]],predicts6[[43]],predicts6[[44]],predicts6[[45]],predicts6[[46]],predicts6[[47]],predicts6[[48]],predicts6[[49]],predicts6[[50]])
  opp.cost6$col8 <- c(predicts6[[51]],predicts6[[52]],predicts6[[53]],predicts6[[54]],predicts6[[55]],predicts6[[56]],predicts6[[57]],predicts6[[58]],predicts6[[59]],predicts6[[60]])
  opp.cost6$col9 <- c(predicts6[[61]],predicts6[[62]],predicts6[[63]],predicts6[[64]],predicts6[[65]],predicts6[[66]],predicts6[[63]],predicts6[[64]],predicts6[[65]],predicts6[[66]])
  opp.cost6$col10 <- c(predicts6[[71]],predicts6[[72]],predicts6[[73]],predicts6[[74]],predicts6[[75]],predicts6[[76]],predicts6[[77]],predicts6[[78]],predicts6[[79]],predicts6[[80]])
  opp.cost6$col11 <- c(predicts6[[81]],predicts6[[82]],predicts6[[83]],predicts6[[84]],predicts6[[85]],predicts6[[86]],predicts6[[87]],predicts6[[88]],predicts6[[89]],predicts6[[90]])
  opp.cost6$col12 <- c(predicts6[[91]],predicts6[[92]],predicts6[[93]],predicts6[[94]],predicts6[[95]],predicts6[[96]],predicts6[[97]],predicts6[[98]],predicts6[[99]],predicts6[[100]])
  opp.cost6$col13 <- c(predicts6[[101]],predicts6[[102]],predicts6[[103]],predicts6[[104]],predicts6[[105]],predicts6[[106]],predicts6[[107]],predicts6[[108]],predicts6[[109]],predicts6[[110]])
  
  
  opp.cost6 <- opp.cost6 %>%
    set_names("cc_count", 
              "squr_fkt_bmol", 
              fitted_models6$id[1],
              fitted_models6$id[2],
              "NIKE RISE.Footwear.KIDS.PERFORMANCE.Global Football",
              fitted_models6$id[3],
              "NIKE RISE.Footwear.MENS.LIFESTYLE.NikeSB",
              fitted_models6$id[4],
              fitted_models6$id[5],
              fitted_models6$id[6],
              fitted_models6$id[7],
              fitted_models6$id[8],
              fitted_models6$id[9],
              fitted_models6$id[10],
              fitted_models6$id[11])
              
  
  # # # # # # # # # # # # # #        
  # Spline 7, 61 to 70 ccs
  fitted_models7 = c.ag.60_70 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc7 <- data.frame(c(61, 62, 63, 64, 65, 66, 67, 68, 69, 70))
  names(pred.cc7) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc7$squr_ft_fkt_bmol <- mean(c.ag.60_70$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc7) <- c("61_CCs","62_CCs","63_CCs","64_CCs","65_CCs","66_CCs","67_CCs","68_CCs","69_CCs","70_CCs")
  
  predicts7 <- list()
  for(i in 1:length(fitted_models7[[2]])) {
    predicts7 <- c(predicts7, exp(predict(fitted_models7[[2]][[i]], pred.cc7)))
  }  # vector of predictions
  
  
  # Spline model 7 fit for 61-70 ccs
  # model generates 10 predictions (61cc to 70ccs) for each ID (prod) group
  # put model revenue predictions into a table for each product group (13 total)
  opp.cost7 <- pred.cc7
  opp.cost7$col1 <- c(predicts7[[1]],predicts7[[2]],predicts7[[3]],predicts7[[4]],predicts7[[5]],predicts7[[6]],predicts7[[7]],predicts7[[8]],predicts7[[9]],predicts7[[10]])
  opp.cost7$col2 <- c(predicts7[[11]],predicts7[[12]],predicts7[[13]],predicts7[[14]],predicts7[[15]],predicts7[[16]],predicts7[[17]],predicts7[[18]],predicts7[[19]],predicts7[[20]])
  opp.cost7$col3 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost7$col4 <- c(predicts7[[21]],predicts7[[22]],predicts7[[23]],predicts7[[24]],predicts7[[25]],predicts7[[26]],predicts7[[27]],predicts7[[28]],predicts7[[29]],predicts7[[30]])
  opp.cost7$col5 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost7$col6 <- c(predicts7[[31]],predicts7[[32]],predicts7[[33]],predicts7[[34]],predicts7[[35]],predicts7[[36]],predicts7[[37]],predicts7[[38]],predicts7[[39]],predicts7[[40]])
  opp.cost7$col7 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost7$col8 <- c(predicts7[[41]],predicts7[[42]],predicts7[[43]],predicts7[[44]],predicts7[[45]],predicts7[[46]],predicts7[[47]],predicts7[[48]],predicts7[[49]],predicts7[[50]])
  opp.cost7$col9 <- c(predicts7[[51]],predicts7[[52]],predicts7[[53]],predicts7[[54]],predicts7[[55]],predicts7[[56]],predicts7[[57]],predicts7[[58]],predicts7[[59]],predicts7[[60]])
  opp.cost7$col10 <- c(predicts7[[61]],predicts7[[62]],predicts7[[63]],predicts7[[64]],predicts7[[65]],predicts7[[66]],predicts7[[63]],predicts7[[64]],predicts7[[65]],predicts7[[66]])
  opp.cost7$col11 <- c(predicts7[[71]],predicts7[[72]],predicts7[[73]],predicts7[[74]],predicts7[[75]],predicts7[[76]],predicts7[[77]],predicts7[[78]],predicts7[[79]],predicts7[[80]])
  opp.cost7$col12 <- c(predicts7[[81]],predicts7[[82]],predicts7[[83]],predicts7[[84]],predicts7[[85]],predicts7[[86]],predicts7[[87]],predicts7[[88]],predicts7[[89]],predicts7[[90]])
  opp.cost7$col13 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  
  
  opp.cost7 <- opp.cost7 %>%
    set_names("cc_count",
              "squr_fkt_bmol", 
              fitted_models7$id[1],
              fitted_models7$id[2],
              "NIKE RISE.Footwear.KIDS.PERFORMANCE.Global Football",
              fitted_models7$id[3],
              "NIKE RISE.Footwear.MENS.LIFESTYLE.NikeSB",
              fitted_models7$id[4],
              "NIKE RISE.Footwear.MENS.PERFORMANCE.Athletic Training",
              fitted_models7$id[5],
              fitted_models7$id[6],
              fitted_models7$id[7],
              fitted_models7$id[8],
              fitted_models7$id[9],
              fitted_models7$id[10])
              
  
  # # # # # # # # # # # # # #        
  # Spline 8, 71 to 80 ccs
  fitted_models8 = c.ag.70_80 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc8 <- data.frame(c(71, 72, 73, 74, 75, 76, 77, 78, 79, 80))
  names(pred.cc8) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc8$squr_ft_fkt_bmol <- mean(c.ag.70_80$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc8) <- c("71_CCs","72_CCs","73_CCs","74_CCs","75_CCs","76_CCs","77_CCs","78_CCs","79_CCs","80_CCs")
  
  predicts8 <- list()
  for(i in 1:length(fitted_models8[[2]])) {
    predicts8 <- c(predicts8, exp(predict(fitted_models8[[2]][[i]], pred.cc8)))
  }  # vector of predictions
  
  
  # Spline model 8 fit for 71-80 ccs
  # model generates 10 predictions (71cc to 80ccs) for each ID (prod) group
  # put model revenue predictions into a table for each product group (13 total)
  opp.cost8 <- pred.cc8
  opp.cost8$col1 <- c(predicts8[[1]],predicts8[[2]],predicts8[[3]],predicts8[[4]],predicts8[[5]],predicts8[[6]],predicts8[[7]],predicts8[[8]],predicts8[[9]],predicts8[[10]])
  opp.cost8$col2 <- c(predicts8[[11]],predicts8[[12]],predicts8[[13]],predicts8[[14]],predicts8[[15]],predicts8[[16]],predicts8[[17]],predicts8[[18]],predicts8[[19]],predicts8[[20]])
  opp.cost8$col3 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost8$col4 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost8$col5 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost8$col6 <- c(predicts8[[31]],predicts8[[32]],predicts8[[33]],predicts8[[34]],predicts8[[35]],predicts8[[36]],predicts8[[37]],predicts8[[38]],predicts8[[39]],predicts8[[40]])
  opp.cost8$col7 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost8$col8 <- c(predicts8[[41]],predicts8[[42]],predicts8[[43]],predicts8[[44]],predicts8[[45]],predicts8[[46]],predicts8[[47]],predicts8[[48]],predicts8[[49]],predicts8[[50]])
  opp.cost8$col9 <- c(predicts8[[51]],predicts8[[52]],predicts8[[53]],predicts8[[54]],predicts8[[55]],predicts8[[56]],predicts8[[57]],predicts8[[58]],predicts8[[59]],predicts8[[60]])
  opp.cost8$col10 <- c(predicts8[[61]],predicts8[[62]],predicts8[[63]],predicts8[[64]],predicts8[[65]],predicts8[[66]],predicts8[[63]],predicts8[[64]],predicts8[[65]],predicts8[[66]])
  opp.cost8$col11 <- c(predicts8[[71]],predicts8[[72]],predicts8[[73]],predicts8[[74]],predicts8[[75]],predicts8[[76]],predicts8[[77]],predicts8[[78]],predicts8[[79]],predicts8[[80]])
  opp.cost8$col12 <- c(predicts8[[81]],predicts8[[82]],predicts8[[83]],predicts8[[84]],predicts8[[85]],predicts8[[86]],predicts8[[87]],predicts8[[88]],predicts8[[89]],predicts8[[90]])
  opp.cost8$col13 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

  
  opp.cost8 <- opp.cost8 %>%
    set_names("cc_count",
              "squr_fkt_bmol", 
              fitted_models9$id[1],
              fitted_models9$id[2],
              "NIKE RISE.Footwear.KIDS.PERFORMANCE.Global Football",
              fitted_models9$id[3],
              "NIKE RISE.Footwear.MENS.LIFESTYLE.NikeSB",
              fitted_models9$id[4],
              "NIKE RISE.Footwear.MENS.PERFORMANCE.Athletic Training",
              fitted_models9$id[5],
              fitted_models9$id[6],
              fitted_models9$id[7],
              fitted_models9$id[8],
              fitted_models9$id[9],
              "NIKE RISE.Footwear.WOMENS.PERFORMANCE.Womens Training")
  
  
  # # # # # # # # # # # # # #        
  # Spline 9, 81 to 90 ccs
  fitted_models9 = c.ag.80_90 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc9 <- data.frame(c(81, 82, 83, 84, 85, 86, 87, 88, 89, 90))
  names(pred.cc9) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc9$squr_ft_fkt_bmol <- mean(c.ag.80_90$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc9) <- c("81_CCs","82_CCs","83_CCs","84_CCs","85_CCs","86_CCs","87_CCs","88_CCs","89_CCs","90_CCs")
  
  predicts9 <- list()
  for(i in 1:length(fitted_models9[[2]])) {
    predicts9 <- c(predicts9, exp(predict(fitted_models9[[2]][[i]], pred.cc9)))
  }  # vector of predictions
  
  
  # Spline model 9 fit for 81-90 ccs
  # model generates 10 predictions (81cc to 90ccs) for each ID (prod) group
  # put model revenue predictions into a table for each product group (13 total)
  opp.cost9 <- pred.cc9
  opp.cost9$col1 <- c(predicts9[[1]],predicts9[[2]],predicts9[[3]],predicts9[[4]],predicts9[[5]],predicts9[[6]],predicts9[[7]],predicts9[[8]],predicts9[[9]],predicts9[[10]])
  opp.cost9$col2 <- c(predicts9[[11]],predicts9[[12]],predicts9[[13]],predicts9[[14]],predicts9[[15]],predicts9[[16]],predicts9[[17]],predicts9[[18]],predicts9[[19]],predicts9[[20]])
  opp.cost9$col3 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost9$col4 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost9$col5 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost9$col6 <- c(predicts9[[31]],predicts9[[32]],predicts9[[33]],predicts9[[34]],predicts9[[35]],predicts9[[36]],predicts9[[37]],predicts9[[38]],predicts9[[39]],predicts9[[40]])
  opp.cost9$col7 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost9$col8 <- c(predicts9[[41]],predicts9[[42]],predicts9[[43]],predicts9[[44]],predicts9[[45]],predicts9[[46]],predicts9[[47]],predicts9[[48]],predicts9[[49]],predicts9[[50]])
  opp.cost9$col9 <- c(predicts9[[51]],predicts9[[52]],predicts9[[53]],predicts9[[54]],predicts9[[55]],predicts9[[56]],predicts9[[57]],predicts9[[58]],predicts9[[59]],predicts9[[60]])
  opp.cost9$col10 <- c(predicts9[[61]],predicts9[[62]],predicts9[[63]],predicts9[[64]],predicts9[[65]],predicts9[[66]],predicts9[[63]],predicts9[[64]],predicts9[[65]],predicts9[[66]])
  opp.cost9$col11 <- c(predicts9[[71]],predicts9[[72]],predicts9[[73]],predicts9[[74]],predicts9[[75]],predicts9[[76]],predicts9[[77]],predicts9[[78]],predicts9[[79]],predicts9[[80]])
  opp.cost9$col12 <- c(predicts9[[81]],predicts9[[82]],predicts9[[83]],predicts9[[84]],predicts9[[85]],predicts9[[86]],predicts9[[87]],predicts9[[88]],predicts9[[89]],predicts9[[90]])
  opp.cost9$col13 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  
  
  opp.cost9 <- opp.cost9 %>%
    set_names("cc_count",
              "squr_fkt_bmol", 
              fitted_models9$id[1],
              fitted_models9$id[2],
              "NIKE RISE.Footwear.KIDS.PERFORMANCE.Global Football",
              fitted_models9$id[3],
              "NIKE RISE.Footwear.MENS.LIFESTYLE.NikeSB",
              fitted_models9$id[4],
              "NIKE RISE.Footwear.MENS.PERFORMANCE.Athletic Training",
              fitted_models9$id[5],
              fitted_models9$id[6],
              fitted_models9$id[7],
              fitted_models9$id[8],
              fitted_models9$id[9],
              "NIKE RISE.Footwear.WOMENS.PERFORMANCE.Womens Training")
  
  # # # # # # # # # # # # # #        
  # Spline 10, 91 to 100 ccs
  fitted_models10 = c.ag.90_100 %>% 
    group_by(id) %>% 
    do(model = lm(log(gross_revenue)~log(cc_count) + log(squr_ft_fkt_bmol), data = .))
  
  #   
  pred.cc10 <- data.frame(c(91, 92, 93, 94, 95, 96, 97, 98, 99, 100))
  names(pred.cc10) <- "cc_count"
  
  
  # log square fot (predictive independent var)
  #  pred.cc$squr_ft_fkt_bmol <- log(mean(c.ag.less60$squr_ft_fkt_bmol))
  pred.cc10$squr_ft_fkt_bmol <- mean(c.ag.90_100$squr_ft_fkt_bmol)
  
  # name rows
  rownames(pred.cc10) <- c("91_CCs","92_CCs","93_CCs","94_CCs","95_CCs","96_CCs","97_CCs","98_CCs","99_CCs","100_CCs")
  
  predicts10 <- list()
  for(i in 1:length(fitted_models10[[2]])) {
    predicts10 <- c(predicts10, exp(predict(fitted_models10[[2]][[i]], pred.cc10)))
  }  # vector of predictions
  
  
  # Spline model 10 fit for 91-100 ccs
  # model generates 10 predictions (91cc to 100ccs) for each ID (prod) group
  # put model revenue predictions into a table for each product group (13 total)
  opp.cost10 <- pred.cc10
  opp.cost10$col1 <- c(predicts10[[1]],predicts10[[2]],predicts10[[3]],predicts10[[4]],predicts10[[5]],predicts10[[6]],predicts10[[7]],predicts10[[8]],predicts10[[9]],predicts10[[10]])
  opp.cost10$col2 <- c(predicts10[[11]],predicts10[[12]],predicts10[[13]],predicts10[[14]],predicts10[[15]],predicts10[[16]],predicts10[[17]],predicts10[[18]],predicts10[[19]],predicts10[[20]])
  opp.cost10$col3 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost10$col4 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost10$col5 <- c(predicts10[[21]],predicts10[[22]],predicts10[[23]],predicts10[[24]],predicts10[[25]],predicts10[[26]],predicts10[[27]],predicts10[[28]],predicts10[[29]],predicts10[[30]])
  opp.cost10$col6 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost10$col7 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost10$col8 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  opp.cost10$col9 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA) 
  opp.cost10$col10 <- c(predicts10[[51]],predicts10[[52]],predicts10[[53]],predicts10[[54]],predicts10[[55]],predicts10[[56]],predicts10[[57]],predicts10[[58]],predicts10[[59]],predicts10[[60]]) 
  opp.cost10$col11 <- c(predicts10[[61]],predicts10[[62]],predicts10[[63]],predicts10[[64]],predicts10[[65]],predicts10[[66]],predicts10[[67]],predicts10[[68]],predicts10[[69]],predicts10[[70]])
  opp.cost10$col12 <- c(predicts10[[71]],predicts10[[72]],predicts10[[73]],predicts10[[74]],predicts10[[75]],predicts10[[76]],predicts10[[77]],predicts10[[78]],predicts10[[79]],predicts10[[80]])
  opp.cost10$col13 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  
  
  opp.cost10 <- opp.cost10 %>%
    set_names("cc_count",
              "squr_fkt_bmol", 
              fitted_models10$id[1],
              fitted_models10$id[2],
              "NIKE RISE.Footwear.KIDS.PERFORMANCE.Young Athlete",
              "NIKE RISE.Footwear.KIDS.PERFORMANCE.Global Football",
              fitted_models10$id[3],
              "NIKE RISE.Footwear.MENS.LIFESTYLE.NikeSB",
              fitted_models10$id[4],
              "NIKE RISE.Footwear.MENS.PERFORMANCE.Athletic Training",
              fitted_models10$id[5],
              fitted_models10$id[6],
              fitted_models10$id[7],
              fitted_models10$id[8],
              "NIKE RISE.Footwear.WOMENS.PERFORMANCE.Womens Training")
  
      
  opp.cost.all <- rbind(opp.cost, opp.cost2, opp.cost3, opp.cost4, opp.cost5, opp.cost6, opp.cost7, opp.cost8, opp.cost9, opp.cost10)
  
  # round all numbers in the data frame  
  opp.cost.all <- opp.cost.all %>%
    mutate_if(is.numeric, round)
  
#   write_xlsx(opp.cost.all, "/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/cc_scenario_tables/opp_cost_rise_fw_bycat.xlsx")

   # SUBTRACT THE VALUE FROM THE 25TH CC
   col25 <- opp.cost.all - opp.cost.all[25,][col(opp.cost.all[])]
   #  colMeans(opp.cost.all[], na.rm=TRUE)[col(opp.cost.all[])]
   col25 <- colavg %>%
        mutate_if(is.numeric, round)
   
   
   
   # SADLY THIS DOESN'T WORK  
  # SUBTRACT THE COLUMN MEANS FROM EACH VALUE
#   colavg <- opp.cost.all - colMeans(opp.cost.all[], na.rm=TRUE)[col(opp.cost.all[])]
#   colavg <- colavg %>%
#     mutate_if(is.numeric, round)
   
#   write_xlsx(colavg, "/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/cc_scenario_tables/opp_cost_difavg_rise_fw_bycat.xlsx")
   
    
#  fitted_models9$id[3]
#  [1] "NIKE RISE.Footwear.KIDS.PERFORMANCE.Young Athlete"
#  >      fitted_models9$id[5]
#  [1] "NIKE RISE.Footwear.MENS.PERFORMANCE.Basketball"
  
              
              
              
              
              
              
              
              

#  write_xlsx(look, "/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/cc_scenario_tables/all.xlsx")
#  write_xlsx(see, "/Users/kwhi14/Library/CloudStorage/Box-Box/Nike Direct Analytics/NSTAR (Nike Stores Analytics and Research)/03. Direct Stores Analytics (DSA)/1. Projects/Assortment science/cc_scenario_tables/all.ag.xlsx")
  
    # how to input to Shiny with adjustable knobs  
  
  