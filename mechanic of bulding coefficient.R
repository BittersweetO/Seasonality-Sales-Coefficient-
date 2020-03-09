library('RODBC')
library('dplyr')
library('lubridate')
library('purrr')
library('tidyr')
library('data.table')
library('ggplot2')

t <- Sys.Date()

SQLcon12<-odbcDriverConnect('driver={SQL Server}; server=s-kv-center-s12; database=ForeCastR')
SQLcon64<-odbcDriverConnect('driver={SQL Server}; server=s-kv-center-s64; database=SILPOAnalitic') 

SalesSQL <- sqlQuery(SQLcon12, paste("SELECT   [date], lagerID, kolvo, cntFils
                                      FROM [ForeCastR].[fs].[tab_view_filteredSales] (nolock)
                                      WHERE [date] >= '2017-01-01' AND [date] < '",t,"' AND BizID = 1 
                                      ORDER BY [date]"))
PromoData <- sqlQuery(SQLcon64 ,     "SELECT a.[activityId], [dateFrom], [dateTo], ll.lagerId
                                      FROM [CB].[dbo].[Activities] a with (nolock)
                                      JOIN [CB].[DBO].[LinkActivityLager] ll with (nolock)  ON a.activityId = ll.activityId
                                      WHERE a.status <> 'Refused' AND a.businessId = 1 -- Silpo = 1, Trash = 8.
                                      ORDER BY activityId")
Classifier <- sqlQuery(SQLcon64, "SELECT LagerID, ClassifierID, ClassifierName, LinkCommodityGroupLagerSapClassifier.commodityGroupId, ListAssortmentCommodityGroups.commodityGroupName
                  								    FROM [SILPOAnalitic].[dbo].[UL_data_Lagers] with (nolock)
                  								    JOIN [MasterData].[dbo].[LinkCommodityGroupLagerSapClassifier] with (nolock) on ClassifierID = lagerSapClassifierId
                  								    JOIN [MasterData].[dbo].[ListAssortmentCommodityGroups] with(nolock) on LinkCommodityGroupLagerSapClassifier.commodityGroupId =  ListAssortmentCommodityGroups.commodityGroupId
                  								    WHERE disableRow = 0
                  								    ORDER by LagerID")
odbcClose(SQLcon12)

odbcClose(SQLcon64)
`%nin%` <- Negate(`%in%`)



Sales <- SalesSQL %>%
  mutate(Date = as.Date(date)) %>%
  select(-date) %>%
  filter( lagerID %nin% unique( SalesSQL$lagerID[SalesSQL$kolvo < 0]), lagerID %nin% unique(PromoData$lagerId[PromoData$CountDays > 150])) %>%
  mutate(Fils = replace(cntFils, cntFils == 0, 1)) %>%
  mutate(Quant = kolvo/Fils) %>%
  select(-kolvo, -cntFils, -Fils)

Promo <- PromoData %>%
  mutate( From = as.Date(dateFrom), To = as.Date(dateTo)) %>%
  select(-dateFrom, - dateTo ) %>%
  filter( year(From) > 2016 ) %>%
  mutate( Days =  difftime(To, From, units = 'days') + 1 )

Promo <-  do.call(rbind, with(Promo, lapply(1:nrow(Promo), function(i)
  data.frame(lagerId = Promo$lagerId[i], 
             date = seq(as.Date(Promo$From[i]), as.Date(Promo$From[i]) + Promo$Days[i], by = "day")
  ))))


Classifier <- Classifier[Classifier$commodityGroupId %nin% Classifier$commodityGroupId[grep("Ne_", Classifier$commodityGroupName)],]

Class <-  unique(Classifier$commodityGroupId)
EKT <- unique(Classifier$ClassifierID)

Seasons_by_year_Class <- function(YR){  
  y <- YR
  WPromo <- Promo %>%
    filter(year(date) > y - 1, year(date) < y + 1) %>%
    group_by(lagerId, Week = isoweek(date)) %>%
    distinct(lagerId, Week)
  WPromo$Ind <- 1
  
  WSales <- Sales %>%
    filter(year(Sales$Date) < y + 1, year(Sales$Date) > y - 1) %>% 
    group_by(lagerID, Week = isoweek(Date)) %>%
    summarize(S = sum(Quant)) %>%
    filter(S > 0)
  
  PNA <- left_join(WSales, WPromo, by = c("lagerID" = "lagerId", "Week" = "Week"))
  PNA$S[PNA$Ind == 1] <- NA
  
  PNA <- left_join(PNA, Classifier, by = c("lagerID" = "LagerID"))  %>%
    select(lagerID,Week,Ind,S, commodityGroupId)
  
  df <- data.frame(Week = 1:52)
  for(i in 1:length(Class)) {
    GBY <- PNA%>%
      filter(commodityGroupId == Class[i])%>%
      select(lagerID, Week , S)%>%
      spread(key = lagerID, value = S)
    if(nrow(GBY) == 0) next
    GBY <- GBY[lapply(GBY, function(x) sum(is.na(x))/length(x)) < 0.7]
    if(any(is.na(GBY))  == T){
      if((ncol(GBY) < 150)==T){
        LWP <- colnames(GBY)[colSums(is.na(GBY)) > 0]
        LWOP <- colnames(GBY)[colSums(is.na(GBY)) == 0]
        LWOP <- LWOP[-1]
        GBY_WPR <- GBY[,LWP]
        GBY_WOPR <- GBY[,LWOP]
        KOR <- cor(GBY_WPR, GBY_WOPR, use = 'pairwise.complete.obs')
        for (j in 1:length(LWP)) {
          cormax <- max(KOR[rownames(KOR) == LWP[j]])
          if(is.na(cormax) == T) next 
          tofit <- colnames(KOR)[KOR[LWP[j],]== cormax]
          m <- GBY_WPR[,LWP[j]]
          n <- GBY_WOPR[,tofit]
          mndf <- cbind(m,n)
          lnames <- colnames(mndf)
          colnames(mndf) <- c("x", "y")
          lmcoef <- lm(x ~ y, data = mndf)
          inter<-lmcoef[["coefficients"]][["(Intercept)"]]
          slope<-lmcoef[["coefficients"]][["y"]]
          for (k in 1:nrow(m)){
            if(is.na(m[k,]) == T){
              m[k,] <- inter + slope*n[k,]
            }
          }
          GBY[colnames(GBY) == colnames(m)] <- m
          
        }
      }
    }
    GBYK <- sweep(GBY[,-1],2, colMeans(GBY[,-1], na.rm = T),'/')
    if(nrow(GBYK) < 51 && y != year(t)) next
    if(y == year(t) && nrow(GBYK) < week(t) - 1) next
    ClassKoef <- rowMeans(GBYK , na.rm = T)
    ClassKoef <- data.frame(ClassKoef)
    colnames(ClassKoef)[1] <- "CLSF"
    ClassKoef$Week <- GBY$Week
    p <- ggplot(ClassKoef,mapping =  aes(x = Week, y = CLSF)) + stat_smooth(span = 0.18, n = nrow(ClassKoef), method = 'loess')
    build <- ggplot_build(p)
    SMKOEF <- build$data[[1]]$y
    SMKOEF <- as.data.frame(SMKOEF)
    colnames(SMKOEF)[1] <- "CLSF"
    SMKOEF$Week <- 1:nrow(SMKOEF)
    colnames(SMKOEF)[1] <- Class[i]
    df  <- merge(df, SMKOEF , by = c("Week"), all = T)
  }
  assign(paste("SEASONS",y, sep = ""), df, envir = .GlobalEnv)
}

Seasons_by_year_Class(2017)
Seasons_by_year_Class(2018)
Seasons_by_year_Class(2019)



Seasons_by_year_EKT <- function(YR){  
  y <- YR
  WPromo <- Promo %>%
    filter(year(date) > y - 1, year(date) < y + 1) %>%
    group_by(lagerId, Week = isoweek(date)) %>%
    distinct(lagerId, Week)
  WPromo$Ind <- 1
  
  WSales <- Sales %>%
    filter(year(Sales$Date) < y + 1, year(Sales$Date) > y - 1) %>% 
    group_by(lagerID, Week = isoweek(Date)) %>%
    summarize(S = sum(Quant)) %>%
    filter(S > 0)
  
  PNA <- left_join(WSales, WPromo, by = c("lagerID" = "lagerId", "Week" = "Week"))
  PNA$S[PNA$Ind == 1] <- NA
  
  Sales_Classifier <- left_join(PNA, Classifier, by = c("lagerID" = "LagerID"))  %>%
    select(lagerID,Week,Ind,S, ClassifierID)
  
  df <- data.frame(Week = 1:52)
  for(i in 1:length(EKT)) {
    GBY <- Sales_Classifier%>%
      filter(ClassifierID == EKT[i])%>%
      select(lagerID, Week , S)%>%
      spread(key = lagerID, value = S)
    if(nrow(GBY) == 0) next
    GBY <- GBY[lapply(GBY, function(x) sum(is.na(x))/length(x)) < 0.7]
    if(any(is.na(GBY))  == T){
      if((ncol(GBY) < 150)==T){
        LWP <- colnames(GBY)[colSums(is.na(GBY)) > 0]
        LWOP <- colnames(GBY)[colSums(is.na(GBY)) == 0]
        LWOP <- LWOP[-1]
        GBY_WPR <- GBY[,LWP]
        GBY_WOPR <- GBY[,LWOP]
        KOR <- cor(GBY_WPR, GBY_WOPR, use = 'pairwise.complete.obs')
        for (j in 1:length(LWP)) {
          cormax <- max(KOR[rownames(KOR) == LWP[j]])
          if(is.na(cormax) == T) next 
          tofit <- colnames(KOR)[KOR[LWP[j],]== cormax]
          m <- GBY_WPR[,LWP[j]]
          n <- GBY_WOPR[,tofit]
          mndf <- cbind(m,n)
          lnames <- colnames(mndf)
          colnames(mndf) <- c("x", "y")
          lmcoef <- lm(x ~ y, data = mndf)
          inter<-lmcoef[["coefficients"]][["(Intercept)"]]
          slope<-lmcoef[["coefficients"]][["y"]]
          for (k in 1:nrow(m)){
            if(is.na(m[k,]) == T){
              m[k,] <- inter + slope*n[k,]
            }
          }
          GBY[colnames(GBY) == colnames(m)] <- m
          
        }
      }
    }
    GBYK <- sweep(GBY[,-1],2, colMeans(GBY[,-1], na.rm = T),'/')
    if(nrow(GBYK) < 51 && y != year(t)) next
    if(y == year(t) && nrow(GBYK) < week(t) - 1) next
    ClassKoef <- rowMeans(GBYK , na.rm = T)
    ClassKoef <- data.frame(ClassKoef)
    colnames(ClassKoef)[1] <- "CLSF"
    ClassKoef$Week <- GBY$Week
    p <- ggplot(ClassKoef,mapping =  aes(x = Week, y = CLSF)) + stat_smooth(span = 0.18, n = nrow(ClassKoef), method = 'loess')
    build <- ggplot_build(p)
    SMKOEF <- build$data[[1]]$y
    SMKOEF <- as.data.frame(SMKOEF)
    colnames(SMKOEF)[1] <- "CLSF"
    SMKOEF$Week <- 1:nrow(SMKOEF)
    colnames(SMKOEF)[1] <- EKT[i]
    df  <- merge(df, SMKOEF , by = c("Week"), all = T)
  }
  assign(paste("SEASONS_EKT",y, sep = ""), df, envir = .GlobalEnv)
}
Seasons_by_year_EKT(2017)
Seasons_by_year_EKT(2018)
Seasons_by_year_EKT(2019)



