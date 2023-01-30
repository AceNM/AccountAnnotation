#===============================================================================
#loading needed libraries 
library(tidyverse)
library(ggpubr)
library(moments)
library(utils)
library(readr)
library(ez)
library(ARTool)
library(readr)
library(dplyr)
library(xlsx)
library(writexl)
library(rstatix)
library(varhandle)
library(dunn.test)
library(FSA)
#===============================================================================
#loading data

RawSurveyData <- read_csv("RawSurveyData.csv")
RawSurveyData 

Demographics <- RawSurveyData[ , 14:22 ]
Demographics$RespondentID  <- RawSurveyData$RespondentID
#Demographics <- Demographics[!apply(is.na(Demographics), 1, all),]
Demographics <- Demographics[!(is.na(Demographics$Age) | Demographics$Age==""), ]
Demographics$PV <- ifelse(Demographics$PV=='Completely Conservative',"-2", ifelse(Demographics$PV=='Moderate Conservative', "-1", ifelse(Demographics$PV=='Neither Liberal nor Conservative', "0", ifelse(Demographics$PV=='Moderate Liberal', "1", "2"))))
Demographics$PV  <- as.numeric(Demographics$PV)
Demographics$RespondentID  <- as_factor(Demographics$RespondentID)
Demographics
write.table(Demographics , sep=",", file = "Demographics.csv")

SL<- RawSurveyData[ , 12:13 ]
SL$RespondentID  <- RawSurveyData$RespondentID
SL <- SL[!(is.na(SL$ATSat) | SL$ATSat==""), ]
SL$RespondentID  <- as_factor(SL$RespondentID)
#SL <- SL[!apply(is.na(SL), 1, all),]
SL

SurveyData <- RawSurveyData[ , 1:10 ]
SurveyData
write.table(SurveyData , sep=",", file = "SurveyData.csv")
#replacing Character values with numerical ones
SurveyData$AccountTrustPre <- ifelse(SurveyData$AccountTrustPre=='Complete Distrust', 1, ifelse(SurveyData$AccountTrustPre=='Moderately Distrust', 2, ifelse(SurveyData$AccountTrustPre=='Neither trust nor distrust', 3, ifelse(SurveyData$AccountTrustPre=='Moderately trust', 4, 5))))
SurveyData$AccountTrustPost <- ifelse(SurveyData$AccountTrustPost=='Complete Distrust', 1, ifelse(SurveyData$AccountTrustPost=='Moderately Distrust', 2, ifelse(SurveyData$AccountTrustPost=='Neither trust nor distrust', 3, ifelse(SurveyData$AccountTrustPost=='Moderately trust', 4, 5))))
SurveyData$PostTrust <- ifelse(SurveyData$PostTrust=='Complete Distrust', 1, ifelse(SurveyData$PostTrust=='Moderately Distrust', 2, ifelse(SurveyData$PostTrust=='Neither trust nor distrust', 3, ifelse(SurveyData$PostTrust=='Moderately trust', 4, 5))))
SurveyData$Like <- ifelse(SurveyData$Like=='Disliked', -1, ifelse(SurveyData$Like=='Liked', 1, 0))
#SurveyData$Forward <- ifelse(SurveyData$Forward=='Yes', 1,  0)
#SurveyData$Comment <- ifelse(SurveyData$Comment=='Yes', 1,  0)

SurveyData$AT  <- as_factor(SurveyData$AT)
SurveyData$AL  <- as_factor(SurveyData$AL)
SurveyData$PT  <- as_factor(SurveyData$PT)
SurveyData$RespondentID  <- as.numeric(SurveyData$RespondentID)
SurveyData$RespondentID  <- as_factor(SurveyData$RespondentID)

SurveyData$AccountTrustPre  <- as.numeric(SurveyData$AccountTrustPre)


SurveyData$AccountTrustPost  <- as.numeric(SurveyData$AccountTrustPost)
SurveyData$AccountTrustVar <- SurveyData$AccountTrustPost - SurveyData$AccountTrustPre
SurveyData$PostTrust <- as.numeric(SurveyData$PostTrust)
SurveyData$Like <- as.numeric(SurveyData$Like)
#SurveyData$Forward <- as.numeric(SurveyData$Forward)
#SurveyData$Comment <- as.numeric(SurveyData$Comment)
SurveyData

SL$SatLev <- ifelse(SL$SatLev=='Completely unsatisfied',-2, ifelse(SL$SatLev=='Somewhat unsatisfied', -1, ifelse(SL$SatLev=='Neither unsatisfied nor satisfied', 0, ifelse(SL$SatLev=='Somewhat satisfied', 1, 2))))
SL
write.table(SL , sep=",", file = "SL.csv")
SurveyDataPro <- dplyr::filter(SurveyData, PT %in% c("Pro"))
#===============================================================================
#Pre-Analyses

AccountTrustPreNormalityShapiro<- shapiro.test(SurveyData$AccountTrustPre)
AccountTrustPreNormalityShapiro

AccountTrustPreNormalityagostino<- agostino.test(SurveyDataPro$AccountTrustPre)
AccountTrustPreNormalityagostino

AccountTrustPreDensity <- ggdensity(SurveyData, x = "AccountTrustPre", fill = "lightgray", title = "CONT") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
AccountTrustPreDensity 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

AccountTrustPostNormalityShapiro<- shapiro.test(SurveyData$AccountTrustPost)
AccountTrustPostNormalityShapiro

AccountTrustPostNormalityagostino<- agostino.test(SurveyData$AccountTrustPost)
AccountTrustPostNormalityagostino

AccountTrustPostDensity <- ggdensity(SurveyData, x = "AccountTrustPost", fill = "lightgray", title = "CONT") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
AccountTrustPostDensity

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

AccountTrustVarNormalityShapiro<- shapiro.test(SurveyData$AccountTrustVar)
AccountTrustVarNormalityShapiro

AccountTrustVarNormalityagostino<- agostino.test(SurveyData$AccountTrustVar)
AccountTrustVarNormalityagostino

AccountTrustVarDensity <- ggdensity(SurveyData, x = "AccountTrustVar", fill = "lightgray", title = "CONT") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
AccountTrustVarDensity

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PostTrustNormalityShapiro<- shapiro.test(SurveyData$PostTrust)
PostTrustNormalityShapiro

PostTrustVarNormalityagostino<- agostino.test(SurveyData$PostTrust)
PostTrustVarNormalityagostino

PostTrustDensity <- ggdensity(SurveyData, x = "PostTrust", fill = "lightgray", title = "CONT") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
PostTrustDensity

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LikeNormalityShapiro<- shapiro.test(SurveyData$Like)
LikeNormalityShapiro

LikeNormalityagostino<- agostino.test(SurveyData$Like)
LikeNormalityagostino

LikeDensity <- ggdensity(SurveyData, x = "Like", fill = "lightgray", title = "CONT") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
LikeDensity


SatLevNormalityShapiro<- shapiro.test(SL$SatLev)
SatLevNormalityShapiro
SatLevNormalityagostino<- agostino.test(SL$SatLev)
SatLevNormalityagostino

SatLevDensity <- ggdensity(SL, x = "SatLev", fill = "lightgray", title = "CONT") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

SatLevDensity 

#=================================================================
#Analyses

#_______________________________________________________________________________
#AT, AL, PT, and AccountTrustPre

bxpAccountTrustPre <- ggboxplot(
  SurveyData, x = "AL", y = "AccountTrustPre", 
  facet.by = "AT", add = "mean"
)

bxpAccountTrustPre

SummaryAccountTrustPreAT  <- SurveyData %>% 
  group_by(AT,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustPre, na.rm = TRUE),
  )
SummaryAccountTrustPreAT

FriedmanAccountTrustPreAT <-  friedman.test( Mean ~ AT|RespondentID,SummaryAccountTrustPreAT)
FriedmanAccountTrustPreAT

PostHocAccountTrustPreAT <- pairwise.wilcox.test(SummaryAccountTrustPreAT$Mean,SummaryAccountTrustPreAT$AT, paired = TRUE, p.adj = "none")
PostHocAccountTrustPreAT

SummaryAccountTrustPreAL  <- SurveyData %>% 
  group_by(AL,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustPre, na.rm = TRUE),
  )
SummaryAccountTrustPreAL

FriedmanAccountTrustPreAL <-  friedman.test( Mean ~ AL|RespondentID,SummaryAccountTrustPreAL)
FriedmanAccountTrustPreAL

PostHocAccountTrustPreAL <- pairwise.wilcox.test(SummaryAccountTrustPreAL$Mean,SummaryAccountTrustPreAL$AL, paired = TRUE, p.adj = "none")
PostHocAccountTrustPreAL

#_______________________________________________________________________________
bxpAccountTrustPost <- ggboxplot(
  SurveyData, x = "AL", y = "AccountTrustPost", 
  color = "PT", palette = "jco", facet.by = "AT", add = "mean"
)
bxpAccountTrustPost

SummaryAccountTrustPostAT  <- SurveyData %>% 
  group_by(AT,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustPost, na.rm = TRUE),
  )
SummaryAccountTrustPostAT

FriedmanAccountTrustPostAT <-  friedman.test( Mean ~ AT|RespondentID,SummaryAccountTrustPostAT)
FriedmanAccountTrustPostAT

PostHocAccountTrustPostAT <- pairwise.wilcox.test(SummaryAccountTrustPostAT$Mean,SummaryAccountTrustPostAT$AT, paired = TRUE, p.adj = "none")
PostHocAccountTrustPostAT

SummaryAccountTrustPostAL  <- SurveyData %>% 
  group_by(AL,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustPost, na.rm = TRUE),
  )
SummaryAccountTrustPostAL

FriedmanAccountTrustPostAL <-  friedman.test( Mean ~ AL|RespondentID,SummaryAccountTrustPostAL)
FriedmanAccountTrustPostAL

PostHocAccountTrustPostAL <- pairwise.wilcox.test(SummaryAccountTrustPostAL$Mean,SummaryAccountTrustPostAL$AL, paired = TRUE, p.adj = "none")
PostHocAccountTrustPostAL

SummaryAccountTrustPostPT  <- SurveyData %>% 
  group_by(PT,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustPost, na.rm = TRUE),
  )
SummaryAccountTrustPostPT



wilcoxAccountTrustPostPT <-  wilcox.test(SummaryAccountTrustPostPT$Mean ~ SummaryAccountTrustPostPT$PT, paired = TRUE, p.adj = "none") 
wilcoxAccountTrustPostPT


#_______________________________________________________________________________

bxpAccountTrustVar <- ggboxplot(
  SurveyData, x = "AL", y = "AccountTrustVar", 
  color = "PT", palette = "jco", facet.by = "AT", add = "mean",
)
bxpAccountTrustVar

SummaryAccountTrustVarAT  <- SurveyData %>% 
  group_by(AT,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustVar, na.rm = TRUE),
  )
SummaryAccountTrustVarAT

FriedmanAccountTrustVarAT <-  friedman.test( Mean ~ AT|RespondentID,SummaryAccountTrustVarAT)
FriedmanAccountTrustVarAT

PostHocAccountTrustVarAT <- pairwise.wilcox.test(SummaryAccountTrustVarAT$Mean,SummaryAccountTrustVarAT$AT, paired = TRUE, p.adj = "none")
PostHocAccountTrustVarAT

SummaryAccountTrustVarAL  <- SurveyData %>% 
  group_by(AL,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustVar, na.rm = TRUE),
  )
SummaryAccountTrustVarAL

FriedmanAccountTrustVarAL <-  friedman.test( Mean ~ AL|RespondentID,SummaryAccountTrustVarAL)
FriedmanAccountTrustVarAL

PostHocAccountTrustVarAL <- pairwise.wilcox.test(SummaryAccountTrustVarAL$Mean,SummaryAccountTrustVarAL$AL, paired = TRUE, p.adj = "none")
PostHocAccountTrustVarAL

SummaryAccountTrustVarPT  <- SurveyData %>% 
  group_by(PT,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustVar, na.rm = TRUE),
  )
SummaryAccountTrustVarPT

WilcoxAccountTrustVarPT <- wilcox.test(SummaryAccountTrustVarPT$Mean ~SummaryAccountTrustVarPT$PT, paired = TRUE, p.adj = "none")
WilcoxAccountTrustVarPT

#_______________________________________________________________________________
#AT, AL, PT, and PostTrust

bxpPostTrust <- ggboxplot(
  SurveyData,  x = "AL", y = "PostTrust", 
  color = "PT", palette = "jco", facet.by = "AT", add = "mean", 
)

bxpPostTrust

SummaryPostTrustAT  <- SurveyData %>% 
  group_by(AT,RespondentID) %>% 
  summarise(Mean = mean(AccountTrustPost, na.rm = TRUE),
  )
SummaryPostTrustAT

FriedmanPostTrustAT <-  friedman.test( Mean ~ AT|RespondentID,SummaryPostTrustAT)
FriedmanPostTrustAT

PostHocPostTrustAT <- pairwise.wilcox.test(SummaryPostTrustAT$Mean,SummaryPostTrustAT$AT, paired = TRUE, p.adj = "none")
PostHocPostTrustAT

SummaryPostTrustAL  <- SurveyData %>% 
  group_by(AL,RespondentID) %>% 
  summarise(Mean = mean(PostTrust, na.rm = TRUE),
  )
SummaryPostTrustAL

FriedmanPostTrustAL <-  friedman.test( Mean ~ AL|RespondentID,SummaryPostTrustAL)
FriedmanPostTrustAL

PostHocPostTrustAL <- pairwise.wilcox.test(SummaryPostTrustAL$Mean,SummaryPostTrustAL$AL, paired = TRUE, p.adj = "none")
PostHocPostTrustAL

SummaryPostTrustPT  <- SurveyData %>% 
  group_by(PT,RespondentID) %>% 
  summarise(Mean = mean(PostTrust, na.rm = TRUE),
  )
SummaryPostTrustPT


WilcoxPostTrustPT <- wilcox.test(SummaryPostTrustPT$Mean~SummaryPostTrustPT$PT, paired = TRUE, p.adj = "none")
WilcoxPostTrustPT

#_______________________________________________________________________________
#relation between TAVar and TP

bxpAccountTrustVarPostTrust <- ggboxplot(
  SurveyData, x = "PostTrust", y = "AccountTrustVar", 
  color = "PT", palette = "jco", add = "mean"
)
bxpAccountTrustVarPostTrust 




SurveyData1 <- dplyr::filter(SurveyData, PostTrust %in% c(1))
SurveyData1
SurveyData2 <- dplyr::filter(SurveyData, PostTrust %in% c(2))
SurveyData2
SurveyData3 <- dplyr::filter(SurveyData, PostTrust %in% c(3))
SurveyData3
SurveyData4 <- dplyr::filter(SurveyData, PostTrust %in% c(4))
SurveyData4
SurveyData5 <- dplyr::filter(SurveyData, PostTrust %in% c(5))
SurveyData5

SurveyDataRefined <- dplyr::filter(SurveyData, ( RespondentID %in% SurveyData1$RespondentID
                                               & RespondentID %in% SurveyData2$RespondentID 
                                               & RespondentID %in% SurveyData3$RespondentID 
                                               & RespondentID %in% SurveyData4$RespondentID 
                                               & RespondentID %in% SurveyData5$RespondentID ))
SurveyDataRefined

SummaryAccountTrustVarPostTrust  <- SurveyDataRefined %>% 
  group_by(PostTrust, RespondentID) %>% 
  summarise(Mean = mean(AccountTrustVar, na.rm = TRUE),
  )
SummaryAccountTrustVarPostTrust 


SummaryAccountTrustVarPostTrust$PostTrust <- factor(SummaryAccountTrustVarPostTrust$PostTrust)

SummaryAccountTrustVarPostTrust$RespondentID <- factor(SummaryAccountTrustVarPostTrust$RespondentID)
SummaryAccountTrustVarPostTrust

FriedmanAccountTrustVarPostTrust <- friedman.test( Mean ~ PostTrust |RespondentID , data = SummaryAccountTrustVarPostTrust)
FriedmanAccountTrustVarPostTrust

PostHocAccountTrustVarPostTrust  <- pairwise.wilcox.test(SummaryAccountTrustVarPostTrust$Mean, SummaryAccountTrustVarPostTrust$PostTrust, paired = TRUE, p.adj = "none")
PostHocAccountTrustVarPostTrust

#_______________________________________________________________________________
#AT, AL, PT, and Like

bxpLike <- ggboxplot(
  SurveyData, x = "AL", y = "Like", 
  color = "PT", palette = "jco", facet.by = "AT", add = "mean", 
)
bxpLike

SummaryLikeAT  <- SurveyData %>% 
  group_by(AT,RespondentID) %>% 
  summarise(Mean = mean(Like, na.rm = TRUE),
  )
SummaryLikeAT

FriedmanLikeAT <-  friedman.test( Mean ~ AT|RespondentID, SummaryLikeAT)
FriedmanLikeAT

PostHocLikeAT <- pairwise.wilcox.test(SummaryLikeAT$Mean,SummaryLikeAT$AT, paired = TRUE, p.adj = "none")
PostHocLikeAT

SummaryLikeAL  <- SurveyData %>% 
  group_by(AL,RespondentID) %>% 
  summarise(Mean = mean(Like, na.rm = TRUE),
  )
SummaryLikeAL

FriedmanLikeAL <-  friedman.test( Mean ~ AL|RespondentID,SummaryLikeAL)
FriedmanLikeAL

PostHocLikeAL <- pairwise.wilcox.test(SummaryLikeAL$Mean,SummaryLikeAL$AL, paired = TRUE, p.adj = "none")
PostHocLikeAL

SummaryLikePT  <- SurveyData %>% 
  group_by(PT,RespondentID) %>% 
  summarise(Mean = mean(Like, na.rm = TRUE),
  )
SummaryLikePT

WilcoxLikePT <- wilcox.test(SummaryLikePT$Mean~SummaryLikePT$PT, paired = TRUE, p.adj = "none")
WilcoxLikePT

#_______________________________________________________________________________
#AT,PT, and Forward

SurveyData$Forward <- factor(SurveyData$Forward, levels = c("Yes", "No"))
SummaryForward <- count(SurveyData, AT ,AL ,PT ,RespondentID ,Forward ,.drop = FALSE)
SummaryForward 
aggSummaryForward <- mutate(SummaryForward,
                            AT = reorder(AT, -n, sum),
                            AL = reorder(AL, -n, sum),
                            PT = reorder(PT, -n, sum),
                            RespondentID  = reorder(RespondentID, -n, sum),
                            Forward = reorder(Forward, -n, sum))
aggSummaryForward

aggSummaryForwardPos <- dplyr::filter(aggSummaryForward, Forward %in% c("Yes"))
aggSummaryForwardPos
aggSummaryForwardNeg <- dplyr::filter(aggSummaryForward, Forward %in% c("No"))
aggSummaryForwardNeg

aggSummaryForwardPos$n <- ifelse(aggSummaryForwardPos$AT!='NoA',aggSummaryForwardPos$n/5 ,aggSummaryForwardPos$n)

bxpForward <- ggbarplot(
  aggSummaryForwardPos  , "AT", "n", ylab = "Forwards per post",
  fill = "AL", color = "AL",  palette = "jco", facet.by = "PT",
)
bxpForward


SummaryForwardPT  <- aggSummaryForwardPos %>% 
  group_by(PT,RespondentID) %>% 
  summarise(Sum = sum(n, na.rm = TRUE),)
SummaryForwardPT


WilcoxForwardPT <- wilcox.test(SummaryForwardPT$Sum~SummaryForwardPT$PT, paired = TRUE, p.adj = "none")
WilcoxForwardPT

SummaryForwardAT  <- aggSummaryForwardPos %>% 
  group_by(AT,RespondentID) %>% 
  summarise(Sum = sum(n, na.rm = TRUE),)
SummaryForwardAT

FriedmanForwardAT <- friedman.test(Sum ~ AT | RespondentID, SummaryForwardAT)
FriedmanForwardAT

PostHocForwardAT <- pairwise.wilcox.test(SummaryForwardAT$Sum,SummaryForwardAT$AT, paired = TRUE, p.adj = "none")
PostHocForwardAT

SummaryForwardAL  <- aggSummaryForwardPos %>% 
  group_by(AL,RespondentID) %>% 
  summarise(Sum = sum(n, na.rm = TRUE),)
SummaryForwardAL

FriedmanForwardAL <- friedman.test(Sum ~ AL | RespondentID, SummaryForwardAL)
FriedmanForwardAL

PostHocForwardAL <- pairwise.wilcox.test(SummaryForwardAL$Sum, SummaryForwardAL$AL, paired = TRUE, p.adj = "none")
PostHocForwardAL
#_______________________________________________________________________________
#AT, AL, PT, and Comment

SurveyData$Comment <- factor(SurveyData$Comment, levels = c("Yes", "No"))
SummaryComment <- count(SurveyData, AT,AL,PT,RespondentID,Comment,.drop = FALSE)
SummaryComment 
aggSummaryComment <- mutate(SummaryComment,
                            AT = reorder(AT, -n, sum),
                            AL = reorder(AL, -n, sum),
                            PT = reorder(PT, -n, sum),
                            Comment = reorder(Comment, -n, sum))
aggSummaryComment

aggSummaryCommentPos <- dplyr::filter(aggSummaryComment, Comment %in% c("Yes"))
aggSummaryCommentPos

aggSummaryCommentPos$n <- ifelse(aggSummaryCommentPos$AT!='NoA',aggSummaryCommentPos$n/5 ,aggSummaryCommentPos$n)

bxpComment <- ggbarplot(
  aggSummaryCommentPos  , "AT", "n", ylab = "Comment per post",
  fill = "AL", color = "AL",  palette = "jco", facet.by = "PT",
)
bxpComment

SummaryCommentPT  <- aggSummaryCommentPos %>% 
  group_by(PT,RespondentID) %>% 
  summarise(Sum = sum(n, na.rm = TRUE),)
SummaryCommentPT


WilcoxCommentPT <-wilcox.test(SummaryCommentPT$Sum ~ SummaryCommentPT$PT, paired = TRUE, p.adj = "none")
WilcoxCommentPT

SummaryCommentAT  <- aggSummaryCommentPos %>% 
  group_by(AT,RespondentID) %>% 
  summarise(Sum = sum(n, na.rm = TRUE),)
SummaryCommentAT

FriedmanCommentAT <- friedman.test(Sum ~ AT | RespondentID, SummaryCommentAT)
FriedmanCommentAT

PostHocCommentAT <- pairwise.wilcox.test(SummaryCommentAT$Sum, SummaryCommentAT$AT, paired = TRUE, p.adj = "none")
PostHocCommentAT

SummaryCommentAL  <- aggSummaryCommentPos %>% 
  group_by(AL,RespondentID) %>% 
  summarise(Sum = sum(n, na.rm = TRUE),)
SummaryCommentAL

FriedmanCommentAL <- friedman.test(Sum ~ AL | RespondentID, SummaryCommentAL)
FriedmanCommentAL

PostHocCommentAL <- pairwise.wilcox.test(SummaryCommentAL$Sum, SummaryCommentAL$AL, paired = TRUE, p.adj = "none")
PostHocCommentAL
#-------------------------------------------------------------------------------
ezPrecis(Demographics)
sapply(Demographics, mean, na.rm = T)
lapply(Demographics, mean, na.rm = T)

SummaryDemographicsPV <- count(Demographics, PV,.drop = FALSE)
SummaryDemographicsPV






bxpDemographicsPV<- ggplot(
  SummaryDemographicsPV , aes(x =PV, y = n) )+ geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +geom_bar(stat = "identity")

bxpDemographicsPV


SummaryDemographicsGender <- count(Demographics, Gender,.drop = FALSE)
SummaryDemographicsGender

SummaryDemographicsAge <- count(Demographics, Age,.drop = FALSE)
SummaryDemographicsAge

SummaryDemographicsCulture <- count(Demographics, Culture,.drop = FALSE)
SummaryDemographicsCulture

SummaryDemographicsDegree <- count(Demographics, Degree,.drop = FALSE)
SummaryDemographicsDegree


ezPrecis(SL)
sapply(SL, mean, na.rm = T)
lapply(SL, mean, na.rm = T)
#-------------------------------------------------------------------------------

joined <- full_join(SL,Demographics, by="RespondentID")
#_______________________________________________________________________________


#-------------------------------------------------------------------------------

# SLSA <- dplyr::filter(SL, ATSat =="SA" )
# SLCA <- dplyr::filter(SL, ATSat =="CA" )
# SLNoA<- dplyr::filter(SL, ATSat =="NoA" )
# 
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 
# SatLevSANormalityShapiro<- shapiro.test(SLSA$SatLev)
# SatLevSANormalityShapiro
# SatLevSANormalityagostino<- agostino.test(SLSA$SatLev)
# SatLevSANormalityagostino
# 
# 
# SatLevSADensity <- ggdensity(SLSA, x = "SatLev", fill = "lightgray", title = "CONT") +
#   stat_overlay_normal_density(color = "red", linetype = "dashed")
# SatLevSADensity 
# #_______________________________________________________________________________
# SatLevCANormalityShapiro<- shapiro.test(SLCA$SatLev)
# SatLevCANormalityShapiro
# SatLevCANormalityagostino<- agostino.test(SLCA$SatLev)
# SatLevCANormalityagostino
# 
# 
# SatLevCADensity <- ggdensity(SLCA, x = "SatLev", fill = "lightgray", title = "CONT") +
#   stat_overlay_normal_density(color = "red", linetype = "dashed")
# 
# SatLevCADensity 
# 
# #_______________________________________________________________________________
# SatLevNoANormalityShapiro<- shapiro.test(SLNoA$SatLev)
# SatLevNoANormalityShapiro
# SatLevNoANormalityagostino<- agostino.test(SLNoA$SatLev)
# SatLevNoANormalityagostino
# 
# 
# SatLevNoADensity <- ggdensity(SLNoA, x = "SatLev", fill = "lightgray", title = "CONT") +
#   stat_overlay_normal_density(color = "red", linetype = "dashed")
# 
# SatLevNoADensity 

#_______________________________________________________________________________


bxpSatLevAT <- ggboxplot(
  joined  , x = "ATSat", y = "SatLev",
  palette = "aaam", add = "mean"
)
bxpSatLevAT

friedmanSatLevAT <- friedman.test(SatLev ~ ATSat | RespondentID, joined)
friedmanSatLevAT 

PostHocSatLevAT <- pairwise.wilcox.test(joined$SatLev, joined$ATSat, paired = TRUE, p.adj = "none")
PostHocSatLevAT 
#_______________________________________________________________________________


bxpSatLevPV <- ggboxplot(
  joined  , x = "PV", y = "SatLev", color = "ATSat",
  palette = "aaam", add = "mean"
)
bxpSatLevPV

kruskalSatLevPV <- kruskal.test(SatLev ~ PV, data = joined)
kruskalSatLevPV 

#_______________________________________________________________________________

bxpSatLevGender <- ggboxplot(
  joined  , x = "Gender", y = "SatLev", color = "ATSat",
  palette = "aaam", add = "mean"
)
bxpSatLevGender


kruskalSatLevGender <- kruskal.test(SatLev ~ Gender, data = joined)
kruskalSatLevGender 

PostHocSatLevGender  <- dunn.test(joined$SatLev, joined$Gender, method = "bonferroni")
PostHocSatLevGender 
#_______________________________________________________________________________

bxpATSatDegree <- ggboxplot(
  joined  , x = "Degree", y = "SatLev", color = "ATSat",
  palette = "aaam", add = "mean"
)
bxpATSatDegree


MannSatLevDegree <- wilcox.test(SatLev ~ Degree, data= joined, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannSatLevDegree

#_______________________________________________________________________________
bxpSatLevCulture <- ggboxplot(
  joined  , x = "Culture", y = "SatLev", color = "ATSat",
  palette = "aaam", add = "mean"
)
bxpSatLevCulture


MannSatLevCulture <- wilcox.test(SatLev ~ Culture, data= joined, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannSatLevCulture
#-------------------------------------------------------------------------------
#relation between demographics and main DVs

joinedDataDemo <- inner_join(SurveyData,Demographics, by="RespondentID", match ="all")
joinedDataDemo

bxpAccountTrustPreDegree <- ggboxplot(
   joinedDataDemo  , x = "Degree", y = "AccountTrustPre", color = "AL",
   palette = "jco", add = "mean"
)
bxpAccountTrustPreDegree

MannAccountTrustPreDegree <- wilcox.test(AccountTrustPre ~ Degree, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannAccountTrustPreDegree

PostHocAccountTrustPreDegree <- dunn.test(joinedDataDemo$AccountTrustPre, joinedDataDemo$Degree, method = "bonferroni")
PostHocAccountTrustPreDegree

#-------------------------------------------------------------------------------
bxpAccountTrustPrePV <- ggboxplot(
  joinedDataDemo  , x = "PV", y = "AccountTrustPre", color = "AL",
  palette = "jco", add = "mean"
)
bxpAccountTrustPrePV

kruskalAccountTrustPrePV <- kruskal.test(AccountTrustPre ~ PV, data = joinedDataDemo)
kruskalAccountTrustPrePV

#-------------------------------------------------------------------------------
bxpAccountTrustPreGender <- ggboxplot(
  joinedDataDemo  , x = "Gender", y = "AccountTrustPre",
  palette = "jco", add = "mean"
)
bxpAccountTrustPreGender

KruskalAccountTrustPreGender <- kruskal.test(AccountTrustPre ~ Gender, data = joinedDataDemo)
KruskalAccountTrustPreGender


PostHocAccountTrustPreGender  <- dunn.test(joinedDataDemo$AccountTrustPre, joinedDataDemo$Gender, method = "bonferroni")
PostHocAccountTrustPreGender 

#-------------------------------------------------------------------------------

bxpAccountTrustPreCulture <- ggboxplot(
  joinedDataDemo  , x = "Culture", y = "AccountTrustPre",
  palette = "jco", add = "mean"
)
bxpAccountTrustPreCulture


MannAccountTrustPreCulture <- wilcox.test(AccountTrustPre ~ Culture, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannAccountTrustPreCulture



bxpAccountTrustPreDCul <- ggboxplot(
  joinedDataDemo  , x = "Degree", y = "AccountTrustPre", color = "Culture",
  palette = "jco", add = "mean"
)
bxpAccountTrustPreDCul



#_______________________________________________________________________________
bxpAccountTrustPostPV <- ggboxplot(
  joinedDataDemo  , x = "PV", y = "AccountTrustPost", color = "AL", facet.by = "PT",
   palette = "jco", add = "mean"
 )
bxpAccountTrustPostPV
 
kruskalAccountTrustPostPV <- kruskal.test(AccountTrustPost ~ PV, data = joinedDataDemo)
kruskalAccountTrustPostPV

bxpAccountTrustPostGender <- ggboxplot(
   joinedDataDemo  , x = "Gender", y = "AccountTrustPost", color = "AL",facet.by = "PT",
   palette = "jco", add = "mean")
bxpAccountTrustPostGender
 
kruskalAccountTrustPostGender <- kruskal.test(AccountTrustPost ~ Gender, data = joinedDataDemo)
kruskalAccountTrustPostGender

 
bxpAccountTrustPostCulture <- ggboxplot(
  joinedDataDemo  , x = "Culture", y = "AccountTrustPost", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpAccountTrustPostCulture

MannAccountTrustPostCulture <- wilcox.test(AccountTrustPost ~ Culture, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannAccountTrustPostCulture


bxpAccountTrustPostDegree <- ggboxplot(
  joinedDataDemo  , x = "Degree", y = "AccountTrustPost", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpAccountTrustPostDegree

MannAccountTrustPostDegree <- wilcox.test(AccountTrustPost ~ Degree, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannAccountTrustPostDegree

#_______________________________________________________________________________
bxpAccountTrustVarPV <- ggboxplot(
  joinedDataDemo  , x = "PV", y = "AccountTrustVar", color = "AL", facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpAccountTrustVarPV

kruskalAccountTrustVarPV <- kruskal.test(AccountTrustVar ~ PV, data = joinedDataDemo)
kruskalAccountTrustVarPV


bxpAccountTrustVarGender <- ggboxplot(
  joinedDataDemo  , x = "Gender", y = "AccountTrustVar", color = "PT",
  palette = "jco", add = "mean"
)
bxpAccountTrustVarGender

kruskalAccountTrustVarGender <- kruskal.test(AccountTrustVar ~ Gender, data = joinedDataDemo)
kruskalAccountTrustVarGender


bxpAccountTrustVarCulture <- ggboxplot(
  joinedDataDemo  , x = "Culture", y = "AccountTrustVar",color = "PT",
  palette = "jco", add = "mean"
)
bxpAccountTrustVarCulture


MannAccountTrustVarCulture <- wilcox.test(AccountTrustVar ~ Culture, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannAccountTrustVarCulture

bxpAccountTrustVarDegree <- ggboxplot(
  joinedDataDemo  , x = "Degree", y = "AccountTrustVar", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpAccountTrustVarDegree

MannAccountTrustVarDegree <- wilcox.test(AccountTrustVar ~ Degree, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannAccountTrustVarDegree

#_______________________________________________________________________________
bxpPostTrustPV <- ggboxplot(
  joinedDataDemo  , x = "PV", y = "PostTrust", color = "AL", facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpPostTrustPV


kruskalPostTrustPV <- kruskal.test(PostTrust ~ PV, data = joinedDataDemo)
kruskalPostTrustPV


bxpPostTrustGender <- ggboxplot(
  joinedDataDemo  , x = "Gender", y = "PostTrust", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpPostTrustGender

kruskalPostTrustGender <- kruskal.test(PostTrust ~ Gender, data = joinedDataDemo)
kruskalPostTrustGender


bxpPostTrustCulture <- ggboxplot(
  joinedDataDemo  , x = "Culture", y = "PostTrust", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpPostTrustCulture


MannPostTrustCulture <- wilcox.test(PostTrust ~ Culture, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannPostTrustCulture

bxpPostTrustDegree <- ggboxplot(
  joinedDataDemo  , x = "Degree", y = "PostTrust", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpPostTrustDegree


MannPostTrustDegree <- wilcox.test(PostTrust ~ Degree, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannPostTrustDegree
#_______________________________________________________________________________
bxpLikePV <- ggboxplot(
  joinedDataDemo  , x = "PV", y = "Like", color = "PT",
  palette = "jco", add = "mean"
)
bxpLikePV


kruskalLikePV <- kruskal.test(Like ~ PV, data = joinedDataDemo)
kruskalLikePV

PostHocLikePV  <- dunn.test(joinedDataDemo$Like, joinedDataDemo$PV, method = "bonferroni")
PostHocLikePV

bxpLikeGender <- ggboxplot(
  joinedDataDemo  , x = "Gender", y = "Like", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpLikeGender

kruskalLikeGender <- kruskal.test(Like ~ Gender, data = joinedDataDemo)
kruskalLikeGender


bxpLikeCulture <- ggboxplot(
  joinedDataDemo  , x = "Culture", y = "Like", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpLikeCulture


MannLikeCulture <- wilcox.test(Like ~ Culture, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannLikeCulture

bxpLikeDegree <- ggboxplot(
  joinedDataDemo  , x = "Degree", y = "Like", color = "AL",facet.by = "PT",
  palette = "jco", add = "mean"
)
bxpLikeDegree


MannLikeDegree <- wilcox.test(Like ~ Degree, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannLikeDegree

#_______________________________________________________________________________



joinedDataDemo$Forward <- ifelse(joinedDataDemo$Forward=='Yes', 1,  0)
joinedDataDemo$Comment <- ifelse(joinedDataDemo$Comment=='Yes', 1,  0)
joinedDataDemo

SummaryForwardPV <- count(joinedDataDemo, PV , Forward,.drop = FALSE)
SummaryForwardPV
SummaryForwardPVPos <- dplyr::filter(SummaryForwardPV, Forward == 1)
SummaryForwardPVPos
SummaryForwardPVPos$n <- SummaryForwardPVPos$n/SummaryDemographicsPV$n
SummaryForwardPVPos

bxpForwardPV<- ggplot(
  SummaryForwardPVPos , aes(x = PV, y = n)) + labs (y="Number of forwards per person") + geom_bar(stat = "identity") 
bxpForwardPV

kruskalForwardPV<- kruskal.test(Forward~PV, data = joinedDataDemo) 
kruskalForwardPV

PostHocAccountTrustPrePV  <- dunn.test(joinedDataDemo$Forward, joinedDataDemo$PV, method = "bonferroni")
PostHocAccountTrustPrePV

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SummaryForwardGender <- count(joinedDataDemo, Gender , Forward,.drop = FALSE)
SummaryForwardGender
SummaryForwardGenderPos <- dplyr::filter(SummaryForwardGender, Forward ==0)
SummaryForwardGenderPos
SummaryForwardGenderPos$n <- (SummaryDemographicsGender$n * 22 - SummaryForwardGenderPos$n)/SummaryDemographicsGender$n
SummaryForwardGenderPos

bxpForwardGender<- ggplot(
  SummaryForwardGenderPos , aes(x = Gender, y = n) )+ labs (y="Number of forwards per person")+ geom_bar(stat = "identity")

bxpForwardGender

kruskalForwardGender<- kruskal.test(Forward~Gender, data = joinedDataDemo)
kruskalForwardGender


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SummaryForwardDegree <- count(joinedDataDemo, Degree , Forward,.drop = FALSE)
SummaryForwardDegree
SummaryForwardDegreePos <- dplyr::filter(SummaryForwardDegree, Forward == 1)
SummaryForwardDegreePos
SummaryForwardDegreePos$n <- SummaryForwardDegreePos$n/SummaryDemographicsDegree$n
SummaryForwardDegreePos

bxpForwardDegree<- ggplot(
  SummaryForwardDegreePos , aes(x = Degree, y = n) )+ labs (y="Number of forwards per person")+ geom_bar(stat = "identity")

bxpForwardDegree

MannForwardDegree <- wilcox.test(Forward ~ Degree, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannForwardDegree


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SummaryForwardCulture <- count(joinedDataDemo, Culture , Forward,.drop = FALSE)
SummaryForwardCulture
SummaryForwardCulturePos <- dplyr::filter(SummaryForwardCulture, Forward == 1)
SummaryForwardCulturePos
SummaryForwardCulturePos$n <- SummaryForwardCulturePos$n/SummaryDemographicsCulture$n
SummaryForwardCulturePos

bxpForwardCulture<- ggplot(
  SummaryForwardCulturePos , aes(x = Culture, y = n) )+ labs (y="Number of forwards per person")+ geom_bar(stat = "identity")

bxpForwardCulture

MannForwardCulture <- wilcox.test(Forward ~ Culture, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannForwardCulture


#-------------------------------------------------------------------------------


SummaryCommentPV <- count(joinedDataDemo, PV , Comment,.drop = FALSE)
SummaryCommentPV
SummaryCommentPVPos <- dplyr::filter(SummaryCommentPV, Comment == 1)
SummaryCommentPVPos
SummaryCommentPVPos$n <- SummaryCommentPVPos$n/SummaryDemographicsPV$n
SummaryCommentPVPos

bxpCommentPV<- ggplot(
  SummaryCommentPVPos , aes(x = PV, y = n) )+  labs (y="Number of Comments per person") + geom_bar(stat = "identity")

bxpCommentPV

kruskalCommentPV<- kruskal.test(Comment~PV, data = joinedDataDemo)
kruskalCommentPV

PostHocAccountTrustPrePV  <- dunn.test(joinedDataDemo$Comment, joinedDataDemo$PV, method = "bonferroni")
PostHocAccountTrustPrePV

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SummaryCommentGender <- count(joinedDataDemo, Gender , Comment,.drop = FALSE)
SummaryCommentGender
SummaryCommentGenderPos <- dplyr::filter(SummaryCommentGender, Comment == 1)
SummaryCommentGenderPos
SummaryCommentGenderPos$n <- SummaryCommentGenderPos$n/SummaryDemographicsGender$n
SummaryCommentGenderPos

bxpCommentGender<- ggplot(
  SummaryCommentGenderPos , aes(x = Gender, y = n) )+  labs (y="Number of Comments per person")+ geom_bar(stat = "identity")

bxpCommentGender
kruskalCommentGender<- kruskal.test(Comment~Gender, data = joinedDataDemo)
kruskalCommentGender

PostHocCommentGender  <- dunn.test(joinedDataDemo$Comment, joinedDataDemo$Gender, method = "bonferroni")
PostHocCommentGender
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SummaryCommentDegree <- count(joinedDataDemo, Degree , Comment,.drop = FALSE)
SummaryCommentDegree
SummaryCommentDegreePos <- dplyr::filter(SummaryCommentDegree, Comment == 1)
SummaryCommentDegreePos
SummaryCommentDegreePos$n <- SummaryCommentDegreePos$n/SummaryDemographicsDegree$n
SummaryCommentDegreePos

bxpCommentDegree<- ggplot(
  SummaryCommentDegreePos , aes(x = Degree, y = n) )+  labs (y="Number of Comments per person")+ geom_bar(stat = "identity")

bxpCommentDegree

MannCommentegree <- wilcox.test(Comment ~ Degree, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannCommentegree


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SummaryCommentCulture <- count(joinedDataDemo, Culture , Comment,.drop = FALSE)
SummaryCommentCulture
SummaryCommentCulturePos <- dplyr::filter(SummaryCommentCulture, Comment == 1)
SummaryCommentCulturePos
SummaryCommentCulturePos$n <- SummaryCommentCulturePos$n/SummaryDemographicsCulture$n
SummaryCommentCulturePos

bxpCommentCulture<- ggplot(
  SummaryCommentCulturePos , aes(x = Culture, y = n) )+  labs (y="Number of Comments per person")+ geom_bar(stat = "identity")

bxpCommentCulture

MannCommentCulture <- wilcox.test(Comment ~ Culture, data= joinedDataDemo, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
MannCommentCulture



