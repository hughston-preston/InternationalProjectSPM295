library(Lahman)
library(lubridate)
library(tidyverse)

#Lahman and People
people <- read_csv("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")
people_ID <- select(people, key_bbref, key_person, key_uuid, key_mlbam, key_retro, key_bbref_minors, key_fangraphs, key_npb, key_sr_nfl, key_sr_nba, key_sr_nhl, key_findagrave)
Master %>% left_join(people_ID, by = c("playerID" = "key_bbref")) -> Master_ID
study_people <- filter(Master_ID, debut > 1979) %>% select(-c("playerID", 
                                                              "birthYear", 
                                                              "birthMonth", 
                                                              "birthDay", 
                                                              "deathYear", 
                                                              "deathMonth", 
                                                              "deathDay", 
                                                              "deathCountry", 
                                                              "deathState", 
                                                              "deathCity", 
                                                              "nameFirst", 
                                                              "nameLast", 
                                                              "nameGiven", 
                                                              "retroID", 
                                                              "bbrefID", 
                                                              "deathDate", 
                                                              "key_person", 
                                                              "key_uuid", 
                                                              "key_mlbam", 
                                                              "key_retro", 
                                                              "key_bbref_minors", 
                                                              "key_sr_nfl", 
                                                              "key_sr_nba", 
                                                              "key_sr_nhl", 
                                                              "key_findagrave"))

Pitching <- read.csv("Pitching.csv")
Hitting <- read.csv("Hitting.csv")

Pitching_joined <- study_people %>% left_join(Pitching, by = c("key_fangraphs" = "playerid")) %>% filter(!is.na(Name))
Hitting_joined <- study_people %>% left_join(Hitting, by = c("key_fangraphs" = "playerid")) %>% filter(!is.na(Name))
Pitching_clean <- Pitching_joined %>% select(c("Name", "birthCountry":"birthCity", "W":"WAR","weight":"key_npb"))
Hitting_clean <- Hitting_joined %>% select(c("Name", "birthCountry":"birthCity", "G":"WAR","weight":"key_npb"))

Pitching_averages <- Pitching_clean %>% mutate("CareerLength" = (as.numeric(difftime(finalGame, debut, units = "weeks")) / 52.25)) %>% mutate("avW" = W / CareerLength,
                                                                                                                                              "avL" = L / CareerLength,
                                                                                                                                              "avSV" = SV / CareerLength, 
                                                                                                                                              "avG" = G / CareerLength,
                                                                                                                                              "avGS" = GS / CareerLength,
                                                                                                                                              "avIP" = IP / CareerLength,
                                                                                                                                              "avWAR" = WAR / CareerLength)

Hitting_averages <- Hitting_clean %>% mutate("CareerLength" = (as.numeric(difftime(finalGame, debut, units = "weeks")) / 52.25)) %>% mutate("avG" = G / CareerLength,
                                                                                                                                             "avPA" = PA / CareerLength,
                                                                                                                                             "avHR" = HR / CareerLength,
                                                                                                                                             "avR" = R / CareerLength,
                                                                                                                                             "avRBI" = RBI / CareerLength,
                                                                                                                                             "avSB" = SB / CareerLength,
                                                                                                                                             "avOFF" = Off / CareerLength,
                                                                                                                                             "avDEF" = Def / CareerLength,
                                                                                                                                             "avWAR" = WAR / CareerLength)



write.csv(Pitching_averages, "InternationalProject_PitchingData.csv")
write.csv(Hitting_averages, "InternationalProject_HittingData.csv")



#Regressions
library(fastDummies)
nationality_pitchers <- read_csv("InternationalProject_PitchingData.csv")
nationality_pitchers <- nationality_pitchers %>% mutate(LOBr = (as.numeric(sub("%","", LOB.))/100), GBr = (as.numeric(sub("%","", GB.))/100), HR.FBr = (as.numeric(sub("%","", HR.FB))/100)) %>% select(-c(LOB., GB., HR.FB))
nationality_hitters<- read_csv("InternationalProject_HittingData.csv")
nationality_hitters <- nationality_hitters %>% mutate(BBr = (as.numeric(sub("%","", BB.))/100), Kr = (as.numeric(sub("%","", K.))/100)) %>% select(-c(BB., K.))


fastDummies::dummy_cols(nationality_pitchers, select_columns = "birthCountry") -> pitcherdummy

fastDummies::dummy_cols(nationality_hitters, select_columns = "birthCountry") -> hitterdummy

colnames(pitcherdummy)[colnames(pitcherdummy)=="birthCountry_South Korea"] <- "birthCountry_SouthKorea"
colnames(hitterdummy)[colnames(hitterdummy)=="birthCountry_Saudi Arabia"] <- "birthCountry_SaudiArabia"
colnames(pitcherdummy)[colnames(pitcherdummy)=="birthCountry_Saudi Arabia"] <- "birthCountry_SaudiArabia"
colnames(pitcherdummy)[colnames(pitcherdummy)=="birthCountry_United Kingdom"] <- "birthCountry_UnitedKingdom"
colnames(hitterdummy)[colnames(hitterdummy)=="birthCountry_South Korea"] <- "birthCountry_SouthKorea"

pitchreg <- lm(avIP ~ birthCountry_Aruba + birthCountry_Australia + birthCountry_CAN + birthCountry_Colombia + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_France + birthCountry_Germany + birthCountry_Jamaica + birthCountry_Japan + birthCountry_Mexico + birthCountry_Nicaragua + birthCountry_P.R. + birthCountry_Panama + birthCountry_SaudiArabia + birthCountry_SouthKorea + birthCountry_Taiwan + birthCountry_UnitedKingdom + birthCountry_USA + birthCountry_Venezuela, data = pitcherdummy)

hitterreg <- lm(avSB ~ birthCountry_Aruba + birthCountry_Australia + birthCountry_Brazil + birthCountry_CAN + birthCountry_Colombia + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_France + birthCountry_Germany + birthCountry_Honduras + birthCountry_Jamaica + birthCountry_Japan + birthCountry_Mexico + birthCountry_Netherlands + birthCountry_Nicaragua + birthCountry_P.R. + birthCountry_Panama + birthCountry_SouthKorea + birthCountry_USA + birthCountry_Venezuela, data = hitterdummy)

summary(pitchreg)
summary(hitterreg)

#Batter Regressions
SBreg <- lm(SB ~ birthCountry_D.R. + birthCountry_Honduras + birthCountry_Jamaica + birthCountry_Japan + birthCountry_USA, data = hitterdummy)
summary(SBreg)

KRATEreg <- lm(Kr ~ birthCountry_Brazil + birthCountry_CAN + birthCountry_D.R. + birthCountry_Germany + birthCountry_SouthKorea + birthCountry_USA, data = hitterdummy)
summary(KRATEreg)

DEFreg <- lm(Def ~ birthCountry_Brazil + birthCountry_Colombia + birthCountry_Curacao + birthCountry_Mexico + birthCountry_P.R. + birthCountry_SouthKorea + birthCountry_Venezuela, data = hitterdummy)
summary(DEFreg)

avDEFreg <- lm(avDEF ~ birthCountry_Brazil + birthCountry_Colombia + birthCountry_Curacao + birthCountry_D.R. + birthCountry_Japan + birthCountry_Netherlands + birthCountry_P.R. + birthCountry_SouthKorea + birthCountry_USA, data = hitterdummy)
summary(avDEFreg)

HRreg <- lm(HR ~ birthCountry_CAN + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_Jamaica + birthCountry_P.R. + birthCountry_USA, data = hitterdummy)
summary(HRreg)

ISOreg <- lm(ISO ~ birthCountry_CAN + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_France + birthCountry_Germany + birthCountry_Honduras + birthCountry_P.R. + birthCountry_SouthKorea + birthCountry_USA, data = hitterdummy)
summary(ISOreg)

wRCreg <- lm(wRC. ~ birthCountry_Aruba + birthCountry_CAN + birthCountry_Colombia + birthCountry_Cuba + birthCountry_France + birthCountry_Jamaica + birthCountry_Mexico + birthCountry_SouthKorea + birthCountry_USA, data = hitterdummy)
summary(wRCreg)

BBRATEreg <- lm(BBr ~ birthCountry_Australia + birthCountry_Brazil + birthCountry_CAN + birthCountry_Colombia + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_France + birthCountry_Mexico + birthCountry_Netherlands + birthCountry_P.R. + birthCountry_Panama + birthCountry_SouthKorea + birthCountry_Venezuela, data = hitterdummy)
summary(BBRATEreg)

OBPreg <- lm(OBP ~ birthCountry_Brazil + birthCountry_CAN + birthCountry_Colombia + birthCountry_Cuba + birthCountry_D.R. + birthCountry_Japan + birthCountry_Mexico + birthCountry_P.R. + birthCountry_SouthKorea + birthCountry_Venezuela, data = hitterdummy)
summary(OBPreg)

wOBAreg <- lm(wOBA ~ birthCountry_CAN + birthCountry_Colombia + birthCountry_France + birthCountry_Mexico + birthCountry_Nicaragua + birthCountry_SouthKorea + birthCountry_USA + birthCountry_Venezuela, data = hitterdummy)
summary(wOBAreg)

Careerreg <- lm(CareerLength ~ birthCountry_CAN + birthCountry_Cuba + birthCountry_Curacao + birthCountry_Germany + birthCountry_Jamaica + birthCountry_Japan + birthCountry_Mexico + birthCountry_Nicaragua + birthCountry_P.R. + birthCountry_SouthKorea + birthCountry_Venezuela, data = hitterdummy)
summary(Careerreg)

BABIPreg <- lm(BABIP ~ birthCountry_Aruba + birthCountry_CAN + birthCountry_Colombia + birthCountry_Curacao + birthCountry_D.R. + birthCountry_France + birthCountry_Japan + birthCountry_Mexico + birthCountry_Nicaragua + birthCountry_P.R. + birthCountry_Venezuela, data = hitterdummy)
summary(BABIPreg)

BsRreg <- lm(BsR ~ birthCountry_Aruba + birthCountry_Cuba + birthCountry_Japan + birthCountry_Netherlands + birthCountry_P.R. + birthCountry_USA + birthCountry_Venezuela, data = hitterdummy)
summary(BsRreg)

hitterreg <- lm(R ~ birthCountry_Aruba + birthCountry_Australia + birthCountry_Brazil + birthCountry_CAN + birthCountry_Colombia + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_France + birthCountry_Germany + birthCountry_Honduras + birthCountry_Jamaica + birthCountry_Japan + birthCountry_Mexico + birthCountry_Netherlands + birthCountry_Nicaragua + birthCountry_P.R. + birthCountry_Panama + birthCountry_SouthKorea + birthCountry_USA + birthCountry_Venezuela, data = hitterdummy)
summary(hitterreg)

#Batter Regressions Ordered by R-squared Significance (highest significance to lowest)
summary(BBRATEreg) -> Batter_BB_Rate_Summary
summary(OBPreg) -> Batter_OBP_Summary
summary(Careerreg) -> Batter_Career_Length_Summary
summary(wOBAreg) -> Batter_wOBA_Summary
summary(wRCreg) -> Batter_wRC_Plus_Summary
summary(BABIPreg) -> Batter_BABIP_Summary
summary(BsRreg) -> Batter_BsR_Summary
summary(ISOreg) -> Batter_ISO_Summary
summary(avDEFreg) -> Batter_Average_DEF_Per_Season_Summary
#Below R-Squared = 0.02
summary(DEFreg) -> Batter_DEF_Summary
summary(HRreg) -> Batter_HR_Summary
summary(KRATEreg) -> Batter_K_Rate_Summary
summary(SBreg) -> Batter_SB_Summary

#Pitcher Regressions
K9reg <- lm(K.9 ~ birthCountry_CAN + birthCountry_Colombia + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_Japan + birthCountry_Mexico + birthCountry_USA + birthCountry_Venezuela, data = pitcherdummy)
summary(K9reg)

BB9reg <- lm(BB.9 ~ birthCountry_Curacao + birthCountry_D.R. + birthCountry_Nicaragua + birthCountry_P.R. + birthCountry_Panama + birthCountry_SaudiArabia + birthCountry_Taiwan, data = pitcherdummy)
summary(BB9reg)

avSVreg <- lm(avSV ~ birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_Panama, data = pitcherdummy)
summary(avSVreg)

LOBreg <- lm(LOBr ~ birthCountry_Aruba + birthCountry_CAN + birthCountry_Colombia + birthCountry_Curacao + birthCountry_Japan + birthCountry_Panama + birthCountry_USA, data = pitcherdummy)
summary(LOBreg)

GBRATEreg <- lm(GBr ~ birthCountry_Colombia + birthCountry_Cuba + birthCountry_Curacao + birthCountry_D.R. + birthCountry_Jamaica + birthCountry_Japan + birthCountry_Mexico + birthCountry_USA + birthCountry_Venezuela, data = pitcherdummy)
summary(GBRATEreg)

CareerLengthPreg <- lm(CareerLength ~ birthCountry_D.R. + birthCountry_Germany + birthCountry_Japan + birthCountry_Mexico + birthCountry_Panama + birthCountry_UnitedKingdom + birthCountry_USA, data = pitcherdummy)
summary(CareerLengthPreg)

#Pitcher Regressions Ranked by R-Squared Significance
summary(K9reg) -> Pitcher_K_PerNine_Summary
summary(BB9reg) -> Pitcher_BB_PerNine_Summary
summary(avSVreg) -> Pitcher_AverageSavesPerSeason_Summary
summary(LOBreg) -> Pitcher_LeftOnBasePercentage_Summary
summary(GBRATEreg) -> Pitcher_GroundBallPercentage_Summary
summary(CareerLengthPreg) -> Pitcher_CareerLength_Summary

# All Regressions Ranked by R-Squared Significance
Batter_BB_Rate_Summary 
Pitcher_K_PerNine_Summary
Batter_OBP_Summary
Batter_Career_Length_Summary
Batter_wOBA_Summary
Batter_wRC_Plus_Summary
Batter_BABIP_Summary
Batter_BsR_Summary
Batter_ISO_Summary
Batter_Average_DEF_Per_Season_Summary
Batter_DEF_Summary
Batter_HR_Summary
Pitcher_BB_PerNine_Summary
Pitcher_AverageSavesPerSeason_Summary
Pitcher_LeftOnBasePercentage_Summary
Pitcher_GroundBallPercentage_Summary
Pitcher_CareerLength_Summary
Batter_K_Rate_Summary
Batter_SB_Summary

summary(pitcherdummy$birthCountry)
count <- count(pitcherdummy, birthCountry)
count2 <- count(hitterdummy, birthCountry)

