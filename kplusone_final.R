library(readr)
library(car)
library(MASS)
library(ggplot2)
library(reshape2)
library(Lahman)
library(dplyr)
library(olsrr)
################################################################################
## Subgroup all Teams data since 1970
teams <- Teams %>% 
  filter(yearID >= 1970 & yearID != 1972  & yearID != 1981 & yearID != 1994 & yearID < 2020 & lgID %in% c("AL", "NL")) %>%
  group_by(yearID, teamID) %>%
  select(yearID, teamID, lgID, franchID, WCWin, DivWin, WSWin, G, W, L, R, AB, H,
         X2B, X3B, HR, BB, SO, HBP, SF, RA, ER, ERA, 
         IPouts, HA, HRA, BBA, SOA) %>%
  mutate(TB = H + X2B + 2 * X3B + 3 * HR,
         BA = H/AB,
         WinPct = W/G,
         xbpg = (X2B+X3B+HR)/G,
         kpg = SO/G,
         bbpg = BB/G,
         k2bb = SO/BB,
         hapg = HA/G,
         soapg = SOA/G,
         bbapg = BBA/G,
         whip = 3 * (HA + BBA)/IPouts,
         rdiff = R - RA)

## fit full
fit_all <- lm(WinPct~BA+xbpg+kpg+bbpg+hapg+soapg+bbapg+ERA, data = teams)
summary(fit_all)

## fit reduced (removing variables with p-value >= 0.05)
fit_all_reduced <- lm(WinPct~BA+xbpg+bbpg+hapg+bbapg+ERA, data = teams)
summary(fit_all_reduced)

## anova check
anova(fit_all_reduced, fit_all)

## removing variables with extreme multicollinearity
vif(fit_all_reduced)
## vif values < 10; passes check

## check for outliers and influential points
Inf_all_reduced <- influence.measures(fit_all_reduced)
summary(Inf_all_reduced)
dfbetasPlots(fit_all_reduced, intercept = T)
influenceIndexPlot(fit_all_reduced)
ols_plot_resid_lev(fit_all_reduced)
## Studentized Residuals
ggplot(fit_all, aes(x = fitted(fit_all), y = rstudent(fit_all))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (full) vs. Studentized Residuals: 1970 to 2019")

ggplot(fit_all_reduced, aes(x = fitted(fit_all_reduced), y = rstudent(fit_all_reduced))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (reduced) vs. Studentized Residuals: 1970 to 2019")

stuall <- t(studres(fit_all_reduced))
which(stuall > 3)
which(stuall < -3)

################################################################################
## Regression plots

## BA
ggplot(data = teams, mapping = aes(x = BA, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = BA, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$BA)), color = "orange") +
  labs(x = "BA", y = "Win Pct", title = "Relationship between BA and Winning Percentage: 1970 to 2019")

## XBPG
ggplot(data = teams, mapping = aes(x = xbpg, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = xbpg, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$xbpg)), color = "orange") +
  labs(x = "XBPG", y = "Win Pct", title = "Relationship between XBPG and Winning Percentage: 1970 to 2019")

## KPG
ggplot(data = teams, mapping = aes(x = kpg, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = kpg, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$kpg)), color = "orange") +
  labs(x = "KPG", y = "Win Pct", title = "Relationship between KPG and Winning Percentage: 1970 to 2019")

## BBPG
ggplot(data = teams, mapping = aes(x = bbpg, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = bbpg, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$bbpg)), color = "orange") +
  labs(x = "BBPG", y = "Win Pct", title = "Relationship between BBPG and Winning Percentage: 1970 to 2019")

## HAPG
ggplot(data = teams, mapping = aes(x = hapg, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = hapg, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$hapg)), color = "orange") +
  labs(x = "HAPG", y = "Win Pct", title = "Relationship between HAPG and Winning Percentage: 1970 to 2019")

## SOAPG
ggplot(data = teams, mapping = aes(x = soapg, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = soapg, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$soapg)), color = "orange") +
  labs(x = "SOAPG", y = "Win Pct", title = "Relationship between SOAPG and Winning Percentage: 1970 to 2019")

## BBAPG
ggplot(data = teams, mapping = aes(x = bbapg, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = bbapg, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$bbapg)), color = "orange") +
  labs(x = "BBAPG", y = "Win Pct", title = "Relationship between BBAPG and Winning Percentage: 1970 to 2019")

## ERA
ggplot(data = teams, mapping = aes(x = ERA, y = WinPct, group = WSWin)) +
  geom_point(aes(shape = WSWin, color = WSWin), size = 1.5) +
  scale_color_manual(values = c("dark green", "red")) +
  geom_smooth(mapping = aes(x = ERA, y = WinPct), method = "lm", se = F) +
  geom_hline(yintercept = 0.5, color = "orange") +
  geom_vline(xintercept = median(range(teams$ERA)), color = "orange") +
  labs(x = "ERA", y = "Win Pct", title = "Relationship between ERA and Winning Percentage: 1970 to 2019")

################################################################################
## Subgroup Teams data for 70s
teams_70s <- Teams %>% 
  filter(yearID %in% 1970:1979 & yearID != 1972 & lgID %in% c("AL", "NL")) %>%
  group_by(yearID, teamID) %>%
  select(yearID, teamID, lgID, franchID, DivWin, WSWin, G, W, L, R, AB, H,
         X2B, X3B, HR, BB, SO, HBP, SF, RA, ER, ERA, 
         IPouts, HA, HRA, BBA, SOA) %>%
  mutate(TB = H + X2B + 2 * X3B + 3 * HR,
         BA = H/AB,
         WinPct = W/G,
         xbpg = (X2B+X3B+HR)/G,
         kpg = SO/G,
         bbpg = BB/G,
         k2bb = SO/BB,
         hapg = HA/G,
         soapg = SOA/G,
         bbapg = BBA/G,
         whip = 3 * (HA + BBA)/IPouts,
         rdiff = R - RA)

## fit full 
fit_70s <- lm(WinPct~BA+xbpg+kpg+bbpg+hapg+soapg+bbapg+ERA, data = teams_70s)
summary(fit_70s)

## fit reduced (removing variables with p-value >= 0.05)
fit_70s_reduced <- lm(WinPct~BA+xbpg+bbpg+bbapg+ERA, data = teams_70s)
summary(fit_70s_reduced)

## anova check
anova(fit_70s_reduced, fit_70s)

## removing variables with extreme multicollinearity
vif(fit_70s_reduced)
## vif values < 10; passes check

## check for outliers and influential points
Inf_70s_reduced <- influence.measures(fit_70s_reduced)
summary(Inf_70s_reduced)
dfbetasPlots(fit_70s_reduced, intercept = T)
influenceIndexPlot(fit_70s_reduced)
ols_plot_resid_lev(fit_70s_reduced)
## Studentized Residuals
ggplot(fit_70s, aes(x = fitted(fit_70s), y = rstudent(fit_70s))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (full) vs. Studentized Residuals: 1970s")

ggplot(fit_70s_reduced, aes(x = fitted(fit_70s_reduced), y = rstudent(fit_70s_reduced))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (reduced) vs. Studentized Residuals: 1970s")

stu70s <- t(studres(fit_70s_reduced))
which(stu70s > 3)
which(stu70s < -3)
teams_70s[183,]

################################################################################
## Subgroup Teams data for 80s
teams_80s <- Teams %>% 
  filter(yearID %in% 1980:1989 & yearID != 1981 & lgID %in% c("AL", "NL")) %>%
  group_by(yearID, teamID) %>%
  select(yearID, teamID, lgID, franchID, DivWin, WSWin, G, W, L, R, AB, H,
         X2B, X3B, HR, BB, SO, HBP, SF, RA, ER, ERA, 
         IPouts, HA, HRA, BBA, SOA) %>%
  mutate(TB = H + X2B + 2 * X3B + 3 * HR,
         BA = H/AB,
         WinPct = W/G,
         xbpg = (X2B+X3B+HR)/G,
         kpg = SO/G,
         bbpg = BB/G,
         k2bb = SO/BB,
         hapg = HA/G,
         soapg = SOA/G,
         bbapg = BBA/G,
         whip = 3 * (HA + BBA)/IPouts,
         rdiff = R - RA)

## fit full 
fit_80s <- lm(WinPct~BA+xbpg+kpg+bbpg+hapg+soapg+bbapg+ERA, data = teams_80s)
summary(fit_80s)

## fit reduced (removing variables with p-value >= 0.05)
fit_80s_reduced <- lm(WinPct~BA+xbpg+kpg+bbpg+hapg+bbapg+ERA, data = teams_80s)
summary(fit_80s_reduced)

## anova check
anova(fit_80s_reduced, fit_80s)

## removing variables with extreme multicollinearity
vif(fit_80s_reduced)
## vif values < 10; passes check

## check for outliers and influential points
Inf_80s_reduced <- influence.measures(fit_80s_reduced)
summary(Inf_80s_reduced)
dfbetasPlots(fit_80s_reduced, intercept = T)
influenceIndexPlot(fit_80s_reduced)
ols_plot_resid_lev(fit_80s_reduced)
## Studentized Residuals
ggplot(fit_80s, aes(x = fitted(fit_80s), y = rstudent(fit_80s))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (full) vs. Studentized Residuals: 1980s")

ggplot(fit_80s_reduced, aes(x = fitted(fit_80s_reduced), y = rstudent(fit_80s_reduced))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (reduced) vs. Studentized Residuals: 1980s")

stu80s <- t(studres(fit_80s_reduced))
which(stu80s > 3)
which(stu80s < -3)
teams_80s[150,]

################################################################################
## Subgroup Teams data for 90s
teams_90s <- Teams %>% 
  filter(yearID %in% 1990:1999 & yearID != 1994 & lgID %in% c("AL", "NL")) %>%
  group_by(yearID, teamID) %>%
  select(yearID, teamID, lgID, franchID, DivWin, WSWin, G, W, L, R, AB, H,
         X2B, X3B, HR, BB, SO, HBP, SF, RA, ER, ERA, 
         IPouts, HA, HRA, BBA, SOA) %>%
  mutate(TB = H + X2B + 2 * X3B + 3 * HR,
         BA = H/AB,
         WinPct = W/G,
         xbpg = (X2B+X3B+HR)/G,
         kpg = SO/G,
         bbpg = BB/G,
         k2bb = SO/BB,
         hapg = HA/G,
         soapg = SOA/G,
         bbapg = BBA/G,
         whip = 3 * (HA + BBA)/IPouts,
         rdiff = R - RA)

## fit full 
fit_90s <- lm(WinPct~BA+xbpg+kpg+bbpg+hapg+soapg+bbapg+ERA, data = teams_90s)
summary(fit_90s)

## fit reduced (removing variables with p-value >= 0.05)
fit_90s_reduced <- lm(WinPct~BA+xbpg+bbpg+hapg+ERA, data = teams_90s)
summary(fit_90s_reduced)

## anova check
anova(fit_90s_reduced, fit_90s)

## removing variables with extreme multicollinearity
vif(fit_90s_reduced)
## vif values < 10; passes check

## check for outliers and influential points
Inf_90s_reduced <- influence.measures(fit_90s_reduced)
summary(Inf_90s_reduced)
dfbetasPlots(fit_90s_reduced, intercept = T)
influenceIndexPlot(fit_90s_reduced)
ols_plot_resid_lev(fit_90s_reduced)
## Studentized Residuals
ggplot(fit_90s, aes(x = fitted(fit_90s), y = rstudent(fit_90s))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (full) vs. Studentized Residuals: 1990s")

ggplot(fit_90s_reduced, aes(x = fitted(fit_90s_reduced), y = rstudent(fit_90s_reduced))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (reduced) vs. Studentized Residuals: 1990s")

stu90s <- t(studres(fit_90s_reduced))
which(stu80s > 3)
which(stu80s < -3)
teams_90s[64,]

################################################################################
## Subgroup Teams data for 00s
teams_00s <- Teams %>% 
  filter(yearID %in% 2000:2009 & lgID %in% c("AL", "NL")) %>%
  group_by(yearID, teamID) %>%
  select(yearID, teamID, lgID, franchID, DivWin, WSWin, G, W, L, R, AB, H,
         X2B, X3B, HR, BB, SO, HBP, SF, RA, ER, ERA, 
         IPouts, HA, HRA, BBA, SOA) %>%
  mutate(TB = H + X2B + 2 * X3B + 3 * HR,
         BA = H/AB,
         WinPct = W/G,
         xbpg = (X2B+X3B+HR)/G,
         kpg = SO/G,
         bbpg = BB/G,
         k2bb = SO/BB,
         hapg = HA/G,
         soapg = SOA/G,
         bbapg = BBA/G,
         whip = 3 * (HA + BBA)/IPouts,
         rdiff = R - RA)

## fit full 
fit_00s <- lm(WinPct~BA+xbpg+kpg+bbpg+hapg+soapg+bbapg+ERA, data = teams_00s)
summary(fit_00s)

## fit reduced (removing variables with p-value >= 0.05)
fit_00s_reduced <- lm(WinPct~BA+xbpg+bbpg+bbapg+ERA, data = teams_00s)
summary(fit_00s_reduced)

## anova check
anova(fit_00s_reduced, fit_00s)

## removing variables with extreme multicollinearity
vif(fit_00s_reduced)
## vif values < 10; passes check

## check for outliers and influential points
Inf_00s_reduced <- influence.measures(fit_00s_reduced)
summary(Inf_00s_reduced)
dfbetasPlots(fit_00s_reduced, intercept = T)
influenceIndexPlot(fit_00s_reduced)
ols_plot_resid_lev(fit_00s_reduced)
## Studentized Residuals
ggplot(fit_00s, aes(x = fitted(fit_00s), y = rstudent(fit_00s))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (full) vs. Studentized Residuals: 2000s")

ggplot(fit_00s_reduced, aes(x = fitted(fit_00s_reduced), y = rstudent(fit_00s_reduced))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (reduced) vs. Studentized Residuals: 2000s")

stu00s <- t(studres(fit_00s_reduced))
which(stu00s > 3)
which(stu00s < -3)
teams_00s[139,]
teams_00s[188,]

################################################################################
## Subgroup Teams data for 10s
teams_10s <- Teams %>% 
  filter(yearID %in% 2010:2019 & lgID %in% c("AL", "NL")) %>%
  group_by(yearID, teamID) %>%
  select(yearID, teamID, lgID, franchID, DivWin, WSWin, G, W, L, R, AB, H,
         X2B, X3B, HR, BB, SO, HBP, SF, RA, ER, ERA, 
         IPouts, HA, HRA, BBA, SOA) %>%
  mutate(TB = H + X2B + 2 * X3B + 3 * HR,
         BA = H/AB,
         WinPct = W/G,
         xbpg = (X2B+X3B+HR)/G,
         kpg = SO/G,
         bbpg = BB/G,
         k2bb = SO/BB,
         hapg = HA/G,
         soapg = SOA/G,
         bbapg = BBA/G,
         whip = 3 * (HA + BBA)/IPouts,
         rdiff = R - RA)

## fit full 
fit_10s <- lm(WinPct~BA+xbpg+kpg+bbpg+hapg+soapg+bbapg+ERA, data = teams_10s)
summary(fit_10s)

## fit reduced (removing variables with p-value >= 0.05)
fit_10s_reduced <- lm(WinPct~BA+xbpg+bbpg+hapg+ERA, data = teams_10s)
summary(fit_10s_reduced)

## anova check
anova(fit_10s_reduced, fit_10s)

## removing variables with extreme multicollinearity
vif(fit_10s_reduced)
## vif values < 10; passes check

## check for outliers and influential points
Inf_10s_reduced <- influence.measures(fit_10s_reduced)
summary(Inf_10s_reduced)
dfbetasPlots(fit_10s_reduced, intercept = T)
influenceIndexPlot(fit_10s_reduced)
ols_plot_resid_lev(fit_10s_reduced)
## Studentized Residuals
ggplot(fit_10s, aes(x = fitted(fit_10s), y = rstudent(fit_10s))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (full) vs. Studentized Residuals: 2010s")

ggplot(fit_10s_reduced, aes(x = fitted(fit_10s_reduced), y = rstudent(fit_10s_reduced))) +
  geom_point() +
  geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = -3, color = "orange") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Fitted Values (reduced) vs. Studentized Residuals: 2010s")

stu10s <- t(studres(fit_10s_reduced))
which(stu10s > 3)
which(stu10s < -3)
teams_10s[208,]