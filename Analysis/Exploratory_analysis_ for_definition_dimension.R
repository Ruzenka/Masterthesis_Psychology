sink("/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Analysis/dimension2.txt", split = TRUE)

# use def_g instead of group_id / inv_g
# use def_m instead of mammal_id / inv_m
# use sheet "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment_definition_dimension.xlsx"

library(effectsize)
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)

input_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment_definition_dimension.xlsx"
df <- read_excel(input_file)

# centering Leach scales
df <- df %>%
  mutate(
    def_g  = as.numeric(as.character(def_g)),
    def_m = as.numeric(as.character(def_m)),
    
    # overwrite with mean-centered versions
    def_g  = def_g  - mean(def_g,  na.rm = TRUE),
    def_m = def_m - mean(def_m, na.rm = TRUE)
  )

'The effect of conflict intensity on parochialism (H1) and the association between the degree of group 
identification and parochialism (H2) will be tested simultaneously using the same analysis. 
A 2 (conflict intensity: high vs. low) × 2 (meat-consumption profile: non-meat consumer vs. meat consumer)
between-participants ANCOVA will be conducted, with group identification as a covariate and parochialism 
(measured by gSVO) as a dependent variable.'
# conflict intensity in column A203: value for low 2, and for high 1
# meat-consumption profile in column QUESTNNR: non-meat consumer value is base, meat consumer value is base01
# group identification is numeric from Leach scale in column def_g
# parochialism is in column gSVO_deg, numeric, low value means higher parochialism

#preparign variables
df$conflict <- factor(df$A203,
                      levels = c(2, 1),
                      labels = c("low", "high"))

df$meat_profile <- factor(df$QUESTNNR,
                          levels = c("base", "base01"),
                          labels = c("non-meat", "meat"))
df$def_g <- as.numeric(as.character(df$def_g))
df$gSVO_deg <- as.numeric(as.character(df$gSVO_deg))
sum(is.na(df$group_id))
sum(is.na(df$gSVO_deg))

'
df %>%
  group_by(conflict, meat_profile) %>%
  summarise(
    n = sum(!is.na(gSVO_deg) & !is.na(def_g)),
    mean_gSVO = mean(gSVO_deg, na.rm = TRUE),
    sd_gSVO = sd(gSVO_deg, na.rm = TRUE),
    mean_def_g = mean(def_g, na.rm = TRUE),
    .groups = "drop"
  )'

fit <- lm(gSVO_deg ~ conflict * meat_profile + def_g, data = df)
Anova(fit, type = 3)
summary(fit, type = 3)$coefficients

# assumption independence of obseravtions, factor structure
#->correct

# assumption linearity (cov, dv)
plot(df$def_g, df$gSVO_deg,
     xlab = "def_g", ylab = "gSVO_deg")
abline(lm(gSVO_deg ~ def_g, data = df))
#->The relationship between the covariate (def_g) and the DV (gSVO_deg) can be reasonably approximated by a straight line.

# assumption Homogeneity of regression slopes - The effect of def_g must be the same across all groups
fit_slopes <- lm(gSVO_deg ~ conflict * meat_profile * def_g, data = df)
Anova(fit_slopes, type = 3)
#->non-sign def_g:conflict, def_g:meat_profile, def_g:conflict:meat_profile

# assumption Homoscedasticity (equal error variance)
plot(fitted(fit), residuals(fit))
abline(h = 0)
#->so so
bptest(fit)
#->homoskedasticity rejected
Anova(fit, type = 3, vcov = vcovHC(fit, type = "HC3")) # more robust SE ANCOVA
coeftest(fit, type = 3, vcov. = vcovHC(fit, type = "HC3"))

# assumption Normality of residuals
qqnorm(residuals(fit))
qqline(residuals(fit))
# -> heavier than normal tails 
shapiro.test(residuals(fit))  # interpret cautiously with large N
# sign -> not normal

#outliers
plot(fit, which = 4)  # Cook's distance
plot(fit, which = 5)  # Residuals vs leverage
which(cooks.distance(fit) > 4 / nrow(df))
which(abs(rstudent(fit)) > 3)
idx <- c(1,  50,  53, 120, 126, 129, 154, 179, 239 )
df[idx, ]
# sensitivity analysis
fit_full <- lm(gSVO_deg ~ conflict * meat_profile + def_g, data = df)
fit_drop <- lm(gSVO_deg ~ conflict * meat_profile + def_g, data = df[-idx, ])
Anova(fit_full, type = 3, vcov. = vcovHC(fit_full, type = "HC3"))
Anova(fit_drop, type = 3, vcov. = vcovHC(fit_drop, type = "HC3"))
coeftest(fit_full, vcov. = vcovHC(fit_full, type = "HC3"))
coeftest(fit_drop, vcov. = vcovHC(fit_drop, type = "HC3"))


'
To investigate whether non-meat consumers identify stronger with mammals (H3), 
a one-way ANOVA will compare mammal identification scores between non-meat consumers and meat consumers.'
# mammal identification scores are def_m numeric Leach scale
# meat-consumption profile in column QUESTNNR: non-meat consumer value is base, meat consumer value is base01
# H3: one-way ANOVA comparing mammal identification scores (def_m) between groups

df_h3 <- df %>%
  mutate(
    meat_profile = factor(as.character(QUESTNNR),
                          levels = c("base", "base01"),
                          labels = c("non-meat", "meat")),
    def_m = as.numeric(as.character(def_m))
  ) %>%
  filter(!is.na(meat_profile), !is.na(def_m))
m_h3 <- lm(def_m ~ meat_profile, data = df_h3)
anova(m_h3)          # ANOVA table
summary(m_h3)        # group effect + estimates

# Assumptions outliers
plot(m_h3, which = 4)  # Cook's distance
plot(m_h3, which = 5)  # Residuals vs leverage
which(cooks.distance(m_h3) > 4 / nrow(df_h3))
which(abs(rstudent(m_h3)) > 3)
idx <- c(81,  86, 123, 160, 195 )
df_h3[idx, ]
m_full <- lm(def_m ~ meat_profile, data = df_h3)
m_drop <- lm(def_m ~ meat_profile, data = df_h3[-idx, ])
anova(m_full)
anova(m_drop)
summary(m_full)$coefficients
summary(m_drop)$coefficients

# Assumption normality of residuals
qqnorm(residuals(m_h3)); qqline(residuals(m_h3))
hist(residuals(m_h3))
shapiro.test(residuals(m_h3))
# non sign -> good

# assumption homogenity of variances
leveneTest(def_m ~ meat_profile, data = df_h3)
# non sign -> not violated

# Cohens d
cohens_d(def_m ~ meat_profile, data = df_h3)


'To investigate whether non-meat consumers show higher in-group altruism (H4), 
one-way ANOVA will be conducted with meat consumption profile (non-meat consumer vs. meat consumer) 
as the only factor and mammalian in-group altruism (measured by iSVO) as a dependent variable. 
In a second step, the mammalian identification score will be added as a covariate (ANCOVA).'
# mammalian in-group altruism is numeric stored in column iSVOm_deg
# mammal identification scores are def_m numeric Leach scale
# meat-consumption profile in column QUESTNNR: non-meat consumer value is base, meat consumer value is base01
# H4: one-way ANOVA with iSVOm_deg; then ANCOVA adding def_m as covariate

df_h4 <- df %>%
  mutate(
    meat_profile = factor(as.character(QUESTNNR),
                          levels = c("base", "base01"),
                          labels = c("non-meat", "meat")),
    iSVOm_deg = as.numeric(as.character(iSVOm_deg)),
    def_m = as.numeric(as.character(def_m))
  ) %>%
  filter(!is.na(meat_profile),
         !is.na(iSVOm_deg),
         !is.na(def_m))
m_h4_anova <- lm(iSVOm_deg ~ meat_profile, data = df_h4)
anova(m_h4_anova)
summary(m_h4_anova)

# assumption Homogenity of variance
leveneTest(iSVOm_deg ~ meat_profile, data = df_h4)

# assumption normality of residuals
qqnorm(residuals(m_h4_anova))
qqline(residuals(m_h4_anova))
shapiro.test(residuals(m_h4_anova))  # interpret cautiously if N is large
# very bad
# Welch
t.test(iSVOm_deg ~ meat_profile, data = df_h4, var.equal = FALSE)
# wilcox
wilcox.test(iSVOm_deg ~ meat_profile, data = df_h4, exact = FALSE)


# assumptions outliers
plot(m_h4_anova, which = 4)  # Cook's distance
plot(m_h4_anova, which = 5)  # Residuals vs leverage
which(cooks.distance(m_h4_anova) > 4 / nrow(df_h4))
which(abs(rstudent(m_h4_anova)) > 3)
idx <- c(3,  28,  85, 117, 125, 148, 164, 225, 240)
df_h4[idx, ]
m_full <- lm(iSVOm_deg ~ meat_profile, data = df_h4)
m_drop <- lm(iSVOm_deg ~ meat_profile, data = df_h4[-idx, ])
anova(m_full)
anova(m_drop)
# Residual normality diagnostics
shapiro.test(residuals(m_full))
shapiro.test(residuals(m_drop))
qqnorm(residuals(m_full)); qqline(residuals(m_full))
qqnorm(residuals(m_drop)); qqline(residuals(m_drop))
# -> not helpful for normality and robust

#2. step
# ancova
m_h4_ancova <- lm(iSVOm_deg ~ meat_profile + def_m, data = df_h4)
anova(m_h4_ancova)
summary(m_h4_ancova)

# ancova assumption linearity (dv, cov)
plot(df_h4$def_m, df_h4$iSVOm_deg)
abline(lm(iSVOm_deg ~ def_m, data = df_h4))
# -> ok

# homogenity of regression slopes
m_h4_slopes <- lm(iSVOm_deg ~ meat_profile * def_m, data = df_h4)
anova(m_h4_slopes)
#-> meat_profile:def_m is not significant - ANCOVA assumption met

#homoskedasticity
plot(fitted(m_h4_ancova), residuals(m_h4_ancova))
abline(h = 0)
#->ok


sink()

