sink("/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Analysis/whole_multiplicative.txt", split = TRUE)

library(effectsize)
library(readxl)
library(dplyr)
#library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(emmeans)
citation('dplyr')

input_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_preprocessed.xlsx"
df <- read_excel(input_file)

# centering Leach scales
df <- df %>%
  mutate(
    group_id  = as.numeric(as.character(group_id)),
    mammal_id = as.numeric(as.character(mammal_id)),
    
    # overwrite with mean-centered versions
    group_id  = group_id  - mean(group_id,  na.rm = TRUE),
    mammal_id = mammal_id - mean(mammal_id, na.rm = TRUE)
  )

'The effect of conflict intensity on parochialism (H1) and the association between the degree of group 
identification and parochialism (H2) will be tested simultaneously using the same analysis. 
A 2 (conflict intensity: high vs. low) × 2 (meat-consumption profile: non-meat consumer vs. meat consumer)
between-participants ANCOVA will be conducted, with group identification as a covariate and parochialism 
(measured by gSVO) as a dependent variable.'
# conflict intensity in column A203: value for low 2, and for high 1
# meat-consumption profile in column QUESTNNR: non-meat consumer value is base, meat consumer value is base01
# group identification is numeric from Leach scale in column group_id
# parochialism is in column gSVO_deg, numeric, low value means higher parochialism

#preparing variables to fit model correctly
df$conflict <- factor(df$A203,
                      levels = c(2, 1),
                      labels = c("low", "high"))

df$meat_profile <- factor(df$QUESTNNR,
                          levels = c("base", "base01"),
                          labels = c("non-meat", "meat"))
df$group_id <- as.numeric(as.character(df$group_id))
df$gSVO_deg <- as.numeric(as.character(df$gSVO_deg))
# check NAs
sum(is.na(df$group_id))
sum(is.na(df$gSVO_deg))



'
df %>%
  group_by(conflict, meat_profile) %>%
  summarise(
    n = sum(!is.na(gSVO_deg) & !is.na(group_id)),
    mean_gSVO = mean(gSVO_deg, na.rm = TRUE),
    sd_gSVO = sd(gSVO_deg, na.rm = TRUE),
    mean_group_id = mean(group_id, na.rm = TRUE),
    .groups = "drop"
  )'


#fit <- lm(gSVO_deg ~ conflict + meat_profile + group_id, data = df) 
fit <- lm(gSVO_deg ~ conflict * meat_profile + group_id, data = df)# ??? Whether the effect of conflict intensity on gSVO differs between meat eaters and non–meat eaters.
Anova(fit, type = 3)
#summary(fit, type = 3)$coefficients - ordinary OLS SEs, not HC3 SEs

# assumption independence of obseravtions, factor structure
#->correct

# assumption linearity (cov, dv)
plot(df$group_id, df$gSVO_deg,
     xlab = "group_id", ylab = "gSVO_deg")
abline(lm(gSVO_deg ~ group_id, data = df))
#->The relationship between the covariate (group_id) and the DV (gSVO_deg) can be reasonably approximated by a straight line.
anova(
  lm(gSVO_deg ~ conflict * meat_profile + group_id, data = df),
  lm(gSVO_deg ~ conflict * meat_profile + group_id + I(group_id^2), data = df)
)
#->Quadratic term not significant → linearity holds

# assumption Homogeneity of regression slopes - The effect of group_id must be the same across all groups
fit_slopes <- lm(gSVO_deg ~ conflict * meat_profile * group_id, data = df)
Anova(fit_slopes, type = 3)
#->non-sign group_id:conflict, group_id:meat_profile, group_id:conflict:meat_profile

# assumption Homoscedasticity (equal error variance)
plot(fitted(fit), residuals(fit))
abline(h = 0)
#->so so
bptest(fit)
#->homoskedasticity rejected
Anova(fit, type = 3, vcov. = vcovHC(fit, type = "HC3")) # more robust SE for more reliable interpretation
coeftest(fit, type = 3, vcov. = vcovHC(fit, type = "HC3"))

#effect size H1
# adjusted means for conflict (averaged over meat_profile; covariate held at its mean)
emm_conflict <- emmeans(fit, ~ conflict)
# high - low adjusted difference
diff_conflict <- contrast(emm_conflict, method = "revpairwise")  # gives high - low
diff_sum <- summary(diff_conflict, infer = TRUE)
# Cohen's d using model residual SD
d_adj <- as.numeric(diff_sum$estimate) / sigma(fit)
diff_sum
d_adj

#effect size H2
library(lmtest)
library(sandwich)
ct <- coeftest(fit, vcov. = vcovHC(fit, type = "HC3"))
ct
t_group <- ct["group_id", "t value"]
df_res  <- df.residual(fit)
r_partial <- sign(t_group) * sqrt(t_group^2 / (t_group^2 + df_res))
r_partial



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
idx <- c(1,  47,  50, 120, 126, 129, 154, 179, 239)
df[idx, ] # add idx to df
# sensitivity analysis
fit_full <- lm(gSVO_deg ~ conflict * meat_profile + group_id, data = df)
fit_drop <- lm(gSVO_deg ~ conflict * meat_profile + group_id, data = df[-idx, ])
Anova(fit_full, type = 3, vcov. = vcovHC(fit_full, type = "HC3"))
Anova(fit_drop, type = 3, vcov. = vcovHC(fit_drop, type = "HC3"))
#Sensitivity analyses excluding influential cases identified via Cook’s distance (D > 4/N; n = 8) yielded the same pattern of results.
coeftest(fit_full, vcov. = vcovHC(fit_full, type = "HC3"))
coeftest(fit_drop, vcov. = vcovHC(fit_drop, type = "HC3"))


ct_drop <- coeftest(fit_drop,
                    vcov. = vcovHC(fit_drop, type = "HC3"))
b_conflict  <- ct_drop["conflicthigh", "Estimate"]
se_conflict <- ct_drop["conflicthigh", "Std. Error"]
b_group  <- ct_drop["group_id", "Estimate"]
se_group <- ct_drop["group_id", "Std. Error"]
emm_conflict_drop <- emmeans(fit_drop, ~ conflict)
diff_conflict_drop <- contrast(emm_conflict_drop,
                               method = "revpairwise")
diff_sum_drop <- summary(diff_conflict_drop,
                         infer = TRUE)
d_adj_drop <- as.numeric(diff_sum_drop$estimate) /
  sigma(fit_drop)
d_adj_drop # d conflict
t_group_drop <- ct_drop["group_id", "t value"]
df_res_drop <- df.residual(fit_drop)
r_partial_drop <- sign(t_group_drop) *
  sqrt(t_group_drop^2 /
         (t_group_drop^2 + df_res_drop))
r_partial_drop # d dietary identification

'
To investigate whether non-meat consumers identify stronger with mammals (H3), 
a one-way ANOVA will compare mammal identification scores between non-meat consumers and meat consumers.'
# mammal identification scores are mammal_id numeric Leach scale
# meat-consumption profile in column QUESTNNR: non-meat consumer value is base, meat consumer value is base01
df_h3 <- df %>%
  mutate(
    meat_profile = factor(as.character(QUESTNNR),
                          levels = c("base", "base01"),
                          labels = c("non-meat", "meat")),
    mammal_id = as.numeric(as.character(mammal_id))
  ) #%>%
  #filter(!is.na(meat_profile), !is.na(mammal_id)) - model does it automatically
sum(is.na(df$mammal_id))
sum(is.na(df$meat_profile))

m_h3 <- lm(mammal_id ~ meat_profile, data = df_h3)
anova(m_h3)          # ANOVA table
summary(m_h3)        # group effect + estimates

# Assumptions outliers
plot(m_h3, which = 4)  # Cook's distance
plot(m_h3, which = 5)  # Residuals vs leverage
which(cooks.distance(m_h3) > 4 / nrow(df_h3))
which(abs(rstudent(m_h3)) > 3)
idx <- c(80, 96, 135, 140, 145, 179, 195, 224, 238)
df_h3[idx, ]
m_full <- lm(mammal_id ~ meat_profile, data = df_h3)
m_drop <- lm(mammal_id ~ meat_profile, data = df_h3[-idx, ])
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
leveneTest(mammal_id ~ meat_profile, data = df_h3)
# non sign -> not violated

# Cohens d
cohens_d(mammal_id ~ meat_profile, data = df_h3)
cohens_d(mammal_id ~ meat_profile, data = df_h3[-idx, ])  # sensitivity sample

'To investigate whether non-meat consumers show higher in-group altruism (H4), 
one-way ANOVA will be conducted with meat consumption profile (non-meat consumer vs. meat consumer) 
as the only factor and mammalian in-group altruism (measured by iSVO) as a dependent variable. 
In a second step, the mammalian identification score will be added as a covariate (ANCOVA).'
# mammalian in-group altruism is numeric stored in column iSVOm_deg
# mammal identification scores are mammal_id numeric Leach scale
# meat-consumption profile in column QUESTNNR: non-meat consumer value is base, meat consumer value is base01

dev.off()

df_h4 <- df %>%
  mutate(
    meat_profile = factor(as.character(QUESTNNR),
                          levels = c("base", "base01"),
                          labels = c("non-meat", "meat")),
    iSVOm_deg = as.numeric(as.character(iSVOm_deg)),
    mammal_id = as.numeric(as.character(mammal_id))
  ) #%>%
  #filter(!is.na(meat_profile),
         #!is.na(iSVOm_deg),
         #!is.na(mammal_id))
sum(is.na(df$meat_profile))
sum(is.na(df$iSVOm_deg))
sum(is.na(df$mammal_id))

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
# Welch’s t test is the two-group ANOVA with unequal variances
t.test(iSVOm_deg ~ meat_profile, data = df_h4, var.equal = FALSE)
# wilcox
wilcox.test(iSVOm_deg ~ meat_profile, data = df_h4, exact = FALSE)

# assumptions outliers
plot(m_h4_anova, which = 4)  # Cook's distance
plot(m_h4_anova, which = 5)  # Residuals vs leverage
which(cooks.distance(m_h4_anova) > 4 / nrow(df_h4))
which(abs(rstudent(m_h4_anova)) > 3)
#idx <- c(28)
idx <- c(3, 28, 85, 117, 125, 148, 164, 225, 240)
df_h4[idx, ]
m_full <- lm(iSVOm_deg ~ meat_profile, data = df_h4)
m_drop <- lm(iSVOm_deg ~ meat_profile, data = df_h4[-idx, ])
anova(m_full)
anova(m_drop)
# Residual normality diagnostics of sensitivity test
shapiro.test(residuals(m_full))
shapiro.test(residuals(m_drop))
qqnorm(residuals(m_full)); qqline(residuals(m_full))
qqnorm(residuals(m_drop)); qqline(residuals(m_drop))
# -> not helpful for normality and robust to outliers

#effect size
library(effectsize)
cohens_d(iSVOm_deg ~ meat_profile, data = df_h4, pooled_sd = TRUE, ci = 0.95)

# Sensitivity analysis: coefficients
summary(m_drop)

# Sensitivity analysis: effect size
cohens_d(iSVOm_deg ~ meat_profile, data = df_h4[-idx, ], pooled_sd = TRUE, ci = 0.95)

# ANCOVA sensitivity model
m_h4_ancova_drop <- lm(iSVOm_deg ~ meat_profile + mammal_id, data = df_h4[-idx, ])

anova(m_h4_ancova_drop)
summary(m_h4_ancova_drop)

# Optional: HC3 robust version if needed
library(lmtest)
library(sandwich)

coeftest(m_h4_ancova_drop, vcov. = vcovHC(m_h4_ancova_drop, type = "HC3"))

#-----
#Step 2
# ancova
m_h4_ancova <- lm(iSVOm_deg ~ meat_profile + mammal_id, data = df_h4)
anova(m_h4_ancova)
summary(m_h4_ancova)

# ancova assumption linearity (dv, cov)
plot(df_h4$mammal_id, df_h4$iSVOm_deg)
abline(lm(iSVOm_deg ~ mammal_id, data = df_h4))
# -> ok

# homogenity of regression slopes
m_h4_slopes <- lm(iSVOm_deg ~ meat_profile * mammal_id, data = df_h4)
anova(m_h4_slopes)
#-> meat_profile:mammal_id is not significant - ANCOVA assumption met

#homoskedasticity
plot(fitted(m_h4_ancova), residuals(m_h4_ancova))
abline(h = 0)
#->ok

# run a formal normality check for the ANCOVA residuals too
shapiro.test(residuals(m_h4_ancova))


sink()

