library(readxl)
library(dplyr)
library(writexl)
#library(ggplot2)

input_file  <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_solid.xlsx"
#input_file  <-"/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026.xlsx" # for the returned submissions
#output_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026.xlsx"

df <- read_excel(input_file)

#-------------------------
#OVERALL (base & base01)
# gender count
df %>%
  count(A007, sort = TRUE, name = "n")

# age mean
df <- df %>%
  mutate(age = as.numeric(A008_01))
mean_age <- df %>% summarise(mean_age = mean(age, na.rm = TRUE))
mean_age

# age per gender
df2 <- df %>%
  mutate(gender = as.factor(A007))

df2 %>%
  group_by(gender) %>%
  summarise(
    n_age = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_age))

# Histogram age per gender
ggplot(df2 %>% filter(!is.na(age), !is.na(gender)), aes(x = age)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ gender) +
  labs(x = "Age (years)", y = "Count")

# conflict intensity binary count
df %>%
  count(A203, sort = TRUE, name = "n")

# plot conflict intensity binary per age per gender
df_plot <- df %>%
  mutate(
    age = as.numeric(A008_01),
    gender = factor(A007),
    A203 = factor(A203) # 1 / 2 as categories
  ) %>%
  filter(!is.na(age), !is.na(gender), !is.na(A203))

ggplot(df_plot, aes(x = age, fill = A203)) +
  geom_histogram(bins = 20, position = "stack") +
  facet_wrap(~ gender) +
  labs(x = "Age (years)", y = "Count", fill = "Manipulation")


# plot dietary group per age per gender
df_plot <- df %>%
  mutate(
    age    = as.numeric(A008_01),
    gender = factor(A007),
    manipulation = factor(A203),
    diet = factor(QUESTNNR)
  ) %>%
  filter(!is.na(age), !is.na(gender), !is.na(diet))

ggplot(df_plot, aes(x = age, fill = diet)) +
  geom_histogram(bins = 20, position = "stack") +
  facet_wrap(~ gender) +
  labs(x = "Age (years)", y = "Count", fill = "Diet")

p <- ggplot(df_plot, aes(x = age, fill = diet)) +
  geom_histogram(bins = 20, position = "stack") +
  facet_wrap(~ gender) +
  labs(x = "Age (years)", y = "Count", fill = "Diet")

library(ggplot2)

# Ensure desired labels + reversed color assignment by setting factor order
df_plot$diet <- factor(df_plot$diet, levels = c("base01", "base"))

library(ggplot2)

# (Optional) keep your preferred order
df_plot$diet <- factor(df_plot$diet, levels = c("base", "base01"))

p <- ggplot(df_plot, aes(x = age, fill = diet)) +
  geom_histogram(bins = 20, position = "stack") +
  facet_wrap(~ gender) +
  labs(x = "Age (years)", y = "Count", fill = "Diet") +
  scale_fill_manual(
    values = c(
      base01   = "#F8766D",  # give base the OTHER group's old default color
      base = "#00BFC4"   # and base01 the OTHER one
    ),
    labels = c(
      base   = "veg*ans",
      base01 = "meat-consumers"
    )
  )

ggsave("/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/hist_age_diet_by_gender.png", plot = p,
       width = 10, height = 6, units = "in", dpi = 600)

range(df_plot$age, na.rm = TRUE)


# Leach mean mammal id
df <- df %>%
  mutate(leach_m = as.numeric(mammal_id))
mean_leach_m <- df %>% summarise(mean_leach_m = mean(leach_m, na.rm = TRUE))
mean_leach_m

# Leach mean group id
df <- df %>%
  mutate(leach_g = as.numeric(group_id))
mean_leach_g <- df %>% summarise(mean_leach_g = mean(leach_g, na.rm = TRUE))
mean_leach_g

# gSVO mean
df <- df %>%
  mutate(gSVO = as.numeric(gSVO_deg))
mean_gSVO <- df %>% summarise(mean_gSVO = mean(gSVO_deg, na.rm = TRUE))
mean_gSVO

# gSVO count
df %>%
  count(gSVO_trait, sort = TRUE, name = "n")

# iSVO mean
df <- df %>%
  mutate(iSVO = as.numeric(iSVO_deg))
mean_iSVO <- df %>% summarise(mean_iSVO = mean(iSVO_deg, na.rm = TRUE))
mean_iSVO

# iSVO count
df %>%
  count(iSVO_trait, sort = TRUE, name = "n")

# iSVO mammals mean
df <- df %>%
  mutate(iSVOm = as.numeric(iSVOm_deg))
mean_iSVOm <- df %>% summarise(mean_iSVOm = mean(iSVOm_deg, na.rm = TRUE))
mean_iSVOm

# iSVO mammals count
df %>%
  count(iSVOm_trait, sort = TRUE, name = "n")

#-------------------
#VEG (base)
df_base <- df %>%
  filter(tolower(trimws(as.character(QUESTNNR))) == "base") %>%
  mutate(
    age = as.numeric(A008_01),
    gender = factor(A007),
    manipulation = factor(A203)
  )

# gender count
df_base %>%
  count(A007, sort = TRUE, name = "n")

# age mean
mean_age <- df_base %>% summarise(mean_age = mean(age, na.rm = TRUE))
mean_age

# age per gender
df_base %>%
  group_by(gender) %>%
  summarise(
    n_age = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_age))

# conflict intensity binary count
df_base %>%
  count(A203, sort = TRUE, name = "n")

# Leach
stats_leach <- df_base %>%
  mutate(
    leach_m = as.numeric(mammal_id),
    leach_g = as.numeric(group_id)
  ) %>%
  summarise(
    mean_leach_m = mean(leach_m, na.rm = TRUE),
    sd_leach_m = sd(leach_m, na.rm = TRUE),
    mean_leach_g = mean(leach_g, na.rm = TRUE),
    sd_leach_g = sd(leach_g, na.rm = TRUE)
  )
stats_leach

# gSVO mean + SD
df_base <- df_base %>%
  mutate(gSVO = as.numeric(gSVO_deg))
stats_gSVO <- df_base %>%
  summarise(
    mean_gSVO = mean(gSVO, na.rm = TRUE),
    sd_gSVO = sd(gSVO, na.rm = TRUE)
  )
stats_gSVO
# gSVO count
df_base %>%
  count(gSVO_trait, sort = TRUE, name = "n")


# iSVO mean + SD
df_base <- df_base %>%
  mutate(iSVO = as.numeric(iSVO_deg))
stats_iSVO <- df_base %>%
  summarise(
    mean_iSVO = mean(iSVO, na.rm = TRUE),
    sd_iSVO = sd(iSVO, na.rm = TRUE)
  )
stats_iSVO
# iSVO count
df_base %>%
  count(iSVO_trait, sort = TRUE, name = "n")

# iSVO mammals mean + SD
df_base <- df_base %>%
  mutate(iSVOm = as.numeric(iSVOm_deg))
stats_iSVOm <- df_base %>%
  summarise(
    mean_iSVOm = mean(iSVOm, na.rm = TRUE),
    sd_iSVOm = sd(iSVOm, na.rm = TRUE)
  )
stats_iSVOm
# iSVO mammals count
df_base %>%
  count(iSVOm_trait, sort = TRUE, name = "n")

#--------------------------
#MEAT (base01)
df_base01 <- df %>%
  filter(tolower(trimws(as.character(QUESTNNR))) == "base01") %>%
  mutate(
    age = as.numeric(A008_01),
    gender = factor(A007),
    manipulation = factor(A203)
  )

# gender count
df_base01 %>%
  count(A007, sort = TRUE, name = "n")

# age mean
mean_age <- df_base01 %>% summarise(mean_age = mean(age, na.rm = TRUE))
mean_age

# age per gender
df_base01 %>%
  group_by(gender) %>%
  summarise(
    n_age = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_age))

# conflict intensity binary count
df_base01 %>%
  count(A203, sort = TRUE, name = "n")

# Leach
df_base01 <- df_base01 %>%
  mutate(leach_m = as.numeric(mammal_id))
stats_leach <- df_base01 %>%
  mutate(
    leach_m = as.numeric(mammal_id),
    leach_g = as.numeric(group_id)
  ) %>%
  summarise(
    mean_leach_m = mean(leach_m, na.rm = TRUE),
    sd_leach_m = sd(leach_m, na.rm = TRUE),
    mean_leach_g = mean(leach_g, na.rm = TRUE),
    sd_leach_g = sd(leach_g, na.rm = TRUE)
  )
stats_leach

# gSVO mean
df_base01 <- df_base01 %>%
  mutate(gSVO = as.numeric(gSVO_deg))
mean_gSVO <- df_base01 %>% summarise(mean_gSVO = mean(gSVO_deg, na.rm = TRUE),
                                     sd_gSVO = sd(gSVO_deg, na.rm = TRUE))
mean_gSVO

# gSVO count
df_base01 %>%
  count(gSVO_trait, sort = TRUE, name = "n")

# iSVO mean
df_base01 <- df_base01 %>%
  mutate(iSVO = as.numeric(iSVO_deg))
mean_iSVO <- df_base01 %>% summarise(mean_iSVO = mean(iSVO_deg, na.rm = TRUE),
                                     sd_iSVO = sd(iSVO_deg, na.rm = TRUE))
mean_iSVO

# iSVO count
df_base01 %>%
  count(iSVO_trait, sort = TRUE, name = "n")

# iSVO mammals mean
df_base01 <- df_base01 %>%
  mutate(iSVOm = as.numeric(iSVOm_deg))
mean_iSVOm <- df_base01 %>% summarise(mean_iSVOm = mean(iSVOm_deg, na.rm = TRUE),
                                      sd_iSVOm = sd(iSVOm_deg, na.rm = TRUE))
mean_iSVOm

# iSVO mammals count
df_base01 %>%
  count(iSVOm_trait, sort = TRUE, name = "n")

########-------------------
# mean numerical value from column TIME004, minimum and histogram

library(dplyr)
library(ggplot2)

df_time <- df %>%
  mutate(TIME004_num = as.numeric(TIME004))

# Mean + minimum (and n)
df_time %>%
  summarise(
    n = sum(!is.na(TIME004_num)),
    mean_TIME004 = mean(TIME004_num, na.rm = TRUE),
    min_TIME004  = min(TIME004_num, na.rm = TRUE)
  )

# Histogram
ggplot(df_time %>% filter(!is.na(TIME004_num)), aes(x = TIME004_num)) +
  geom_histogram(bins = 30) +
  labs(x = "TIME004", y = "Count")

# 10 quickest readers
df %>%
  mutate(TIME004_num = as.numeric(TIME004)) %>%
  filter(!is.na(TIME004_num)) %>%
  arrange(TIME004_num) %>%
  slice_head(n = 10) %>%
  select(CASE, TIME004_num, A011_RV1)

#------------------
# internal consistence of identification scales
library(readxl)
library(psych)

data <- read_excel(
  "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_solid.xlsx"
)

# Define item names
vegan_items <- sprintf("A101_%02d", 1:14)
meat_items  <- sprintf("A102_%02d", 1:14)
mammal_items <- sprintf("A103_%02d", 1:14)


# Function to calculate Cronbach's alpha
calculate_alpha <- function(data, items) {
  
  # Select the 14 relevant items
  x <- data[, items]
  
  # Remove participants who did not answer ANY item of this scale
  #x <- x[rowSums(!is.na(x)) > 0, ]
  
  # Make sure variables are numeric
  x[] <- lapply(x, as.numeric)
  
  # Calculate Cronbach's alpha
  psych::alpha(
    x,
    check.keys = FALSE
  )
}


# 1. Veg*ans identifying with their vegan/veg*an ingroup
alpha_vegan <- calculate_alpha(data, vegan_items)

# 2. Meat consumers identifying with their meat-consumer ingroup
alpha_meat <- calculate_alpha(data, meat_items)

# 3. Full sample identifying with mammals
alpha_mammal <- calculate_alpha(data, mammal_items)

#show
alpha_vegan$total
alpha_meat$total
alpha_mammal$total

#-----
#APA

#packageVersion("psych")
#citation("psych")
