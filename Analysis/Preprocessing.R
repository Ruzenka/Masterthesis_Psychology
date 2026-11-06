library(readxl)
library(dplyr)
library(writexl)
#citation("readxl")
#citation("dplyr")
#citation("writexl")
#version

input_file  <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_raw.xlsx"
output_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026.xlsx"

df <- read_excel(input_file)

# remove first 34 rows which were used for testing
df_2026 <- df %>%
  slice(-(1:33))

#remove when status was not != complete
df_2026 <- df_2026 %>%
  filter(!is.na(STATUS), tolower(trimws(as.character(STATUS))) == "complete")

# save to new Excel file
write_xlsx(df_2026, output_file)

#--------------------------
# add murphy SVO text information from new_file
base_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026.xlsx"
new_file  <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_raw_text.xlsx"
out_file  <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_substituted.xlsx"

# columns to substitute
cols <- c(
  "A326_01","A320_01","A307_01","A302_01","A321_01","A308_01","A303_01",
  "A322_01","A309_01","A304_01","A323_01","A310_01","A305_01","A324_01",
  "A311_01","A306_01","A325_01","A312_01"
)

# read data
base <- read_excel(base_file)
new  <- read_excel(new_file)

# keep only CASE + columns we need from the new file
new_keep <- new %>%
  select(CASE, all_of(cols))

# join and replace base columns with new values when they exist
result <- base %>%
  left_join(new_keep, by = "CASE", suffix = c("", ".new")) %>%
  mutate(across(all_of(cols), ~ coalesce(get(paste0(cur_column(), ".new")), .))) %>%
  select(-all_of(paste0(cols, ".new")))

# save
write_xlsx(result, out_file)


#----------------------
# split strings
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

in_file  <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_substituted.xlsx"
out_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_substituted_split.xlsx"

cols <- c(
  "A326_01","A320_01","A307_01","A302_01","A321_01","A308_01","A303_01",
  "A322_01","A309_01","A304_01","A323_01","A310_01","A305_01","A324_01",
  "A311_01","A306_01","A325_01","A312_01"
)

df <- read_excel(in_file)

# fucntion to extract numbers next to specific labels
get_num_after <- function(x, label) {
  # returns numeric after "label", e.g. "85 self 80 other" -> self = 85
  out <- str_match(x, paste0("(\\d+)\\s+", label))[ ,2]
  suppressWarnings(as.numeric(out))
}

df2 <- df %>%
  mutate(across(all_of(cols), as.character)) %>%  # ensure text
  rowwise() %>%
  mutate(
    # SELF/OTHER pairs
    across(all_of(cols),
           ~ get_num_after(.x, "self"),
           .names = "{.col}_self"),
    across(all_of(cols),
           ~ get_num_after(.x, "other"),
           .names = "{.col}_other"),
    
    # INGROUP/OUTGROUP pairs
    across(all_of(cols),
           ~ get_num_after(.x, "ingroup"),
           .names = "{.col}_ingroup"),
    across(all_of(cols),
           ~ get_num_after(.x, "outgroup"),
           .names = "{.col}_outgroup")
  ) %>%
  ungroup()

write_xlsx(df2, out_file)


#------------
# substract 50
in_file  <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_substituted_split.xlsx"
out_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_substituted_split_minus50.xlsx"

df2 <- read_excel(in_file) %>%
  mutate(across(
    matches("_(self|other|ingroup|outgroup)$"),
    ~ as.numeric(.) - 50
  ))

write_xlsx(df2, out_file)
#---------
#average
df2 <- df2 %>%
  mutate(
    iSVO_s  = rowMeans(across(c(A326_01_self, A302_01_self, A303_01_self, A304_01_self, A324_01_self, A325_01_self)), na.rm = TRUE),
    iSVO_o  = rowMeans(across(c(A326_01_other, A302_01_other, A303_01_other, A304_01_other, A324_01_other, A325_01_other)), na.rm = TRUE),
    
    iSVOm_s = rowMeans(across(c(A320_01_self, A321_01_self, A322_01_self, A323_01_self, A305_01_self, A306_01_self)), na.rm = TRUE),
    iSVOm_o = rowMeans(across(c(A320_01_other, A321_01_other, A322_01_other, A323_01_other, A305_01_other, A306_01_other)), na.rm = TRUE),
    
    gSVO_i  = rowMeans(across(c(A307_01_ingroup, A308_01_ingroup, A309_01_ingroup, A310_01_ingroup, A311_01_ingroup, A312_01_ingroup)), na.rm = TRUE),
    gSVO_o  = rowMeans(across(c(A307_01_outgroup, A308_01_outgroup, A309_01_outgroup, A310_01_outgroup, A311_01_outgroup, A312_01_outgroup)), na.rm = TRUE)
  )

write_xlsx(df2, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_averaged.xlsx")

#--------------
#ratio, atan, deg

df2 <- df2 %>%
  mutate(
    iSVO_ratio  = ifelse(is.na(iSVO_s)  | iSVO_s  == 0, NA, iSVO_o  / iSVO_s),
    iSVOm_ratio = ifelse(is.na(iSVOm_s) | iSVOm_s == 0, NA, iSVOm_o / iSVOm_s),
    gSVO_ratio  = ifelse(is.na(gSVO_i)  | gSVO_i  == 0, NA, gSVO_o  / gSVO_i)
  )

df2 <- df2 %>%
  mutate(
    iSVO_atan  = atan(iSVO_ratio),
    iSVOm_atan = atan(iSVOm_ratio),
    gSVO_atan  = atan(gSVO_ratio)
  )

df2 <- df2 %>%
  mutate(
    iSVO_deg  = iSVO_atan  * 180 / pi,
    iSVOm_deg = iSVOm_atan * 180 / pi,
    gSVO_deg  = gSVO_atan  * 180 / pi
  )


write_xlsx(df2, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_deg.xlsx")

#-------------
#word descriptions

c1 <- -12.04
c2 <-  22.45
c3 <-  57.15

df2 <- df2 %>%
  mutate(
    iSVO_trait = case_when(
      is.na(iSVO_deg)        ~ NA_character_,
      iSVO_deg <  c1         ~ "Competitive",
      iSVO_deg <  c2         ~ "Individualistic",
      iSVO_deg <  c3         ~ "Prosocial",
      TRUE                   ~ "Altruistic"
    ),
    iSVOm_trait = case_when(
      is.na(iSVOm_deg)       ~ NA_character_,
      iSVOm_deg < c1         ~ "Competitive",
      iSVOm_deg < c2         ~ "Individualistic",
      iSVOm_deg < c3         ~ "Prosocial",
      TRUE                   ~ "Altruistic"
    ),
    gSVO_trait = case_when(
      is.na(gSVO_deg)        ~ NA_character_,
      gSVO_deg <  c1         ~ "Strongly parochial",
      gSVO_deg <  c2         ~ "Weakly parochial",
      gSVO_deg <  c3         ~ "Universalistic",
      TRUE                   ~ "Xenial"
    )
  )
write_xlsx(df2, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_trait.xlsx")

#-----------------
#Leach score overall

df2 <- df2 %>%
  mutate(
    across(A101_01:A101_14, ~ as.numeric(.)),
    across(A102_01:A102_14, ~ as.numeric(.)),
    across(A103_01:A103_14, ~ as.numeric(.)),
    group_id_v = rowMeans(across(A101_01:A101_14), na.rm = TRUE),
    group_id_m = rowMeans(across(A102_01:A102_14), na.rm = TRUE),
    mammal_id = rowMeans(across(A103_01:A103_14), na.rm = TRUE)#,
    #meat_mammal_id = rowMeans(across(A103_01:A103_14), na.rm = TRUE)
  )
write_xlsx(df2, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_leach.xlsx")

#---------------
#delete quick performers

df2 <- df2 %>%
  filter(!A011_RV1 %in% c("667eba97a530c308f48c8846", "5bef64b373e44f0001092811"))
write_xlsx(df2, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_solid.xlsx")

#----------
#merge leach scales for dietary groups
input_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_solid.xlsx"
df2 <- read_excel(input_file)

df2 <- df2 %>%
  mutate(group_id = coalesce(group_id_m, group_id_v))
write_xlsx(df2, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_solid.xlsx")
write_xlsx(df2, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_preprocessed.xlsx")


#--------
# Exploratory analyses
#------
#which part of leach scales of dietary identification is most different bw meat eaters and non meat eaters

library(dplyr)
library(tidyr)
library(readxl)

input_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_solid.xlsx"
df <- read_excel(input_file)

dat_items <- df %>%
  mutate(
    meat_profile = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base"   ~ "nonmeat",
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~ "meat",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(meat_profile)) %>%
  mutate(
    across(A101_01:A101_14, as.numeric),
    across(A102_01:A102_14, as.numeric),
    
    item01 = ifelse(meat_profile == "nonmeat", A101_01, A102_01),
    item02 = ifelse(meat_profile == "nonmeat", A101_02, A102_02),
    item03 = ifelse(meat_profile == "nonmeat", A101_03, A102_03),
    item04 = ifelse(meat_profile == "nonmeat", A101_04, A102_04),
    item05 = ifelse(meat_profile == "nonmeat", A101_05, A102_05),
    item06 = ifelse(meat_profile == "nonmeat", A101_06, A102_06),
    item07 = ifelse(meat_profile == "nonmeat", A101_07, A102_07),
    item08 = ifelse(meat_profile == "nonmeat", A101_08, A102_08),
    item09 = ifelse(meat_profile == "nonmeat", A101_09, A102_09),
    item10 = ifelse(meat_profile == "nonmeat", A101_10, A102_10),
    item11 = ifelse(meat_profile == "nonmeat", A101_11, A102_11),
    item12 = ifelse(meat_profile == "nonmeat", A101_12, A102_12),
    item13 = ifelse(meat_profile == "nonmeat", A101_13, A102_13),
    item14 = ifelse(meat_profile == "nonmeat", A101_14, A102_14)
  ) %>%
  select(meat_profile, starts_with("item"))

item_diffs <- dat_items %>%
  pivot_longer(starts_with("item"), names_to = "item", values_to = "value") %>%
  group_by(item, meat_profile) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    n    = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = meat_profile, values_from = c(mean, sd, n)) %>%
  mutate(
    diff_mean = mean_nonmeat - mean_meat,
    abs_diff  = abs(diff_mean)
  ) %>%
  arrange(desc(abs_diff))

item_diffs

#----
#difference in mammal identification
library(readxl)
library(dplyr)
library(tidyr)

input_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_solid.xlsx"
df <- read_excel(input_file)

cols_a103 <- paste0("A103_", sprintf("%02d", 1:14))

rank_a103 <- df %>%
  mutate(
    meat_profile = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base"   ~ "nonmeat",
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~ "meat",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(meat_profile)) %>%
  mutate(across(all_of(cols_a103), as.numeric)) %>%
  pivot_longer(all_of(cols_a103), names_to = "item", values_to = "value") %>%
  group_by(item, meat_profile) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    n    = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = meat_profile, values_from = c(mean, sd, n)) %>%
  mutate(
    diff_mean = mean_nonmeat - mean_meat,
    abs_diff  = abs(diff_mean)
  ) %>%
  arrange(desc(abs_diff))

rank_a103

'for mammal identification it is solidarity, for identification with dietary groups it is centrality and satisfaction
both belong to self-investment'
# create a column sol_m which takes vlaues from A103_01	A103_02	A103_03 and creates a mean for each row.
# create a column cent_sat_g which takes vlaues from A101_04	to	A101_10 in base rows and A101_04	to	A101_10 in base01 rows, and creates a mean for each row.

df <- df %>%
  # make sure these columns are numeric
  mutate(
    across(A103_01:A103_03, as.numeric),
    across(A101_04:A101_10, as.numeric),
    across(A102_04:A102_10, as.numeric)
  ) %>%
  mutate(
    # Solidarity (mammals): mean of A103_01..A103_03
    sol_m = rowMeans(across(A103_01:A103_03), na.rm = TRUE),
    
    # Centrality+Satisfaction (group): take A101_04..A101_10 for base,
    # and A102_04..A102_10 for base01, then row mean
    cent_sat_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_04:A101_10), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_04:A102_10), na.rm = TRUE),
      TRUE ~ NA_real_
    )
  )

write_xlsx(df, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment.xlsx")

#--------
#create columns for self-investment identification - group and mammal
df <- df %>%
  # make sure these columns are numeric
  mutate(
    across(A103_01:A103_10, as.numeric),
    across(A101_01:A101_10, as.numeric),
    across(A102_01:A102_10, as.numeric)
  ) %>%
  mutate(
    inv_m = rowMeans(across(A103_01:A103_10), na.rm = TRUE),
    inv_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_01:A101_10), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_01:A102_10), na.rm = TRUE),
      TRUE ~ NA_real_
    )
  )
write_xlsx(df, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment_dimension.xlsx")

#------------
# create columns for self-definition - group and mammal
input_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment_dimension.xlsx"
df <- read_excel(input_file)
df <- df %>%
  # make sure these columns are numeric
  mutate(
    across(A103_11:A103_14, as.numeric),
    across(A101_11:A101_14, as.numeric),
    across(A102_11:A102_14, as.numeric)
  ) %>%
  mutate(
    def_m = rowMeans(across(A103_11:A103_14), na.rm = TRUE),
    def_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_11:A101_14), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_11:A102_14), na.rm = TRUE),
      TRUE ~ NA_real_
    )
  )
write_xlsx(df, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment_definition_dimension.xlsx")

#---------------------
#create columns for all components of all dimensions for both mammal and group identification
input_file <- "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment.xlsx"
df <- read_excel(input_file)
# create a column sol_m which takes vlaues from A103_01	A103_02	A103_03 and creates a mean for each row.
# create a column sat_m which takes vlaues from A103_04	A103_05	A103_06 A103_07 and creates a mean for each row.
# create a column cent_m which takes vlaues from A103_08	A103_09	A103_10 and creates a mean for each row.
# create a column st_m which takes vlaues from A103_11	A103_12 and creates a mean for each row.
# create a column homo_m which takes vlaues from A103_13	A103_14 and creates a mean for each row.
# create a column sol_g which takes vlaues from A101_01	to	A101_03 in base rows and A102_01	to	A102_03 in base01 rows, and creates a mean for each row.
# create a column sat_g which takes vlaues from A101_04	to	A101_07 in base rows and A102_04	to	A102_07 in base01 rows, and creates a mean for each row.
# create a column cent_g which takes vlaues from A101_08	to	A101_10 in base rows and A102_08	to	A102_10 in base01 rows, and creates a mean for each row.
# create a column st_g which takes vlaues from A101_11	to	A101_12 in base rows and A102_11	to	A102_12 in base01 rows, and creates a mean for each row.
# create a column homo_g which takes vlaues from A101_13	to	A101_14 in base rows and A102_13	to	A102_14 in base01 rows, and creates a mean for each row.

df <- df %>%
  # make sure these columns are numeric
  mutate(
    across(A103_01:A103_14, as.numeric),
    across(A101_01:A101_14, as.numeric),
    across(A102_01:A102_14, as.numeric)
  ) %>%
  mutate(
    sol_m = rowMeans(across(A103_01:A103_03), na.rm = TRUE),
    sat_m = rowMeans(across(A103_04:A103_07), na.rm = TRUE),
    cent_m = rowMeans(across(A103_08:A103_10), na.rm = TRUE),
    st_m = rowMeans(across(A103_11:A103_12), na.rm = TRUE),
    homo_m = rowMeans(across(A103_13:A103_14), na.rm = TRUE),
    
    sol_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_01:A101_03), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_01:A102_03), na.rm = TRUE),
      TRUE ~ NA_real_),
    sat_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_04:A101_07), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_04:A102_07), na.rm = TRUE),
      TRUE ~ NA_real_),
    cent_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_08:A101_10), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_08:A102_10), na.rm = TRUE),
      TRUE ~ NA_real_),
    st_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_11:A101_12), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_11:A102_12), na.rm = TRUE),
      TRUE ~ NA_real_),
    homo_g = case_when(
      tolower(trimws(as.character(QUESTNNR))) == "base" ~
        rowMeans(across(A101_13:A101_14), na.rm = TRUE),
      tolower(trimws(as.character(QUESTNNR))) == "base01" ~
        rowMeans(across(A102_13:A102_14), na.rm = TRUE),
      TRUE ~ NA_real_
    )
  )

write_xlsx(df, "/Users/ruzenkakaldenbach/Desktop/Psychologie_Masterarbeit/Datasets/data_2026_investment.xlsx")


