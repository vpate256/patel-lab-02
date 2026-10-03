library(tidyverse)
library(psych)
library(readxl)
library(gt)
library(janitor)

data<-read_excel("./Attachment_Anxiety_Data.xlsx")

head(data)

anx.clean <- data |>
  mutate(
    Gender = case_when(
      Gender == 1 ~ "Male",
      Gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) |> 
  mutate(
    Ethnicity = case_when(
      Ethnicity == 1 ~ "White/Caucasian",
      Ethnicity == 2 ~ "Asian/Asian British",
      Ethnicity == 5 ~ "Other ethnic group",
      Ethnicity == 6 ~ "Prefer not to say",
      TRUE ~ NA_character_
    )
  )

anx.clean <- anx.clean |>
  mutate(
    across(
      c(starts_with("SA_"),
        starts_with("AA_"),
        starts_with("SEst_")),
      as.integer
    )
  )

SA.keys <- list(
  SocialAnxiety = c(
    "SA_1", "SA_2", "SA_3", "SA_4", "-SA_5",
    "SA_6", "SA_7", "SA_8", "-SA_9", "SA_10",
    "-SA_11", "SA_12", "SA_13", "SA_14", "SA_15",
    "SA_16", "SA_17", "SA_18", "SA_19", "SA_20"
  )
)

AA.keys <- list(
  AttachmentAnxiety = c(
    "AA_1", "AA_2", "AA_3", "AA_4", "AA_5",
    "AA_6", "AA_7", "AA_8", "AA_9"
  )
)

SEst.keys <- list(
  SelfEsteem = c(
    "SEst_1", "-SEst_2", "SEst_3", "SEst_4", "-SEst_5",
    "-SEst_6", "SEst_7", "-SEst_8", "-SEst_9", "SEst_10"
  )
)


# Social Interaction Anxiety Scale
# Minimum = 0, Maximum = 4
SA.scores <- scoreItems(
  SA.keys,
  anx.clean,
  totals = FALSE,
  min = 0,
  max = 4
)

# Cronbach's alpha
SA.scores$alpha

# Convert average scores to dataframe
SA.scores.df <- as.data.frame(SA.scores$scores)


# Experiences in Close Relationships Anxiety Subscale
# Minimum = 1, Maximum = 7
AA.scores <- scoreItems(
  AA.keys,
  anx.clean,
  totals = FALSE,
  min = 1,
  max = 7
)

# Cronbach's alpha
AA.scores$alpha

# Convert average scores to dataframe
AA.scores.df <- as.data.frame(AA.scores$scores)


# Rosenberg Self-Esteem Scale
# Minimum = 1, Maximum = 4
SEst.scores <- scoreItems(
  SEst.keys,
  anx.clean,
  totals = FALSE,
  min = 1,
  max = 4
)

# Cronbach's alpha
SEst.scores$alpha

# Convert average scores to dataframe
SEst.scores.df <- as.data.frame(SEst.scores$scores)

allscores.df <- cbind(
  SA.scores.df,
  AA.scores.df,
  SEst.scores.df
)

head(allscores.df)
anx.clean <- cbind(anx.clean, allscores.df)
head(anx.clean)

anx.final <- anx.clean |>
  select(
    Gender,
    Ethnicity,
    SocialAnxiety,
    AttachmentAnxiety,
    SelfEsteem
  ) |>
  mutate_if(is.numeric, round, 2)

head(anx.final)

#Part 3 - Tables

# Create a demographics table of ethnicity by gender
demographictable <- anx.final |>
  tabyl(Ethnicity, Gender)

# Create reproducible gt table
gttable.1 <- demographictable |>
  gt() |>
  tab_header(
    title = md("*Distribution of Gender by Ethnicity*")
  )

gttable.1


# Round Cronbach's alpha values to 2 decimal places
alphas <- round(c(
  SA.scores$alpha,
  AA.scores$alpha,
  SEst.scores$alpha
), 2)

# Summary statistics for the three scale scores
summary <- describe(
  select(
    anx.final,
    SocialAnxiety,
    AttachmentAnxiety,
    SelfEsteem
  )
)

summary
n <- nrow(anx.final)

# Scale titles
Scales <- c(
  "Social Interaction Anxiety Scale",
  "Experiences in Close Relationships Anxiety Subscale",
  "Rosenberg Self-Esteem Scale"
)

# Add scale names to summary table
summary$Scales <- Scales


# Select required descriptive statistics
summary <- summary |>
  select(
    Scales,
    mean,
    median,
    sd,
    range
  ) |>
  mutate_if(is.numeric, round, 2)

alphas.df <- data.frame(alpha = alphas)

final.reptable <- cbind(
  summary,
  alphas.df
)

gttable.2 <- final.reptable |>
  gt() |>
  tab_header(
    title = md("*Descriptive Statistics and Reliability for Anxiety and Self-Esteem Scales*"),
    subtitle = paste0("N = ", n)
  ) |>
  tab_stubhead(
    label = "Scales"
  ) |>
  tab_spanner(
    label = "Central Tendency",
    columns = c(mean, median)
  ) |>
  tab_spanner(
    label = "Variability",
    columns = c(sd, range)
  ) |>
  tab_spanner(
    label = "Reliability",
    columns = c(alpha)
  )

gttable.2

anx.filtered <- anx.final |>
  filter(Gender == "Female")

# Recalculate Cronbach's alpha for females only

SA.scores.filtered <- scoreItems(
  SA.keys,
  anx.clean |> filter(Gender == "Female"),
  totals = FALSE,
  min = 0,
  max = 4
)

AA.scores.filtered <- scoreItems(
  AA.keys,
  anx.clean |> filter(Gender == "Female"),
  totals = FALSE,
  min = 1,
  max = 7
)

SEst.scores.filtered <- scoreItems(
  SEst.keys,
  anx.clean |> filter(Gender == "Female"),
  totals = FALSE,
  min = 1,
  max = 4
)

alphas_filtered <- round(c(
  SA.scores.filtered$alpha,
  AA.scores.filtered$alpha,
  SEst.scores.filtered$alpha
), 2)

summary_filtered <- describe(
  select(
    anx.filtered,
    SocialAnxiety,
    AttachmentAnxiety,
    SelfEsteem
  )
)

n_filtered <- nrow(anx.filtered)

summary_filtered$Scales <- c(
  "Social Interaction Anxiety Scale",
  "Experiences in Close Relationships Anxiety Subscale",
  "Rosenberg Self-Esteem Scale"
)

summary_filtered <- summary_filtered |>
  select(
    Scales,
    mean,
    median,
    sd,
    range
  ) |>
  mutate_if(is.numeric, round, 2)

alphas_filtered.df <- data.frame(
  alpha = alphas_filtered
)

filtered.reptable <- cbind(
  summary_filtered,
  alphas_filtered.df
)

gttable.3 <- filtered.reptable |>
  gt() |>
  tab_header(
    title = md("*Descriptive Statistics and Reliability for Female Participants*"),
    subtitle = paste0("N = ", n_filtered)
  ) |>
  tab_spanner(
    label = "Central Tendency",
    columns = c(mean, median)
  ) |>
  tab_spanner(
    label = "Variability",
    columns = c(sd, range)
  ) |>
  tab_spanner(
    label = "Reliability (Cronbach's alpha)",
    columns = c(alpha)
  )

gttable.3



