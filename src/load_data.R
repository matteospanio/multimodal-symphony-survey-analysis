library(tidyverse)
source("src/utils.R") # load a set of custom functions and constants

url <- "https://files.de-1.osf.io/v1/resources/sfe2x/providers/osfstorage/679fe1264be2e2288f468d54"
tmp_data <- tempfile(fileext = ".csv")
download.file(url, tmp_data, method = "auto", mode = "wb")
data_raw <- read_csv(tmp_data, show_col_types = FALSE)

# select the portion of data related to survey's first task
data_task1 <- data_raw |>
  select(all_of(cols_task1)) |>
  drop_na() |>
  mutate(across(contains("label"), as.factor)) |>
  # rename columns with names that make sense to the analysis
  rename(
    seed = random_number_1_1,
    lmodel1 = rng_model_label_1_1,
    lmodel2 = rng_model_label_2_1,
    lmodel3 = rng_model_label_3_1,
    lmodel4 = rng_model_label_4_1,
    lmodel5 = rng_model_label_5_1,
    taste1 = random_taste_label_1_1,
    taste2 = random_taste_label_2_1,
    taste3 = random_taste_label_3_1,
    taste4 = random_taste_label_4_1,
    taste5 = random_taste_label_5_1,
    answer1 = task1_1_1,
    answer2 = task1_2_1,
    answer3 = task1_3_1,
    answer4 = task1_4_1,
    answer5 = task1_5_1,
    sex = anagrafica_1,
    hearing_experience = hearing_experience_1,
    eating_experience = eating_experience_1,
  ) |>
  # we just need to know which model generated the left song
  # then, on the opposite side, there will be the other model
  pivot_longer(
    cols = starts_with(c("lmodel", "taste", "answer")),
    names_to = c(".value", "set"),
    names_pattern = "(\\D+)(\\d+)"
  ) |>
  mutate(
    seed = (seed + as.integer(set) - 1) %% 25, # compute the song id
    # we move the fine tuned model to the right side so that the left side is always the baseline
    # therefore scores near to 0 favor the base model while scores near to 10 favor the fine tuned model
    answer = if_else(lmodel == "finetuned", 10 - answer, answer),
    hearing_experience = factor(hearing_experience, levels=1:3, labels=c("Professional", "Amateur", "Not-experienced")),
    eating_experience = factor(eating_experience, levels=1:3, labels=c("Professional", "Amateur", "Not-experienced")),
    sex = factor(sex, levels=1:4, labels=c("Male", "Female", "Other", "Not specified")),
  ) |>
  # then organize the data in a more readable way
  rename(score=answer) |>
  select(taste, score, seed, sex, eating_experience, hearing_experience) |>
  arrange(taste, seed) |>
  group_by(taste) |>
  rename(prompt=taste)

.data_task2 <- data_raw |>
  mutate(
    taste_1 = random_taste_label_1_1,
    song_1_id = random_number_4_1,
    taste_2 = random_taste_label_3_1,
    song_2_id = random_number_5_1,
    taste_3 = random_taste_label_4_1,
    song_3_id = random_number_1_1,
  ) |>
  select(all_of(cols_task2)) |>
  drop_na()

# data frame of values related to survey's second task
data_task2 <- bind_rows(
    rename_task2(.data_task2, 1),
    rename_task2(.data_task2, 2),
    rename_task2(.data_task2, 3)
  ) |>
  arrange(taste, song_id) |>
  mutate(taste = factor(taste)) |>
  select(-song_id) |>
  rename(prompt=taste)

# data frame of demographic information
data_pinfo <- data_raw |>
  select(all_of(cols_pinfo)) |>
  drop_na() |>
  rename(
    sex = anagrafica_1,
    ethnicity = etnia_1,
    age = age_1,
    hearing_experience = hearing_experience_1,
    eating_experience = eating_experience_1,
    hearing_impairments = hearing_impairments_1,
    taste_impairments = taste_impairments_1,
    device = device_1,
    time = TIME_total
  ) |>
  mutate(
    sex = factor(sex, levels=1:4, labels=c("Male", "Female", "Other", "Not specified")),
    hearing_impairments = factor(hearing_impairments, levels=1:2, labels=c("Yes", "No")),
    taste_impairments = factor(taste_impairments, levels=1:2, labels=c("Yes", "No")),
    device = factor(device, levels=1:4, labels=c("Headphones", "Speakers", "HiFi system", "Other")),
    ethnicity = factor(ethnicity, levels=1:11, labels=ethnics),
    hearing_experience = factor(hearing_experience, levels=1:3, labels=c("Professional", "Amateur", "Not-experienced")),
    eating_experience = factor(eating_experience, levels=1:3, labels=c("Professional", "Amateur", "Not-experienced")),
    age = as.numeric(age),
    time = as.numeric(time)
  )

data_aov <- bind_rows(
    rename_task2(choose_cols_aov(data_raw), 1, for_aov=TRUE),
    rename_task2(choose_cols_aov(data_raw), 2, for_aov=TRUE),
    rename_task2(choose_cols_aov(data_raw), 3, for_aov=TRUE)
  ) |>
  arrange(taste, song_id) |>
  mutate(taste = factor(taste)) |>
  select(-song_id) |>
  rename(prompt=taste)

data_aov <- rearrange_data(data_aov, all_of(c("salty", "sweet", "bitter", "sour", "happy", "sad", "anger", "disgust", "fear", "surprise", "hot", "cold")))
