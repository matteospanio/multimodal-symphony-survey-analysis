# define the columns of interest
cols_task1 <- c(
  "rng_model_label_1_1", "random_taste_label_1_1", "task1_1_1",
  "rng_model_label_2_1", "random_taste_label_2_1", "task1_2_1",
  "rng_model_label_3_1", "random_taste_label_3_1", "task1_3_1",
  "rng_model_label_4_1", "random_taste_label_4_1", "task1_4_1",
  "rng_model_label_5_1", "random_taste_label_5_1", "task1_5_1",
  "random_number_1_1", "anagrafica_1", "hearing_experience_1", "eating_experience_1"
)

cols_task2 <- expand_grid(y = 1:12, x = 1:3) |>
  mutate(column_name = sprintf("task_2_%d_%d", x, y)) |>
  pull(column_name) |>
  append(c("song_1_id", "song_2_id", "song_3_id", "taste_1", "taste_2", "taste_3"))

cols_aov <- cols_task2 |> append(c("hearing_experience", "sex", "eating_experience"))

cols_pinfo <- c(
  "anagrafica_1", "etnia_1", "age_1", "hearing_experience_1",
  "eating_experience_1", "hearing_impairments_1", "taste_impairments_1",
  "device_1", "TIME_total"
)

ethnics <- c(
  "American Indian, Alaska Native or Indigenous",
  "Arab American, Middle Eastern, or North African",
  "Asian or Asian American",
  "Black or African American",
  "Bi/Multi-racial",
  "Latino/a/x or Spanish Origin",
  "Native Hawaiian or Other Pacific Islander",
  "Southeast Asian",
  "White/European American",
  "Not listed",
  "Prefer not to say"
)

#' @title Select columns of interest
#' @description This function selects the columns of interest from the raw data and renames them according to the task 2 requirements.
choose_cols_aov <- function(data, columns=cols_aov) {
  data |>
    rename(
      hearing_experience = hearing_experience_1,
      eating_experience = eating_experience_1,
      sex = anagrafica_1,
    ) |>
    mutate(
      taste_1 = random_taste_label_1_1,
      song_1_id = random_number_4_1,
      taste_2 = random_taste_label_3_1,
      song_2_id = random_number_5_1,
      taste_3 = random_taste_label_4_1,
      song_3_id = random_number_1_1,
      hearing_experience = factor(hearing_experience, levels=1:3, labels=c("Professional", "Amateur", "Not-experienced")),
      eating_experience = factor(eating_experience, levels=1:3, labels=c("Professional", "Amateur", "Not-experienced")),
      sex = factor(sex, levels=1:4, labels=c("Male", "Female", "Other", "Not specified")),
    ) |>
    select(all_of(columns)) |>
    drop_na()
}

#' @title Rename task 2 columns
#' @description Rename the columns of the task 2 data according to the task 2 requirements.
rename_task2 <- function(data, id, new_label = "value_", for_aov=FALSE) {
  label <- sprintf("task_2_%d_", id)
  song_id_str <- sprintf("song_%d_id", id)
  taste_str <- sprintf("taste_%d", id)

  if (for_aov) {
    selections <- data |> select(starts_with(label) | starts_with(song_id_str) | starts_with(taste_str) | starts_with("hearing") | starts_with("sex") | starts_with("eating"))
  } else {
    selections <- data |> select(starts_with(label) | starts_with(song_id_str) | starts_with(taste_str))
  }

  selections |>
    rename_with(~ paste0(new_label, seq_along(.)), starts_with(label)) |>
    rename(
      song_id = starts_with(song_id_str),
      taste = starts_with(taste_str)
    ) |>
    rename(
      salty = value_1,
      sweet = value_2,
      bitter = value_3,
      sour = value_4,
      happy = value_5,
      sad = value_6,
      anger = value_7,
      disgust = value_8,
      fear = value_9,
      surprise = value_10,
      hot = value_11,
      cold = value_12
    )
}

#' @title Rearrange data
#' @description This function rearranges the data from wide to long format.
#' @param df A data frame.
#' @param columns A tidy-select expression to select the columns to pivot.
#' @return A data frame in long format.
rearrange_data <- function(df, columns) {
  df |>
    pivot_longer(
      cols = columns,
      names_to = "adjective",
      values_to = "value"
    )
}
