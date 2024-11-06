library(dplyr)
head(starwars)
starwars_mass_summary <- function(group_var) {
  starwars |>
    group_by({{group_var}}) |>
    summarize(
      mean_mass = mean(.data$mass, na.rm = TRUE),
      sd_mass = sd(.data$mass, na.rm = TRUE)
    )
}
starwars_mass_summary(gender)

height_sum <- function(data, group_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}

height_sum(starwars, hair_color)

height_sum <- function(data, ...) {
  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}
#use_import_from(“rlang”, “:=“)
height_sum(starwars, hair_color, eye_color)

var_summary <- function(data, var) {
  data |>
    summarise(
      "{{var}}_min" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

sw_summary <- function(df, var, ...){
  df |>
    group_by(...) |>
    summarise(
      "{{var}}_median" := median({{ var }}, na.rm = TRUE)
    )
}
sw_summary(starwars,eye_color,red)

starwars_dynamic_summary <- function(...) {
  starwars |>
    summarize(
      across(
        c(...),
        .fns = list(mean = mean, min = min),
        .names = "{.col}_{.fn}"
      )
    )
}
summy <- function(df, group_var, cols) {
  df |>
    group_by({{ group_var }}) |>
    summarise(
      across({{ cols }}, .fns = list(min = min, max = max))
    )
}

mtcars |>
  summy(cyl, c(mpg, disp))

mtcars |>
  summy(cyl, starts_with("mp"))

mtcars |>
  summy(cyl, where(is.numeric))
