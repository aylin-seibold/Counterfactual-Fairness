library(ggplot2)
library(gridExtra)
library(dplyr)

# Data Manipulation
## This function connects the test data with the basic variables with the 
## prediction for every model.

data.te <- X_U_TE
data.fun <- function(model, data) {
  if (model == "Full") {
    data$predictions <- pred_u_te
  } else if (model == "Unaware") {
    data$predictions <- pred_un_te
  } else if (model == "FairK") {
    data$predictions <- pred_f_te
  } else if (model == "FairAdd") {
    data$predictions <- pred_det_te
  }
  data <- data %>% 
    mutate(black = ifelse(black == 1, "black", black)) %>%
    mutate(asian = ifelse(asian == 1, "asian", asian)) %>%
    mutate(mexican = ifelse(mexican == 1, "mexican", mexican))
  data <- data %>%
    mutate(combined_column = paste0(black, white, asian, mexican))
  return(data)
}

data.full <- data.fun("Full", data.te)
data.unaware <- data.fun("Unaware", data.te)
data.K <- data.fun("FairK", data.te)
data.Add <- data.fun("FairAdd", data.te)

# Boxplots Gender
gender.plot.fun <- function(data) {
  ggplot(data, aes(x = predictions, y = as.factor(female), fill = as.factor(female))) +
    geom_boxplot() +
    theme_bw() + 
    coord_flip() +
    labs(title = NULL,
         x = "Predicted ZFYA",
         y = "Gender")  +
    guides(fill = "none")
}

# Boxplots Race
race.plot.fun <- function(data) { 
  ggplot(data, aes(x = predictions, y = as.factor(combined_column), fill = as.factor(combined_column))) +
    geom_boxplot() +
    theme_bw() + 
    coord_flip() +
    labs(title = NULL,
         x = "Predicted ZFYA",
         y = "Race")  +
    guides(fill = "none") 
}


data.full <- data.fun("Full", data.te)
data.unaware <- data.fun("Unaware", data.te)
data.K <- data.fun("FairK", data.te)
data.Add <- data.fun("FairAdd", data.te)

race.plot.fun(data.full)
race.plot.fun(data.unaware)
race.plot.fun(data.K)
race.plot.fun(data.Add)

gender.plot.fun(data.full)
gender.plot.fun(data.unaware)
gender.plot.fun(data.K)
gender.plot.fun(data.Add)
