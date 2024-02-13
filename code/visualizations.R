library(ggplot2)
library(patchwork)
library(dplyr)
library(cowplot)

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
    guides(fill = "none") + 
    xlim(-1.25,0.75) +
    labs(title = NULL, ylab = "prediciton", xlab = "gender") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(labels = c("male", "female"))
    
}

# Boxplots Race
race.plot.fun <- function(data) { 
  ggplot(data, aes(x = predictions, y = as.factor(combined_column), fill = as.factor(combined_column))) +
    geom_boxplot() +
    theme_bw() + 
    coord_flip() +
    xlim(-1.25,0.75) +
    guides(fill = "none") + 
  labs(title = NULL, ylab = "prediciton", xlab = "race") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(labels = c("other", "mexican", "asian", "white", "black"))
}


data.full <- data.fun("Full", data.te)
data.unaware <- data.fun("Unaware", data.te)
data.K <- data.fun("FairK", data.te)
data.Add <- data.fun("FairAdd", data.te)

plot1 <- gender.plot.fun(data.full) + labs(title = "Full") 
plot2 <- gender.plot.fun(data.unaware) + labs(title = "Unaware") 
plot3 <- gender.plot.fun(data.K) + labs(title = "Fair K")
plot4 <- gender.plot.fun(data.Add) + labs(title = "Fair Add") 

plot5 <- race.plot.fun(data.full) + labs(title = "Full")
plot6 <- race.plot.fun(data.unaware) + labs(title = "Unaware")
plot7 <- race.plot.fun(data.K) + labs(title = "Fair K")
plot8 <- race.plot.fun(data.Add) + labs(title = "Fair Add")

combined.plot.gender <- plot1 + plot2 + plot3 + plot4 +
  plot_layout(ncol = 2) 
combined.plot.gender <- patchwork::patchworkGrob(combined.plot.gender)
gridExtra::grid.arrange(combined.plot.gender, left = "Predictions", bottom = "Gender")

combined.plot.race <- plot5 + plot6 + plot7 + plot8 + 
  plot_layout(ncol = 2)
combined.plot.race <- patchwork::patchworkGrob(combined.plot.race)
gridExtra::grid.arrange(combined.plot.race, left = "Predictions", bottom = "Race")

ggsave("Boxplot.Gender", plot = combined.plot.gender, device = "PNG", path = "../../plots", width = 160, height = 90, units = "mm")


