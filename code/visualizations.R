library(ggplot2)
library(patchwork)
library(dplyr)
library(cowplot)

RMSE <- c(rmse_u_te, rmse_un_te, rmse_f_te, rmse_det_te)
Model <- c("Full", "Unaware", "Fair K", "Fair Add")
RMSE.Table <- data.frame(Model,RMSE)

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
  ggplot(data, aes(x = predictions, fill = as.factor(female))) +
    geom_boxplot() +
    theme_bw() + 
    guides(fill = guide_legend(title = "Gender")) + 
    xlim(-1.25,0.75) +
    labs(title = NULL, ylab = NULL, xlab = NULL) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 13, face = "bold"),
          title = element_text(size = 14, face = "bold")) +
    scale_fill_manual(name = "Gender",
                      values = c( "1" = "#D55E00", "0" = "#0072B2"),
                      labels = c("1" = "female", "0" = "male"),
                      breaks = c("1", "0"))
}

# Boxplots Race
race.plot.fun <- function(data) { 
  ggplot(data, aes(x = predictions, fill = as.factor(combined_column))) +
    geom_boxplot() +
    theme_bw() + 
    xlim(-1.25, 0.75) +
    guides(fill = guide_legend(title = "Race")) +
  labs(title = NULL, ylab = NULL, xlab = NULL) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 13, face = "bold"),
          title = element_text(size = 14, face = "bold")) + 
    scale_fill_manual(name = "Race",
                      values = c("0100" = "#D55E00", "00asian0" = "#0072B2",
                                  "black000" = "#009E73", "0000" = "#999999", 
                                  "000mexican" =  "#CC79A7"),
                      labels = c("0100" = "white", "00asian0" = "asian",
                                 "black000" = "black", "0000" = "other", 
                                 "000mexican" =  "mexican"),
                      breaks = c("black000","0100", "00asian0", "000mexican", "0000"))
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
  plot_layout(ncol = 2, guides = "collect")

combined.plot.gender <- patchwork::patchworkGrob(combined.plot.gender)
gridExtra::grid.arrange(combined.plot.gender,
                        bottom = textGrob(bquote(bold(widehat(ZFYA))),
                                          gp = gpar(fontsize = 14)))


combined.plot.race <- plot5 + plot6 + plot7 + plot8 + 
  plot_layout(ncol = 2, guides = "collect")

combined.plot.race <- patchwork::patchworkGrob(combined.plot.race)
gridExtra::grid.arrange(combined.plot.race, 
                        bottom = textGrob(bquote(bold(widehat(ZFYA))),
                                          gp = gpar(fontsize = 14)))


