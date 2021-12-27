library(tidyverse)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(lme4)
library(lattice)


# input data
exam <- read.table("Data/Exam.txt", header = F)
colnames(exam) <- c("SchoolId", "StudentId", "Score", "Const", 
                    "STANDLRT", "StudentGender", "SchoolGender", "SchoolIntakeScore", 
                    "VRBand", "IntakeBand")
exam <- exam %>% 
  dplyr::select(-Const) %>%
  mutate(SchoolId = as.factor(SchoolId),
         StudentId = as.factor(StudentId),
         StudentGender = as.factor(StudentGender),
         SchoolGender = as.factor(SchoolGender),
         VRBand = as.factor(VRBand),
         IntakeBand = as.factor(IntakeBand))


# print model result
print_mod <- function(level, STANDLRT=F, StudentGender=F, SchoolGender=F,
                      SchoolIntakeScore=F, VRBand=F, IntakeBand=F) {
  
  mod_data <- exam
  var_list <- c(STANDLRT, StudentGender, SchoolGender,
                SchoolIntakeScore, VRBand, IntakeBand)
  var_name_list <- c("STANDLRT", "StudentGender", "SchoolGender",
                     "SchoolIntakeScore", "VRBand", "IntakeBand")
  remove_list <- c()
  
  for (i in 1:length(var_list)) {
    if (var_list[i] == F) {
      remove_list <- c(remove_list, i+3)
    }
  }
  
  mod_data <- mod_data[, -remove_list]
  
  if (level == "Pooled") {
    mod_data <- mod_data[, -(1:2)]
    mod <- lm(Score ~ ., data = mod_data)
  } else if (level == "Unpooled"){
    mod_data <- mod_data[, -2]
    mod <- lm(Score ~ ., data = mod_data)
  } else {
    mod_data <- mod_data[, -2]
    mod <- lmer(Score ~ . + (1 | SchoolId) - SchoolId, data = mod_data)
  }
  
  return (summary(mod))
}


# model interpretation
## final model
final_model <- lmer(Score ~ (1 | SchoolId) + STANDLRT + StudentGender 
                    + SchoolIntakeScore + IntakeBand, data = exam)

view_coef <- function(model) {

  a <- summary(model)$coefficients %>%
    as.data.frame() %>% 
    mutate(`2.5%` = Estimate - 1.96 * `Std. Error`,
           `97.5%` = Estimate + 1.96 * `Std. Error`) %>%
    mutate(across(everything(),~ round(., 4))) %>%
    mutate(Coeffecient = rownames(.)) %>%
    relocate(Coeffecient, .before=Estimate) %>%
    relocate(`2.5%`, .before = `t value`) %>%
    relocate(`97.5%`, .before = `t value`)
  
  # print(a,digits =4)
    # kable(caption = "Estimates of fixed effects",
    #       digits = 4)
    # kable_classic(full_width=FALSE)
  
  return(a)
}



plot_random <- function(model) {
  return(dotplot(ranef(final_model, condVar = TRUE))$SchoolId)
}

tbl_random <- function(model) {
  ranef(model, condVar = TRUE) %>%
    as.data.frame() %>%
    mutate(condval = round(condval, 4),
           condsd = round(condsd, 4)) %>%
    arrange(desc(condval)) %>%
    mutate(`2.5%` = round(condval - 1.96 * condsd, 4),
           `97.5%` = round(condval + 1.96 * condsd, 4))
    
}



plot_qq <- function(model) {
  df <- data.frame(
    res=residuals(model, scaled=TRUE)
  )
  
  p <- ggplot(df, aes(sample=res)) + 
    stat_qq(
      size=0.75
    ) + 
    stat_qq_line(
      linetype='dashed',
      color='red',
      size=0.5
    ) +
    labs(
      x='Theoretical Quantiles',
      y='Standardized Residuals'
    ) + 
    ggtitle("Normal QQ for Residuals") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}


plot_ranef_qq <- function(model) {
  df <- data.frame(
    res=ranef(model)[[1]][[1]]
  )
  
  p <- ggplot(df, aes(sample=res)) + 
    stat_qq(
      size=0.5
    ) + 
    stat_qq_line(
      linetype='dashed',
      color='red',
      size=0.5
    ) +
    labs(
      x='Theoretical Quantiles',
      y='State Intercept'
    ) + 
    ggtitle("Normal QQ for Random Effects") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

plot_res_fit <- function(model) {
  df <- data.frame(
    res=residuals(model),
    fit=fitted(model)
  )
  
  p <- ggplot(df, aes(x=fit, y=res)) +
    geom_point(
      size=0.75
    ) +
    geom_hline(
      yintercept=0,
      linetype="dashed"
    ) +
    geom_smooth() +
    labs(
      x='Fitted',
      y='Residuals'
    ) + 
    ggtitle("Residuals vs. Fitted") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}



plot_cooks_distance <- function(model){
  data <- model.frame(model)
  cutoff <- 4/length(unique(data$SchoolId))
  cooks_dist <- data.frame(cooksd = cooks.distance(influence(model, "SchoolId"))) %>%
    mutate(SchoolId = unique(data$SchoolId)) %>%
    arrange(desc(cooksd)) %>%
    mutate(influential = cooksd > cutoff)
  p <- cooks_dist %>%
    ggplot(aes(x = reorder(SchoolId, cooksd), y = cooksd, color = influential)) + 
    geom_point() + 
    scale_color_manual("Influential", values = c("black", "red")) +
    geom_abline(slope = 0, intercept = cutoff, color = "red", linetype = "dashed") + 
    ylab("Cook's Distance") +
    xlab("Lab") +
    coord_flip() +
    ggtitle("Cook's Distance across school") +
    theme(plot.title = element_text(hjust = 0.5))
  

  return(p)
}

