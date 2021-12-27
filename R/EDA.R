library(tidyverse)
library(ggplot2)
library(gridExtra)


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


plot_eda <- function(var, level){
  
  # score by individual
  p_score_indi <- exam %>%
    ggplot(aes(x = Score, y = ..density..)) +
    geom_histogram(color='black', 
                   linetype='dashed',
                   size=0.5,
                   fill='lightblue', 
                   alpha=0.5,
                   bins=20
    ) +
    geom_density(size=0.75, bw=0.3) +
    ggtitle("Distribution of Normalized Score") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # score by school
  grand_mean <- mean(exam$Score)
  
  p1 <- exam %>%
    group_by(SchoolId) %>%
    summarise(n = n(), mean = mean(Score)) %>%
    ggplot(aes(x=n, y=mean)) +
    geom_hline(
      aes(yintercept=grand_mean),
      linetype='dashed',
      color='red',
      size=0.75
    ) +
    geom_point() +
    labs(x='sample size', y='mean Score') +
    ggtitle("Group mean vs. sample size") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  p2 <- exam %>%
    ggplot(aes(y=Score, x=SchoolId, fill=SchoolId)) +
    geom_boxplot(
      alpha=0.5,
      show.legend = FALSE
    ) +
    #scale_x_discrete(guide=guide_axis(angle = 90)) +
    ggtitle("boxplot of score across schools") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x='School')
  
  
  # STANDLRT by individual
  p_lrt_indi <- exam %>%
    ggplot(aes(y = Score, x = STANDLRT)) +
    geom_point() +
    geom_smooth(method = "lm", se = F, aes(color = "grand_slope")) +
    ggtitle("Score vs. STANDLRT") +
    theme(plot.title = element_text(hjust = 0.5))

  
  # STANDLRT by school
  p_lrt_school <- exam %>%
    ggplot(aes(y = Score, x = STANDLRT)) +
    geom_point(aes(color = SchoolId), alpha=0.2, show.legend = FALSE) +
    stat_smooth(aes(color = SchoolId), geom='line', method = "lm", 
                alpha=0.4, se=FALSE, show.legend = FALSE) +
    geom_smooth(method = "lm", se = F, aes(color = "grand_slope"), 
                linetype = "dashed", show.legend = FALSE) +
    ggtitle("Score vs. STANDLRT (by school)") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # StudentGender by individual
  p_stu_gen_indi <- exam %>%
    ggplot(aes(x = StudentGender, y = Score, fill = StudentGender)) +
    geom_boxplot() +
    labs(y = "Score", x = "Student Gender")  +
    ggtitle("Score vs. Student Gender") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # StudentGender by school
  p_stu_gen_school <- exam %>%
    ggplot(aes(x = StudentGender, y = Score, fill = SchoolId)) +
    geom_boxplot(show.legend = FALSE) +
    labs(y = "Score", x = "Student Gender")  +
    ggtitle("Score vs. Student Gender (by school)") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # SchoolGender
  p_sch_gen <- exam %>%
    ggplot(aes(x = SchoolGender, y = Score, fill = SchoolId)) +
    geom_boxplot(show.legend = FALSE) +
    labs(y = "Score", x = "School Gender")  +
    ggtitle("Score vs. School Gender (by school)") +
    theme(plot.title = element_text(hjust = 0.5))
    
  
  # SchoolIntakeScore
  p_sc_intake <- exam %>%
    ggplot(aes(y = Score, x = SchoolIntakeScore)) +
    geom_point(aes(color = SchoolId), alpha=0.3, show.legend = FALSE) +
    geom_smooth(method = "lm", se = F, aes(color = "grand_slope"), 
                linetype = "dashed", show.legend = FALSE) +
    ggtitle("Score vs. School Intake Score (by school)") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # VRBand by individual
  p_VRB_indi <- exam %>%
    ggplot(aes(x = VRBand, y = Score, fill = VRBand)) +
    geom_boxplot() +
    labs(y = "Score", x = "Verbal Reasoning (VR) Score Band")  +
    ggtitle("Score vs. Verbal Reasoning (VR) Score Band") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # VRBand by school
  p_VRB_school <- exam %>%
    ggplot(aes(x = VRBand, y = Score, fill = SchoolId)) +
    geom_boxplot(show.legend = FALSE) +
    labs(y = "Score", x = "Verbal Reasoning (VR) Score Band")  +
    ggtitle("Score vs. Verbal Reasoning (VR) Score Band (by school)") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # IntakeBand by individual
  p_intake_band_indi <- exam %>%
    ggplot(aes(x = IntakeBand, y = Score, fill = IntakeBand)) +
    geom_boxplot() +
    labs(y = "Score", x = "Intake Score Band")  +
    ggtitle("Score vs. Intake Score Band") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # IntakeBand by school
  p_intake_band_school <- exam %>%
    ggplot(aes(x = IntakeBand, y = Score, fill = SchoolId)) +
    geom_boxplot(show.legend = FALSE) +
    labs(y = "Score", x = "Intake Score Band")  +
    ggtitle("Score vs. Intake Score Band (by school)") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  if (var == "Score" & level == "Individual") {return (p_score_indi)}
  if (var == "Score" & level == "School") {return (cowplot::plot_grid(p1, p2, rel_widths = c(1, 2.5)))}
  if (var == "STANDLRT" & level == "Individual") {return (p_lrt_indi)}
  if (var == "STANDLRT" & level == "School") {return (p_lrt_school)}
  if (var == "StudentGender" & level == "Individual") {return (p_stu_gen_indi)}
  if (var == "StudentGender" & level == "School") {return (p_stu_gen_school)}
  if (var == "SchoolGender" & level == "Individual") {return (NULL)}
  if (var == "SchoolGender" & level == "School") {return (p_sch_gen)}
  if (var == "SchoolIntakeScore" & level == "Individual") {return (NULL)}
  if (var == "SchoolIntakeScore" & level == "School") {return (p_sc_intake)}
  if (var == "VRBand" & level == "Individual") {return (p_VRB_indi)}
  if (var == "VRBand" & level == "School") {return (p_VRB_school)}
  if (var == "IntakeBand" & level == "Individual") {return (p_intake_band_indi)}
  if (var == "IntakeBand" & level == "School") {return (p_intake_band_school)}
}

