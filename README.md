# hierarchical_shiny


## 1 Introduction

In the real world, many kinds of data have a nested or hierarchical structure. For instance, children grow up in the same family are more likely to share similar personalities and habits, compared with individuals chosen from the population randomly. Longitudinal data also have multilevel structure, since the observations are collected from the same individuals at different time, which should have some correlation.

Multilevel models can recognize the existing hierarchical nature of the data, and both account for the heterogeneity across groups and allow the information to be shared.

The aim of this project is to create a helpful Shiny app that illustrate the hierarchical model. Users will be able to conduct a mini case study with the sample data provided. In the case study, they can adjust the side panel to visualize the variables, customize their own models, inspect and compare the model outputs.


### Case Study

Standardized tests can show students' academic strengths, weaknesses, and higher-order thinking skills, which are being accepted in many countries. On the one hand, it represents students' knowledge of facts, skills, and academic development. On the other hand, standardized test scores show differences in student performance from one school to another. 

Besides individual-level characteristics, including intellectual level, verbal reasoning abilities, gender, etc., there is no denying that schools can substantially impact the performance of standardized tests in general. Students attending selective schools outperform non-selective students in exams with a higher probability. The difference is attributed to the value added by schools; that is, education quality plays a significant role in exam scores.

In the mini case study, our sample dataset is from `Centre for Multilevel Modelling, University of Bristol`. The dataset contains the examination score and corresponding features, including intake achievement, pupil gender, and school type, of 4,059 students from 65 inner London schools.


## 2 Shiny App

The Link to the Shiny App: **https://liuwy0915.shinyapps.io/HierarchicalModel/**

The Shiny app mainly consists of three parts, the `HOMEPAGE`, the `DATA`, and the `CASE STUDY`.

### HOMEPAGE

In the **HOMEPAGE** section, users are given a general introduction of the hierarchical model (also known as multilevel model). Users can also learn about the purpose of this Shiny app and some instructions to explore hierarchical model with this App. In addition, references and sources are also included.

### DATA

In the **DATA** section, users will be given the background of the case study project by clicking **ABOUT THE DATASET**, and view the raw data by clicking **VIEW THE RAW DATA**.
 
### CASE STUDY

In the **CASE STUDY** section, users have the opportunity to conduct their own case study to explore hierarchical model with the dataset we provide.


#### EDA

In this page, users can select a `variable` and a `level` at the sidebar, and click `VISUALIZE` to show the exploratory analysis plot. To be more specific, for the response variable, `Score`, a histogram and a density plot will be displayed if the `Individual Level` is selected. If the `School Level` is selected,we will plot the sample mean against the sample size and a boxplot of `Score` across schools on the main panel. For a numerical predictor, a scatter plot of `Score` against the given predictor will be displayed. For a categorical predictor, a boxplot of `Score` against different level of the given predictor will be shown.
 

#### MODEL

In this page, users can customize their model by selecting `predictors` included and `level` of the model.

There are basically three choices of levels, `Individual Level (Pooled)`, `School Level (Unpooled)`, and `Hierarchical`. The pooled model treat all the schools as the same and completely ignores the heterogeneity across schools. The unpooled model regard each school as independent and fit a separate model for each school that, which does not allow any information to be shared across schools. Hierarchical model is in between. It both account for the heterogeneity across groups and allow the information in different groups to be shared.

After clicking `FIT MODEL`, the summary table will be displayed on the main panel. Users can inspect the residuals and coefficient estimates (random effect and fixed effects for hierarchical model).


#### HIERARCHICAL MODEL RESULT

In this section, we show and interpret the model result of our final hierarchical model. The final model is selected by backward elimination.

 + **MODEL**: In this page, we clearly specified the model and the model assumption.
 
 + **FIXED EFFECTS**: In this page, we displayed the coefficient estimates of the fixed effects and the corresponding 95% confidence intervals.
 
 + **RANDOM EFFECTS**: In this page, we calculated and interpreted the interclass correlation. In addition, we displayed the point estimates and corresponding 95% confidence intervals of random intercepts for each school by dotplot and table.

 + **MODEL DIAGNOSTIC**: In this page, we created several plots and correponding interpretations to diagnose the model assumptions, including `Residuals vs. Fitted plot`, `Normal QQ for Residuals`, `Normal QQ for Random Effects`, and `Cook's Distance plot`.
  

## 3 References

 + `Centre for Multilevel Modelling, University of Bristol`: http://bristol.ac.uk/cmm/
 
 + Goldstein, H., Rasbash, J., et al (1993). A multilevel analysis of school examination results. Oxford Review of Education, 19: 425-433.
           
 + Shiny Tutorial: https://shiny.rstudio.com/
