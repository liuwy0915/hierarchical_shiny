
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinybusy)
library(shinyalert)
library(tidyverse)
library(ggplot2)
library(gridExtra)

source("EDA.R")
source("model.R")

ui <- fluidPage(
    
    theme = shinytheme("sandstone"),
    
    useShinyalert(),

    # Application title
    titlePanel(h1("Hierarchical Model Exploration")),
    
    navbarPage(
        
        title = strong("MENU"),
        
        tabPanel(
            
            strong("HOMEPAGE"),
            column(
                align = "left",
                width = 8,
                h2("About this project"),
                br(),
                h4("Hierarchical/Multilevel Model"),
                p("In the real world, many kinds of data have a nested or hierarchical structure. 
                  For instance, children grow up in the same family are more likely to share similar
                  personalities and habits, compared with individuals chosen from the population 
                  randomly. Longitudinal data also have multilevel structure, since the observations
                  are collected from the same individuals at different time, which should have some
                  correlation."),
                br(),
                p("Hierarchical models (also known as multilevel models) can recognize the existing
                hierarchical nature of the data, and
                  both account for the heterogeneity across groups and allow the information to be
                  shared."),
                br(),
                h4("About the App"),
                p("The aim of this project is to create a helpful Shiny app that illustrate the
                  hierarchical model. Users will be able to conduct a mini case study with the
                  sample data provided. In the case study, they can adjust the side panel to
                  visualize the variables, customize their own models, inspect and compare the
                  model outputs."),
                br(),
                h4("User Guide"),
                p("In the ",
                   span("DATA", style = "color:blue"), 
                   "section, users will be given the background of the case study
                   project by clicking ",
                   span("ABOUT THE DATASET", style = "color:blue"), 
                   ", and view the raw data by clicking ",
                   span("VIEW THE RAW DATA", style = "color:blue"), 
                   "."),
                p("In the ",
                   span("CASE STUDY", style = "color:blue"),
                   " section, users have the opportunity to conduct their 
                   own case study to explore hierarchical model with the dataset we provide."),
                p("In the ",
                   span("EDA", style = "color:blue"),
                   " page, users can select a ", strong("variable"), " and a ",
                   strong("level"), " at the sidebar, and click ",
                   span("VISUALIZE", style = "color:blue"),
                   " to show the exploratory analysis plot."),
                p("In the ",
                   span("MODEL", style = "color:blue"),
                   " page, users can customize their model by selecting ",
                   strong("predictors")," included and ", strong("level"), 
                   " of the model. After clicking ",
                   span("FIT MODEL", style = "color:blue"),
                   " the summary table will be displayed on the main panel."),
                p("In the ",
                   span("HIERARCHICAL MODEL RESULT", style = "color:blue"),
                   " section, we show the model result and
                  model assessment of our final hierarchical model. The final model is selected
                  by backward elimination.")
            ),
            column(
                width = 4,
                h2("References"),
                htmlOutput("references"),
            )
            
            

            
            
        ),
        
        navbarMenu(
            strong("DATA"),
            
            tabPanel(
                "About the dataset",
                h2("About the dataset"),
                br(),
                p("Standardized tests can show students' academic strengths, weaknesses, 
                and higher-order thinking skills, which are being accepted in many countries. 
                On the one hand, it represents students' knowledge of facts, skills, 
                and academic development. On the other hand, standardized test scores show 
                differences in student performance from one school to another."),
                br(),
                p("Besides individual-level characteristics, including intellectual level, 
                verbal reasoning abilities, gender, etc., there is no denying that schools 
                can substantially impact the performance of standardized tests in general. 
                Students attending selective schools outperform non-selective students in 
                exams with a higher probability. The difference is attributed to the value 
                added by schools; that is, education quality plays a significant role in exam 
                scores."),
                br(),
                p("Our sample dataset is from Centre for Multilevel Modelling, University
                of Bristol. The dataset contains the examination score and corresponding features,
                including intake achievement, pupil gender, and school type, of 4,059 students
                from 65 inner London schools."),
                br(),
                p("Below is the variable table."),
                dataTableOutput("var_exp")
            ),
            
            tabPanel(
                "View the raw data",
                column(width = 10,
                       h3("Raw Data"),
                       dataTableOutput(outputId = "table")
                ),
                column(width = 2,
                       br(),
                       downloadButton(outputId = "download",
                                      label = strong("Download data"))
                )
            )
            
        ),
        
        navbarMenu(
            strong("CASE STUDY"),
            
            tabPanel(
                "EDA",
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        selectInput(
                            inputId = "variable1",
                            label = h4("Select a variable to visualize"),
                            choices = list("Score (Response)" = "Score", 
                                           "STANDLRT",
                                           "StudentGender",
                                           "SchoolGender", 
                                           "SchoolIntakeScore",
                                           "VRBand",
                                           "IntakeBand")
                        ),
                        
                        selectInput(
                            inputId = "level1",
                            label = h4("Select the level to visualize"),
                            choices = list("Individual Level" = "Individual",
                                           "School Level" = "School")
                        ),
                        
                        actionButton(
                            inputId = "visualize",
                            label = strong("Visualize")
                        )
                    ),
                    
                    mainPanel(
                        width = 9,
                        h3("Exploratory Analysis Plot"),
                        plotOutput(outputId = "eda_plot")
                    )
                    
                )
            ),
            
            tabPanel(
                "Model",
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        selectInput(
                            inputId = "response",
                            label = h4("Select the response variable"),
                            choices = list("Score")
                        ),
                        helpText(h4("Select the predictors")),
                        helpText(" "),
                        checkboxInput(
                            inputId = "STANDLRT",
                            label = "STANDLRT",
                            value = FALSE
                        ),
                        checkboxInput(
                            inputId = "StudentGender",
                            label = "StudentGender",
                            value = FALSE
                        ),
                        checkboxInput(
                            inputId = "SchoolGender",
                            label = "SchoolGender",
                            value = FALSE
                        ),
                        checkboxInput(
                            inputId = "SchoolIntakeScore",
                            label = "SchoolIntakeScore",
                            value = FALSE
                        ),
                        checkboxInput(
                            inputId = "VRBand",
                            label = "VRBand",
                            value = FALSE
                        ),
                        checkboxInput(
                            inputId = "IntakeBand",
                            label = "IntakeBand",
                            value = FALSE
                        ),
                        selectInput(
                            inputId = "level2",
                            label = h4("Select the level of your model"),
                            choices = list("Individual Level (Pooled)" = "Pooled",
                                           "School Level (Unpooled)" = "Unpooled",
                                           "Hierarchical")
                        ),
                        actionButton(
                            inputId = "model",
                            label = strong("Fit Model")
                        )
                    ),
                    
                    mainPanel(
                        width = 9,
                        h3("Model Output"),
                        verbatimTextOutput("model")
                    )
                )
                
            ),
            
            tabPanel(
                "Hierarchical Model Result",
                
                tabsetPanel(
                    tabPanel(
                        strong("Model"),
                        h2("Hierarchical Model Specification"),
                        br(),
                        br(),
                        h3("Final Model"),
                        uiOutput("formula"),
                        br(),
                        br(),
                        h3("Model Assumption"),
                        uiOutput("assumption1"),
                        uiOutput("assumption2"),
                        uiOutput("assumption3"),
                        uiOutput("assumption4")
                        
                    ),
                    
                    tabPanel(
                        strong("Fixed Effects"),
                        h2("Fixed Effects"),
                        dataTableOutput(outputId = "fixed")
                        
                    ),
                    
                    tabPanel(
                        strong("Random Effects"),
                        h2("Random Effects"),
                        h3("Inter Class Correlation"),
                        uiOutput("icc"),
                        br(),
                        br(),
                        h3("Random Effects Plot"),
                        plotOutput(outputId = "random_plt"),
                        br(),
                        br(),
                        h3("Estimated Random Intercepts"),
                        dataTableOutput(outputId = "random_tbl"),
                        
                    ),
                    
                    tabPanel(
                        strong("Model Diagnostic"),
                        column(
                            width = 4,
                            h3("Residuals vs. Fitted"),
                            plotOutput("diag1") %>%
                                withSpinner(),
                            br(),
                            h3("Normal QQ for Random Effects"),
                            plotOutput("diag3") %>%
                                withSpinner()
                        ),
                        column(
                            width = 4,
                            h3("Normal QQ for Residuals"),
                            plotOutput("diag2") %>%
                                withSpinner(),
                            br(),
                            h3("Cook's Distance across schools"),
                            plotOutput("diag4") %>%
                                withSpinner()
                        ),
                        column(
                            width = 4,
                            checkboxInput('ex_visible', 'Show Interpretation', FALSE),
                            uiOutput('expand')
                        )
                    )
                )
                
            )
            
        )
            
        
        
    )

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$references <- renderPrint(
        HTML(
            "
            Centre for Multilevel Modelling, University of Bristrol:
            <a href='http://bristol.ac.uk/cmm/'>
            http://bristol.ac.uk/cmm/ </a>
            <br><br>
            Goldstein, H., Rasbash, J., et al (1993). A multilevel analysis of school examination
            results. Oxford Review of Education, 19: 425-433.
            <br><br>
            Shiny Tutorial:
            <a href='https://shiny.rstudio.com/'>
            https://shiny.rstudio.com/ </a>
            <br><br>
            Shiny app by <a href='https://github.com/liuwy0915'>
            Weiyi Liu </a>
            "
        )
    )
    
    var_name <- c("SchoolId", "StudentId", "Score", 
                  "STANDLRT", "StudentGender", "SchoolGender",
                  "SchoolIntakeScore", "VRBand", "IntakeBand")
    var_description <- c("School ID",
                         "Student ID",
                         "Normalised exam score (Response Variable)",
                         "Standardised LR test scor",
                         "Student gender (0-boys, 1-girls)",
                         "School gender (1-mixed school, 2-boys school, 3-girls school)",
                         "School average of intake score",
                         "Student level Verbal Reasoning (VR) score band at intake 
                         (1-bottom 25%, 2-mid 50%, 3-top 25%)",
                         "Student intake score band (1-bottom 25%, 2-mid 50%, 3-top 25%)")
    var_type <- c("factor",
                  "factor",
                  "numerical",
                  "numerical",
                  "factor",
                  "factor",
                  "numerical",
                  "factor",
                  "factor")
    var_level <- c("School",
                   "Individual",
                   "Individual",
                   "Individual",
                   "Individual",
                   "School",
                   "School",
                   "Individual",
                   "Individual")
    variable_explain <- data.frame(var_name, var_description, var_type, var_level)
    colnames(variable_explain) <- c("Name", "Description", "Type", "Level")
    
    output$var_exp <- renderDataTable(variable_explain,
                                      options = list(pageLength = 25, lengthChange = FALSE))
    
    exam <- read.table("Data/Exam.txt", header = FALSE)
    colnames(exam) <- c("SchoolId", "StudentId", "Score", "Const", 
                        "STANDLRT", "StudentGender", "SchoolGender",
                        "SchoolIntakeScore", "VRBand", "IntakeBand")
    exam <- exam %>% 
        dplyr::select(-Const) %>%
        mutate(SchoolId = as.factor(SchoolId),
               StudentId = as.factor(StudentId),
               StudentGender = as.factor(StudentGender),
               SchoolGender = as.factor(SchoolGender),
               VRBand = as.factor(VRBand),
               IntakeBand = as.factor(IntakeBand))
    

    output$table <- renderDataTable(exam)
    
    output$download <- downloadHandler(
        filename = "Exam.csv",
        content = function(file) write_csv(exam, file)
    )
    
    observeEvent(input$visualize, {
    
        if ((input$variable1=="SchoolGender" | input$variable1=="SchoolIntakeScore") 
            & input$level1=="Individual") {
            shinyalert(title = "Error!",
                       text = "This variable is a school-level variable,
                       and cannot be visualized by individual level!",
                       type = "error")
            NULL
        } else {
            plot1 <- plot_eda(input$variable1, input$level1)
            output$eda_plot <- renderPlot({
                plot1
            })
        }
    }) 
    
    observeEvent(input$model, {
        if (any(c(input$STANDLRT, input$StudentGender,
                  input$SchoolGender, input$SchoolIntakeScore, 
                  input$VRBand, input$IntakeBand)) == FALSE) {
            shinyalert(title = "Error!",
                       text = "You need to select at least one predictor!",
                       type = "error")
            NULL
        } else {
            model_result <- print_mod(level = input$level2, 
                                      STANDLRT = input$STANDLRT, 
                                      StudentGender = input$StudentGender,
                                      SchoolGender = input$SchoolGender, 
                                      SchoolIntakeScore = input$SchoolIntakeScore, 
                                      VRBand = input$VRBand, 
                                      IntakeBand = input$IntakeBand)
            output$model <- renderPrint(
                model_result
            )
        }
    })
    
    output$formula <- renderUI({
        withMathJax(helpText("
        Our final model (selected from backward elimination) is
        $$Score_{ij} = \\beta_0 + b_{0j} + \\beta_1 STANDLRT_{ij} 
        + \\beta_2 StudentGender_{ij} + \\beta_3 SchoolIntakeScore_{j}
        + \\beta_4 IntakeBand_{ij} + \\epsilon_{ij}$$
        where 
        $$b_{0j} \\stackrel{iid} \\sim \\mathcal{N}(0, \\tau^2) \\perp \\epsilon_{ij} 
        \\stackrel{iid} \\sim \\mathcal{N}(0, \\sigma^2)$$
        and \\(Score_{ij}\\) denote the i-th individual in the j-th school.
        "))
    })
    
    output$assumption1 <- renderUI({
        withMathJax(helpText("
        1. There is a linear relationship between the dependent variable, 
        Score, and the predictors
         "))
    })
    
    output$assumption2 <- renderUI({
        withMathJax(helpText("
        2. \\(Score_{ij}\\) 's are independent
         "))
    })
    output$assumption3 <- renderUI({
        withMathJax(helpText("
        3. The variance of \\(Score_{ij}\\) in each group (i.e. school) should be the same
                             "))
    })
    output$assumption4 <- renderUI({
        withMathJax(helpText("
        4. \\(\\epsilon_{ij} \\stackrel{iid} \\sim \\mathcal{N}(0, \\sigma^2)\\)
             "))
    })
    
    output$fixed <- renderDataTable(
        view_coef(final_model),
        options = list(pageLength = 25, lengthChange = FALSE)
    )
    
    output$random_plt <- renderPlot({
        plot_random(final_model)
    })
    
    output$random_tbl <- renderDataTable(
        tbl_random(final_model)
    )
    
    output$icc <- renderUI({
        withMathJax(helpText("
        $$ ICC = \\frac{\\hat{\\tau^2}}{\\hat{\\tau^2} + \\hat{\\sigma^2}} \\approx 
        \\frac{0.0728}{0.0727 + 0.5334} =  0.1201
        $$
        where
        \\(\\hat{\\tau^2}\\) is the estimated within-group (unexplained) variance in response,
         \\(\\hat{\\sigma^2}\\) is the estimated across-group (explained) variance in response.
             "))
    })
    
    output$diag1 <- renderPlot({plot_res_fit(final_model)})
    
    output$diag2 <- renderPlot({plot_qq(final_model)})
    
    output$diag3 <- renderPlot({plot_ranef_qq(final_model)})
    
    output$diag4 <- renderPlot({plot_cooks_distance(final_model)})
    
    output$expand <- renderUI({
        if (!input$ex_visible) return()
        withMathJax(
            helpText("1. `Residual vs. Fitted plot`: The residuals are spread equally around 
                     the horizontal line, indicating there is no non-linear relationship. 
                     Besides, the equal variance assumption is met."),
            helpText("2. `Normal QQ plot for residuals`: The normality assumption is met since 
                     our residuals adhere closely around the diagonal line without heavy tails
                     on the sidesm representing normality. "),
            helpText("3. `Normal QQ plot for Random Effects`: The random effects are normally 
                     distributed with points adhere around the diagonal line. "),
            helpText("4. `Cook's Distance`: We have 3 highly influential schools (School 53,  
                     School 43, abd School 54) whose Cook's distance exceeds cutoff 4/n, 
                     where n denotes the number of states.")
        )
    })

        
}

# Run the application 
shinyApp(ui = ui, server = server)
