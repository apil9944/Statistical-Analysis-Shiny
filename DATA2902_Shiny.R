library(shiny)
library(ggplot2)
library(bomrang)
library(plotly)
library(tidyverse)
library(janitor)
library(shinythemes)
library(sjPlot)
library(pander)
library(knitr)

data <- readr::read_csv("cleaned_data_new.csv")
df <- data.frame(data) %>% na.omit()

# App choices
chi_var_choices = c("Time since last dental checkup" = "dentist",
                    "Gender" = "gender",
                    "Social media preference" = "social_media",
                    "Has asthma" = "have_asthma",
                    "Lives with parents currently" = "live_with_parents",
                    "Had a childhood pet" = "dog_or_cat",
                    "Eye colour" = "eye_colour",
                    "Favourite season" = "fav_season",
                    "Frequency of teeth flossing" = "floss_frequency",
                    "Wears glasses or contacs" = "glasses_or_contacts",
                    "Dominant hand" = "dominant_hand",
                    "Steak preference" = "steak_preference")

numeric_choices = c(
    "Number of COVID-19 tests taken" = "covid_test",
    "Hours spent on University work per week on average" = "university_work",
    "Hours spent exercising per week on average" = "exercising",
    "Hours spent on paid employment per week on average" = "paid_work",
    "Height (cm)" = "height",
    "Stress level" = "stress_level"
)

tail_choices = c("Greater (>)" = "greater",
                 "Less (<)" = "less",
                 "two.sided")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # Application title
                titlePanel(h1(strong("DATA2902 Dynamic Hypothesis Tests"))),
                br(),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                    sidebarPanel(
                        
                        conditionalPanel("input.tabs == 'Chi-squared Goodness of Fit Test' ",
                                         selectizeInput(inputId = "x_variable",
                                                        "Select variable:",
                                                        choices = chi_var_choices)
                        ),
                        
                        conditionalPanel("input.tabs == 'Two sample t-test' ",
                                         selectizeInput(inputId = "two_sample_t_variable_x",
                                                        "Select first variable:",
                                                        choices = numeric_choices),
                                         selectizeInput(inputId = "two_sample_t_variable_y",
                                                        "Select second variable:",
                                                        choices = numeric_choices)
                        ),
                        conditionalPanel("input.tabs == 'One sample t-test' ",
                                         selectizeInput(inputId = "one_sample_t_variable",
                                                        "Select variable:",
                                                        choices = numeric_choices),
                                         selectizeInput(inputId = "tail",
                                                        "Select alternative:",
                                                        choices = tail_choices),
                                         numericInput(inputId = "user_input",
                                                      label = "Enter a true/hypothetical population mean under the null hypothesis:",
                                                      value = 0
                                         )
                        )
                        
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        tabsetPanel(id = "tabs",
                                    tabPanel(
                                        title = "About",
                                        h3(strong("Objective")),
                                        h4("This Shiny app implements dynamic hypothesis tests and visualisations based on the selected variable(s) from the DATA2X02 survey data set."),
                                        br(),
                                        h3(strong("About this Dataset")),
                                        h4("This data explored was collected from a survey of DATA2X02 students enrolled at The University of Sydney during Semester 2 2020. There are approximately 572 people enrolled across the DATA2002 and DATA2902 streams. This survey was voluntary and was made available for completion for a relatively short period of time between 2/9/2020 and 7/9/2020."),
                                        br(),
                                        h3(strong("Dynamically supported Hypothesis Tests")),
                                        h4("Chi-squared Goodness of Fit Tests for Uniformity"),
                                        h4(em("Tests whether the data comes from a Uniform distribution.")),
                                        br(),
                                        h4("Chi-squared Test for Independence"),
                                        h4(em("Tests if two variables can be considered independent when sampling from one population.")),
                                        br(),
                                        h4("One sample t-test"),
                                        h4(em("Tests if the mean of a variable is significantly different to the true or hypothesised population mean.")),
                                        br(),
                                        h4("Two sample t-test for unequal population variance"),
                                        h4(em(" Tests if the population means of two samples are significantly different, assuming unequal popluation variances."))
                                        
                                    ),
                                    tabPanel(
                                        title = "Chi-squared Goodness of Fit Test",
                                        h2(strong("Chi-squared Goodness of Fit Tests for Uniformity")),
                                        h3("Hypotheses"),
                                        h4("H0: Data follows a uniform distribution"),
                                        h4("H1: Data does not follow a uniform distribution"),
                                        h3("Assumptions"),
                                        verbatimTextOutput("assumptions"),
                                        plotOutput("cst"),
                                        h3("Test results"),
                                        verbatimTextOutput("cst2")
                                        
                                    ),
                                    tabPanel(
                                        title = "One sample t-test",
                                        h2(strong("One sample t-test")),
                                        h3("Hypotheses"),
                                        h4("H0: Sample mean is equal to population mean"),
                                        h4("H1: Sample mean is less than or greater than or not equal to population mean"),
                                        h3("Assumptions"),
                                        verbatimTextOutput("assumptions3"),
                                        plotOutput("onettest"),
                                        h3("Test results"),
                                        verbatimTextOutput("onettest2")
                                        
                                    ),
                                    
                                    tabPanel(
                                        title = "Two sample t-test",
                                        h2(strong("Two sample t-test Assuming Unequal Population Variances")),
                                        h3("Hypotheses"),
                                        h4("H0: True difference in means is equal to 0"),
                                        h4("H1: True difference in means is not equal to 0"),
                                        h3("Assumptions"),
                                        verbatimTextOutput("assumptions4"),
                                        plotOutput("ttest"),
                                        h3("Test results"),
                                        verbatimTextOutput("ttest2")
                                     
                                    )
                        )
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Reactive functions
    whole_data = reactive({
        df
    })
    
    x_data = reactive({
        df[[input$x_variable]] %>% na.omit()
    })
    
    y_data = reactive({
        df[[input$y_variable]] %>% na.omit()
    })
    
    
    
    #Chi-square goodness of fit test plot
    output$cst = renderPlot({
        y = table(x_data())
        n = length(x_data())
        c = chisq.test(y, p = rep(1/length(y), length(y)))
        dist_chisq(deg.f = c$parameter, p = c$p.value)
    })
    
    output$assumptions = renderPrint({
        y = table(x_data())
        n = length(x_data())
        p = rep(1/length(y), length(y))
        ey = n * p
        if (ey >= 5) {
            print("Assumptions of expected cell counts are met.")
        } else {
            print("Assumptions of expected cell counts are not met!")
        }
    })
    
    #Chi-square goodness of fit test stat
    output$cst2 = renderPrint({
        y = table(x_data())
        n = length(x_data())
        c = chisq.test(y, p = rep(1/length(y), length(y)))
        pander(c)
        if (c$p.value < 0.05) {
            print("Reject the null and conclude that the data is not distributed uniformly at 5% significance")
        } else {
            print("Fail to reject the null and conclude that the data is distributed uniformly at 5% significance")
        }
    })
    
    #One sample t-test
    output$onettest = renderPlot({
        x = df[[input$one_sample_t_variable]]
        z = t.test(x, mu = input$user_input, alternative = input$tail)
        dist_t(z$statistic, deg.f = z$parameter)
        
    })
    
    output$assumptions3 = renderPrint({
        x = df[[input$one_sample_t_variable]]
        s = shapiro.test(x)
        pander(s)
        if (s$p.value > 0.05) {
            print("Normality assumption holds.")
        } else {
            print("Normality assumption is violated.")
        }
    })
    
    output$onettest2 = renderPrint({
        x = df[[input$one_sample_t_variable]]
        z = t.test(x, mu = input$user_input, alternative = input$tail)
        pander(z)
    })
    
    output$ttest = renderPlot({
        x = df[[input$two_sample_t_variable_x]]
        y = df[[input$two_sample_t_variable_y]]
        z1 = t.test(x, y, alternative = "two.sided") 
        dist_t(z1$statistic, deg.f = z1$parameter)
        print(z1)
    })
    
    output$assumptions4 = renderPrint({
        x = df[[input$two_sample_t_variable_x]]
        y = df[[input$two_sample_t_variable_y]]
        s1 = shapiro.test(x) 
        s2 = shapiro.test(y)
        pander(s1)
        pander(s2)
        if (s1$p.value > 0.05) {
            print("Normality assumption holds for first variable")
        } else {
            print("Normality assumption is violated for first variable.")
        }
  
        if (s2$p.value > 0.05) {
            print("Normality assumption holds for second variable.")
        } else {
            print("Normality assumption is violated for second variable.")
        }
    })
    
    output$ttest2 = renderPrint({
        x = df[[input$two_sample_t_variable_x]]
        y = df[[input$two_sample_t_variable_y]]
        z = t.test(x, y, alternative = "two.sided")
        pander(z)
        if (z$p.value < 0.05) {
            print("Reject the null and conclude that the true difference in means is not equal to 0.")
        } else {
            print("Fail to reject the null and conclude that the true difference in means is equal to 0.")
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
