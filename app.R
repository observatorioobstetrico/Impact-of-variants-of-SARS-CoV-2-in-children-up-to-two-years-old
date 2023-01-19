library(shiny)
library(dplyr)
library(magrittr)
library(readxl)
library(shinydashboard)
library(questionr)
library(kableExtra)
library(ggplot2)
library(highcharter)
library(summarytools)
library(modelsummary)
library(abjData)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(reactable)
library(htmltools)
library(zoo)
library(plotly)
library(lubridate)
library(googlesheets4)
library(shinyjs)
library(DescTools)
library(tidymodels)
library(themis)
library(dplyr)
library(glmtoolbox)

# Load dataset
data5 <- readRDS("data_paper_1000d.rds")

# Preparation
data5$variants <- factor(data5$variants,
                          levels = c("original", "gamma", "delta","omicron"))

data5$raca_sel <- data5$ethnicity
data5$raca_sel <-
  ifelse(is.na(data5$ethnicity), "uninformed", data5$ethnicity)

data5$CLASSI_FIN <- as.factor(data5$CLASSI_FIN)

data5$DT_SIN_PRI <- dmy(data5$DT_SIN_PRI)
data5$DT_EVOLUCA <- dmy(data5$DT_EVOLUCA)


# Smote
dados_smote <- function(dados,var){
  dados1 <- dados %>% select(variants, var) %>% drop_na()
  
  # set.seed(69)
  
  ds_rec <- recipe(variants ~., data = dados1) %>%
    step_smotenc(variants,seed=69) %>%
    prep()
  
  new_data <- juice(ds_rec)
  
  return(new_data)
  
}

# Downsample
dados_downsample <- function(dados,var){
  dados1 <- dados %>% select(variants, var) %>% drop_na()
  
  # set.seed(69)
  
  ds_rec <- recipe(variants ~., data = dados1) %>%
    step_downsample(variants,seed = 69) %>%
    prep()
  
  new_data <- juice(ds_rec)
  
  return(new_data)
  
}

sticky_style <-
  list(
    position = "sticky",
    left = 0,
    background = "#fff",
    zIndex = 1,
    borderRight = "1px solid #eee"
  )

today <- Sys.Date()


humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

table <- "responses"

appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# User Interface ----
ui <-
  dashboardPage(
    title = "Analysis of SARS-CoV-2 Variants: children up to two years old",
    dashboardHeader(
      title = strong('Analysis of Variants', titleWidth = 200)
    ),
    dashboardSidebar(
      width = 200,
      ## Menu ----
      sidebarMenu(
        style = "position: fixed; overflow: visible;",
        menuItem("Documentation", tabName = "doc"),
        menuItem("General Analysis", tabName = "tab_cruzada"),
        menuItem("Models", tabName = "modelos"),
        menuItem("Analysis with Smote", tabName = "anal_var"),
        menuItem("Analysis with Downsample", tabName = "anal_downsample")
      )
    ),
    ### body ----
    dashboardBody(
      tags$head(
        tags$style(
        HTML(
          '
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0A1E3C;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0A1E3C;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0A1E3C;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0A1E3C;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #32A0FF;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0A1E3C;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #32A0FF;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #32A0FF;
                              }
                              '
        ),
        HTML("hr {border-top: 1px solid #0A1E3C;}")
      )),
      tabItems(
        # Documentation
        tabItem(
          tabName = "doc",
          tabPanel("pdf", tags$iframe(style = "height:800px; width:100%", src = "1000d_variants.pdf"))
        ),
        ### General Analysis ----
        tabItem(tabName = "tab_cruzada",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Select",
                    status = "primary",
                    solidHeader = FALSE,
                    selectInput(
                      inputId = "caracteristicas1",
                      label = "Row variable:",
                      choices = c(
                        "Brazil region"  = "region",
                        "Federative unit of Brazil"  = "SG_UF",
                        "Ethnicity" = "ethnicity",
                        "Sex" = "sex",
                        "Travel history" = "travel_hist",
                        "Change of municipality for service" = "changed_muni",
                        "Area of residence" = "zone",
                        "Flu syndrome evolved into SRAG" = "flu_into_srag",
                        "Infection acquired in the hospital" = "inf_inter",
                        "Contact with poultry or swine" = "cont_pty_swine",
                        "Flu vaccine" = "vacine",
                        "Antiviral" = "antiviral",
                        "Fever" = "fever",
                        "Cough" = "cough",
                        "Sore throat" = "sore_throat",
                        "Dyspnea" = "dyspnea",
                        "Respiratory discomfort" = "resp_disc",
                        "Desaturation" = "desaturation",
                        "Diarrhea" = "diarrhea",
                        "Vomit" = "vomit",
                        "Abdominal pain" = "abd_pain",
                        "Fatigue" = "fatigue",
                        "Olfactory loss" = "olfac_loss",
                        "Loss of taste" = "loss_taste",
                        "Heart disease" = "cardiac",
                        "Hematologic" = "hematologic",
                        "Hepatic" = "hepatic",
                        "Asthma" = "asthma",
                        "Diabetes" = "diabetes",
                        "Neurologic" = "neurologic",
                        "Lung disease" = "pneumologic",
                        "Imunossupression" = "imuno",
                        "Renal" = "renal",
                        "Obesity" = "obesity",
                        "ICU admission" = "icu",
                        "Invasive respiratory support" = "intubation",
                        "Evolution" = "death"
                      ),
                      selected = "death",
                      width = "220px"
                    ),
                    checkboxInput("na1", "Delete missing data?",
                                  value = TRUE),
                    hr(),
                    sliderInput(
                      inputId = "idade1",
                      label = "Age range (months):",
                      min = min(data5$age_month),
                      max = max(data5$age_month),
                      value = c(min(data5$age_month), max(data5$age_month))
                    ),
                    hr()
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Cross-tabulation",
                               highcharter::highchartOutput("plot11"),
                               verbatimTextOutput("table1"),
                               h3(strong("Fisher's test")),
                               verbatimTextOutput("print1"))
                    )),
                    h3(strong("Obs")),
                    p(
                      "<NA> in the table above indicates the missing or ignored (non-response) cases of the variables in question. In the chart, this information appears in the numbered category (for example, number 2)."
                    ),
                  p(
                    "If you only want to analyze the valid cases (without considering the missing cases), in the upper left corner, select the 'Delete missing data?' box."
                  )
                  )
                )),
        ### Analysis with Smote ----
        tabItem(tabName = "anal_var",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Select",
                    status = "primary",
                    solidHeader = FALSE,
                    selectInput(
                      inputId = "caracteristicas2",
                      label = "Row variable:",
                      choices = c(
                        "Brazil region"  = "region",
                        "Federative unit of Brazil"  = "SG_UF",
                        "Ethnicity" = "ethnicity",
                        "Sex" = "sex",
                        "Travel history" = "travel_hist",
                        "Change of municipality for service" = "changed_muni",
                        "Area of residence" = "zone",
                        "Flu syndrome evolved into SRAG" = "flu_into_srag",
                        "Infection acquired in the hospital" = "inf_inter",
                        "Contact with poultry or swine" = "cont_pty_swine",
                        "Flu vaccine" = "vacine",
                        "Antiviral" = "antiviral",
                        "Fever" = "fever",
                        "Cough" = "cough",
                        "Sore throat" = "sore_throat",
                        "Dyspnea" = "dyspnea",
                        "Respiratory discomfort" = "resp_disc",
                        "Desaturation" = "desaturation",
                        "Diarrhea" = "diarrhea",
                        "Vomit" = "vomit",
                        "Abdominal pain" = "abd_pain",
                        "Fatigue" = "fatigue",
                        "Olfactory loss" = "olfac_loss",
                        "Loss of taste" = "loss_taste",
                        "Heart disease" = "cardiac",
                        "Hematologic" = "hematologic",
                        "Hepatic" = "hepatic",
                        "Asthma" = "asthma",
                        "Diabetes" = "diabetes",
                        "Neurologic" = "neurologic",
                        "Lung disease" = "pneumologic",
                        "Imunossupression" = "imuno",
                        "Renal" = "renal",
                        "Obesity" = "obesity",
                        "ICU admission" = "icu",
                        "Invasive respiratory support" = "intubation",
                        "Evolution" = "death"
                      ),
                      selected = "death",
                      width = "220px"
                    ),
                    hr(),
                    sliderInput(
                      inputId = "idade2",
                      label = "Age range (months):",
                      min = min(data5$age_month),
                      max = max(data5$age_month),
                      value = c(min(data5$age_month), max(data5$age_month))
                    )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Cross-tabulation",
                               highcharter::highchartOutput("plot21"),
                               verbatimTextOutput("table2"),
                               h3(strong("Fisher's test")),
                               verbatimTextOutput("print2"))
                    )),
                    h3(strong("Obs")),
                    p(
                      "<NA> in the table above indicates the missing or ignored (non-response) cases of the variables in question. In the chart, this information appears in the numbered category (for example, number 2)."
                    )
                  )
                )
                
        ),
        ### Analysis with Downsample ----
        tabItem(tabName = "anal_downsample",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Select",
                    status = "primary",
                    solidHeader = FALSE,
                    selectInput(
                      inputId = "caracteristicas3",
                      label = "Row variable:",
                      choices = c(
                        "Brazil region"  = "region",
                        "Federative unit of Brazil"  = "SG_UF",
                        "Ethnicity" = "ethnicity",
                        "Sex" = "sex",
                        "Travel history" = "travel_hist",
                        "Change of municipality for service" = "changed_muni",
                        "Area of residence" = "zone",
                        "Flu syndrome evolved into SRAG" = "flu_into_srag",
                        "Infection acquired in the hospital" = "inf_inter",
                        "Contact with poultry or swine" = "cont_pty_swine",
                        "Flu vaccine" = "vacine",
                        "Antiviral" = "antiviral",
                        "Fever" = "fever",
                        "Cough" = "cough",
                        "Sore throat" = "sore_throat",
                        "Dyspnea" = "dyspnea",
                        "Respiratory discomfort" = "resp_disc",
                        "Desaturation" = "desaturation",
                        "Diarrhea" = "diarrhea",
                        "Vomit" = "vomit",
                        "Abdominal pain" = "abd_pain",
                        "Fatigue" = "fatigue",
                        "Olfactory loss" = "olfac_loss",
                        "Loss of taste" = "loss_taste",
                        "Heart disease" = "cardiac",
                        "Hematologic" = "hematologic",
                        "Hepatic" = "hepatic",
                        "Asthma" = "asthma",
                        "Diabetes" = "diabetes",
                        "Neurologic" = "neurologic",
                        "Lung disease" = "pneumologic",
                        "Imunossupression" = "imuno",
                        "Renal" = "renal",
                        "Obesity" = "obesity",
                        "ICU admission" = "icu",
                        "Invasive respiratory support" = "intubation",
                        "Evolution" = "death"
                      ),
                      selected = "death",
                      width = "220px"
                    ),
                    hr(),
                    sliderInput(
                      inputId = "idade3",
                      label = "Age range (months):",
                      min = min(data5$age_month),
                      max = max(data5$age_month),
                      value = c(min(data5$age_month), max(data5$age_month))
                    )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Cross-tabulation",
                               highcharter::highchartOutput("plot31"),
                               verbatimTextOutput("table3"),
                               h3(strong("Fisher's test")),
                               verbatimTextOutput("print3")
                      )
                    )),
                    h3(strong("Obs")),
                    p(
                      "<NA> in the table above indicates the missing or ignored (non-response) cases of the variables in question. In the chart, this information appears in the numbered category (for example, number 2)."
                    )
                  )
                )
                
        ),
        
        ### Models ----
        tabItem(tabName = "modelos",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Select",
                    status = "primary",
                    solidHeader = FALSE,
                    selectInput(
                      inputId = "caracteristicas4",
                      label = "Row variable:",
                      choices = c(
                        "Fever" = "fever",
                        "Cough" = "cough",
                        "Sore throat" = "sore_throat",
                        "Dyspnea" = "dyspnea",
                        "Respiratory discomfort" = "resp_disc",
                        "Desaturation" = "desaturation",
                        "Diarrhea" = "diarrhea",
                        "Vomit" = "vomit",
                        "Abdominal pain" = "abd_pain",
                        "Fatigue" = "fatigue",
                        "Olfactory loss" = "olfac_loss",
                        "Loss of taste" = "loss_taste",
                        "ICU admission" = "icu",
                        "Invasive respiratory support" = "intubation",
                        "Evolution" = "death"
                      ),
                      selected = "death",
                      width = "220px"
                    ),
                    hr(),
                    sliderInput(
                      inputId = "idade4",
                      label = "Age range (months):",
                      min = min(data5$age_month),
                      max = max(data5$age_month),
                      value = c(min(data5$age_month), max(data5$age_month))
                    )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Normal logistic model",
                               verbatimTextOutput("print7"),
                               h3(strong("Envelope Chart")),
                               plotOutput("plot4")
                               )
                    ))
                  )
                ))
      )))

# Server ----
server <- function(input, output, session) {

  output$doc <- renderUI({})
  
  ## Dataset with input filtering----
  selectData2 <- reactive({
    data5 %>%
      dplyr::filter(age_month >= input$idade1[1]) %>%
      dplyr::filter(age_month <= input$idade1[2]) %>%
      #dplyr::filter(variants %in% input$classivariants) %>%
      {
        if (input$na1 == TRUE)
          dplyr::filter(., !is.na(get(input$caracteristicas1)))
        else
          dplyr::filter(., (!is.na(get(
            input$caracteristicas1
          )) | is.na(get(
            input$caracteristicas1
          ))))
      }
  })
  
  ### Cross-table chart ----
  dados_hc_aux <- reactive({
    selectData2() %>%
      count(var = .[["variants"]]) %>%
      mutate(ntot = n) %>%
      select(-n)
  })
  
  dados_hc <- reactive({
    selectData2() %>%
      count(var = .[["variants"]],
            var2 = .[[input$caracteristicas1]]) %>%
      full_join(dados_hc_aux(), by = "var") %>%
      mutate(porc = round((n / ntot) * 100, 2))
  })
  
  output$plot11 <- highcharter::renderHighchart({
    hchart(dados_hc(), type = "column",
           hcaes(x = var,
                 y = porc, group = var2)) %>%
      hc_xAxis(title = list(text = "Variants")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$table1 <- renderPrint({
    st_options(headings = FALSE, display.labels = FALSE)
    with(
      selectData2(),
      summarytools::ctable(
        get(input$caracteristicas1),
        variants,
        prop = "c",
        headings = st_options("headings"),
        display.labels = st_options("display.labels"),
        useNA = "ifany",
        dnn = c(input$caracteristicas1, "Variants"),
        OR = TRUE,
        chisq = TRUE
      )
    )
  })
  
  output$print1 <- renderPrint({
    with(selectData2(),
         fisher.test(variants,get(input$caracteristicas1),simulate.p.value = TRUE))
  })
  
  ## Models 
  
  selectData5 <- reactive({
    data5 %>%
      dplyr::filter(age_month >= input$idade4[1]) %>%
      dplyr::filter(age_month <= input$idade4[2])
  })
  
  # Logistic
  
  output$print7 <- renderPrint({
    summary(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ variants, family = binomial))
  })
  
  output$plot4 <- renderPlot({
    hnp::hnp(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ variants, family = binomial))
  })
  
  ## Dataset with input filtering (smote) ----
  
  selectData_smote <- reactive({
    data5 %>%
      dplyr::filter(age_month >= input$idade2[1]) %>%
      dplyr::filter(age_month <= input$idade2[2]) 
  })
  
  ### Cross-table/plot (original) ----
  
  selectData3 <- reactive({
    dados_smote(selectData_smote(),input$caracteristicas2) #%>% 
      #dplyr::filter(variants %in% input$classivariants2)  
  })
  
  dados_hc_aux2 <- reactive({
    selectData3() %>%
      count(var = .[["variants"]]) %>%
      mutate(ntot = n) %>%
      select(-n)
  })
  
  dados_hc2 <- reactive({
    selectData3() %>%
      count(var = .[["variants"]],
            var2 = .[[input$caracteristicas2]]) %>%
      full_join(dados_hc_aux2(), by = "var") %>%
      mutate(porc = round((n / ntot) * 100, 2))
  })
  
  output$plot21 <- highcharter::renderHighchart({
    hchart(dados_hc2(), type = "column",
           hcaes(x = var,
                 y = porc, group = var2)) %>%
      hc_xAxis(title = list(text = "variants")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$table2 <- renderPrint({
    st_options(headings = FALSE, display.labels = FALSE)
    with(
      selectData3(),
      summarytools::ctable(
        get(input$caracteristicas2),
        variants,
        prop = "c",
        headings = st_options("headings"),
        display.labels = st_options("display.labels"),
        useNA = "ifany",
        dnn = c(input$caracteristicas2, "variants"),
        OR = TRUE,
        chisq = TRUE
      )
    )
  })
  
  output$print2 <- renderPrint({
    with(selectData3(),
         fisher.test(variants,get(input$caracteristicas2),simulate.p.value = TRUE))
  })
  
  ## Dataset with input filtering (downsample) ----
  
  selectData_dowsample <- reactive({
    data5 %>%
      dplyr::filter(age_month >= input$idade3[1]) %>%
      dplyr::filter(age_month <= input$idade3[2]) 
  })
  
  ### Cross-table/plot (original) ----
  
  selectData4 <- reactive({
    dados_downsample(selectData_dowsample(),input$caracteristicas3) #%>% 
     # dplyr::filter(variants %in% input$classivariants3) 
  })
  
  dados_hc_aux3 <- reactive({
    selectData4() %>%
      count(var = .[["variants"]]) %>%
      mutate(ntot = n) %>%
      select(-n)
  })
  
  dados_hc3 <- reactive({
    selectData4() %>%
      count(var = .[["variants"]],
            var2 = .[[input$caracteristicas3]]) %>%
      full_join(dados_hc_aux3(), by = "var") %>%
      mutate(porc = round((n / ntot) * 100, 2))
  })
  
  output$plot31 <- highcharter::renderHighchart({
    hchart(dados_hc3(), type = "column",
           hcaes(x = var,
                 y = porc, group = var2)) %>%
      hc_xAxis(title = list(text = "Variants")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$table3 <- renderPrint({
    st_options(headings = FALSE, display.labels = FALSE)
    with(
      selectData4(),
      summarytools::ctable(
        get(input$caracteristicas3),
        variants,
        prop = "c",
        headings = st_options("headings"),
        display.labels = st_options("display.labels"),
        useNA = "ifany",
        dnn = c(input$caracteristicas3, "Variants"),
        OR = TRUE,
        chisq = TRUE
      )
    )
  })
  
  output$print3 <- renderPrint({
    with(selectData4(),
         fisher.test(variants,get(input$caracteristicas3),simulate.p.value = TRUE))
  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x)input[[x]])
    data <- c(data, timestamp = humanTime())
    data <- t(data)
    data
  })
  
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # Submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
}

shinyApp(ui, server)
