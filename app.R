source("global.R")
source("ui_elements.R")
addResourcePath(prefix = 'www', directoryPath = './www')

#### UI----
ui <-  tagList(
  # List the first level UI elements here
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$html(class = "no-js", lang = "en"),
    HTML("<div id='eq-disclaimer-banner' class='padding-1 text-center text-white bg-secondary-dark'><strong>EPA development environment:</strong> The
      content on this page is not production ready. This site is being used
      for <strong>development</strong> and/or <strong>testing</strong> purposes
      only.</div>"),
    includeHTML("www/header.html")
  ),
  tags$body(
    gfonts::use_pkg_gfont("roboto"),
    id="custom",
      navbarPage(
        title=span("Water Quality Dashboard", 
                   style = "font-weight: bold; font-size: 40px;"),
        collapsible=TRUE,
        fluid=FALSE,
        id="tabs",
        column(12, align="center",
             conditionalPanel(
               condition = "input.org_idcover == ''",
               selectInput("org_idcover",
                           "Select a State/Territory/Tribe to View Survey Data", 
                           width = "300px", 
                           c("Select State/Territory/Tribe"="", ORGID)
               ),
               column(12, align="left",
                      "Section 305(b) of the Federal Clean Water Act (CWA) requires each State to monitor, assess and report on the quality of its waters relative to
            designated uses established in accordance with state defined water quality standards. The data illustrated in the dashboard were collected using 
            probability, or statistically based sampling, which allows states to extrapolate the results from the sample sites to the broader population of aquatic resources. 
            Information presented are intended to assist states and the public to track progress in addressing water pollution, identify trends over time, recognize emerging problems 
            and determine effectiveness of water management programs."
            )
          )
        ),
        tabPanel(
          "Condition Summary View",
          value = "all_indicator",
          allIndicatorsOneConditionCategory()
        ),
        tabPanel(
          "Indicator Summary View",
          value = "one_indicator",
          oneIndicatorAllConditionCategories()
        ),
        conditionalPanel(
          condition = "input.resource_pop == ''",
          div(style="color: #888; font-weight: bold; font-size: x-large;",
              textOutput("exists")), 
          br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
        ),
        conditionalPanel(
          condition = "input.org_idcover !== ''",
          style = "display: none;",
          controlPanel(),
          br(),br(),br(),br(),br(),br()
        )
      )#navbar
  )#$tagsBody
  ,includeHTML("www/footer.html")
)#tag$list  

server <- function(input, output, session) {
  
  observe_helpers()
  
  # Example: https://rstudio-connect.dmap-stage.aws.epa.gov/content/74a7f241-6aa1-49e7-99c2-3a5278363e29/?org_idcover=WIDNR
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['org_idcover']])) {
      updateSelectInput(session, "org_idcover", selected = query[['org_idcover']])
    }
  })
  
  observe({
    req(input$org_idcover != "")
    insertTab(inputId = "tabs",
                         tabPanel(value="help",
                                  icon = icon('circle-info'),
                                  verify_fa = FALSE,
                                  span("Help",
                                       style = "font-style:italic;")
                                  ))
  })
  
  rv = reactiveValues()
  #trigger event on tab selection change
  observeEvent(input$tabs, {
    req(input$tabs %in% c("all_indicator","one_indicator"))
    #store old current tab as last tab reactive value
    rv$last_tab = input$tabs
  })

  observe({
    req(input$tabs=="help")
    if(rv$last_tab == "all_indicator"){
      shinyalert(
                 imageUrl ='www/help_graphic_ALL.svg',
                 closeOnClickOutside = TRUE,
                 imageWidth = '900',
                 imageHeight = '550',
                 size = "l"
      )} else {
        shinyalert(
          imageUrl ='www/help_graphic_ONE.svg',
          closeOnClickOutside = TRUE,
          imageWidth = '900',
          imageHeight = '550',
          size = "l"
      )}
      updateNavbarPage(session, "tabs",
                       selected = rv$last_tab)
  })

  statecover <- eventReactive(input$org_idcover,{
    req(input$org_idcover != "")
    statecover <- input$org_idcover
  })
  
  output$state <- renderUI({
    req(statecover())
    statecover <- statecover()
    selectInput("org_id",
                span("Select a State/Territory/Tribe", 
                     style = "font-weight: bold; font-size: 16px"),
                choices=ORGID,
                selected=statecover)
  })
  
  #### ObserveEvents----
  observeEvent(input$org_id, {
    req(input$org_id != "")
    updateSelectInput(session,
                      "resource_pop",
                      choices=unique(Data()$Resource)
    )
  })
  observeEvent(c(input$resource_pop, input$org_id), {
    updateSelectInput(session,
                      "primary_subpop",
                      choices=Data() %>% filter(Resource==input$resource_pop) %>% select(Subpopulation) %>% unique() %>% 
                        arrange(factor(Subpopulation, levels = c("Statewide (Miles)", "Statewide (Acres)"))) %>% pull()
    )
  })
  observeEvent(c(input$primary_subpop, input$resource_pop, input$org_id), {
    updateSelectInput(session,
                      "condition_category",
                      choices=Data() %>% filter(Resource==input$resource_pop & 
                                                Subpopulation==input$primary_subpop) %>% select(Condition) %>% unique() %>%
                        arrange(factor(Condition, levels = c("Excellent", "Excellent Condition", "Very Good", "Optimal", "Pass", "Good", "Supporting Use", "Meeting", "Fully Supporting", "Fully supporting", "Meets", "Supports", "Support", "Not Detected", "At or Below Benchmark", "Low", "Attaining", "Good Condition", "Least Disturbed",
                                                             "Fair", "Inconclusive", "Partially Supporting", "Satisfactory", "Moderate", "Potentially Not Supporting", "Fair Condition", "Intermediate",
                                                             "Poor", "Fail", "Not Supporting Use", "Violating", "Suboptimal", "Not Supporting", "Not supporting", "Violates", "Impaired", "Violates Natural", "Detected", "Above Benchmark", "High", "Poor Condition", "Most Disturbed",
                                                             "Missing", "Not Assessed", "Insufficient Information"))) %>% pull()
    )
    updateSelectInput(session,
                      "indicator",
                      choices=Data() %>% filter(Resource==input$resource_pop & 
                                                Subpopulation==input$primary_subpop) %>% select(Indicator) %>% unique() %>% pull()
    )
  })
  

  observeEvent(c(input$primary_subpop, input$resource_pop, input$org_id), {
    req(ChangeYears())
    updateSelectInput(session,
                      "changediff",
                      choices=rev(ChangeYears())
    )
  })
  #### ORG Data ----
  output$exists <- renderText({  
    req(input$org_idcover != "")
    url <- paste0("https://attains.epa.gov/attains-public/api/surveys?organizationId=",input$org_idcover)
    res <- GET(url) 
    json<- fromJSON(rawToChar(res$content))
    data <- json$items
    
    if(length(data)==0){
      exists <- 'Data Not Available for State/Territory/Tribe.'
    } else {
      exists <- ""
    }
    exists 
    })
  
  
  Data <- eventReactive(input$org_id, {
    req(input$org_id != "" )
    
    source("data_prep_trials.R", local = TRUE)$value
  })
  
  #### Dashboard Data ----
  Dashboarddata <- eventReactive(c(input$primary_subpop, input$resource_pop, input$org_id),{
    req(input$primary_subpop, input$resource_pop, input$org_id)
    filter(Data(), 
           Resource == input$resource_pop &
             Subpopulation == input$primary_subpop) %>%
      select_if(~any(!is.na(.)))
  })
  
  
  
  
  #### Change Years ----
  ChangeYears <- eventReactive(c(input$primary_subpop, input$resource_pop, input$org_id, Dashboarddata()), {
    req(Dashboarddata(), input$primary_subpop, input$resource_pop, input$org_id)
    
    changechoices <- Dashboarddata() 
    
    if(length(grep(x = colnames(changechoices), pattern = "_Year")) > 1) {
      changenames <- c()
      changeyears <- c()
      for(i in 2:length(grep(x = colnames(changechoices), pattern = ".P.Estimate"))) {
        T1 <- changechoices %>% pull(T1_Year) %>% unique() %>% na.omit()
        T2 <- changechoices %>% pull(paste0("T",i,"_Year")) %>% unique() %>% na.omit()
        tperiod <- paste0("Change from ",T2," to ",T1)
        changenames <- c(changenames, tperiod)
        changeyears <- c(changeyears, paste0("T1T",i,"_CHANGE"))
        names(changeyears) <- changenames
      }
    } else {
      changeyears <- c("No Change Available"="Only One Year Available")
    }
    changeyears
  })
  
  
  
  allindicator_data <- reactive({
    req(ChangeYears(), input$condition_category %in% unique(Dashboarddata()$Condition))
    filter(Dashboarddata(),
           Condition == input$condition_category)
  })
  
  
  oneindicator_data <- reactive({
    req(ChangeYears(), input$indicator %in% unique(Dashboarddata()$Indicator))
    filter(Dashboarddata(),
           Indicator == input$indicator)
  })
  

  bg <- reactive({
    case_when(input$condition_category %in% c("Fair", "Partially Supporting", "Satisfactory", "Moderate", "Potentially Not Supporting", "Fair Condition", "Intermediate") ~ 
                  '#condition_category ~ .selectize-control.single .selectize-input {border-color: #ffe083; border-width: 3px;
                                        }',
              input$condition_category %in% c("Poor", "Fail", "Not Supporting Use", "Violating", "Suboptimal", "Not Supporting", "Not supporting", "Violates", "Impaired", "Violates Natural", "Detected", "Above Benchmark", "High", "Poor Condition", "Most Disturbed") ~ '#condition_category ~ .selectize-control.single .selectize-input {
                                            border-color: #f99c9c; border-width: 3px;
                                        }',
              input$condition_category %in% c("Missing", "Not Assessed", "Insufficient Information") ~ '#condition_category ~ .selectize-control.single .selectize-input {
                                            border-color: #e8ccbe; border-width: 3px;
                                        }',
              TRUE  ~ '#condition_category ~ .selectize-control.single .selectize-input {
                border-color: #6ba3d6; border-width: 3px;
                }')
  })
  
  
  
  output$background_change <- renderUI({
    tagList(fluidPage(tags$style(HTML(bg()))))
  })
  

  
  #### All Indicators Dashboard ----
observeEvent(allindicator_data(),{
    # Builds the dashboard located in the All Indicators, One Condition Category tab
    output$allindicators <- r2d3::renderD3({
      req(input$org_id, input$resource_pop, input$primary_subpop, input$condition_category, input$changediff, !is.na(Dashboarddata()), ChangeYears())
      
      all_indicators_data <- allindicator_data()
      
      if(length(grep(x = colnames(all_indicators_data), pattern = "_Year")) > 1) {
        early_estimate <- paste0(str_sub(input$changediff, start= 3, end=4), ".P.Estimate")
        early_year <- paste0(str_sub(input$changediff, start= 3, end=4), "_Year")
        #Delete when crow fixes diamond tooltip
        early_lcb <- paste0(str_sub(input$changediff, start= 3, end=4), ".LCB")
        early_ucb <- paste0(str_sub(input$changediff, start= 3, end=4), ".UCB")
        
        
        
        all_indicators_data <- all_indicators_data %>%
          rename(T1T2_DIFF.P = input$changediff,
                 Early.P.Estimate = early_estimate,
                 Early_Year = early_year,
                 Early.LCB = early_lcb,
                 Early.UCB = early_ucb) %>%
          mutate(changeT1.P.Estimate = case_when(is.na(Early.P.Estimate) ~ NA,
                                                 TRUE ~ T1.P.Estimate),
                 #remove once Crow fixes diamond tooltip
                 CHANGE_LCB = T1T2_DIFF.P-1,
                 CHANGE_UCB = T1T2_DIFF.P+1)
      } else {
        all_indicators_data <- all_indicators_data %>%
          mutate(changeT1.P.Estimate = NA,
                 T1T2_DIFF.P = NA)
      }
      
      getDashboardHeight <- function(data, primary_subpop, indicator, view) {
        primary_subpop_data <- data %>% dplyr::filter(
          Resource == input$resource_pop &
            Subpopulation == primary_subpop)
        
        num_rows <- primary_subpop_data$Indicator %>% unique() %>% length()
        if (view == "one") {
          num_rows <- primary_subpop_data$Condition %>% unique() %>% length()
        }
        
        dashboard_height <- (num_rows * bar_height) + margin_top + margin_bottom
        dashboard_height
      }
      
      
      dashboard_height <- getDashboardHeight(all_indicators_data, input$primary_subpop, NA, "all")
      year <- all_indicators_data %>% select(T1_Year) %>% unique() %>% pull()
      units <- all_indicators_data %>% select(Units) %>% unique() %>% pull()
      survey_comment <- all_indicators_data %>% select(survey_comment) %>% unique() %>% pull()
      if(input$changediff != "Only One Year Available"){
        T1 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 15, end=16)
        T2 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 23, end=24)
      } else {
        T1 <- "T1"
        T2 <- "T2"
      }
      
      # options passed to javascript
      options <- list(survey_comment=survey_comment, units=units, T1=T1, T2=T2, resource = input$resource_pop, primary_subpop = input$primary_subpop, comp_subpop = input$comp_subpop, condition = input$condition_category, label_format=input$label, height=dashboard_height, margin_top=margin_top, margin_bottom=margin_bottom, state=names(ORGID_choices[ORGID_choices==input$org_id]), year=year, change=names(ChangeYears()[ChangeYears()==input$changediff]))
      
      # The dashboard is written in d3.js
      # The source code is located in the all_indicators.js file (with additional dependencies)
      r2d3::r2d3(script="www/all_indicators.js", css="www/svg.css", dependencies = c("www/d3-textwrap.min.js", "www/tooltip.js", "www/utils.js", "www/global_var.js"), data=all_indicators_data, d3_version = "6", options = options)
    })
})
  observeEvent(oneindicator_data(),{
  #### One Indicator Dashboard ----
  # Builds the dashboard located in the One Indicator, All Condition Categories tab
  output$oneindicator <- r2d3::renderD3({
    req(input$primary_subpop, input$resource_pop, input$indicator, input$changediff, Dashboarddata(), ChangeYears())
    
    one_indicator_data <- oneindicator_data()
    
    if(length(grep(x = colnames(one_indicator_data), pattern = "_Year")) > 1) {
      early_estimate <- paste0(str_sub(input$changediff, start= 3, end=4), ".P.Estimate")
      early_year <- paste0(str_sub(input$changediff, start= 3, end=4), "_Year")
      #Delete when crow fixes diamond tooltip
      early_lcb <- paste0(str_sub(input$changediff, start= 3, end=4), ".LCB")
      early_ucb <- paste0(str_sub(input$changediff, start= 3, end=4), ".UCB")
      
      one_indicator_data <- one_indicator_data %>%
        rename(T1T2_DIFF.P = input$changediff,
               Early.P.Estimate = early_estimate,
               Early_Year = early_year,
               Early.LCB = early_lcb,
               Early.UCB = early_ucb) %>%
        mutate(changeT1.P.Estimate = case_when(is.na(Early.P.Estimate) ~ NA,
                                               TRUE ~ T1.P.Estimate),
               CHANGE_LCB = T1T2_DIFF.P-1,
               CHANGE_UCB = T1T2_DIFF.P+1)
    } else {
      one_indicator_data <- one_indicator_data %>%
        mutate(changeT1.P.Estimate = NA,
               T1T2_DIFF.P = NA)
    }
    
    getDashboardHeight <- function(data, primary_subpop, indicator, view) {
      primary_subpop_data <- data %>% dplyr::filter(
        Resource == input$resource_pop &
          Subpopulation == primary_subpop)
      
      num_rows <- primary_subpop_data$Indicator %>% unique() %>% length()
      if (view == "one") {
        num_rows <- primary_subpop_data$Condition %>% unique() %>% length()
      }
      
      dashboard_height <- (num_rows * bar_height) + margin_top + margin_bottom
      dashboard_height
    }
    dashboard_height <- getDashboardHeight(one_indicator_data, input$primary_subpop, input$indicator, "one")
    year <- one_indicator_data %>% select(T1_Year) %>% unique() %>% pull()
    units <- one_indicator_data %>% select(Units) %>% unique() %>% pull()
    survey_comment <- one_indicator_data %>% select(survey_comment) %>% unique() %>% pull()
    if(input$changediff != "Only One Year Available"){
      T1 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 15, end=16)
      T2 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 23, end=24)
    } else {
      T1 <- "T1"
      T2 <- "T2"
    }
    
    # option to add asterick to designated use
    use <- one_indicator_data %>% filter(Indicator == input$indicator) %>% mutate(use_label= case_when(USEsort == "B"~ str_c(Indicator, "*"), 
                                                                                                       TRUE ~ str_c(Indicator))) %>% select(use_label) %>% unique() %>% pull()
    
    # options passed to javascript
    options <- list(use=use, survey_comment=survey_comment, units=units,T1=T1, T2=T2, resource = input$resource_pop, primary_subpop = input$primary_subpop, comp_subpop = input$comp_subpop, indicator = input$indicator, label_format=input$label, height=dashboard_height, margin_top=margin_top, margin_bottom=margin_bottom, state=names(ORGID_choices[ORGID_choices==input$org_id]), year=year, change=names(ChangeYears()[ChangeYears()==input$changediff]))
    
    # The dashboard is written in d3.js
    # The source code is located in the one_indicator.js file (with additional dependencies)
    r2d3::r2d3(script="www/one_indicator.js", css="www/svg.css", dependencies = c("www/d3-textwrap.min.js", "www/tooltip.js", "www/utils.js", "www/global_var.js"), data=one_indicator_data, d3_version = "6", options = options)
  })
})  
  
  session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)