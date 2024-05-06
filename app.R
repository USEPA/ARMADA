source("global.R")
source("ui_elements.R")
addResourcePath(prefix = 'www', directoryPath = './www')
steps <- read.csv("www/help.csv")
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
    includeHTML("www/header.html"),
    includeCSS("www/introjs.min.css"),
    includeScript("www/intro.min.js"),
    includeScript("www/app.js")
  ),
  tags$body(
    gfonts::use_pkg_gfont("roboto"),
    id="custom",
      navbarPage(
        title=
         span("Water Quality Dashboard",
         style = "font-weight: bold; font-size: 40px; text-align: center;
                  text-shadow: 0 3px 2px rgba(0,0,0,.75);"),
        collapsible=TRUE,
        fluid=FALSE,
        id="tabs",
        column(12, align="center",
             conditionalPanel(
               condition = "input.org_idcode == ''",
               selectInput("org_idcode",
                           "Select a State/Territory/Tribe to View Survey Data", 
                           width = "300px", 
                           c("State/Territory/Tribe"="", ORGID)
               ),
               column(12, align="left",
            "Welcome to the Water Quality Dashboard. Section 305(b) of the Federal Clean Water Act requires each state to monitor, assess and report on the quality of its waters relative to
            designated uses established in accordance with state defined water quality standards. The data illustrated in the dashboard were collected using 
            statistically-valid surveys which allow states to extrapolate the results from the sample sites to the broader population of aquatic resources. 
            Information presented are intended to assist states and the public to track progress in addressing water pollution, identify trends over time, recognize emerging problems 
            and determine effectiveness of water management programs. These information are publicly available and accessed through the", tags$a(href='https://www.epa.gov/waterdata/attains', "ATTAINS database.", target="blank")
            )
          )
        ),
        tabPanel(
          div(id="step2",
          span("Condition Summary View",
               style="text-shadow: 0 3px 2px rgba(0,0,0,.75);")),
          value = "all_indicator",
          div(id="step1",
          allIndicatorsOneConditionCategory())
        ),
        tabPanel(
          div(id="step3",
          span("Indicator Summary View",
               style="text-shadow: 0 3px 2px rgba(0,0,0,.75);")),
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
          condition = "input.org_idcode !== ''",
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

    session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(steps)))
    
    myObserver <- observe({
      req(input$org_idcode!="", allindicators_data())
      session$sendCustomMessage(type = 'startHelp', message = list(""))
      myObserver$destroy()
    })

    
    
  # Example: https://rstudio-connect.dmap-stage.aws.epa.gov/content/74a7f241-6aa1-49e7-99c2-3a5278363e29/?org_idcode=WIDNR
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['org_idcode']])) {
      updateSelectInput(session, "org_idcode", selected = query[['org_idcode']])
    }
  })
  
  observe({
    req(input$org_idcode != "")
    insertTab(inputId = "tabs",
              tabPanel(value="help",
                       div(id="step6",
                                  icon('circle-info',
                                  class = "help-icon fa-pull-left"),
                       verify_fa = FALSE,
                       span("Help",
                       style = "font-style:italic;
                                text-shadow: 0 3px 2px rgba(0,0,0,.75);
                                display: flex;
                                flex-wrap: wrap;"))
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


  output$state <- renderUI({
    req(input$org_idcode != "")
    
    selectInput("org_id",
                span("Select a State/Territory/Tribe", 
                     style = "font-weight: bold; font-size: 16px"),
                choices=ORGID,
                selected=input$org_idcode)
  })
  
  Data <- eventReactive(input$org_id, {
    req(input$org_id != "" )
    source("data_prep_trials.R", local = TRUE)$value
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
    req(input$resource_pop != "")
    updateSelectInput(session,
                      "primary_subpop",
                      choices=Data() %>% filter(Resource==input$resource_pop) %>% 
                        arrange(desc(T1_Year)) %>%
                        select(Subpopulation) %>% unique() %>% 
                        pull()
    )
  })
  
  observeEvent(c(input$resource_pop, input$primary_subpop, input$org_id), {
    req(input$resource_pop != "", input$primary_subpop != "")
    updateSelectInput(session,
                      "comp_subpop",
                      choices=c("None", Data() %>% filter(Resource==input$resource_pop & 
                                                          Subpopulation != input$primary_subpop) %>% select(Subpopulation) %>% unique() %>% pull())
    )
    updateSelectInput(session,
                      "condition_category",
                      choices=Data() %>% filter(Resource==input$resource_pop & 
                                                Subpopulation==input$primary_subpop) %>%
                        arrange(desc(T1_Year), factor(Condition, levels = condition_levels))  %>% select(Condition) %>% unique() %>% pull()
    )
    updateSelectInput(session,
                      "indicator",
                      choices=Data() %>% filter(Resource==input$resource_pop & 
                                                Subpopulation==input$primary_subpop) %>% select(Indicator) %>% unique() %>% pull() %>% unname()
    )
  })
  
  observeEvent(c(input$resource_pop, input$primary_subpop, input$org_id), {
    req(ChangeYears())
    updateSelectInput(session,
                      "changediff",
                      choices=rev(ChangeYears())
    )
  })
  
  #### ORG Data ----
  output$exists <- renderText({  
    req(input$org_idcode != "")
    url <- paste0("https://attains.epa.gov/attains-public/api/surveys?organizationId=",input$org_idcode)
    res <- GET(url) 
    json<- fromJSON(rawToChar(res$content))
    data <- json$items
    if("msg" %in% names(json)){
      exists <- 'Please try again later due to an internal server error.'
    } else if(length(data)==0){
      exists <- 'Data Not Available for State/Territory/Tribe.'
    } else {
      exists <- ""
    }
    exists 
    })
  
  #### Subpopualtion Comparison ----
  comp_exists <- reactiveVal(NULL)
  
  output$comp_exists <- eventReactive(c(allindicators_data(), oneindicator_data(), input$indicator),{
    req(Data())
    primary_all_Ind <- Data() %>% filter(Resource==input$resource_pop & Subpopulation == input$primary_subpop & Condition==input$condition_category) %>% 
      select(Indicator) %>% unique() %>% pull() %>% unname()
    secondary_all_Ind <-  Data() %>% filter(Resource==input$resource_pop & Subpopulation != input$primary_subpop & Condition==input$condition_category) %>% 
      select(Indicator) %>% unique() %>% pull() %>% unname()
    primary_all_Cond <- Data() %>% filter(Resource==input$resource_pop & Subpopulation == input$primary_subpop & Condition==input$condition_category) %>% 
      select(Condition) %>% unique() %>% pull() %>% unname()
    secondary_all_Cond <-  Data() %>% filter(Resource==input$resource_pop & Subpopulation != input$primary_subpop & Condition==input$condition_category) %>% 
      select(Condition) %>% unique() %>% pull() %>% unname()
    
    primary_one_Ind <- Data() %>% filter(Resource==input$resource_pop & Subpopulation == input$primary_subpop & Indicator == input$indicator) %>% 
      select(Indicator) %>% unique() %>% pull() %>% unname()
    secondary_one_Ind <-  Data() %>% filter(Resource==input$resource_pop & Subpopulation != input$primary_subpop & Indicator == input$indicator) %>% 
      select(Indicator) %>% unique() %>% pull() %>% unname()
    primary_one_Cond <- Data() %>% filter(Resource==input$resource_pop & Subpopulation == input$primary_subpop & Indicator == input$indicator) %>% 
      select(Condition) %>% unique() %>% pull() %>% unname()
    secondary_one_Cond <-  Data() %>% filter(Resource==input$resource_pop & Subpopulation != input$primary_subpop & Indicator == input$indicator) %>% 
      select(Condition) %>% unique() %>% pull() %>% unname()
    
    if(input$tabs == "all_indicator" & length(intersect(primary_all_Ind, secondary_all_Ind)) > 0 & length(intersect(primary_all_Cond, secondary_all_Cond)) > 0 | 
       input$tabs == "one_indicator" & length(intersect(primary_one_Ind, secondary_one_Ind)) > 0 & length(intersect(primary_one_Cond, secondary_one_Cond)) > 0){
      answer <- "TRUE"
    } else{
      answer <- "FALSE"
    }
    
    comp_exists(answer)
    return(answer)
  })
  
  outputOptions(output, "comp_exists", suspendWhenHidden = FALSE)
  
  
  #### Dashboard Data ----
  Dashboarddata <- eventReactive(c(input$comp_subpop, input$primary_subpop, input$resource_pop, input$org_id),{
    req(input$primary_subpop!="", input$resource_pop!="", input$org_id!="")
    filter(Data(), 
           Resource == input$resource_pop &
           Subpopulation == input$primary_subpop) %>%
      select_if(~any(!is.na(.)))
  })
  
  
  
  
  #### Change Years ----
  ChangeYears <- eventReactive(c(input$primary_subpop, input$resource_pop, input$org_id, Dashboarddata()), {
    req(Dashboarddata(), input$primary_subpop!="", input$resource_pop!="", input$org_id!="")
    
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
  
  
  
  allindicators_data <- reactive({
    req(ChangeYears(), input$condition_category %in% unique(Dashboarddata()$Condition))
    filter(Dashboarddata(),
           Condition == input$condition_category) %>% unique()
  })
  
  
  oneindicator_data <- reactive({
    req(ChangeYears(), input$indicator %in% unique(Dashboarddata()$Indicator))
    filter(Dashboarddata(),
           Indicator == input$indicator) %>% 
      #removes duplicate indicator results (sometimes states will include same indicator for different uses)
    distinct(Resource, Subpopulation, Indicator, T1.P.Estimate, .keep_all = TRUE)
  })
  

  bg <- reactive({
    case_when(input$condition_category %in% c("Fair", "Partially Supporting", "Satisfactory", "Moderate", "Potentially Not Supporting", "Fair Condition", "Intermediate") ~ 
                  '#condition_category ~ .selectize-control.single .selectize-input {border-color: #ffe083; border-width: 3px;
                                        }',
              input$condition_category %in% c("Poor", "Fail", "Not Supporting Use", "Violating", "Suboptimal", "Not Supporting", "Not supporting", "Violates", "Impaired", "Violates Natural", "Detected", "Above Benchmark", "High", "Poor Condition", "Most Disturbed") ~ '#condition_category ~ .selectize-control.single .selectize-input {
                                            border-color: #f99c9c; border-width: 3px;
                                        }',
              input$condition_category %in% c("Missing", "Not Assessed", "Insufficient Information", "Indeterminate", "Unassessed", "Unknown") ~ '#condition_category ~ .selectize-control.single .selectize-input {
                                            border-color: #e8ccbe; border-width: 3px;
                                        }',
              TRUE  ~ '#condition_category ~ .selectize-control.single .selectize-input {
                border-color: #6ba3d6; border-width: 3px;
                }')
  })
  
  comp_bg <- reactive({
    case_when(input$comp_subpop != "None" ~ '#comp_subpop ~ .selectize-control.single .selectize-input {border-color: grey; border-width: 3px;
                                        }',
    TRUE ~ '#comp_subpop ~ .selectize-control.single .selectize-input {
    background-color: #eee; font-weight: bold;
    }')
  })
  
  output$background_change <- renderUI({
    tagList(fluidPage(tags$style(HTML(bg()))))
  })
  output$comp_background <- renderUI({
    tagList(fluidPage(tags$style(HTML(comp_bg()))))
  })
  
  #### All Indicators Dashboard ----
observeEvent(c(allindicators_data(), input$tabs), {
    # Builds the dashboard located in the All Indicators, One Condition Category tab
    output$allindicators <- r2d3::renderD3({
    #  req(input$org_id, input$resource_pop, input$primary_subpop, input$condition_category, input$changediff, !is.na(Dashboarddata()), ChangeYears())
      
      all_indicators_data <- allindicators_data()
      
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
      
      year <- all_indicators_data$T1_Year %>% unique()
      units <- all_indicators_data$Units %>% unique()
      
 comp_exists <- comp_exists()
 margin_top <- if(input$comp_subpop == "None" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ",units, " in ", input$condition_category, " Category")) > 76 &
                  nchar(paste0(input$primary_subpop, " Estimates")) > 115){
   190
 } else if(input$comp_subpop == "None" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ",units, " in ", input$condition_category, " Category")) > 76 &
           nchar(paste0(input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]))) > 115){
   190
 } else if(input$comp_subpop != "None" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) > 76 &
           nchar(paste0("Comparison: ", year, " ", input$primary_subpop, " Estimates | ", year, " ", input$comp_subpop, " Estimates")) > 115){
   190
 } else if(input$comp_subpop != "None" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ", units, " in ", input$condition_category, " Category")) > 76 &
           nchar(paste0("Comparison: ", year, " ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]), " | ", year, " ", input$comp_subpop, " Estimates")) > 115){
   190
 } else if(input$comp_subpop == "None" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ",units, " in ", input$condition_category, " Category")) <= 76 &
           nchar(paste0(input$primary_subpop, " Estimates")) <= 115){
   120
 } else if(input$comp_subpop == "None" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ",units, " in ", input$condition_category, " Category")) <= 76 &
           nchar(paste0(input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]))) <= 115){
   120
 } else if(input$comp_subpop != "None" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ",units, " in ", input$condition_category, " Category")) <= 76 &
           nchar(paste0("Comparison: ", year, " ", input$primary_subpop, " Estimates | ", year, " ", input$comp_subpop, " Estimates")) <= 115){
   120
 } else if(input$comp_subpop != "None" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ",units, " in ", input$condition_category, " Category")) <= 76 &
           nchar(paste0("Comparison: ", year, " ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]), " | ", year, " ", input$comp_subpop, " Estimates")) <= 115){
   120 
 } else {
   160
 } 
      
      getDashboardHeight <- function(data, primary_subpop, indicator, view) {
        primary_subpop_data <- data %>% dplyr::filter( 
          Resource == input$resource_pop &
          Subpopulation == primary_subpop)
        
          num_rows <- primary_subpop_data %>% select(surveyUseCode, Indicator) %>% unique() %>% pull() %>% length()
        # if (view == "one") {
        #   num_rows <- primary_subpop_data$Condition %>% unique() %>% length()
        # }
        
        dashboard_height <- (num_rows * bar_height) + margin_top + margin_bottom
        dashboard_height
      }
      
      dashboard_height <- getDashboardHeight(all_indicators_data, input$primary_subpop, NA, "all")
      
      survey_comment <- all_indicators_data$survey_comment %>% unique()
      
      if(survey_comment==""){
        survey_comment <- "No comments available from State/Territory/Tribe."
      }
      
      if(input$changediff != "Only One Year Available"){
        T1 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 15, end=16)
        T2 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 23, end=24)
      } else {
        T1 <- "T1"
        T2 <- "T2"
      }
      
      comp_year= ""
      if(comp_exists == "TRUE" & input$comp_subpop != "None"){
      all_subpop_data <- filter(Data(), 
                                Resource == input$resource_pop &
                                Subpopulation == input$comp_subpop &
                                Condition == input$condition_category) %>%
        select_if(~any(!is.na(.)))
      comp_year <- all_subpop_data$T1_Year %>% unique()
      
      all_indicators_data <- all_indicators_data %>% bind_rows(all_subpop_data)
      }
      
      # options passed to javascript
      options <- list(survey_comment=survey_comment, units=units, T1=T1, T2=T2, resource = input$resource_pop, primary_subpop = input$primary_subpop, comp_year=comp_year, comp_exists=comp_exists, comp_subpop = input$comp_subpop, condition = input$condition_category, label_format=input$label, height=dashboard_height, comp_subpop=input$comp_subpop, margin_top=margin_top, margin_bottom=margin_bottom, state=names(ORGID_choices[ORGID_choices==input$org_id]), year=year, change=names(ChangeYears()[ChangeYears()==input$changediff]))
      
      # The dashboard is written in d3.js
      # The source code is located in the all_indicators.js file (with additional dependencies)
      r2d3::r2d3(script="www/all_indicators.js", css="www/svg.css", dependencies = c("www/d3-textwrap.min.js", "www/tooltip.js", "www/utils.js", "www/global_var.js"), data=all_indicators_data, d3_version = "6", options = options)
    })
})
  

observeEvent(c(oneindicator_data(), input$tabs), {
  #### One Indicator Dashboard ----
  # Builds the dashboard located in the One Indicator, All Condition Categories tab
  output$oneindicator <- r2d3::renderD3({
    #req(comp_exists(), input$comp_subpop, input$primary_subpop, input$resource_pop, input$indicator, input$changediff, Dashboarddata(), ChangeYears())
    
    one_indicator_data <- oneindicator_data()
    
    if(length(grep(x = colnames(one_indicator_data), pattern = "_Year")) > 1) {
      early_estimate <- paste0(str_sub(input$changediff, start=3, end=4), ".P.Estimate")
      early_year <- paste0(str_sub(input$changediff, start=3, end=4), "_Year")
      #Delete when crow fixes diamond tooltip
      early_lcb <- paste0(str_sub(input$changediff, start=3, end=4), ".LCB")
      early_ucb <- paste0(str_sub(input$changediff, start=3, end=4), ".UCB")
      
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
    
    year <- one_indicator_data$T1_Year %>% unique()
    units <- one_indicator_data$Units %>% unique()
    
 comp_exists <- comp_exists()
 
 
 margin_top <- 
        if(input$comp_subpop == "None" & comp_exists == "FALSE" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) > 76 &
                  nchar(paste0(input$indicator))> 115){
   190
 } else if(input$comp_subpop == "None" & comp_exists == "TRUE" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) > 76 &
           nchar(paste0(input$indicator, " | ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff])))> 115){
   190
 } else if(input$comp_subpop == "None" & comp_exists == "FALSE" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) > 76 &
           nchar(paste0(input$indicator, " | ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]))) > 115){
   190
 } else if(input$comp_subpop != "None" & comp_exists == "TRUE" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) > 76 &
           nchar(paste0(input$indicator, " | Comparison: ", year, " ", input$primary_subpop, " Estimates | ", year, " ", input$comp_subpop, " Estimates")) > 115){
   190
 } else if(input$comp_subpop != "None" & comp_exists == "TRUE" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) > 76 &
           nchar(paste0(input$indicator, " | Comparison: ", year, " ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]), " |", year, " ", input$comp_subpop, " Estimates")) > 115){
   190
 } else if(input$comp_subpop == "None" & comp_exists == "FALSE" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) <= 76 &
           nchar(paste0(input$indicator, " | ", input$primary_subpop, " Estimates")) <= 115){
   120 
 } else if(input$comp_subpop == "None" & comp_exists == "TRUE" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) <= 76 &
           nchar(paste0(input$indicator, " | ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]))) <= 115){
   120 
 } else if(input$comp_subpop == "None" & comp_exists == "FALSE" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | ",year," | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) <= 76 &
           nchar(paste0(input$indicator, " | ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]))) <= 115){
   120 
 } else if(input$comp_subpop != "None" & comp_exists == "TRUE" & input$changediff == "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) <= 76 &
           nchar(paste0(input$indicator, " | Comparison: ", year, " ", input$primary_subpop, " Estimates | ", year, " ", input$comp_subpop, " Estimates")) <= 115){
   120 
 } else if(input$comp_subpop != "None" & comp_exists == "TRUE" & input$changediff != "Only One Year Available" & nchar(paste0(names(ORGID_choices[ORGID_choices==input$org_id]), " | Percent of ", input$resource_pop, " ", units, " in Each Condition Category")) <= 76 &
           nchar(paste0(input$indicator, " | Comparison: ", year, " ", input$primary_subpop, " Estimates and ", names(ChangeYears()[ChangeYears()==input$changediff]), " | ", year, " ", input$comp_subpop, " Estimates")) <= 115){
   120 
 } else {
   160
 } 
 
    getDashboardHeight <- function(data, primary_subpop, indicator, view) {
      primary_subpop_data <- data %>% dplyr::filter(
        Resource == input$resource_pop &
        Subpopulation == primary_subpop)
      
        # num_rows <- primary_subpop_data$Indicator %>% unique() %>% length()
     # if (view == "one") {
        num_rows <- primary_subpop_data$Condition %>% unique() %>% length()
     # }
      
      dashboard_height <- (num_rows * bar_height) + margin_top + margin_bottom
      dashboard_height
    }
    dashboard_height <- getDashboardHeight(one_indicator_data, input$primary_subpop, input$indicator, "one")
    
    survey_comment <- one_indicator_data$survey_comment %>% unique()
    if(survey_comment==""){
      survey_comment <- "No comments available from State/Territory/Tribe."
    }
    
    if(input$changediff != "Only One Year Available"){
      T1 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 15, end=16)
      T2 <- str_sub(names(ChangeYears()[ChangeYears()==input$changediff]), start= 23, end=24)
    } else {
      T1 <- "T1"
      T2 <- "T2"
    }
    
    # option to add asterisk to designated use
    use <- one_indicator_data %>% filter(Indicator == input$indicator) %>% mutate(use_label= case_when(USEsort == "B"~ str_c(Indicator, "*"),
                                                                                                       TRUE ~ str_c(Indicator))) %>% select(use_label) %>% unique() %>% pull()
    
    comp_year <- ""
    
    if(comp_exists == "TRUE" & input$comp_subpop != "None"){
    one_subpop_data <- filter(Data(), 
                              Resource == input$resource_pop &
                              Subpopulation == input$comp_subpop &
                              Indicator == input$indicator) %>%
      select_if(~any(!is.na(.)))
    
    comp_year <- one_subpop_data$T1_Year %>% unique()
    one_indicator_data <- one_indicator_data %>% bind_rows(one_subpop_data) %>% unique()
    }
    # options passed to javascript
    options <- list(use=use, survey_comment=survey_comment, units=units, T1=T1, T2=T2, resource = input$resource_pop, primary_subpop = input$primary_subpop, comp_year=comp_year, comp_exists=comp_exists, comp_subpop = input$comp_subpop, indicator = input$indicator, label_format=input$label, comp_subpop = input$comp_subpop, height=dashboard_height, margin_top=margin_top, margin_bottom=margin_bottom, state=names(ORGID_choices[ORGID_choices==input$org_id]), year=year, change=names(ChangeYears()[ChangeYears()==input$changediff]))
    
    # The dashboard is written in d3.js
    # The source code is located in the one_indicator.js file (with additional dependencies)
    r2d3::r2d3(script="www/one_indicator.js", css="www/svg.css", dependencies = c("www/d3-textwrap.min.js", "www/tooltip.js", "www/utils.js", "www/global_var.js"), data=one_indicator_data, d3_version = "6", options = options)
  })
})  
  
  session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)