# All Indicators, One Condition Category
allIndicatorsOneConditionCategory <- function() {
  tags$div(
    class="dash-container",
    tags$div(
      class="chart-container",
        r2d3::d3Output("allindicators", height="100%", width="100%")
    )
  )
}


# One Indicator, All Condition Categories Dashboard
oneIndicatorAllConditionCategories <- function() {
  tags$div(
    class="dash-container",
    tags$div(
      class="chart-container",
      r2d3::d3Output("oneindicator", height="100%", width="100%")
    )
  )
}


# Dashboard Control Panel
controlPanel <- function() {
 
    tags$div(
      style = "font-weight: bold; font-size: 12px;", 
      class="controls-container",
      uiOutput('state'),
      tags$head(tags$style(HTML("#resource_pop ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
      selectInput("resource_pop",
                  span("Select a Resource Type", 
                       style = "font-weight: bold; font-size: 14px"),
                  choices = "") %>%
        #Resource Type helper
        helper(type = "inline",
               icon = "circle-question",
               title = "Resource Type",
               content = c("The Resource Types shown are the surveys which the state/territory has reported. Explore water quality conditions across Resource Types, if available."),
               size = "s", easyClose = TRUE, fade = TRUE),
      tags$head(tags$style(HTML("#primary_subpop ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
      selectInput("primary_subpop",
                  span("Select a Subpopulation", 
                       style = "font-weight: bold; font-size: 14px"),
                  choices = "") %>%
        #Subpopulation helper
        helper(type = "inline",
               icon = "circle-question",
               title = "Subpopulation",
               content = c("Ocassionally, State/Territories will sample smaller scale areas rather than statewide. Explore water quality conditions across Resource Types, if available."),
               size = "s", easyClose = TRUE, fade = TRUE),
      conditionalPanel(
        condition = "input.tabs == 'all_indicator'",
        tags$head(tags$style(HTML("#condition_category ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
        selectInput("condition_category",
                    span("Select a Condition Category", 
                         style = "font-weight: bold; font-size: 14px"),
                    choices = "") %>%
          #Condition Category helper
          helper(type = "inline",
                 icon = "circle-question",
                 title = "Condition Category",
                 content = c("Condition Categories are set by states to determine condition based on certain thresholds or benchmarks. Use the input to explore the results for different Condition Categories."),
                 size = "s", easyClose = TRUE, fade = TRUE)
      ),
      conditionalPanel(
        condition = "input.tabs == 'one_indicator'",
        tags$head(tags$style(HTML("#indicator ~ .selectize-control.single .selectize-input {border-color: black; border-width: 3px; background-color: #eee; font-weight: bold;}"))),
        selectInput("indicator",
                    span("Select an Indicator", 
                         style = "font-weight: bold; font-size: 14px"),
                    choices = "") %>%
          #Indicator helper
          helper(type = "inline",
                 icon = "circle-question",
                 title = "Indicator",
                 content = c("An Indicator represents the state or trend of certain environmental conditions over a given area and a specified period of time. Use the input to explore the results for different Indicators."),
                 size = "s", easyClose = TRUE, fade = TRUE)
      ),
      uiOutput('background_change'),
      tags$head(tags$style(HTML("#changediff ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
      selectInput("changediff",
                  span("Select Change Comparison", 
                       style = "font-weight: bold; font-size: 14px"),
                  choices = "") %>%
        #Change helper
        helper(type = "inline",
               icon = "circle-question",
               title = "Change Comparison",
               content = c("Often States/Territories have collected data over numerous years. Use the input to compare the results across available years."),
               size = "s", easyClose = TRUE, fade = TRUE),
      tags$head(tags$style(HTML("#label ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
      selectInput("label",
                  span("Select Label Options", 
                       style = "font-weight: bold; font-size: 14px"),
                  list('Point Estimate', 'Confidence Intervals', 'None')) %>%
        #Label helper
        helper(type = "inline",
               icon = "circle-question",
               title = "Label Options",
               content = c("Choose a Label to display in the dashboard."),
               size = "s", easyClose = TRUE, fade = TRUE),
      conditionalPanel(
        condition = "input.tabs == 'all_indicator'",
        capture::capture(
          selector = "#allindicators",
          format = "jpeg",
          filename = "All_Indicators_Dashboard.png",
          icon("camera"), 
          "Screenshot of Dashboard"
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'one_indicator'",
      capture::capture(
        selector = "#oneindicator",
        format = "jpeg",
        filename = "One_Indicators_Dashboard.png",
        icon("camera"), 
        "Screenshot of Dashboard"
      )
     )
    )
}
