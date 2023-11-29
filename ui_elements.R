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
      style = "font-size: 12px", 
      class="controls-container",
      uiOutput('state'),
        tags$head(tags$style(HTML("#resource_pop ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
        tags$head(
          tags$style(
            HTML("#resource_pop {font-size: 14px;}"))),
      
      selectInput("resource_pop",
                  span("Select a Resource Type", 
                       style = "font-weight: bold; font-size: 14px"),
                  choices = ""),
      tags$head(tags$style(HTML("#primary_subpop ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
      selectInput("primary_subpop",
                  span("Select a Subpopulation", 
                       style = "font-weight: bold; font-size: 14px"),
                  choices = ""),
       conditionalPanel(
         condition = "input.tabs == 'all_indicator'",
         tags$head(tags$style(HTML("#condition_category ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
         selectInput("condition_category",
                     span("Select a Condition Category", 
                          style = "font-weight: bold; font-size: 14px"),
                     choices = "")
       ),
       conditionalPanel(
         condition = "input.tabs == 'one_indicator'",
         tags$head(tags$style(HTML("#indicator ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
         selectInput("indicator",
                     span("Select an Indicator", 
                          style = "font-weight: bold; font-size: 14px"),
                     choices = "")
       ),
       tags$head(tags$style(HTML("#changediff ~ .selectize-control.single .selectize-input {background-color: #f07857; font-weight: bold;}"))),
       selectInput("changediff",
                   span("Select Change Comparison", 
                        style = "font-weight: bold; font-size: 14px"),
                   choices = ""
      ),
      tags$head(tags$style(HTML("#label ~ .selectize-control.single .selectize-input {background-color: #eee; font-weight: bold;}"))),
      selectInput("label",
                  span("Select Label Options", 
                       style = "font-weight: bold; font-size: 14px"),
                  list('Point Estimate', 'Confidence Intervals', 'None')),
      
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
