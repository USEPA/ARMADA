# ORGID<-list(States=
# c("Alabama"="21AWIC","Alaska"="AKDECWQ","Arizona"="21ARIZ","Arkansas"="ARDEQH2O","California"="CA_SWRCB",
# "Colorado"="21COL001","Connecticut"="CT_DEP01","Delaware"="21DELAWQ","Georgia"="21GAEPD","Florida"="21FL303D",
# "Hawaii"="21HI","Idaho"="IDEQ","Illinois"="IL_EPA","Indiana"="21IND","Iowa"="21IOWA","Kansas"="21KAN001",
# "Kentucky"="21KY","Louisiana"="LADEQWPD","Maine"="MEDEP","Maryland"="MDE_EASP","Massachusetts"="MA_DEP",
# "Michigan"="21MICH","Minnesota"="MNPCA","Mississippi"="21MSWQ","Missouri"="MDNR","Montana"="MTDEQ","Nebraska"="21NEB001",
# "Nevada"="21NEV1","New Hampshire"="11113300","New Jersey"="21NJDEP1","New Mexico"="21NMEX","New York"="21NYDECA",
# "North Carolina"="21NC01WQ","North Dakota"="21NDHDWQ","Oklahoma"="OKDEQ","Oregon"="OREGONDEQ","Pennsylvania"="21PA",
# "Rhode Island"="RIDEM","South Carolina"="21SC60WQ","South Dakota"="SDDENR","Tennessee"="TDECWR","Texas"="TCEQMAIN",
# "Utah"="UTAHDWQ","Vermont"="1VTDECWQ","Virginia"="21VASWCB","Washington"="WA_ECOLOGY","West Virginia"="WVDEP",
# "Wisconsin"="WIDNR","Wyoming"="WYDEQ"),
#            Territories=
# c("American Samoa"="21AS","District of Columbia"="DOEE","Guam"="21GUAM","Northern Mariana Islands"="21AQ",
# "Puerto Rico"="PR_LAKES","US Virgin Islands"="USVIST"))
#
# 
# 
#url <- paste0("https://attains.epa.gov/attains-public/api/surveys?organizationId=AKDECWQ")
# ATTAINS API ----
# Connect to ATTAINS database and pull survey data by Organization ID 

url <- paste0("https://attains.epa.gov/attains-public/api/surveys?organizationId=",input$org_id)
res <- GET(url) 
json <- fromJSON(rawToChar(res$content))
data <- json$items

shiny::validate(
  need(!("msg" %in% names(json)), 'The request could not be completed due to an internal server error.'),
  need(length(data)!=0, 'Data Not Available for State/Territory/Tribe.')
)

show_modal_spinner(spin = "circle", text = 'Fetching Data')

#Format Survey Data ----
year <- pluck(data[["surveys"]][[1]][["year"]])
surveys <- list_flatten(data$surveys[[1]][["surveyWaterGroups"]]) %>%
  set_names(year)



df <- as.data.frame(do.call(rbind, lapply(surveys , as.data.frame))) %>% 
  mutate(Year=sub("\\..*","",row.names(.)), .before=waterTypeGroupCode)
resource <- pluck(df$waterTypeGroupCode)
subpop <- pluck(df$subPopulationCode)
year <- pluck(df$Year)
units <- pluck(df$unitCode)
survey_comment <- df %>% mutate(Year=as.numeric(Year)) %>% filter(Year==max(Year)) %>%
  select(T1_Year=Year, Resource=waterTypeGroupCode, Subpopulation=subPopulationCode, Units=unitCode, Survey_Size=size, siteNumber, survey_comment=surveyWaterGroupCommentText) %>%
  #remove_rownames() %>% 
  pluck()
row.names(survey_comment) <- NULL

df2 <- list_flatten(df$surveyWaterGroupUseParameters) %>%
  set_names(paste0(year,"_",resource,"_",subpop,"_",units))

df3 <- as.data.frame(do.call(rbind, lapply(df2 , as.data.frame))) %>% 
  mutate(GROUP=sub("\\..*","",row.names(.)),
         stressor=case_when(is.na(stressor) ~ surveyUseCode, 
                            TRUE ~ stressor),
         .before=stressor) %>%
  mutate(LCB=metricValue-marginOfError,
         UCB=metricValue+marginOfError,
         .after=marginOfError) %>%
  separate(GROUP,into=c("Year", "Resource", "Subpopulation", "Units"), 
           sep="_") %>%
  #remove_rownames() %>% 
  mutate(Year=as.numeric(Year)) %>%
  arrange(desc(Year), Resource, stressor) %>%
  select(-statistic, -marginOfError) %>%
  rename(Indicator=stressor,
         Condition=surveyCategoryCode)

row.names(df3 ) <- NULL
# Current Surveys----
current_surveys <- df3 %>%
  group_by(Resource, Subpopulation, Units) %>%
  filter(Year==max(Year)) %>%
  ungroup() %>%
  rename(T1_Year=Year,
         T1.P.Estimate=metricValue,
         T1.LCB=LCB,
         T1.UCB=UCB)

all_surveys <- current_surveys

#Identifies if there are any past surveys
past_surveys <- df3 %>%
  group_by(Resource, Subpopulation, Units) %>%
  filter(!(Year==max(Year))) %>%
  ungroup()

# Calculation of past surveys
if(nrow(past_surveys) > 0) {

# Past Surveys----
list <- list()
past_surveys <- df3 %>%
  group_by(Resource, Subpopulation, Units) %>%
  filter(!(Year==max(Year))) %>%
  ungroup() %>%
  select(-commentText, -confidenceLevel) %>%
  group_split(Year) %>%
  set_names(map_chr(., ~as.character(.x$Year[1]))) %>%
  append(list) %>%
  rev()

#Notice i+1 for joining to current surveys later
for(i in 1:length(past_surveys)) {
  timename <- paste0("T", i+1)
  names(past_surveys)[i] <- timename
}

#consider deleting
past_surveys <- discard(past_surveys, is.null)

# Past Surveys----
add_my_col <- function(df, index_name) {
  df %>% 
    rename("{index_name}_Year" := Year,
           "{index_name}.P.Estimate" := metricValue,
           "{index_name}.LCB" := LCB,
           "{index_name}.UCB" := UCB)
}

past_surveys <- imap(past_surveys, add_my_col) %>%
  purrr::reduce(dplyr::full_join, by = c("Resource", "Subpopulation", "Indicator", "surveyUseCode", "Condition", "Units")) 

all_surveys <- left_join(current_surveys, past_surveys, by = join_by(Resource, Subpopulation, Units, Indicator, surveyUseCode, Condition))


#loop to calculate change estimates
for(i in 2:length(grep(x = colnames(all_surveys), pattern = ".P.Estimate"))) {
  diffnew <- all_surveys %>%
    mutate(!!paste0("T1T",i,"_CHANGE") := T1.P.Estimate - !!sym(paste0("T",i,".P.Estimate"))) %>%
    select(paste0("T1T",i,"_CHANGE"))
  all_surveys <- cbind(all_surveys, diffnew)
  }
}




all_surveys <- all_surveys %>%
  left_join(survey_comment, by = join_by(T1_Year, Resource, Subpopulation, Units)) %>%
  arrange(desc(T1_Year), factor(Subpopulation, levels = c("Statewide"))) %>%
  mutate(Subpopulation = paste0(Subpopulation," (",Units,")")) %>%
  #adding comments and a sort dummy variable to put Designated use estimates above stressors and OVERALL at bottom of dashboard
  mutate(commentText = if_else(is.na(commentText),"",commentText),
         survey_comment = if_else(is.na(survey_comment ),"",survey_comment),
         #Indicator = str_remove(Indicator, "condition|Condition|CONDITION"),
         USEsort = case_when(#str_detect(Indicator, "CONDITION|Condition|condition") ~ "A",
                             surveyUseCode==Indicator ~ "B",
                             TRUE ~ "A"),
         OVERALLsort = case_when(str_detect(Indicator, "OVERALL|Overall|overall") ~ "Z",
                             TRUE ~ "A")) %>%
  #This will remove columns if past surveys were different than current surveys
  #select_if(~any(!is.na(.))) %>%
  mutate_at(vars(contains("UCB")), ~ifelse((.>100), 100, .)) %>%
  mutate_at(vars(contains("LCB")), ~ifelse((.<0), 0, .)) %>%
  mutate(Condition_Size =  round(Survey_Size*(T1.P.Estimate*0.01),0),
        T1.LCB_Size = round(Survey_Size*(T1.LCB*0.01),0),
        T1.UCB_Size = round(Survey_Size*(T1.UCB*0.01),0),
        Survey_Size = round(Survey_Size,0)) %>%
  #Cleans and orders indicators, conditions and resources
  mutate(Indicator = case_when(Indicator %in% c("pH", "PH") ~ "pH",
                               TRUE ~ str_to_title(Indicator)),
         Condition = str_to_title(Condition),
         Resource = str_to_title(Resource)) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "[)]") & USEsort=="A" ~ myCap.2(Indicator),
                               TRUE ~ Indicator)) %>%
  arrange(OVERALLsort, surveyUseCode, desc(USEsort)) %>%
  arrange(factor(Condition, levels = condition_levels))
  
remove_modal_spinner()

all_surveys

