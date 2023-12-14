library(shiny)
library(tidyverse)
library(gfonts)
library(httr)
library(jsonlite)
library(r2d3)
library(capture)
library(shinyalert)
library(shinyhelper)
library(shinybusy)

options(warn=-1)

margin_top <- 160.0
margin_bottom <- 200.0
bar_height <- 30

# ORGID <- c("Alaska"="AKDECWQ", "Arizona"="21ARIZ", "Colorado"="21COL001", "Connecticut"="CT_DEP01", "Florida"="21FL303D", "Iowa"="21IOWA", "Kansas"="21KAN001",
#            "Massachusetts"="MA_DEP", "Missouri"="MDNR", "New Hampshire"="11113300", "North Dakota"="21NDHDWQ",
#            "Oklahoma"="OKDEQ", "Pennsylvania"="21PA", "South Carolina"="21SC60WQ", "South Dakota"="SDDENR",
#            "Vermont"="1VTDECWQ", "Virginia"="21VASWCB", "Wisconsin"="WIDNR", "Wyoming"="WYDEQ")
ORGID<-list(States=
              c("Alabama"="21AWIC","Alaska"="AKDECWQ","Arizona"="21ARIZ","Arkansas"="ARDEQH2O","California"="CA_SWRCB",
                "Colorado"="21COL001","Connecticut"="CT_DEP01","Delaware"="21DELAWQ","Georgia"="21GAEPD","Florida"="21FL303D",
                "Hawaii"="21HI","Idaho"="IDEQ","Illinois"="IL_EPA","Indiana"="21IND","Iowa"="21IOWA","Kansas"="21KAN001",
                "Kentucky"="21KY","Louisiana"="LADEQWPD","Maine"="MEDEP","Maryland"="MDE_EASP","Massachusetts"="MA_DEP",
                "Michigan"="21MICH","Minnesota"="MNPCA","Mississippi"="21MSWQ","Missouri"="MDNR","Montana"="MDNR","Nebraska"="21NEB001",
                "Nevada"="21NEV1","New Hampshire"="11113300","New Jersey"="21NJDEP1","New Mexico"="21NMEX","New York"="21NYDECA",
                "North Carolina"="21NC01WQ","North Dakota"="21NDHDWQ","Oklahoma"="OKDEQ","Oregon"="OREGONDEQ","Pennsylvania"="21PA",
                "Rhode Island"="RIDEM","South Carolina"="21SC60WQ","South Dakota"="SDDENR","Tennessee"="TDECWR","Texas"="TCEQMAIN",
                "Utah"="UTAHDWQ","Vermont"="1VTDECWQ","Virginia"="21VASWCB","Washington"="WA_ECOLOGY","West Virginia"="WVDEP",
                "Wisconsin"="WIDNR","Wyoming"="WYDEQ"),
            Territories=
              c("American Samoa"="21AS","District of Columbia"="DOEE","Guam"="21GUAM","Northern Mariana Islands"="21AQ",
                "Puerto Rico"="PR_LAKES","US Virgin Islands"="USVIST"))

ORGID_choices<-c("Alabama"="21AWIC","Alaska"="AKDECWQ","Arizona"="21ARIZ","Arkansas"="ARDEQH2O","California"="CA_SWRCB",
                "Colorado"="21COL001","Connecticut"="CT_DEP01","Delaware"="21DELAWQ","Georgia"="21GAEPD","Florida"="21FL303D",
                "Hawaii"="21HI","Idaho"="IDEQ","Illinois"="IL_EPA","Indiana"="21IND","Iowa"="21IOWA","Kansas"="21KAN001",
                "Kentucky"="21KY","Louisiana"="LADEQWPD","Maine"="MEDEP","Maryland"="MDE_EASP","Massachusetts"="MA_DEP",
                "Michigan"="21MICH","Minnesota"="MNPCA","Mississippi"="21MSWQ","Missouri"="MDNR","Montana"="MDNR","Nebraska"="21NEB001",
                "Nevada"="21NEV1","New Hampshire"="11113300","New Jersey"="21NJDEP1","New Mexico"="21NMEX","New York"="21NYDECA",
                "North Carolina"="21NC01WQ","North Dakota"="21NDHDWQ","Oklahoma"="OKDEQ","Oregon"="OREGONDEQ","Pennsylvania"="21PA",
                "Rhode Island"="RIDEM","South Carolina"="21SC60WQ","South Dakota"="SDDENR","Tennessee"="TDECWR","Texas"="TCEQMAIN",
                "Utah"="UTAHDWQ","Vermont"="1VTDECWQ","Virginia"="21VASWCB","Washington"="WA_ECOLOGY","West Virginia"="WVDEP",
                "Wisconsin"="WIDNR","Wyoming"="WYDEQ",
                "American Samoa"="21AS","District of Columbia"="DOEE","Guam"="21GUAM","Northern Mariana Islands"="21AQ",
                "Puerto Rico"="PR_LAKES","US Virgin Islands"="USVIST")