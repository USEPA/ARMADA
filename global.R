packages <- c("shiny","dplyr","purrr","stringr","tidyr","gfonts","httr","jsonlite","r2d3","shinybusy","shinyhelper","shinyalert")

installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

 if(!"capture" %in% installed.packages()) {
     remotes::install_github("dreamRs/capture")
 }
 
 # Packages loading
 lapply(packages, library, character.only = TRUE)
 library(capture)

# library(shiny)
# library(dplyr)
# library(purrr)
# library(stringr)
# library(tidyr)
# library(gfonts)
# library(httr)
# library(jsonlite)
# library(r2d3)
# library(capture)
# library(shinyalert)
# library(shinyhelper)
# library(shinybusy)

options(warn=-1)

margin_top <- 160.0
margin_bottom <- 300.0
bar_height <- 30

myCap.2 <- function(x) {
  out <- sapply(x, function(y) {
    idx <- str_locate(y, "\\([^()]+\\)")
    if (!all(is.na(idx[1,]))) {
      str_sub(y, idx[,1], nchar(y)) <- toupper(str_sub(y, idx[,1], nchar(y))) 
    }
    return(y)
  })
  out
}
# ORGID <- c("Alaska"="AKDECWQ", "Arizona"="21ARIZ", "Colorado"="21COL001", "Connecticut"="CT_DEP01", "Florida"="21FL303D", "Iowa"="21IOWA", "Kansas"="21KAN001",
#            "Massachusetts"="MA_DEP", "Missouri"="MDNR", "New Hampshire"="11113300", "North Dakota"="21NDHDWQ",
#            "Oklahoma"="OKDEQ", "Pennsylvania"="21PA", "South Carolina"="21SC60WQ", "South Dakota"="SDDENR",
#            "Vermont"="1VTDECWQ", "Virginia"="21VASWCB", "Wisconsin"="WIDNR", "Wyoming"="WYDEQ")
ORGID<-list(States=
              c("Alabama"="21AWIC","Alaska"="AKDECWQ","Arizona"="21ARIZ","Arkansas"="ARDEQH2O","California"="CA_SWRCB",
                "Colorado"="21COL001","Connecticut"="CT_DEP01","Delaware"="21DELAWQ","District of Columbia"="DOEE","Georgia"="21GAEPD","Florida"="21FL303D",
                "Hawaii"="21HI","Idaho"="IDEQ","Illinois"="IL_EPA","Indiana"="21IND","Iowa"="21IOWA","Kansas"="21KAN001",
                "Kentucky"="21KY","Louisiana"="LADEQWPD","Maine"="MEDEP","Maryland"="MDE_EASP","Massachusetts"="MA_DEP",
                "Michigan"="21MICH","Minnesota"="MNPCA","Mississippi"="21MSWQ","Missouri"="MDNR","Montana"="MDNR","Nebraska"="21NEB001",
                "Nevada"="21NEV1","New Hampshire"="11113300","New Jersey"="21NJDEP1","New Mexico"="21NMEX","New York"="21NYDECA",
                "North Carolina"="21NC01WQ","North Dakota"="21NDHDWQ","Oklahoma"="OKDEQ","Oregon"="OREGONDEQ","Pennsylvania"="21PA",
                "Rhode Island"="RIDEM","South Carolina"="21SC60WQ","South Dakota"="SDDENR","Tennessee"="TDECWR","Texas"="TCEQMAIN",
                "Utah"="UTAHDWQ","Vermont"="1VTDECWQ","Virginia"="21VASWCB","Washington"="WA_ECOLOGY","West Virginia"="WVDEP",
                "Wisconsin"="WIDNR","Wyoming"="WYDEQ"),
            Territories=
              c("American Samoa"="21AS","Guam"="21GUAM","Northern Mariana Islands"="21AQ",
                "Puerto Rico"="PR_LAKES","US Virgin Islands"="USVIST"),
            Tribes=
              c("Cherokee Nation"="CHEROKEE","Citizen Potawatomi Nation, OK"="CPNWATER","Delaware Nation, OK"="DELAWARENATION",
                "Hoopa Valley Tribe, CA"="HVTEPA","Minnesota Chippewa Tribe, MN (Fond du Lac Band)"="FONDULAC","Otoe Missouria Tribe, OK"="O_MTRIBE",
                "Pueblo of Pojoaque, NM"="PUEBLO_POJOAQUE","Pueblo of San Ildefonso, NM"="SANILDEFONSODECP","Pueblo of Santa Ana, NM"="PUEBLO_SANTAANA",
                "Pueblo of Tesuque, NM"="PUEBLOOFTESUQUE","Red Lake Band of Chippewa Indians, MN"="REDLAKE","Sac & Fox Nation, OK"="SFNOES",
                "Seneca-Cayuga Nation"="SCEQ","The Chickasaw Nation"="CNENVSER","The Choctaw Nation of Oklahoma"="CHOCNAT",
                "Ute Mountain Ute Tribe"="UTEMTN"))

ORGID_choices<-c("Alabama"="21AWIC","Alaska"="AKDECWQ","Arizona"="21ARIZ","Arkansas"="ARDEQH2O","California"="CA_SWRCB",
                "Colorado"="21COL001","Connecticut"="CT_DEP01","Delaware"="21DELAWQ","District of Columbia"="DOEE","Georgia"="21GAEPD","Florida"="21FL303D",
                "Hawaii"="21HI","Idaho"="IDEQ","Illinois"="IL_EPA","Indiana"="21IND","Iowa"="21IOWA","Kansas"="21KAN001",
                "Kentucky"="21KY","Louisiana"="LADEQWPD","Maine"="MEDEP","Maryland"="MDE_EASP","Massachusetts"="MA_DEP",
                "Michigan"="21MICH","Minnesota"="MNPCA","Mississippi"="21MSWQ","Missouri"="MDNR","Montana"="MDNR","Nebraska"="21NEB001",
                "Nevada"="21NEV1","New Hampshire"="11113300","New Jersey"="21NJDEP1","New Mexico"="21NMEX","New York"="21NYDECA",
                "North Carolina"="21NC01WQ","North Dakota"="21NDHDWQ","Oklahoma"="OKDEQ","Oregon"="OREGONDEQ","Pennsylvania"="21PA",
                "Rhode Island"="RIDEM","South Carolina"="21SC60WQ","South Dakota"="SDDENR","Tennessee"="TDECWR","Texas"="TCEQMAIN",
                "Utah"="UTAHDWQ","Vermont"="1VTDECWQ","Virginia"="21VASWCB","Washington"="WA_ECOLOGY","West Virginia"="WVDEP",
                "Wisconsin"="WIDNR","Wyoming"="WYDEQ",
                
                "American Samoa"="21AS","Guam"="21GUAM","Northern Mariana Islands"="21AQ",
                "Puerto Rico"="PR_LAKES","US Virgin Islands"="USVIST",
                
                "Cherokee Nation"="CHEROKEE","Citizen Potawatomi Nation, OK"="CPNWATER","Delaware Nation, OK"="DELAWARENATION",
                "Hoopa Valley Tribe, CA"="HVTEPA","Minnesota Chippewa Tribe, MN (Fond du Lac Band)"="FONDULAC","Otoe Missouria Tribe, OK"="O_MTRIBE",
                "Pueblo of Pojoaque, NM"="PUEBLO_POJOAQUE","Pueblo of San Ildefonso, NM"="SANILDEFONSODECP","Pueblo of Santa Ana, NM"="PUEBLO_SANTAANA",
                "Pueblo of Tesuque, NM"="PUEBLOOFTESUQUE","Red Lake Band of Chippewa Indians, MN"="REDLAKE","Sac & Fox Nation, OK"="SFNOES",
                "Seneca-Cayuga Nation"="SCEQ","The Chickasaw Nation"="CNENVSER","The Choctaw Nation of Oklahoma"="CHOCNAT",
                "Ute Mountain Ute Tribe"="UTEMTN")