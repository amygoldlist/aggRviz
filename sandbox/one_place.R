### one place only:

##THis has not been written yet

# one_place <- function(dat){
#   if ("Region" %in% names(dat)){
#     if ("Country" %in% names(dat)){
#       dat <- dat %>%
#         filter((Country == "" & Region == "") | (Country != "" & Region != "")) %>%
#         dplyr::select( -Country)
#     }
#   }
#   if ("State.or.Province" %in% names(dat)){
#     dat <- dat %>%
#       filter((State.or.Province == "" & Region == "") | (State.or.Province != "" & Region != "")) %>%
#       dplyr::select(-Region)
#   }
#   return(dat)
# }

# ### TEST:
# dat <- data.frame("Region"= c(1,2,"",""),
#                   "Country"= c(1,2,3,""),
#                   "State.or.Province" = c(1,"","", ""))
# dat
# one_place(dat)
# dat %>%
#   select("Region", "Country") %>%
#   one_place()
#
# dat %>%
#   select( "Country") %>%
#   one_place()
