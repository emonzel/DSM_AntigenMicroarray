####################################################################################################           
##### Created by Joseph Longworth 
##### Contact josephlongworth@gmail.com
##### This is the Global file for a shiny application providing several tools for Antigen Microarrays
####################################################################################################

# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("janitor")
# install.packages("SuperCurve", repos="http://R-Forge.R-project.org")

require("shiny")
require("shinydashboard")
require("tidyverse")
require("readxl")
require("janitor")
require("SuperCurve")



JPL_num_to_well <- function(x){if(is.na(x)){NA}else{num_to_well(x,plate = 96)}}

