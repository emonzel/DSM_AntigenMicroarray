# AntigenMicroarray
Toolset for Production and an Analysis of Antigen Microarrays


The easiest way to run the shiny based web application is through R as shown below. Alternatively the files be downloaded and run locally or uploaded to a shiny server as described at http://shiny.rstudio.com/.

```R
# 1. If not installed first install the following packages.
install.packages("shiny")
install.packages("shinydashboard")
install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")
install.packages("SuperCurve", repos="http://R-Forge.R-project.org")

# 2. Load Shiny.
library(shiny)

# 3. Run the application from GitHub.
runGitHub("AntigenMicroarray", "JosephLongworth")


