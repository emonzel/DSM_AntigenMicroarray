####################################################################################################           
##### Created by Joseph Longworth 
##### Contact josephlongworth@gmail.com
##### This is the UI file for a shiny application providing several tools for Antigen Microarrays
####################################################################################################           

# UI - Set-up -----------------------------------------
dashboardPage(
  dashboardHeader(title="Antigen Microarray Bioinformatic Pipeline"),
  dashboardSidebar(sidebarMenu(
    menuItem("Info Page", tabName = "Info_Page", icon = icon("info-circle")),
    menuItem("Print Plan Generator", tabName = "Print_Plan_Generator", icon = icon("print")),
    menuItem("Scan Data Processing", tabName = "AMDP", icon = icon("grip-vertical")))),
   dashboardBody(
    tabItems(
# UI - Info Page -----------------------------------------
   tabItem(tabName = "Info_Page",
           h1("Antigen Microarray Toolset Test"),
           tabBox(width = 12,
                  tabPanel("Overview",
                                    # box(h2("Overview:"),width = 12,
                                    "This shiny app provides a toolset for production and an analysis of antigen microarrays as described
                                    in", tags$strong("An antigen microarray protocol for COVID-19 serological analysis"), "by Longworth and Dittmar.",

                                    h3("The sidebar to the left navigates between the two activites:"),
                                    tags$ul(tags$li(tags$strong("Print Plan Generator"), " - Used to design and create a print plan for the GeSim Nanoplotter 2.1."),
                                            tags$li(tags$strong("Antigen Array Data Processing")," - Used to import tabulated intensities in Mapix or Genepix format accross multiple gains
                                            fitting the data to a single response variable for each antigen sera comparison.")),
                                    h3("Citation:"),
                                    "To cite the application in publications please use:",
                                    tags$br(),
                                    "Joseph Longworth and Gunnar Dittmar. An antigen microarray protocol for COVID-19 serological analysis.  STAR Protocols, Volume 2, Issue 3, 2021. (https://doi.org/10.1016/j.xpro.2021.100815.)"),
                  tabPanel("Print Plan Generator Instructions",
                           img(src="Instructions for Print Plan Generator.png",
                               height = "80%",width = "80%")),
                  tabPanel("Scan Data Processing Instructions",
                           img(src="Instructions for Scan Data Processing.png",
                               height = "80%",width = "80%"))
           )
  ),
                                    
# UI - Print Plan Generator -----------------------------------------
    tabItem(tabName = "Print_Plan_Generator",
            h2("Print Plan Generator"),
            box(
              title = "Array Parameters", status = "primary", width = 5,
              numericInput("Number_of_Samples", label = h3("Number of Antigens"), value = 20,min=1),
              splitLayout(cellWidths = c("50%", "50%"),
                          numericInput("First_sample_well", label = h3("First Well"), value = 1,min=1),
                          numericInput("Dilution_shift", label = h3("Dilution Shift"), value = 32,min=1)),
              splitLayout(cellWidths = c("50%", "50%"),
                          numericInput("number_of_dilutons", label = h3("Number of Dilutions"), value = 3,min=1),
                          numericInput("number_of_replicates", label = h3("Number of Replicates"), value = 1,min=1)),
              splitLayout(cellWidths = c("50%", "50%"),
                          numericInput("max_number_of_columns", label = h3("Array Columns"), value = 8),
                          numericInput("max_number_of_rows", label = h3("Array Rows"), value = 8)),
              splitLayout(cellWidths = c("50%", "50%"),
                          checkboxInput("scramble", label = h3("Scramble?"), value = T),
                          checkboxInput("Corners", label = h3("Standard Corners?"), value = T))
            ),
            box(
              title = "Downloads",width = 7,
              splitLayout(cellWidths = c("50%", "50%"),
              uiOutput("Download_button1"),
              uiOutput("Download_button2"))),
            box(
                title = "Array Layout",width = 7,
              dataTableOutput("Print_Plan_Generator")
            ),
    ),
# UI - Antigen Microarray Data Processing (AMDP) -----------------------------------------
  tabItem(tabName = "AMDP",
          h2("Antigen Microarray Data Processing"),
          fluidRow(
            column(width = 3,  
            box(title="New Antigen Microarray Slide",collapsible = TRUE,solidHeader = TRUE,status = "warning", width = NULL,collapsed = TRUE,
                       h3("Import New Slide"),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   fileInput('file3', 'Antigen Names'  ,accept=c('.xlsx')),
                                   downloadButton("Antigen_Names_Template", label = "Template", class = "btn-info")),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   fileInput('file2', 'Print Design' ,accept=c('.tab')),
                                   fileInput('file4', 'Scan Data',accept=c('.txt','.gpr'),multiple = T)),
                       textInput("Slide_Name", "Slide Name:",value = "Slide_Name"),
                       textInput("Sample_Plate", "Sample Plate:",value = "Sample_Plate"),
                       selectInput("Immunoglobulin", "Immunoglobulin:",
                                   choices = c("IgG","IgM","IgA","IgE","NA"),multiple = F),
                       selectInput("Intensity_Metric", "Intensity Metric:",
                                   choices = 'No choices here yet',multiple = F),
                       numericInput("Gain_Normalisation_Point", "Gain Normalisation Point:",
                                   value = 50,min = 0,max = 100),
                       actionButton("Process_Import_Data_1", "Process Import Data", class = "btn-success")
                ),
              box(title="Previously Fitted Data Upload",collapsible = TRUE,solidHeader = TRUE,status = "warning", width = NULL,collapsed = TRUE,
                       fileInput('file5', 'Compiled Dataset Upload',accept=c('.txt')),
                       actionButton("Process_Import_Data_2", "Process Import Data", class = "btn-success")
                  )
            ),
            column(width=8,
            box( width = NULL,
                h3("Import Data"),
                plotOutput("Scan_Data_Import"),
                actionButton("Apply_Slide_Upload_To_Dataset", "Append", class = "btn-success")
                ),
            box( width = NULL,
                h3("Full Dataset"),
                "Currently Loaded Data",
                splitLayout(cellWidths = c("50%", "50%"),
                uiOutput("Download_button3"),
                actionButton("Clear_Dataset", "Clear Loaded Dataset",class = "btn-danger", icon = icon("trash-alt"))),
                dataTableOutput("Current_Dataset")
                )
            ))
          )))
)
