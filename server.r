####################################################################################################           
##### Created by Joseph Longworth 
##### Contact josephlongworth@gmail.com
##### This is the Server file for a shiny application providing several tools for Antigen Microarrays
####################################################################################################

options(shiny.maxRequestSize = 1000*1024^2)
rm(list = ls(),envir = .GlobalEnv )
server <- 
  function(input, output, session) {
    
# Server - Print Plan Generator -----------------------------------------


  # Generate Print Plan and render table
output$Print_Plan_Generator <- renderDataTable(options=list(pageLength = 10),{

  # Inputs passed to Print Plan generator
  Number_of_Samples=input$Number_of_Samples
  Dilution_shift=input$Dilution_shift
  First_sample_well=input$First_sample_well
  number_of_dilutons=input$number_of_dilutons
  number_of_replicates=input$number_of_replicates
  max_number_of_columns=input$max_number_of_columns  
  max_number_of_rows=input$max_number_of_rows
  # Rows_shifted=input$Rows_Shifted
  # Columns_Shifted=input$Columns_shifted
  Repeated_Spotting=input$Repeated_Spotting
  scramble=input$scramble
  Corners=input$Corners

      source("Sub scripts/Print layout generator.R",local = TRUE)
        
  output_PrintPlanGenerator
        
      
    })    
    
 ### Downloadable *.txt of Print Plan of selected dataset
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste0("Print_Plan_",format(Sys.time(), "%Y%m%d%H%M%S"), ".txt")
      },
      content = function(file) {
        write.table(Print_plan,file,quote = F,row.names = F,col.names = F)
        }
    )
    output$Download_button1 <- renderUI({
      downloadButton("downloadData1", "Print_Plan",class="btn-info")
    })
 
  
 ### Downloadable *.tab of Print Design file
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste0("Print_Design",format(Sys.time(),"%Y%m%d%H%M%S"), ".tab")
      },
      content = function(file) {
       write.table(output_PrintPlanGenerator_download,file,quote = T,row.names = F,col.names = F, sep = "\t")
        })
        output$Download_button2 <- renderUI({
      downloadButton("downloadData2", "Print Design",class="btn-info")
    })


# Server - Antigen Array Data Processing -----------------------------------------
# Print currently loaded datasets -----------------------------------------

    observeEvent(c(input$Clear_Dataset,input$Apply_Slide_Upload_To_Dataset),
             
      output$Current_Dataset <- renderDataTable(options=list(pageLength = 10),{
        if(exists("base_dataset")){
          base_dataset %>% 
          ungroup() %>% 
          arrange(desc(slide_name)) %>%
          dplyr::select(slide_name,immunoglobulin,sample_plate) %>%
          rename("Slide Name"=slide_name,
                 "Sample Plate"=sample_plate,
                 "Immunoglobulin"=immunoglobulin) %>% 
          distinct()
        }
      
      }))
    

# Clear Datasets ----------------------------------------------------------

      observeEvent(input$Clear_Dataset, {
        
        if(exists("base_dataset")){
              rm("base_dataset",envir = .GlobalEnv)}
        if(exists("Combined_Antigen_Array_Data")){
              rm("Combined_Antigen_Array_Data",envir = .GlobalEnv)}
        
        base_dataset=NULL

      })
      

# Apply uploaded slide to currently loaded dataset ------------------------


      observeEvent(input$Apply_Slide_Upload_To_Dataset, {
        if(exists("Incoming_Dataset")){
          if(exists("base_dataset")){
            base_dataset<<-base_dataset %>%
              janitor::clean_names() %>%
              ungroup() %>% 
              add_row(Incoming_Dataset)
            
              rm(Incoming_Dataset)
        }else{
          base_dataset<<-Incoming_Dataset
          rm(Incoming_Dataset)}
        }
          })
  

# Update the Intensity_Metric UI input selection based on data file ------------------------------------------------

    observeEvent(input$file4, {
      value=read_delim(input$file4$datapath[1],delim = "\t ",
                                skip = grep("Flags",read_lines(input$file4$datapath[1]),value = F)-1,) %>% 
        janitor::clean_names()
      value=colnames(value)
      selected_value=value[grep("_median_b",value)]
      updateSelectInput(session, "Intensity_Metric", "Intensity Metric:", choices = value,selected = selected_value)
      })
    
# Load placeholder image ------------------------------------------------      

 
      output$Scan_Data_Import <- renderPlot({
        blank_df <- data.frame()
        ggplot(blank_df) + geom_point() + xlim(0, 10) + ylim(0, 100)+
          theme_void()+
          annotate("text", x = 5, y = 50, label = "Upload data to see\n a overview preview.",
                   col = "black", size = 10)
        
      })
    
    
# Process uploaded dataset New ------------------------------------------------      
  observeEvent(input$Process_Import_Data_1, {

    output$Scan_Data_Import <- renderPlot({
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "starting", value = 0)
      
    isolate(source("Sub scripts/Scan Data Import.R",local = T))
      
    progress$set(message = "Plot incoming data", value = 0.9)
      
    Incoming_Dataset<<-new_data

    Incoming_Dataset %>% 
      glimpse() %>% 
      mutate(block=as_factor(block)) %>% 
      ggplot(aes(x=block, y=gain_normalised_intensity)) +
      geom_bar(aes(fill= Immunoglobulin),position = "dodge",stat = "identity")+
      scale_fill_brewer(palette = "Set1") +
      xlab("Patients")+
      ylab("Intensity")+
      ggtitle(Incoming_Dataset$slide_name[1])+
      theme_classic()+
      theme(axis.text.x= element_blank(),axis.ticks.x = element_blank())+ 
      facet_wrap(vars(antigen),ncol=5,scales = "free")
      
      })
      })

# Process uploaded dataset Existing ------------------------------------------------
    
    
observeEvent(input$Process_Import_Data_2, {
      
      output$Scan_Data_Import <- renderPlot({
        
          req(input$file5)
          
        isolate({ 
          Incoming_Dataset<<-read_tsv(input$file5$datapath)%>%
            janitor::clean_names() %>% 
            mutate(sample_plate=as.character(sample_plate))
          
          Incoming_Dataset %>% 
            mutate(Immunoglobulin=immunoglobulin) %>% 
            mutate(block=as_factor(block)) %>% 
            ggplot(aes(x=block, y=gain_normalised_intensity,color=Immunoglobulin)) +
            geom_boxplot( position = "dodge")+
            xlab("Serum Sample")+
            ylab("Intensity")+
            ggtitle(input$file5$name)+
            theme(axis.text.x= element_blank(),axis.ticks.x = element_blank())+ 
            facet_wrap(vars(antigen),ncol=5,scales = "free")
          
       })
      })
    })

# Prepare Download button -------------------------------------------------
  ### Downloadable *.xlsx of Antigen Names Template
  
    output$Download_button3 <- renderUI({
      downloadButton("downloadData3", "Download Combined Slide Data",class = "btn btn-info",)
    })
    
    output$Antigen_Names_Template <- downloadHandler(
      filename = function() {
        paste0(input$dataset, "Antigen_Names_Template.xlsx")
      },
      content <- function(file) {
        file.copy("data/Antigen_Names_Template.xlsx", file)
        }
    )
  ### Downloadable *.txt of Print Plan of selected dataset
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste0("Combined_Slide_Data",".txt")
      },
      content = function(file) {
        write_tsv(base_dataset,file)
      }
    )
    }


