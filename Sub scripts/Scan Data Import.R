Print_Design_filepath =input$file2$datapath
Antigen_names_filepath = input$file3$datapath
Scan_Data_filepath = input$file4$datapath
Scan_Data_name = input$file4$name
Slide_Name=input$Slide_Name
Sample_Plate=input$Sample_Plate
Immunoglobulin=input$Immunoglobulin
Mean_Net=input$Intensity_Metric
Gain_Normalisation_Point=input$Gain_Normalisation_Point


# Import Print Design -------------------------------
progress$set(message = "Import Print Design", value = 0.1)

data <-read_delim(Print_Design_filepath,delim =  "\t ",
                  skip = max(grep("###",read_lines(Print_Design_filepath),value = F))) %>% 
  dplyr::rename(Print_Plan=`Print Plan`) 

data=data %>% 
  mutate(antigen_number=as.double(`Sample number`)) %>% 
  mutate(Spot_Number=as.double(`Spot Number`)) %>% 
  dplyr::select(-c(`Sample number`,`Spot Number`)) %>% 
  glimpse()

Sample_names <- read_excel(Antigen_names_filepath) %>% 
  mutate(Sample = as_factor(antigen)) %>% 
  mutate(Sample = fct_reorder(Sample,as.numeric(display_order)))

data <- inner_join(data,Sample_names) 

# Split Print plan -------------------------------------------------------
progress$set(message = "Split Print Plan", value = 0.2)

data=data %>% 
  mutate(value=Print_Plan) %>%
  separate(value,c("Plate","Portrait_Sample","coordinates"), sep = " ",extra = "merge") %>% 
  separate_rows(coordinates,sep = " ") %>% 
  separate(coordinates,c("Row","Column"), sep = ",") %>%  
  mutate_at("Row",as.double) %>%  
  mutate_at("Column",as.double) 

# Import Slide data -------------------------------------------------------
progress$set(message = "Import Slide data", value = 0.6)

Slide_data <- c()
for(i in 1:length(Scan_Data_filepath)){
  
  Slide_data_temp<-read_delim(Scan_Data_filepath[i],delim = "\t ",skip = grep("Flags",read_lines(Scan_Data_filepath[i]),value = F)-1,)%>%
  mutate(Gain = as.double(gsub(".*PMTGain=(\\d*).*","\\1",grep("Gain=",read_lines(Scan_Data_filepath[i]),value = T))),
  file=Scan_Data_name[i])
  Slide_data=bind_rows(Slide_data,Slide_data_temp)
}


data=full_join(data,Slide_data)


# Join associated data ----------------------------------------------------
progress$set(message = "Join associated data", value = 0.8)

df = data %>% 
  janitor::clean_names() %>% 
  mutate(slide_name=Slide_Name,
         sample_plate=Sample_Plate,
         # wells=Wells,
         immunoglobulin=Immunoglobulin) %>%
  dplyr::mutate(antigen=sample) %>%
  filter(type %in% "Antigen")

colnames(df)[which(colnames(df)==Mean_Net)]= "Mean.Net"
Antigens=unique(df$antigen)
Gains=unique(df$gain)
rm(Serology_data)
# Fit each SuperCurve -------------------------------
for(j in Antigens){
  for(k in Gains){
progress$set(message = "Fit SuperCurves", value = (((which(Antigens==j)-1)*length(Gains))+which(Gains==k))/
                   (length(Antigens)*length(Gains)))

    temp=df %>%
      filter(antigen %in% j) %>%
      filter(gain %in% k) %>%
      transmute(Main.Row=(as.double(block)+3)%/%4,
                Main.Col=(as.double(block)+3)%%4+1,
                Sub.Col=dilution,
                Sub.Row=(as.numeric(as_factor(slide_name))),
                steps=(-dilution)+max(dilution),
                Sample=as_factor(block),
                Mean.Net=Mean.Net) %>%
      arrange(Main.Row,Main.Col,steps) %>%
      mutate(Mean.Net=if_else(Mean.Net<1,as.double(sample(c(1:10),1)),Mean.Net)) %>%
      as.data.frame()


    rm(RPPA)
    RPPA=new("RPPA",data=temp,file=paste0("Antigen:",j," Gain:",k),
             antibody=paste0("Antigen:",j," Gain:",k))

    designRPPA <- RPPADesign(RPPA,
                             steps=temp$steps,
                             series=temp$Sample,
                             center = F,
                             controls = "Control")
    rm(RPPAFit)
    RPPAFit <- RPPAFit(RPPA,
                       designRPPA,
                       measure="Mean.Net",
                       ci=F,
                       trim=T,
                       veryVerbose=F)

    table=tibble(Antigen=j,
                 gain=k,
                 sample=names(RPPAFit@intensities),
                 intensity=RPPAFit@intensities,
                 concentration=RPPAFit@concentrations,
                 upper=RPPAFit@upper,
                 lower=RPPAFit@lower)


    if(!exists("Serology_data")){
      Serology_data=table}else{
        Serology_data=Serology_data %>%
          add_row(table)
      }
  }}

# Fit Gains Curves -------------------------------

if(!length(unique(Serology_data$gain))>1){

  Normalised_intensities=Serology_data %>%
    janitor::clean_names() %>%
    mutate(block=as.double(sample)) %>%
    mutate(antigen=as_factor(antigen)) %>%
    arrange(antigen,block,gain) %>%
    select(antigen,block,gain,intensity) %>% 
    left_join(df) %>%
    dplyr::select(antigen,block,antigen_number,
                  display_order,product_code,lot_number,slide_name,
                  sample_plate,immunoglobulin,intensity) %>%
    distinct() %>%
    mutate(gain_normalised_intensity=intensity)
  
  }else{
progress$set(message = "Fit Gains", value = 0.9)

Antigens=unique(Serology_data$Antigen)
Gains=unique(Serology_data$gain)

intensities=Serology_data%>%
  janitor::clean_names() %>% 
  mutate(block=as.double(sample)) %>% 
  mutate(antigen=as_factor(antigen)) %>% 
  select(antigen,block,gain,intensity) %>%
  distinct() %>%
  arrange(gain) %>%
  spread(key = gain,value = intensity) %>%
  unite(col = intensities,!c(antigen,block),sep = ";")  
  
Serology_data=Serology_data %>%
  janitor::clean_names() %>% 
  mutate(block=as.double(sample)) %>% 
  mutate(antigen=as_factor(antigen),
         block=as_factor(block)) %>%
  arrange(antigen,block,gain) %>% 
  mutate(slope1=(intensity-lag(intensity,n=1))/(gain-lag(gain,n=1))) %>%
  mutate(slope1=ifelse(gain%in%Gains[c(1)],NA,slope1))
  
models=Serology_data %>% 
  dplyr::group_by(antigen,block) %>%
  dplyr::summarise(mean_slope1 = mean(slope1,na.rm = T),
                   max_intensity = max(intensity)) %>% 
  full_join(Serology_data) %>% 
  glimpse() %>%
  mutate(filter=0) %>%
  mutate(filter=ifelse(intensity>(max_intensity/2),ifelse(slope1<(mean_slope1/2),1,0),0)) %>%
  mutate(filter = ifelse(is.na(filter),1,filter)) %>%
  filter(filter %in% 0) %>%
  dplyr::select(antigen,block,intensity,gain,slope1) %>%
  group_by(antigen,block) %>%
  nest() %>%
  mutate(model = map(data, ~lm(intensity ~ as.double(gain), data = .)),
         tidied = map(model, broom::tidy)) %>%
  unnest(cols = c(tidied)) %>%
  dplyr::select(antigen,block,data,term,estimate) %>%
  spread(key = term,value = estimate) %>%
  unnest(cols = c(data)) %>%
  janitor::clean_names() %>%
  dplyr::rename(slope=as_double_gain)

 
trimmed_intensities=models%>%
  mutate(block=as.double(block)) %>%
  full_join(df) %>%
  select(antigen,block,gain,intensity) %>%
  distinct() %>%
  arrange(gain) %>%
  spread(key = gain,value = intensity) %>%
  unite(col = trimmed_intensities,!c(antigen,block),sep = ";") %>%
  mutate(gains=paste0(sort(unique(df$gain)),collapse = ";")) %>%
  glimpse() 
  
Normalised_intensities=models %>%
  mutate(block=as.double(block)) %>%
  left_join(df) %>%
  left_join(intensities) %>%
  left_join(trimmed_intensities) %>%
  dplyr::select(antigen,block,gains,intensities,trimmed_intensities,intercept,slope,antigen_number,
         display_order,product_code,lot_number,slide_name,
         sample_plate,immunoglobulin) %>%
  distinct() %>%
  mutate(gain_normalisation_point=Gain_Normalisation_Point) %>% 
  mutate(gain_normalised_intensity=slope*Gain_Normalisation_Point+intercept) %>% 
  mutate(gain_normalised_intensity=ifelse(is.na(gain_normalised_intensity),intercept,gain_normalised_intensity))


}


new_data<<-Normalised_intensities

