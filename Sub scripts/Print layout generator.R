##### Generate a Print Plan layout for GeSim Nannoplotter 2.1 #####

# Create plate layouts ----------------------------------------------------
landscape_numbers=matrix(c(1:384),nrow=16,ncol=24,byrow = F)
portriat_numbers=matrix(c(1:384),nrow=24,ncol=16,byrow = T)
rotate <- function(x) t(apply(x, 2, rev))
portriat_numbers=rotate(portriat_numbers)

# Empty vectors for processing of print layout data -----------------------
output=tibble(Well=NA,
              Sample_Number=NA,
              Dilution=NA,
              Spot_Number=NA,
              Print_Plan=NA) 

poitions_row=rep(c(1:max_number_of_rows),(max_number_of_columns))
poitions_column=sort(rep(c(1:max_number_of_columns),(max_number_of_rows)))
spots_list=paste0(poitions_row,",",poitions_column)

if(Corners==T){
   spots_list=spots_list[!spots_list %in% c(paste0(1,",",1),paste0(1,",",max_number_of_columns),
                                           paste0(max_number_of_rows,",",1),paste0(max_number_of_rows,",",max_number_of_columns))]
}

if(scramble==T){
  spots_list=sample(spots_list)
  }


# Loop through each sample and dilution determining spotting details --------

for(s in 1:Number_of_Samples){
  for(d in 1:number_of_dilutons){
    spots=""
    for(r in 1:number_of_replicates){
     spots=paste0(spots," ",spots_list[
      #Individual spot number created by mutiplying the sample number by number of dilutions and replicates correcting each by -1 
       (((((s-1)*number_of_dilutons)+d)-1)*number_of_replicates)+r
      ])
    }
    
    landscape_number=First_sample_well+s-1+(Dilution_shift*(d-1))
    portriat_number=portriat_numbers[which(landscape_numbers==landscape_number)]
    
    output=output %>% 
      add_row(Well=as.character(paste0(letters[(landscape_number-1)%%16+1],(landscape_number-1)%/%16+1)),
              Sample_Number=s,
              Dilution=d,
              Spot_Number=(((s-1)*number_of_dilutons)+d),
              Print_Plan=paste0("P1 S",portriat_number,spots)
              ) %>% 
      drop_na() 
  }}
    


if(Corners==T){
  output=output %>% 
    add_row(Well=c("a24","b24","c24","d24"),
            Sample_Number=Number_of_Samples+c(1:4),
            Dilution=c(1,1,1,1),
            Spot_Number=(Number_of_Samples*number_of_dilutons)+c(1:4),Print_Plan=paste0("P1 S",c(1:4),c(
            paste0(" 1,",max_number_of_columns),
            paste0(" ",max_number_of_rows,",1"),
            paste0(" ",max_number_of_rows,",",max_number_of_columns)," 1,1"))
    ) 
  }



# Prepare output and output download of Print Plan Generator to be returned ---------------------------------------------------------------------

Total_Spots=Number_of_Samples*number_of_dilutons*number_of_replicates
Maximum_Spots=length(spots_list)
  
if(Total_Spots>Maximum_Spots){
  Print_plan<<-""
  output<<-paste0(" ERROR: Number of spots to be printed exceeds maximum allowed",Total_Spots," greater than ",Maximum_Spots)
}else{
  Print_plan<<-as.data.frame(output$Print_Plan)
  output_PrintPlanGenerator<<-output
  
  output_download<- matrix(ncol=5,nrow=9)
  output_download[1,]<- c(paste0("### Number_of_samples=",Number_of_Samples),"","","","")
  output_download[2,]<- c(paste0("### Dilution_shift=",Dilution_shift),"","","","")
  output_download[3,]<- c(paste0("### First_sample_well=",First_sample_well),"","","","")
  output_download[4,]<- c(paste0("### Number_of_dilutons=",number_of_dilutons),"","","","")
  output_download[5,]<- c(paste0("### Number_of_replicates=",number_of_replicates),"","","","")
  output_download[6,]<- c(paste0("### Max_number_of_columns=",max_number_of_columns),"","","","")
  output_download[7,]<- c(paste0("### Max_number_of_rows=",max_number_of_rows),"","","","")
  output_download[8,]<- c(paste0("### Scambled_Spotting=",scramble),"","","","")
  output_download[9,]<- c("Well","Sample number","Dilution","Spot Number","Print Plan")
  output_download<-rbind(output_download,as.matrix(output))
  
  output_PrintPlanGenerator_download<<-output_download

  
}

