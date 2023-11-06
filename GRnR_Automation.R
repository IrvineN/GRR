######### GRnR Automation ########

#libraries
library(reticulate)

# Global Variable to change based on actual run
site_no   <<- 16 
in_prefix <<- 'Project1' 
data_folder <<- 'data' #folder name that contain the data


STEP_1_STDF_to_RTDF  <<- 1
STEP_2_SPLIT_RTDF    <<- 2
STEP_3_RTDF_to_CSV   <<- 3
STEP_4_CSV_SN_Update <<- 4
STEP_5_CSV_to_RTDF   <<- 5
STEP_6_MERGE_RTDF    <<- 6
STEP_7_GENERATE_REPORT <<- 7

skip_step_till <<- 0 #choose until which step to skip

# Global variable 
top_level <- sprintf(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(top_level) 

# Source the files from company's lib

source('RADAR_lib_2v4/ConvertStdf.R')
source("RADAR_lib_2v4/SplitSites.R")
source("RADAR_lib_2v4/ConvertCsv.R")
source("RADAR_lib_2v4/MergeRtdf.R")
source("RADAR_lib_2v4/GRR.R")

# R Version Used
R_VER <<- 2.4

# Data Folder
data_dir <<- paste0(top_level, "/",data_folder,"/")
setwd(data_dir) # Set WD to output file


#Baseline File Search
exist_baseline_gz <- file.exists(paste0(in_prefix, '_baseline.gz'))
exist_baseline_stdf <- file.exists(paste0(in_prefix, '_baseline.stdf'))
exist_baseline_stdf_gz <- file.exists(paste0(in_prefix, '_baseline.stdf.gz'))

counter <- 0
if (exist_baseline_gz) {
  stdf_base <- paste0(in_prefix, '_baseline.gz')
  file_name <- gsub('.{3}$',"", stdf_base)
  rtdf_base <- paste0(file_name, '.rtdf' )
  counter <<- 1
} else if (exist_baseline_stdf) {
  stdf_base <- paste0(in_prefix, '_baseline.stdf')
  file_name <- gsub('.{5}$',"", stdf_base)
  rtdf_base <- paste0(file_name, '.rtdf' )
  counter <<- 1
} else if (exist_baseline_stdf_gz) {
  stdf_base <- paste0(in_prefix, '_baseline.stdf.gz')
  file_name <- gsub('.{8}$',"", stdf_base)
  rtdf_base <- paste0(file_name, '.rtdf' )
  counter <<- 1
  
} else {
  print(sprintf("%s baseline file couldn't be found, proceed with site 1 as baseline data", in_prefix))
  
}


print("##################################################")
print("######### Step 1 : Convert stdf to rtdf ##########")
print("##################################################")

in_list <- list() #list of stripped file name (RunN only)

#Site File Conversion
for(i in 1:(site_no)) {
  
  #Accept all stdf extension variations
  exist_stdfgz <- file.exists(paste0(in_prefix,'_Run',i,'.stdf.gz'))
  if (exist_stdfgz == TRUE){
    stdf_file = paste0(in_prefix,'_Run',i,'.stdf.gz')
    file_name  <- gsub('.{8}$',"", stdf_file)
    rtdf_file = paste0(file_name, '.rtdf' )
  }
  
  exist_stdf <- file.exists(paste0(in_prefix,'_Run',i,'.stdf'))
  if (exist_stdf == TRUE){
    stdf_file = paste0(in_prefix,'_Run',i,'.stdf')
    file_name  <- gsub('.{5}$',"", stdf_file)
    rtdf_file = paste0(file_name, '.rtdf')
  }
  
  exist_gz <- file.exists(paste0(in_prefix,'_Run',i,'.gz'))
  if (exist_gz == TRUE){
    stdf_file = paste0(in_prefix,'_Run',i,'.gz')
    file_name  <- gsub('.{3}$',"", stdf_file)
    rtdf_file = paste0(file_name, '.rtdf' )
  }
  
  in_list <- c(in_list, file_name) #list of all file name without extensions
  
  if(skip_step_till < STEP_1_STDF_to_RTDF){
    
    #Baseline file conversion
    exist_baseline_rtdf <- file.exists(paste0(in_prefix,'_baseline.rtdf'))
    if (exist_baseline_rtdf == FALSE & counter == 1 ){ #flag for stdf to rtdf
      counter <<- 2
      ConvertStdf(stdf_name=stdf_base, rtdf_name=rtdf_base, stdf_dir=data_dir,
                  auto_93k=FALSE, do_summary=FALSE, just_fail_tests_summary=FALSE, merge_by_test_number=0)
    }
    
    #Run file conversion
    exist <- file.exists(paste0(in_prefix,'_Run', i , '.rtdf'))
    if (exist == TRUE){ #flag for stdf to rtdf
      print(sprintf("Skip stdf to rtdf conversion for file: %s", stdf_file))
      next
    }
    print(sprintf("Converting %s to %s", stdf_file, rtdf_file))
    ConvertStdf(stdf_name=stdf_file, rtdf_name=rtdf_file, stdf_dir=data_dir,
                auto_93k=FALSE, do_summary=FALSE, just_fail_tests_summary=FALSE, merge_by_test_number=0)
    
    
  }
}




print("##################################################")
print("######### step 2 : Split the rtdf sites ##########")
print("##################################################")

if(skip_step_till < STEP_2_SPLIT_RTDF){
  for (i in 1:site_no) {
    
    in_split= paste0(in_list[i],".rtdf")
    print(sprintf("Splitting sites for file %s", in_split))
    SplitSites(in_file=in_split, out_file=in_split, in_dir=data_dir)
    
  }
}

print("##################################################")
print("######### step 3 : Convert rtdf into csv #########")
print("##################################################")

# All sites file name
site_suffix <- paste0('_site', 0 : (site_no - 1))
file_site_sn_list <- paste0(rep(in_list, each = site_no), site_suffix)


# Create a new list with modified file names for csv
csv_name_ <- vector("list", length(file_site_sn_list))
run_nums <- numeric(length(file_site_sn_list))
site_nums <- numeric(length(file_site_sn_list))

for (i in seq_along(file_site_sn_list)) {
  parts <- strsplit(file_site_sn_list[i], "_")[[1]]
  for (j in seq_along(parts)) {
    if (grepl("Run[[:digit:]]+", parts[j])) {
      run_nums[i] <- as.integer(gsub("Run", "", parts[j]))
    }
    if (grepl("site[[:digit:]]+", parts[j])) {
      site_nums[i] <- as.integer(gsub("site", "", parts[j]))
    }
  
  }
}

for (i in 1:length(file_site_sn_list)) {
  new_file_name <- paste0(in_prefix, "_Run", run_nums[i], "_site", site_nums[i], ".csv")
  csv_name_[[i]] <- new_file_name
}


if(skip_step_till < STEP_3_RTDF_to_CSV){
    
  for (i in 1:(site_no*site_no)){
    rtdf_name = paste0(file_site_sn_list[i],".rtdf")
    csv_name = csv_name_[[i]]
    ConvertCsv(in_name=rtdf_name,out_name=csv_name,
               rows_are_tests=TRUE,in_dir=data_dir)
    
  }
}

print("###########################################################")
print("######### step 4 : Update Csv using python script #########")
print("###########################################################")

if(skip_step_till < STEP_4_CSV_SN_Update){
  upd_csv <- in_prefix #output file from python script
  
  #Generate the specific partID replacement
  partID <- list()
  vec<- as.list(seq(1, site_no))
  vec_length <- site_no + 1
  
  for (i in 1:length(vec)) {
    my_vector <- c(vec[i:length(vec)], vec[1:(i-1)])
    partID[[i]] <- my_vector[1:(vec_length-1)]
  }
  
  rearranged_partID <- c(partID[1], rev(partID[-1]))
  
  #Loop to update the csv files
  for (i in 1:site_no){
    run_name <- paste0(in_prefix,'_Run',i)
    for (j in 1:site_no){
      in_name <- paste0(run_name,'_site',j-1,'.csv')
      out_name <- paste0(run_name, '_site', j-1, '_upd.csv')
      python_script_path <- paste0(top_level, '/update_csv.py')  #change to update_csv only
      system2("python", args = c(python_script_path,  in_name, rearranged_partID[[i]][[j]], out_name, data_dir, j), stdout = TRUE) #input to python script
    }
  }
}

print("######################################################")
print("########## step 5 : convert back csv to rtdf #########")
print("######################################################")

upd_name <- gsub('.{4}$',"", csv_name_)

if(skip_step_till < STEP_5_CSV_to_RTDF){
    
  
  for (i in 1:(site_no*site_no)) {
  
    csv_name = paste(upd_name[i],"_upd.csv",sep = "")
    rtdf_name = gsub(".csv", '.rtdf', csv_name)
    ConvertCsv(in_name=csv_name,out_name=rtdf_name,
               rows_are_tests=TRUE,in_dir=data_dir)
  
  }

}

print("######################################################")
print("########## step 6 : Merged all RTDF files    #########")
print("######################################################")

if(skip_step_till < STEP_6_MERGE_RTDF){
    
  in_files_merge <<- paste0(upd_name, '_upd.rtdf')
  out_file_merge <<- paste0(in_prefix, '_merged.rtdf')
  print(in_files_merge)
  print(out_file_merge)
  MergeRtdf(in_files=in_files_merge, out_file=out_file_merge, in_dirs=data_dir)
  
}

print("######################################################")
print("########## step 7 : Generating Report        #########")
print("######################################################")

if(skip_step_till < STEP_7_GENERATE_REPORT){
  #variable instantiation
  graph_colors <- c("#000000",
                    "#808080",
                    "#a52a2a",
                    "#c48072",
                    "#FF0000", 
                    "#FD8203",
                    "#0000FF",
                    "#1E87D0", 
                    "#00FFFF",
                    "#F300FF", 
                    "#800080", 
                    "#ffff00",
                    "#f0e68c",
                    "#0C7A18", 
                    "#14FF00", 
                    "#C0FF00" 
  )
  
  graph_icons <- vector(mode = "character", length = site_no + 1)
  repeat_dataset_names <-""
  repeat_rtdf_names <- paste0(in_prefix,'_merged.rtdf')
  counter_repeatability <-0
  baseline_rtdf_names <-""
  baseline_rtdf_dirs <- ""
  pdf_name <- paste0(in_prefix, '_Report')
  
  #Baseline file
  exist_baseline <- file.exists(paste0(in_prefix, '_baseline.rtdf'))
  if (exist_baseline){
    baseline_file <<- paste0(in_prefix, '_baseline.rtdf')
  } else {
    baseline_file <<- 'Repeatability_site1.rtdf'}
  
  #Splitting the merged.rtdf
  cat(sprintf("\n...now spliting repeatability site data...\n"))
  if (repeat_dataset_names[1] == "") repeat_dataset_names[1] <- NaN
  if (repeat_dataset_names[1] == "NaN") outfile_name = "Repeatability.rtdf"
  else outfile_name <- paste(repeat_dataset_names[1],".rtdf",sep="")         
  eval(substitute(
    SplitSites(in_file=repeat_rtdf_names[1],
               out_file=outfile_name,
               in_dir=data_dir[1])
  ))
  
  load(repeat_rtdf_names[1])
  sites = as.numeric(DevicesFrame[,"site"])
  unique_sites = unique(sites)
  site_count = length(unique_sites)
  
  for (j in 1:site_count+1) {
    if (file.exists(paste(as.character(strsplit(outfile_name,"[.]rtdf$")),"_site",as.character(unique_sites[j-1]),".rtdf",sep=""))) {
      counter_repeatability <- counter_repeatability + 1
      baseline_rtdf_names[1] <- baseline_file
      baseline_rtdf_dirs[1] <- paste(data_dir[[1]],sep="",collapse=" ")
      baseline_rtdf_names[counter_repeatability + 1] <- paste(as.character(strsplit(outfile_name,"[.]rtdf$")),"_site",as.character(unique_sites[j-1]),".rtdf",sep="")
      baseline_rtdf_dirs[counter_repeatability + 1] <- paste(data_dir[[1]],sep="",collapse=" ")
    }
  }
  
  CustomCode(base_rtdf_name = baseline_rtdf_names, base_rtdf_dirs = baseline_rtdf_dirs,                           
             repeat_rtdf_name = repeat_rtdf_names, repeat_rtdf_dirs = data_dir,
             repeat_dataset_name = baseline_rtdf_names,
             pdf_name = pdf_name , title = "Silabs GRnR",
             plot_colors = graph_colors, plot_icons = graph_icons,
             param_name = "", param_dir = "",
             use_qa_lims = FALSE, qa_limits = "",
             qa_lim_dir = "", plot_scale = "auto_scale",   		
             plot_range = 0.15)
  
}