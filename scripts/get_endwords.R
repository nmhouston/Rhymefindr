#get_endwords.R

####Description####
#This script assumes that the input data is set up with one poem per plain text
#file. Each file should contain only the body of the poem: no titles, author 
#names, footnotes, or other metadata.
#Filenames will be used as textids 
#This script reads in plain text poem files in the selected folder and outputs
#a csv with the following information for each poem:
#the textid (filename), the number of lines in the poem; 
##the number of stanzas in the poem; 
#the number of lines per stanza if they are all the same length;
#a character vector of the endwords from each line of the poem

#Also output, if relevant, are:
#(1) a csv listing any text files flagged during 
#processing because line and stanza counts didn't match up (typically due to 
#extra blank lines in the text file)
#(2) a csv listing any text files that were skipped during processing 
#(unlikely but if it happens, check those text files for inconsistencies)

####install libraries####
library(tidyverse)
library(fs)
library(quanteda)

####USER INPUT REQUIRED: list paths to input and output directories#### 
inputdir<-"EXAMPLE C:/mydata/input/"
outputdir<-"EXAMPLE C:/mydata/output/"

#get file info from inputdir 
texts_paths<-path_filter(dir_ls(inputdir), glob="*.txt")
texts_names<-path_ext_remove(path_file(texts_paths))

####create vectors for output####   
textid<-vector("character", length(texts_paths))
endwords<-vector("character", length(texts_paths))
numtextlines.v<-vector("double", length(texts_paths))
numstanzas.v<-vector("double", length(texts_paths))
stanzaeqlines.v<-vector("double", length(texts_paths))
flag<-vector("character")

####get info and endwords from each poem ####
for (i in 1:length(texts_paths)){
  textid[i]<-texts_names[i]   
  text<-read_lines(texts_paths[i], 
                   skip_empty_rows = FALSE)
  text<-str_trim(text, side="left")
  
  #standardize and count the blanklines between stanzas 
  blanklines<-which(text=="")
  
  if(length(blanklines)==0){
    text<-c(text, "")
    blanklines<-which(text=="")
  }
  if(blanklines[length(blanklines)]<length(text)){
    text<-c(text, "")
    blanklines<-which(text=="")
  }
  if (text[1]!=""){
    text<-c("", text)
    blanklines<-which(text=="")
  }
  if (text[1]=="" & text[2]==""){
    text<-text[-1]
    blanklines<-which(text=="")    
  }
  if (text[1]=="" & text[2]==""){
    text<-text[-1]
    blanklines<-which(text=="")    
  }
  
  #get number of text lines and stanzas in the poem 
  numtextlines<-length(text)-length(blanklines)
  stanzalist<-list()
  for(g in 1:(length(blanklines)-1)){
    stzstart<-blanklines[g]+1
    stzend<-blanklines[g+1]-1
    stanza.v<-text[stzstart:stzend]   
    stanzalist[[g]]<-stanza.v
  }
  
  numstanzas<-length(stanzalist)
  stzlines<-vector()
  for (h in 1:(length(stanzalist))){
    stzlines<-c(stzlines, length(stanzalist[[h]]))
  }
  #consistency check  
  if(sum(stzlines)!=numtextlines) {
    message(sprintf("check your data: %s stanza total doesn't match numtextlines", 
                    texts_names[i]))
    flag<-c(flag, texts_names[i])
  }
  if (all(stzlines[1]==stzlines)) {
    stanzaeqlines<-stzlines[1]
  }else{
    stanzaeqlines<-NA
  }
   

  #record the info 
  numtextlines.v[i]<-numtextlines
  numstanzas.v[i]<-numstanzas
  stanzaeqlines.v[i]<-stanzaeqlines
   
  #tokenize, lowercase, remove punctuation, save out end words
  #NOTE: hyphens are removed and hyphenated words are put together
  #thus "to-day" (a common 19thc spelling) becomes "today"
 
   
  poemtext<-str_trim(text[text !=""], side="both")
  poem_endwords<-character() 

  #NB quanteda::remove_punct argument doesn't remove hyphens or dashes     
  for (n in 1:length(poemtext)){
    hold_tokens<-unlist(tokens_tolower(tokens(poemtext[n], 
                               split_hyphens=FALSE)),
                        recursive = FALSE, 
                        use.names = FALSE)
    hold_tokens<-str_remove_all(hold_tokens, "-+|—+")
    hold_tokens<-unlist(tokens(hold_tokens, remove_punct=TRUE))
    
    if(length(hold_tokens)==0) {
      poem_endwords[n]<-NA
    } 
    
    if (length(hold_tokens)>0) {
      poem_endwords[n]<-hold_tokens[length(hold_tokens)]
    }
    
   
  }
  if(length(which(is.na(poem_endwords)))>0){
    poem_endwords<-poem_endwords[-which(is.na(poem_endwords))]  
  }
  endwords[i]<-paste(poem_endwords, sep="", collapse= " ")
}  

####check if all files were processed ####
if (length(setdiff(texts_names, textid))==0){
  print ("all files processed OK!")
}else{
  print("saving names of unprocessed files")
  unprocessed_files<-setdiff(texts_names, textids)
  unprocessed_files<-tibble(unprocessed_files)
  write_csv(unprocessed_files, paste0(outputdir, "unprocessed_files.csv"))
}

if(length(flag) >0){
  write_csv(tibble(flag), paste0(outputdir, "flagged_files.csv"))
}

####save out poem info csv to output folder ####
poem_info<-tibble(textid=textid, 
                  numtextlines=numtextlines.v, 
                  numstanzas=numstanzas.v, 
                  stanzaeqlines=stanzaeqlines.v,
                  endwords=endwords)
write_csv(poem_info, paste0(outputdir, "poem_info.csv"))
