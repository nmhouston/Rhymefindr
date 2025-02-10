#find_rhymes.R

####Description####

#This script is designed to work with an input csv containing at least two 
#columns, labeled textid and endwords, where endwords contains character 
#vectors of line-end words from each poem.
#This input csv can be produced from plain-text files of poems by the 
#accompanying get_endwords script. 

#The script extracts the final syllable of each endword and performs a 
#series of lookups in the key-value table created from an 1824 edition 
#of John Walker's A Rhyming Dictionary, matching rhyme syllables and 
#rhyme words with the entries in Walker. Words not found in Walker that can
#be matched orthographically with other words in the poem will also be 
#recorded as rhyme words. 

#The script outputs a csv with the following information for each poem: 
#a representation of the rhymes used in the poem as a sequence of capital 
#letters; a vector of the rhyme syllables; a vector of the rhyme words; 
#a numerical vector showing which of the rhymes are perfect rhymes; and 
#a categorical indicator as to the likelihood of the poem being rhymed,
#where 1 signifies it is rhymed, 2 signifies it is possibly rhymed, and 
#0 means it is not rhymed.  



####install libraries ####

library(fs)
library(tidyverse)
library(stringi)
library(data.table)  

####USER INPUT REQUIRED: list paths to input and output directories#### 
inputdir<-"EXAMPLE C:/mydata/input/"
outputdir<-"EXAMPLE C:/mydata/output/"
dictionarydir<-"EXAMPLE C:/mydata/dictionary/"

##### function from Gaston Sanchez, Handling Strings with R####
  #https://www.gastonsanchez.com/r4strings/

reverse_chars<-function(string)
{
  string_split=strsplit(string, split="")
  rev_order=nchar(string):1
  reversed_chars=string_split[[1]][rev_order]
  paste(reversed_chars, collapse="")
}
 

####load Walker dictionary ####

walker<-fread(paste0(dictionarydir, "walker5.csv"))
setkey(walker, syll)

####load a csv with textid and endwords for each poem####
#Here this is created from the poem_info csv output from get_endwords script

#You can also read in another csv containing at minimum two 
#columns: textid, endwords, where endwords contains character vectors 
#of line-end words from each poem

endwords<-read_csv(paste0(inputdir, "poem_info.csv")) %>% 
  select(textid, endwords)

####create containers ####
rhymelist<-vector("list", length(endwords$textid))
names(rhymelist)<-endwords$textid[]

texts.df<-data.frame()
letters.hold<-vector()

#####Run rhyme finder####

#reverse the spelling of the endwords and identify the final syllable
#Note: if the final syllable doesn't parse correctly it will list "zzz"
#to enable error checking 

for (i in 1:length(endwords$textid)){
  lastwords<-unlist(str_split(endwords$endwords[i], " "))
  lastwords.clean<-stri_trans_general(lastwords, 
          id="Any-Latin;Greek-Latin;Latin-ASCII; lower")
  lastwords.rev<-unlist(map(lastwords.clean, ~reverse_chars(.x)))
  rev.rhymes<-str_extract_all(lastwords.rev, 
            "^[t][s][e]|^[s][e](?=[c|s])|^[s][e]?[bcdfghjklmnpqrstvwxyz]+[aeiou]|^[d][\'|aeiou][bcdfghjklmnpqrstvwxyz]{1,2}[aeiou]{1,2}|^[bcdfghjklmnpqrstvwxz]+[aeiouyAEIOUY]{1,2}|^[e][bcdfghjklmnpqrstvwxz]{1,2}+[aeiouyAEIOUY]{1,2}|^[t][s][\'|e][bcdefghjklmnpqrstvwxyz]+[aeiou]{1,2}|^[aeiouy][aeiouy]?|^[e][u]*[bcdfghjklmnpqrstvwxyz]+[aeiouAEIOU]{1,2}|^[d][\']*[bcfghjklmnpqrstvwxz]*[aeiou]{1,2}|^[t|s|r][\'][bcdfghjklmnpqrstvwxyz]+[aeiou]|^[e]|^[o]|^[y]|^[AIO]{1}")
  
  for (ch in 1:length(rev.rhymes)){
    if(length(rev.rhymes[[ch]])==0) {rev.rhymes[[ch]]<-"zzz"}
  }
  rhymes.vec<-unlist(map(rev.rhymes, ~reverse_chars(.x)))

  #create data containers 
  ltrvec<-vector("character", length(rhymes.vec))
  pfvec<-vector("numeric", length(rhymes.vec))
  lastwords.regex<-vector()
  
  for (s in 1:length(lastwords.clean)){
    lastwords.regex[s]<-paste("^", lastwords.clean[s], "$", sep="")
  }
  
  didreplace<-vector(mode="logical")
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  rhymeletter<-LETTERS702[1]
  rhymecount<-1
  

  #Here's where the rhyme loop starts (inside the endword vec loop)
  #for each rhyme in rhymevec, first find matches in the dictionary for 
  #rhyme syllables and words, both perfect and allowable rhymes
  #then it goes through the poem and tags those matching 
  #rhymes with letters (A, B, C)
  #then go back and looks up the next unlettered rhyme in rhymes.vec
  for (e in 1:length(rhymes.vec)){
    if((ltrvec[e]=="")==TRUE){
      s<-length(walker[rhymes.vec[e]]$syll)
      perfwords<-""
      allowwords<-""
      perfrhy<-""
      allowrhy<-""
             
      for (k in 1:s){
        perfwords<-paste(perfwords, walker[rhymes.vec[e]]$perfword[k], sep=", ")
        allowwords<-paste(allowwords, walker[rhymes.vec[e]]$allowword[k], sep=", ")
        perfrhy<-paste(perfrhy, walker[rhymes.vec[e]]$perf[k], sep=", ")
        allowrhy<-paste(allowrhy, walker[rhymes.vec[e]]$allow[k], sep=", ")
      } 
      #removes the commas that are in the front of the data from table  
      
      perfwords<-sub(", ", "", perfwords, fixed=T) 
      allowwords<-sub(", ", "", allowwords, fixed=T) 
      perfrhy<-sub(", ", "", perfrhy, fixed=T) 
      allowrhy<-sub(", ", "", allowrhy, fixed=T) 
      
      #split them up into a vector, clean up, add the regex         
      perfwords2<-strsplit(perfwords, "[,]")
      perfwords3<-str_trim(perfwords2[[1]], side="both")
      perfwords4<-vector()
      for (p in 1:length(perfwords3)){
        perfwords4[p]<-paste("^", perfwords3[p], "$", sep="")
      }
      
      allowwords2<-strsplit(allowwords, "[,]")
      allowwords3<-str_trim(allowwords2[[1]], side="both")
      allowwords4<-vector()
      for (p in 1:length(allowwords3)){
        allowwords4[p]<-paste("^", allowwords3[p], "$", sep="")
      }
      
      perfrhy2<-strsplit(perfrhy, "[,]")
      perfrhy3<-str_trim(perfrhy2[[1]], side="both")
      perfrhy4<-vector()
      for (p in 1:length(perfrhy3)){
        perfrhy4[p]<-paste("^", perfrhy3[p], "$", sep="")
      }
      
      allowrhy2<-strsplit(allowrhy, "[,]")
      allowrhy3<-str_trim(allowrhy2[[1]], side="both")
      allowrhy4<-vector()
      for (a in 1:length(allowrhy3)){
        allowrhy4[a]<-paste("^", allowrhy3[a], "$", sep="")
      } 
      
      #creates a list to check against that are the rhymes in the poem             
      gr.rhymes<-vector()
      for (g in 1:length(rhymes.vec)){
        gr.rhymes[g]<-paste("^", rhymes.vec[g], "$", sep="")
      }
      
      letterold<-""
      
      #checks rhymes.vec against the perfrhy it matched for the poem      
      m3<-unique(grep(paste(perfrhy4, collapse="|"), rhymes.vec, value=FALSE))
      if (length(m3)>0){
        for (j in 1:length(m3)){
          if (ltrvec[m3[j]]==""){
            ltrvec[m3[j]]<-rhymeletter
            didreplace<-TRUE
            pfvec[m3[j]]<-1
          }else{
            letterold<-ltrvec[m3[j]]
            ltrvec<-replace(ltrvec, m3, letterold)
            letterold<-""
          }
        }
      }
      
      
      #checks lastwords against the perfwords 
      m1<-unique(grep(paste(perfwords4, collapse="|"), lastwords.clean, value=FALSE))
      if (length(m1)>0){
        for (j in 1:length(m1)){
          if (ltrvec[m1[j]]==""){
            ltrvec[m1[j]]<-rhymeletter
            didreplace<-TRUE
            pfvec[j]<-1
          }else{
            letterold<-ltrvec[m1[j]]
            ltrvec<-replace(ltrvec, m1, letterold)
            letterold<-""
          }
        }
      }
      
      m2<-unique(grep(paste(allowwords4, collapse="|"), lastwords.clean, value=FALSE))
      if (length(m2)>0){
        for (j in 1:length(m2)){
          if (ltrvec[m2[j]]==""){
            ltrvec[m2[j]]<-rhymeletter
            didreplace<-TRUE
          }else{
            letterold<-ltrvec[m2[j]]
            ltrvec<-replace(ltrvec, m2, letterold)
            letterold<-""
          }
        }
      }
      
      m4<-unique(grep(paste(allowrhy4, collapse="|"), rhymes.vec, value=FALSE))
      if (length(m4)>0){
        for (j in 1:length(m4)){
          if (ltrvec[m4[j]]==""){
            ltrvec[m4[j]]<-rhymeletter
            didreplace<-TRUE
          }else{
            letterold<-ltrvec[m4[j]]
            ltrvec<-replace(ltrvec, m4, letterold)
            letterold<-""
          }
        }
      }
      
      m5<-unique(grep(paste(gr.rhymes[e]), rhymes.vec, value=FALSE))
      if (length(m5)>0){
        for (j in 1:length(m5)){
          if (ltrvec[m5[j]]==""){
            ltrvec[m5[j]]<-rhymeletter
            didreplace<-TRUE
          }else{
            letterold<-ltrvec[m5[j]]
            ltrvec<-replace(ltrvec, m5, letterold)
            letterold<-""
          }
        }
      }
      
      if(isTRUE(didreplace)){
        rhymecount<-(rhymecount+1)
        rhymeletter<-LETTERS702[rhymecount]
        didreplace<-FALSE
      } else {
        next
      }
    } else {
      next
    }
  }
  #close of the big rhyme finder loop (e in rhymes.vec) 
  
  #Evaluate whether the poem is rhymed or not     
  if(length(ltrvec)<75){
    if(length(unique(ltrvec))/length(ltrvec)<.70){
      isrhymed<-1
    }else if(length(unique(ltrvec))/length(ltrvec)>.70 & 
             length(unique(ltrvec))/length(ltrvec)<.86){
      isrhymed<-2      
    }else{
      isrhymed<-0
    }
  }else{
    if (length(unique(ltrvec[1:75]))/75<.70){
      isrhymed<-1
    }else if (length(unique(ltrvec[1:75]))/75>.70 &
              length(unique(ltrvec[1:75]))/75<.86){
      isrhymed<-2
    }else{
      isrhymed<-0
    }
  }
  
  #save out the results to rhymelist for the  poem
  rhymelist[[i]]<-list(lastwords.clean, rhymes.vec, ltrvec, pfvec, isrhymed)
  
  rm(lastwords, lastwords.rev, lastwords.clean, rhymes.vec, ltrvec, pfvec, isrhymed)
  
}

#this is the end of the super big loop for each poem row from endwords csv
 

#### save the results to csv #### 
rhymedata.df<-tibble()

for (i in 1:length(rhymelist)){
  textid<-endwords$textid[i]
  words<-paste(rhymelist[[i]][[1]], collapse=" ")
  sylls<-paste(rhymelist[[i]][[2]], collapse=" ")
  ltrvec<-paste(rhymelist[[i]][[3]], collapse=" ")
  perf<-paste(rhymelist[[i]][[4]], collapse=" ")
  isrhymed<-rhymelist[[i]][[5]]
  this_tib<-tibble(textid, words, sylls, ltrvec, perf, isrhymed)
  rhymedata.df<-bind_rows(rhymedata.df, this_tib)
}
write_csv(rhymedata.df, paste0(outputdir, "rhymedata.csv"))


