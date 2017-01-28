#library(stringdist)
#stringdist("animal","ainmal",method="dl") #the function for damerau-levenshtein distance
#str1 = c("animal"); str2 = c("ainmal") #example strings

d.dist = function(str1, str2) {
  spstr1 = strsplit(str1,"")[[1]]; spstr2 = strsplit(str2,"")[[1]] #form the string
  len1 = length(spstr1); len2 = length(spstr2) #length of strings
  D <- matrix(nrow=len2+1, ncol=len1+1) #make a distance matrix
  D[1,] = 0:len1 #the value for the 1st row
  D[,1] = 0:len2 #the value for the 1st column
  for (i in (1:len2)+1) { #string starts at 2nd row
    for (j in (1:len1)+1) { #string starts at 2nd column
      frpi = D[i-1,j] + 1
      frpj = D[i,j-1] + 1
      frpij = D[i-1,j-1] + ifelse(spstr1[j-1]!=spstr2[i-1],1,0)
      D[i,j] = min(frpi,frpj,frpij) #get the minimum value of three:frpi, frpj, frpij
      
      if (i>2 && j>2 && spstr1[j-1]==spstr2[i-2] && spstr1[j-2]==spstr2[i-1]) {
        fr2pij = D[i-2,j-2] + 1
        D[i,j] = min(D[i,j], fr2pij) #transposition
      }
    
    }
  }
  return(D[len2+1,len1+1]) #get damerau-levenshtein distance: i from str2, j from str1
}

d.dist("animal", "ainmal") #correct answer=1
lv.dist("animal", "ainmal") #to confirm: answer=2
