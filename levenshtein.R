#adist("kitten", "sitting") #the function for levenshtein distance
#str1 = c("kitten"); str2 = c("sitting") #example strings

lv.dist = function(str1, str2) {
  spstr1 = strsplit(str1,"")[[1]]; spstr2 = strsplit(str2,"")[[1]] #form the string
  len1 = length(spstr1); len2 = length(spstr2) #length of strings
  D <- matrix(nrow=len2+1, ncol=len1+1) #make a distance matrix
  D[1,] = 0:len1 #the value for the 1st row
  D[,1] = 0:len2 #the value for the 1st column
  for (i in (1:len2)+1) {
    for (j in (1:len1)+1) {
      frpi = D[i-1,j] + 1
      frpj = D[i,j-1] + 1
      frpij = D[i-1,j-1] + ifelse(spstr1[j-1]!=spstr2[i-1],1,0)
      D[i,j] = min(frpi,frpj,frpij) #get the minimum value of three:frpi, frpj, frpij
    }
  }
  return(D[len2+1,len1+1]) #get levenshtein distance
}
  
lv.dist("kitten", "sitting") #the result is same as adist function
  