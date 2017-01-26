#adist("kitten", "sitting")
#str1 = c("kitten"); str2 = c("sitting")

#function(str1, str2) {
  spstr1 = strsplit(str1,"")[[1]]; spstr2 = strsplit(str2,"")[[1]] #form the string
  len1 = length(spstr1); len2 = length(spstr2) #length of strings
  D <- matrix(nrow=len2+1, ncol=len1+1) #make a distance matrix
  D[1,] = 0:len1 #the value for the 1st row
  D[,1] = 0:len2 #the value for the 1st column
  for (i in 2:len1) {
    for (j in 2:len2) {
      
    }
  }
  
 
#}
  

  