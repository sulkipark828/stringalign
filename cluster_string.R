# data
str.data = c("apple", "animal", "bus", "bear", "cat", "chemical", "tear")
clst = vector("list", length(str.data))
for (i in 1:length(str.data)) {
  clst[[i]] = i
}

# specified values
repl = 1 #replacement cost
k = 3

#function for clustering strings
clust.str = function(str.data) {
  spstr.data = strsplit(str.data, "") #make a list for the data
  ld = length(spstr.data) #length of sequences data
  combnum = combn(ld, 2) #sequence combination to calculate distance
  str.dist = as.matrix(combnum) #1st row & 2nd row sequences' distance on 3rd row
  str.dist = rbind(str.dist, 0)
  #norm.dist = str.dist #normalized sequence matrix: don't need! normalize str.dist matrix!
  
  for (cn in 1:ncol(combnum)) {
    spstr1 = spstr.data[[combnum[1,cn]]]; spstr2 = spstr.data[[combnum[2,cn]]]
    len1 = length(spstr1); len2 = length(spstr2) #length of strings
    D <- matrix(nrow=len2+1, ncol=len1+1) #make a distance matrix
    D[1,] = 0:len1 #the value for the 1st row
    D[,1] = 0:len2 #the value for the 1st column
    for (i in (1:len2)+1) { #string starts at 2nd row
     for (j in (1:len1)+1) { #string starts at 2nd column
       frpi = D[i-1,j] + 1 #deletion
       frpj = D[i,j-1] + 1 #insertion
       frpij = D[i-1,j-1] + ifelse(spstr1[j-1]!=spstr2[i-1],repl,0) #substitution
       D[i,j] = min(frpi,frpj,frpij)
     }
    } 
    #str.dist[3,cn] = D[len2+1,len1+1] #save the distance on the matrix
    str.dist[3,cn] = D[len2+1,len1+1]/max(len1,len2) #save the normalized distance on the matrix
  }
  
  dist.list = vector("list", ld) #list which has each sequence
  denst = vector(length=ld) #density vector for sequences
  for (i in 1:ld) {
    dist.list[[i]] = str.dist[,str.dist[1,]==i|str.dist[2,]==i]
    d = sort(dist.list[[i]][3,])[k]
    dist.list[[i]] = dist.list[[i]][,dist.list[[i]][3,] <= d] #keep only k-NN sequnces
    n = ncol(dist.list[[i]])
    denst[i] = n/(ld*d) #density for each sequences
  }
  ##### step 1
  
  denst.c = denst #density vector for clusters
  for (i in 1:ld) {
    clst[[i]]=i
    for (n in 1:ncol(dist.list[[i]])) {
      j = ifelse(dist.list[[i]][1,n]==i, dist.list[[i]][2,n], dist.list[[i]][1,n])
      if (denst[i]<denst[j]) {
        clst[[i]]=c(clst[[i]],j)
        tf = dist.list[[i]][3,] < dist.list[[i]][3,n]
        nn = which(tf, T)
        jj = ifelse(dist.list[[i]][1,nn]==i, dist.list[[i]][2,nn], dist.list[[i]][1,nn])
        tf2 = denst[i] < denst[jj]
        if (sum(tf) != 0 && sum(tf2) != 0) {
          clst[[i]]=clst[[i]][-length(clst[[i]])]
        }
      }
    }
    denst.c[i] = max(denst[clst[[i]]])
  }
  
  ##### step 2


}  
