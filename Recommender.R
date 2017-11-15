

results.list1 <- lapply(strsplit(readLines("~/Desktop/train1.txt")," "), as.integer)
results.list2 <- lapply(strsplit(readLines("~/Desktop/test1.txt")," "), as.integer)

#use this to install the packages if required during evaluation
#install.packages(plyr)
#library(plyr)

M<-plyr::ldply(results.list1, rbind) #converts the input into a data frame
TEST<-plyr::ldply(results.list2, rbind)


#nnz=20
nnz=M[1,3];

M_col_ind=matrix(0, 1, nnz)
M_val=matrix(0, 1, nnz)
M_row_ptr=matrix(0, 1, nrow(M))

#reading M_col_ind, M_row_ptr and M_val to put train matrix in CSR form

index=1;
row_index=1;
for (row in 2:nrow(M)) {
  col=1;
  if(row==2){
    
    M_row_ptr[row_index]=index;
    row_index=row_index+1;
  }
  
    while(col<ncol(M)){
      if(!is.na(M[row,col])){
      M_col_ind[1,index]=M[row,col]
      M_val[1,index]=M[row,col+1]
      index =index+1;
      }
      col=col+2;
    }
  
    M_row_ptr[row_index]=index;
    row_index=row_index+1;
}
#putting T in CSR
T_nnz=TEST[1,3]
T_col_ind=matrix(0, 1, T_nnz)
T_val=matrix(0, 1, T_nnz)
T_row_ptr=matrix(0, 1, nrow(TEST))

index=1;
row_index=1;
for (row in 2:nrow(TEST)) {
  col=1;
  if(row==2){
    
    T_row_ptr[row_index]=index;
    row_index=row_index+1;
  }
  
  while(col<ncol(TEST)){
    if(!is.na(TEST[row,col])){
      T_col_ind[1,index]=TEST[row,col]
      T_val[1,index]=TEST[row,col+1]
      index =index+1;
    }
    col=col+2;
  }
  
  T_row_ptr[row_index]=index;
  row_index=row_index+1;
}

#transpose matrix of M

M_row_ptr2=matrix(0, 1, nnz)
M_val2=matrix(0, 1, nnz)
M_row_counts2=matrix(0, 1, nnz)
M_col_ind2=matrix(0, 1, nnz)


for (x in 1:(nnz-1)) { #counting the number of values in each column
  y=M_col_ind[x];
  M_row_ptr2[1,y+1]=M_row_ptr2[1,y+1]+1;
}

for (x in 1:ncol(M_row_ptr2)) { #constructing M_row_ptr2
  if(x==1){
    M_row_ptr2[x]=1;
  }
  else{
      M_row_ptr2[x]=M_row_ptr2[x]+M_row_ptr2[x-1];
  }
}

for (a in 1:(nrow(M)-1)) { #filling out the new matrix
  for (b in M_row_ptr[a]:(M_row_ptr[a+1]-1)){
    i2=M_col_ind[b];
    M_col_ind2[1,M_row_ptr2[i2] + M_row_counts2[i2]] = a; 
    M_val2[1,M_row_ptr2[i2] + M_row_counts2[i2]] = M_val[b];  
    M_row_counts2[i2]=M_row_counts2[i2]+1;  
  }
}

#calculate item cosine similarity between values

cosine_sim<-function(i,j){
  
  nrowi = M_row_ptr2[i+1] - M_row_ptr2[i];
  nrowj = M_row_ptr2[j+1] - M_row_ptr2[j];
  
  ni = 0; nj = 0;
  cosine = 0; lengthi = 0; lengthj = 0;
  
  
  while (ni < nrowi || nj < nrowj){
    
    ci = M_row_ptr2[i] + ni;
    cj = M_row_ptr2[j] + nj;
    
    if (M_col_ind2[ci] == M_col_ind2[cj]){
      cosine  = cosine+(M_val2[ci] * M_val2[cj]);
      lengthi = lengthi+(M_val2[ci] * M_val2[ci]);
      lengthj = lengthj+(M_val2[cj] * M_val2[cj]);
      ni=ni+1;
      nj=nj+1;
      #cat(sprintf("NI= %i \n",ni));
      #cat(sprintf("NJ= %i \n",nj));
    }
    else if (M_col_ind2[ci] > M_col_ind2[cj] && nj < nrowj){
      lengthj = lengthj+(M_val2[cj] * M_val2[cj]);
      nj=nj+1;
      #cat(sprintf("NJ= %i \n",nj));
    }
    else if (M_col_ind2[cj] > M_col_ind2[ci] && ni<nrowi){
      lengthi = lengthi+(M_val2[ci] * M_val2[ci]);
      ni=ni+1;
      #cat(sprintf("NI= %i \n",ni));
    }
    else if (M_col_ind2[cj] > M_col_ind2[ci] && ni==nrowi){
      lengthj = lengthj+(M_val2[cj] * M_val2[cj]);
      nj=nj+1;
      #cat(sprintf("NJ= %i \n",nj));
    }
    else if (M_col_ind2[ci] > M_col_ind2[cj] && nj==nrowj){
      lengthi = lengthi+(M_val2[ci] * M_val2[ci]);
      ni=ni+1;
      #cat(sprintf("NI= %i \n",ni));
    }
  }
  
  if (lengthi * lengthj != 0){
   cosine = cosine/(sqrt(lengthi * lengthj));}
  else{
   cosine = 0;}
  
  return(cosine);
  
}
  
#creating the similarity matrix S and representing it in CSR

S=matrix(0, M[1,2], M[1,2])
S_col_ind=matrix(0, 1, (nrow(M)-1)^2)
S_val=matrix(0, 1, (nrow(M)-1)^2)
S_row_ptr=matrix(0, 1, nrow(M))

S_row=1;
S_col=1;
for (x in 2: nrow(M)-1){
  
    S_row_ptr[1,S_row]=S_col;
    S_row=S_row+1;
  for (y in 2: nrow(M)-1){
    
    i=cosine_sim(x,y);
    
    S[x,y]=i;
    
    if (i!= 0){
      S_col_ind[1,S_col]=y;
      S_val[1,S_col]=i;
      S_col=S_col+1;
    }
  }
    if(x==nrow(M)-1){
    S_row_ptr[1,S_row]=S_col;
    S_row=S_row+1;
    }
}

#calculating overall similarity R for user u

R_val=matrix(0, M[1,1], M[1,2]) #number of items

R_sim<-function(u){
  

  counter=1;
  ni=0;
  c1=0;
  
  while(counter<=M[1,2]){
    
    #if(ci<M[1,1]){
    ci=M_row_ptr[u]+ni;
  #}
  #else{
  #  ci=M[1,1];
  #}
    ind=M_col_ind[1,ci];
    

    if(ind==counter){
      R_val[u,counter]= M_val[1,ci];
      counter=counter+1;
    }
    else{
      R_val[u,counter]=0;
      counter=counter+1;
    }
    
  }
  
  #R_val is the row for user in M
  
  #R=R_val*S
  
  return(R_val[u,]);
}
  
R=matrix(0, M[1,2], M[1,2]) #overall similarity matrix

#calculating overall similarity
for(x in 1:M[1,1]){
  #y=R_sim(x);
  R[x,]=R_sim(x)%*%S; #matrix multiplication
}

#testR=R;
#reccomend=matrix(0, 1, 2); #reccomended item

#showing reccomendations form overall similarity
recommendation<-function(u){
  
  #set all non purchased items to NA
  items=M_row_ptr[u+1] - M_row_ptr[u];
  ni=0;
  
  while(ni<items){
    if(ci<M[1,1]){
      ci=M_row_ptr[u]+ni;
    }
    else{
      ci=M[1,1];
    }
    ind=M_col_ind[1,ci];
    R[u,ind]<-NA;
    ni=ni+1;
  }
  max_sim=0;
  
  #chcecking for most similar items
  for(x in 1:M[1,2]){
  if(!is.na(R[u,x])){
    if(R[u,x]>max_sim){
      max_sim=R[u,x];
  }
  }
  else{}
  }
  
  reccomend=which.max(R[u,]);
  
  return(reccomend);
      
  #return(cat(sprintf("The reccomended item for purchase for user %i is item %i with similarity %f. \n",u,which.max(testR[u,]),max_sim)));
}

reccomended_item=matrix(0, 1, M[1,1]);

for(x in 1:M[1,1]){ #prints reccomendation for each user
  
  reccomended_item[1,x]=recommendation(x); #matrix multiplication
}

#output reccomendations
  
  sink("~/Desktop/output1.txt")    # Begin writing output to file
  for(x in 1:M[1,1]){
  cat(sprintf("User: %i Reccomended Item: %i \n",x,(reccomended_item[1,x])));
  }
  sink()  
  
#calculate hit rate
hit=0;
for(x in 1:M[1,1]){
  
  if(reccomended_item[1,x]==T_col_ind[T,x]){
    hit=hit+1;
  }
  print(hit);
  hr=hit/M[1,1];
  print(hr);
}
