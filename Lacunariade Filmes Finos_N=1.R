#Program to calculate the lacunarity of Thin Films

#Step 1: create a variable that contains the height matrix from the specific directory

image1<-read.table (file = "sample.txt")

#Step 2: indicate the Otsu threshold

Threshold<-"K*"  #Otsu threshold

#Step 3: transform the height values into binary data

z<-ifelse (image1[3] >= Threshold, "1","0")

#Step 4: Create a binary matrix with the variable z

mat<-matrix (z, nrow = 256, ncol = 256, byrow = TRUE)

#Step 5: Choose the box size

N=1

#Step 6: cover the entire binary matrix with boxes of size N and count the number of lacunar pixels in each box

vnum0 = c(NULL)
for (n in c(1:256)) {
  
  i<-c(1+(n-1))
  
  for (k in c(2:257)){
    
    j<-c(k-1)
    
    M<-matrix(mat[i,j],nrow=N, ncol=N)
    
    soma<-sum(M==0)
    vnum0 = c(vnum0,soma)
    
  }}

#Step 7: count the number of boxes with s (s = 0,1,2,3,4, ..., NxN) lacunar pixels

ns<-table(vnum0)

print(ns)

#Step 8: calculate the probability of occupying of the boxes

Pns<-ns/(256-N+1)^(2)

print(Pns)

#Step 9: calculate the first and second moments of s

s<-unique(sort(vnum0))

M1<-(s)*Pns

print(M1)

M2<-(s^2)*Pns

print(M2)

#Step 10: calculate the lacunarity

L<-(sum(M2))/((sum(M1))^2)

#calculate the ln of N

logN<-log(N)

#calculate the ln of L

logL<-log(L)

#Sort results for visualization
result = data.frame(N,L,logN,logL)
colnames(result) = c("Size box", "Lacunarity", "logN", "logL")

#View result
result
