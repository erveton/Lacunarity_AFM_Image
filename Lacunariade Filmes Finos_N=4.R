#Programa para calcular a lacunaridade de Filmes Finos

#Passo 1: criar uma variável que contenha a matriz de alturas a partir do diretório específico.

image1<-read.table (file = "sample.txt")

#Passo 2: indicar o limiar.

limiar<-"k*" #limiar Otsu

#Passo 3: Transformar os valores de alturas em dados binários.

z<-ifelse (image1[3] >= limiar, "1","0")

#Passo 4: Criar uma matriz binária com a variável z.

mat<-matrix (z, nrow = 256, ncol = 256, byrow = TRUE)

#Passo 5: Escolher o tamanho da caixa.

N=4

#Passo 6: cobrir toda a matriz binária com as caixas de tamanho N e Contar o número de pixels lacunares em cada caixa.

vnum0 = c(NULL)
for (n in c(1:253)) {
  
  i<-c(1+(n-1),2+(n-1),3+(n-1),4+(n-1))
  
  for (k in c(2:254)){
    
    j<-c(k-1,k,k+1,k+2)
    
    M<-matrix(mat[i,j],nrow=N, ncol=N)
    
    soma<-sum(M==0)
    vnum0 = c(vnum0,soma)
    
  }}

#Passo 7: contar o número de caixas com s (s=0,1,2,3,4,...,NxN) pixels lacunares.

ns<-table(vnum0)

print(ns)

#Passo 8: calcular a probabilidade de ocupação das caixas.

Pns<-ns/(256-N+1)^(2)

print(Pns)

#Passo 9: calcular o primeiro e o segundo momento do s.

s<-c(0:16)

M1<-(s)*Pns

print(M1)

M2<-(s^2)*Pns

print(M2)

#Passo 10: calcular a lacunaridade.

L<-(sum(M2))/((sum(M1))^2)

#calcular ln de N

logN<-log(N)

#calcular ln de L

logL<-log(L)

# Arrumar resultados para visualizacao
result = data.frame(N,L,logN,logL)
colnames(result) = c("Size box", "Lacunarity", "logN", "logL")

# Ver resultado
result

