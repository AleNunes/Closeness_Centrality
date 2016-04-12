# limpar memória do R
rm(list=ls(all=TRUE))

setwd("D:/PROJETOS/Desafios/Semantix")
g = read.table("edges",sep=" ") + 1


# Converter o grafo em uma matriz com os vertices
grafo = table (g)
grafo


# Aplicando algoritmo de Floyd Warshal
# https://pt.wikipedia.org/wiki/Algoritmo_de_Floyd-Warshall
# A escolha do algoritmo foi por ser de implementação mais simples se comparado ao de Dijkstra, por exemplo
fw = function (mtz){
  for (i in 1:nrow(mtz)){ 
    for (j in 1:ncol (mtz))
      if (mtz[i,j] == 0 & i!=j )
        mtz[i,j] = Inf}
  n = ncol (mtz)
  for (k in 1:n)
    for (i in 1:n)
      for (j in 1:n)
        if (mtz[i,j] >  mtz[i,k] + mtz[k,j])
          mtz[i,j] = mtz[i,k] + mtz[k,j]

#Calcula Centralidade
  c = 1/(rowSums( (mtz) ))
  names(c) = as.integer(names (c))-1
  return  (sort (c, decreasing = T))
}


centrality = fw(grafo)
centrality[1]
centrality

