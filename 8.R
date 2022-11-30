#Zhuang Yuxin 222963
#Auf 1
bin_rek = function(n,k){
  if(n==k | k==0){
    return(1)
  }
  else if(n <= 0){
    return("n falsh gegeben")
  }
  else if(n<k){
    return("k falsh gegeben")
  }
  else{
    return(bino((n-1),(k-1))+bino((n-1),k))
  }
}

#a
#i)
bin_it = function(n, k){
  if(n==k | k==0){
    return(1)
  }
  else if(n <= 0){
    return("n falsh gegeben")
  }
  else if(n<k){
    return("k falsh gegeben")
  }
  sum = 1
  for(i in 1:k){
    sum = sum * (n + 1 - i) / i
  }
  return(sum)
}
#ii)
bin_dir = function(n, k){
  if(n==k | k==0){
    return(1)
  }
  else if(n <= 0){
    return("n falsh gegeben")
  }
  else if(n<k){
    return("k falsh gegeben")
  }
  return(prod((n + 1 - 1:k) / 1:k))
}

#b
library("tictoc")
tic()
BK_rek = sapply(0:25, bin_rek, n = 25)
toc()
# 71.35 sec elapsed

tic()
BK_it = sapply(0:25, bin_it, n = 25)
toc()
# 0.06 sec elapsed

tic()
BK_dir = sapply(0:25, bin_dir, n = 25)
toc()
# 0.02 sec elapsed
#Rekursive Implementierung ist am schnellsten

#Auf2
#a
f = function(x, narm = TRUE){
  if(narm){					# Falls fehlende Werte entfernt werden sollen
    a = is.na(x)			# welche Eintraege von x sind NA?
    b = length(x) - sum(a)	# Laenge von x minus Summe der fehlenden Werte
    x[a] = 0				# Fehlende Werte auf 0 setzen
  }else{
    b = length(x)			# Laenge von x
  }
  sum(x) / b					# arithmetisches Mittel
}
#b
# Code sollte strukturiert sein und kommentiert werden, damit er lesbar und 
# nachvollziebar ist. Und vektorwertig rechnen in R. Funktionen und Objekte
# mit eindeutigen Namen zu versehen

#Auf3
#a
Statistikerparty = function(n){
  l = c(rep(1, times=80),rep(2, times=20))
  j = 0
  for (i in 1:n) {
    x = sample(l, 100)
    sum = 0
    for (k in 1:100) {
      if(x[k]==2){
        sum = sum-1
        if(sum < 0){
        j = j+1
        break
        }
      }
      else{
        sum = sum+1
      }
    }
    
  }
  return(j)
}

Statistikerparty(10000) #0.2427

#b
Wechselgeld = function(n){
  l = c(rep(1, times=80),rep(2, times=20))
  for (m in c(0:10)) {
    j = 0
    for (i in 1:n) {
      x = c(rep(1,times=m),sample(l,100))
      sum = 0
      for (k in 1:100) {
        if(x[k]==2){
          sum = sum-1
          if(sum < 0){
            j = j+1
            break
          }
        }
        else{
          sum = sum+1
        }
      }
    }
    if(j == 0){
      return(m)
      break
    }
  }
}

Wechselgeld(10000) #6



