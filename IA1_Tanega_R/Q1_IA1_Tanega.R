Sn<- function(n,e){
  sum=0
  s<-seq(1,n)
  for (i in s){
    sum=sum+(2^(-n)) #applying formula
    if ((2-sum)<e){
      break;
    }
  }
  return (sum)
}

print(Sn(6,e=10))
