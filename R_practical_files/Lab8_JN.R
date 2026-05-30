#strings in R
a <- "abc"
b <- "123"
#print(as.numeric(a))
print(as.numeric(b))
pets <- c("cat","dog","horse","hamster")
print(length(pets))
print(nchar(pets))
print(class(pets))
print(is.factor(pets))
#however if pets was part of a dataframe, ie it was input using read.table, then R will coerce all character variables to act as factors.
#df <- data.frame(pets)
#print(is.factor(df$pets)) THIS DID NOT WORK

print(letters)
print(LETTERS)
print(which(letters=='n'))
print(noquote(letters))

phrase <- "the quick brown fox jumps over the lazy dog"
q <- character(20)
for (i in 1:20) q[i] <- substr(phrase,1,i) #extraction begins at 1 and ends at i
print(q)

#split is a regexp, a character vector
print(nchar(phrase))
print(strsplit(phrase,split=' '))
print(table(lapply(strsplit(phrase,split=' '),nchar)))
print(strsplit(phrase,split=NULL))
print(table(strsplit(phrase,split=character(0))))

strReverse <- function(x) sapply(lapply(strsplit(x,NULL),rev),paste)
print(t(strReverse(phrase)))
strReverse2 <- function(x) sapply(lapply(strsplit(x,NULL),rev),paste,collapse="")
print(t(strReverse2(phrase)))

#count the number of words
words <- 1+table(strsplit(phrase,split=NULL))[1]
print(words)

print(toupper(phrase))

first <- c(5,8,3,5,3,6,4,4,2,8,8,8,4,4,6)
second <- c(8,6,4,2)
print(match(first,second))
subjects <- c("A","B","G","M","N","S","T","V","Z")
suitable.patients <- c("E","G","S","U","Z")
print(match(subjects,suitable.patients))

#the use of ifelse function to carry out a single task if a condition is true 
yvec <- c(1,2,-1,2,4,8,6,7,23,-9,10,-23)
z <- ifelse( yvec<0,-1,1)
print(z)
yvec <- c(1,2.1,9.7,3.4,5.6,7.8,10,2.3,4.7,8.8,4.6,10.2)
z <- ifelse(yvec > median(yvec),"Big","Small")
print(z)
yvec <- c(-Inf,0.693,-Inf,0.0, -Inf,0.0,0.0,-Inf,0.693,1.609,1.386,0.0,1.386,-Inf,0.0,0.0,0.0693,0.693,0.0,-Inf)
z <- ifelse(yvec<0,NA,yvec)
print(z)

drug <- c("new","conventional")
matched_sub <- match(subjects,suitable.patients)
drug_sugges <- drug[ ifelse(is.na(matched_sub),2,1)]
print(drug_sugges) #output has the length of subjects vector

#substituting text within character strings
text <- c("arm","leg","head","foot","hand",'hindleg','elbow')
print(gsub('h','H',text))
print(sub("o","O",text)) #only the first instance
print(gsub("o","O",text))
print(gsub('^.',"O",text)) #^ is the start of the string marker. O will replace the character at the start of any string
#print(gsub("(\\w*)(\\w*)","\\U\\1\\L\\2",text,perl=TRUE)) not working
print(gsub("(\\w*)","\\U\\1",text,perl=TRUE))

stock <- c("car","van")
requests <- c("truck","suv","van","sports","car","waggon","car")
print(which(requests %in% stock))
print(requests[which(requests %in% stock)])
print(stock[match(requests,stock)][!is.na(match(requests,stock))]) #another method to achieve the above
print(which(sapply(requests,"%in%",stock)))

#use of simple regular expressions
st = "filename.doc"
print(strsplit(st,'\\.')) #need \\ for any metacharacter

#converting case
str3 = "This is a sentence"
print(toupper(str3))
print(tolower(str3))

#use of regular expressions
text <- c("arm","leg","head","foot","hand",'hindleg','elbow')
print(grep("o{1}",text,value=T))
print(grep("o{2}",text,value=T))
print(grep("o{3}",text,value=T))
print(grep("[[:alnum:]]{4,}",text,value=T)) #pick up words containing 4 or more alphabets or numbers or both
print(grep("[[:alnum:]]{5,}",text,value=T))
print(grep("[[:alnum:]]{6,}",text,value=T))
print(grep("[[:alnum:]]{7,}",text,value=T))

# One can also use substr to substitute text
text2 = "Mitochondria and Golgi bodies"
substr(text2,4,8) <- "UUUU"
print(text2)

#truncating a string
str2 <- "AECH9939-ALM"
str2cut <- strtrim(str2,4) #truncate from beginning up to 4 characters
print(str2cut)

#counting instances of pattern in a string
library(stringr)
print(str_count("ATTGCGCTATGCGC",pattern="GC"))

## EXERCISES
#1
checkpalin <- function (n) {
  n <- as.character(n)
  nrev <- sapply(lapply(strsplit(n,NULL),rev),paste,collapse="")
  return(nrev)
}
n=124
nrev <- checkpalin(n)
ifelse (nrev==as.character(n),
        paste(n,("n is a palindrome number")),
        paste(n,("n is not a palindrome")))

#2
string <- "seemerightnow"
print(substr(string,1,3))
print(substr(string,4,5))      
print(substr(string,6,10))

#3 
sequen <- "ATTGCGCATAGTCCGGG"
seqsplit <- strsplit(sequen,split="")
print(seqsplit)
nG <- sum(seqsplit[[1]]=="G")
print(paste("The fraction of G: ",round(nG/nchar(sequen),2)))
nC <- sum(seqsplit[[1]]=="C")
print(paste("The fraction of C: ",round(nC/nchar(sequen),2)))

#4
dnaseq="ACCTAGGT"
compseq <- ''
splitdna <- strsplit(dnaseq,split=NULL)[[1]]
nbases <- sum(nchar(splitdna))
print(nbases)
for (i in 1:nbases){
  print(splitdna[i])
  if (splitdna[i]=='A') {compseq <- c(compseq,'T')}
  if (splitdna[i]=='G') {compseq <- c(compseq,'C')}
  if (splitdna[i]=='T') {compseq <- c(compseq,'A')}
  if (splitdna[i]=='C') {compseq <- c(compseq,'G')}
}
compseq <- paste(compseq,collapse='')
print(compseq)
revdnaseq <-  sapply(lapply(strsplit(dnaseq,NULL),rev),paste,collapse="")
print(revdnaseq)
if (revdnaseq==compseq) { print('The DNA sequence is a palindrome!')}

#5 
sentence="She sells hundreds of sea oysters on the sea shore."
wordlist=strsplit(sentence,split=' ')[[1]]
nwords = length(wordlist)
print('Number of words in the sentence = ',nwords)
print(c('The list of words: ',wordlist))
maxlen <- 0
largestword <- ''
for (word in wordlist){
  if (nchar(word)>maxlen) {
  maxlen=nchar(word)
  largestword <- word}}
print(maxlen)
print(paste("The largest word is: ",largestword))
maxlen2 <- 0
largestword2 <- ''
for (word in wordlist){
    if (nchar(word)>maxlen2 && nchar(word)<maxlen){
      maxlen2 = nchar(word)
      largestword2 <- word}}
print(maxlen2)
print(paste("The second largest word is: ",largestword2))

#6
wf = read.table(file='D:/ACADEMIC/TEACHING/TEACHING_IN_IBAB/FY2024-2025/BDBP209/Labs/DATA/worldfloras.txt',header=TRUE)
print(str(wf))
print(is.factor(wf$Continent))
wf$Continent=as.factor(wf$Continent)
print(is.factor(wf$Continent))
print(levels(wf$Continent))
print(nlevels(wf$Continent))
library(moments) #or library(e1071)
for (level in 1:nlevels(wf$Continent)) {
  print(c('Continent data: ',levels(wf$Continent)[level]))
  df <- subset(wf,wf$Continent==levels(wf$Continent)[level])
  print(str(df))
  boxplot(df$Flora)
  florastat <- summary(df$Flora)
  florasd<- sd(df$Flora)
  floraskew <- skewness(df$Flora)
  florakurt <- kurtosis(df$Flora)
  if (!is.nan(floraskew)) {
    if (abs(floraskew) < 0.001 ){ 
    print(c("Floral distn of ",levels(wf$Continent)[level],' is symmetric'))
  } else if (floraskew>0) {print(c("Floral distn of ",levels(wf$Continent)[level],' is skewed to the right'))
  } else if (floraskew<0) {print(c("Floral distn of ",levels(wf$Continent)[level],' is skewed to the left'))}
  }
  boxplot(df$Population)
  Popsumm <- summary(df$Population)
  Popsd <- sd(df$Population)
  Popskew <- skewness(df$Population)
  Popkurt <- kurtosis(df$Population)
  if (!is.nan(Popskew)) {
    if (abs(Popskew) < 0.001 ){ 
      print(c("Popln distn of ",levels(wf$Continent)[level],' is symmetric'))
    } else if (Popskew>0) {print(c("Popln distn of ",levels(wf$Continent)[level],' is skewed to the right'))
    } else if (Popskew<0) {print(c("Popln distn of ",levels(wf$Continent)[level],' is skewed to the left'))}
  }
}

#7
library(readr)
library(stringr)
humanbones=read_file(file='D:/ACADEMIC/TEACHING/TEACHING_IN_IBAB/FY2024-2025/BDBP209/Labs/DATA/HumanBones.txt')
print(humanbones)
flines=strsplit(humanbones,split='\\r\\n')[[1]]
print(flines)
categories <- c("Chest","Spine","Skull","Ear Bones","Arms","Legs")
levels <- factor(categories)
cate_col=c(rep("Chest",3),rep("Spine",5),rep("Skull",17),rep("Ear Bones",6),rep("Arms",8),rep("Legs",7))
print(cate_col)
bones <- ''
bones_num <- ''
for (line in flines){
  if (!(line %in% categories) & line!=''){
    #nums <- regmatches(line,gregexpr("[0-9]+",line)) #method 1
    nums <- unlist(str_extract_all(line,"\\d+")) #method2
    nums <- paste(nums[1:length(nums)],collapse='/')
    if (nums=="NA") { nums <- "1"}
    bones_num <- c(bones_num,nums)
    bone_des <- unlist(regmatches(line,gregexpr("\\w+",line)))  
    bone_des <- setdiff(bone_des,unlist(str_extract_all(line,"\\d+")))
    for (word in bone_des){
      if (word=='or' | word=='in' | word=='pairs'){
        bone_des <- setdiff(bone_des,word)
      }
    }
    bone_des <- paste(bone_des,collapse=' ')
    bones <- c(bones,bone_des)
  }
}
bones <- bones[2:length(bones)]
bones_num <- bones_num[2:length(bones_num)]
print(bones)
print(bones_num)
print(paste(length(cate_col),length(bones),length(bones_num)))
bones_df <- data.frame(cate_col,bones,bones_num)
print(bones_df)
print(dim(bones_df))

#8
maxbones <-0
cate_bone_freq <- ''
for (icate in categories){
  subset_data <- subset(bones_df,bones_df$cate_col==icate)
  cate_bone_num <-0
  for (ibone in 1:dim(subset_data)[1]){
    numbone <- subset_data$bones_num[ibone]
    if (is.na(as.numeric(numbone))) { 
    numvec <- unlist(strsplit(numbone,split='\\/'))
    if (length(numvec)>0) {
      for (ii in 1:length(numvec)){ 
        cate_bone_num <- cate_bone_num + as.numeric(numvec[ii])}
      } 
    } else if(!is.na(numbone)) {
      cate_bone_num <- cate_bone_num+ as.numeric(numbone)}
  }
  cate_bone_freq <- c(cate_bone_freq,cate_bone_num)
  print(paste(icate,cate_bone_num))
}
cate_bone_freq <- cate_bone_freq[2:length(cate_bone_freq)]
print(paste(categories,cate_bone_freq))
maxbones <- max(cate_bone_freq)
maxibones <- which.max(cate_bone_freq)
print(paste(categories[maxibones],maxbones,' is the maximum number of bones in this data.'))
cate_bone_freq <- as.numeric(cate_bone_freq)
bones_freq <- paste(categories,cate_bone_freq)
print(bones_freq)
for (icate in categories){ 
  if (length(strsplit(icate,split=' ')[[1]])>1){
  icate_name <- paste(strwrap(icate,4),collapse='\n')
  categories <- sub(icate,icate_name,categories)
}
}
barplot(cate_bone_freq,names.arg=categories,space=2.5)

#9
subset_legs=subset(bones_df,bones_df$cate_col=="Legs")
print(subset_legs)
print(paste(subset_legs$bones[nchar(subset_legs$bones)>5]))

#10
Mbones <- grep("^M",bones_df$bones,value=T)
print(Mbones)
Mbones_ucase <- gsub("a","A",Mbones)
print(Mbones_ucase)

#11
Ebones <- grep("e$",bones_df$bones,value=T)
print(Ebones)
Ebones_lcase <- tolower(Ebones)
print(Ebones_lcase)

#12 bones with 2 o's in their names
Owords <- ''
for (ibone in bones_df$bones){
  words <- strsplit(ibone,split=' ')[[1]]
  Owords <- c(Owords,ibone[length(grep("o",words))==2])
}
Owords <- Owords[2:length(Owords)]
print(Owords)
