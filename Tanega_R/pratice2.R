text= "Hello World World"

table(lapply(strsplit(text, split = " "), nchar))
lapply(strsplit(text, split = " "), nchar)


table(strsplit(phrase, NULL))
str(table(strsplit(phrase, NULL)))   # here 1st-- no of spaces-entry  is taken for next command
words <- 1+table(strsplit(phrase, NULL))[1]
print(words)

#above one is not seen

irst <- c(5,8,5,3,3,6,4,4,2,8,8,8,4,6) # it gives index where it matches
second<- c(8,6,4,2)
match(first, second) 