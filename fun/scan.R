scan <- function(x,split,n,...){
  sapply(strsplit(x = x,split = split,...),"[",n)
}