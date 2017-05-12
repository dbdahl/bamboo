bamboo.sanitize <- function(center) {
  ccenter <- gsub("L","C",center)
  ccenter <- gsub("([^H])H{4,4}([^H])","\\1CCCC\\2",ccenter)
  ccenter <- gsub("([^H])H{3,3}([^H])","\\1CCC\\2",ccenter)
  ccenter <- gsub("([^H])H{2,2}([^H])","\\1CC\\2",ccenter)
  ccenter <- gsub("([^H])H{1,1}([^H])","\\1C\\2",ccenter)
  ccenter <- gsub("([^E])E{2,2}([^E])","\\1CC\\2",ccenter)
  ccenter <- gsub("([^E])E{1,1}([^E])","\\1C\\2",ccenter)
  ccenter <- gsub("([^T])T{2,2}([^T])","\\1CC\\2",ccenter)
  ccenter <- gsub("([^T])T{1,1}([^T])","\\1C\\2",ccenter)
  n <- nchar(ccenter)
  if ( substring(ccenter,1,1) != 'C' ) ccenter <- paste('C',substring(ccenter,2,n),sep="")
  if ( substring(ccenter,n,n) != 'C' ) ccenter <- paste(substring(ccenter,1,n-1),'C',sep="")
  if ( ccenter == paste(rep('C',n),collapse="") ) ccenter <- paste(c(rep("C",1),rep("E",n-2),rep("C",1)),collapse="")
  if ( ccenter == center ) return(ccenter)
  else return(bamboo.sanitize(ccenter))
}

