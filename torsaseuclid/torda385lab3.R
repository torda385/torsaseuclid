eucl_algo <- function(a,b){
  if (is.numeric(a) && length(a)==1 && a>0  && is.numeric(b) && length(b)==1 && b>0 ) {
      if (a>=b){
      ab<-a
      cd<-b
    } else if (a<b){
      ab<-b
      cd<-a
    } else stop()
        while (ae > 0){
        ae<-ab%%cd
        ab<-cd
        cd<-ae
      }
    return(ab)
  } else stop("Values must be positive integers.")
}
