pad_zeros <- function (input, target_len=3){
   input_str <- as.character(input)
   n_zeros <- target_len - nchar(input_str)
   if (n_zeros<=0){
      input_str
   }
   else {
      for (i in 1:n_zeros) {
         input_str <- paste ('0', input_str, sep='')
      }
      input_str
   }
}

complete <- function (directory, id=1:332){
   nobslist <- NULL
   for (number in id){
      filename <- paste (directory, '/', pad_zeros(number), ".csv", sep='')
      file <- read.csv(filename)
      nobs <- 0
      for (rownr in 1:nrow(file)){
         row <- file[rownr,]
         if (!any(is.na(row))){
            nobs <- nobs +1
         }
      }
      nobslist <- c(nobslist, nobs)
   }
   list2DF(list(id=id, nobs=nobslist))
}