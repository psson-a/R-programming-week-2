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

pollutantmean <- function (directory, pollutant, id=1:332){
   data <- NULL
   for (number in id){
      filename <- paste (directory, '/', pad_zeros(number), ".csv", sep='')
      file <- read.csv(filename)
      datacol <- file[[pollutant]]
      data <- c(data, datacol[!is.na(datacol)])
   }
   mean(data)
}


