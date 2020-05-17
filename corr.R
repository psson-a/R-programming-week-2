corr <- function (directory, threshold = 0){
   output <- NULL
   files <- dir(directory)
   for (filename in files){
      file <- read.csv(paste(directory,'/', filename, sep=''))
      #check if the observation post meets the threshold for NA
      incomplete <- is.na(file[['sulfate']]) | is.na(file[['nitrate']])
      nr_complete <- sum(!incomplete)
      if (nr_complete > threshold){
         #include observation post
         complete <- file[!incomplete, ]
         output <- c(output, (cor(complete[['sulfate']], complete[['nitrate']])))
      }
   }
   if (is.null(output)){
      numeric()
   }
   else{
      output
   }
}