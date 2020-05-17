corr <- function (directory, threshold = 0){
   files <- dir(directory)
   for (filename in files){
      file <- read.csv(paste(directory,'/', filename, sep=''))
      #check if the observation post meets the threshold for NA
      incomplete <- is.na(file[['sulfate']]) | is.na(file[['nitrate']])
      nr_complete <- sum(!incomplete)
      if (nr_complete > threshold){
         #include observation post
         print(cor(file[['sulfate']], file[['nitrate']]))
      }
   }
}