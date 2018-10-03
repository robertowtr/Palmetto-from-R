#Set working directory
rm(list=ls())
local_usr <- Sys.getenv("LOGNAME")
working_dir <- gsub(" ", "", paste("/home/", local_usr, "/Documents/DSProjects/Palmetto-from-R"))
setwd(working_dir)
rm(local_usr, working_dir)

output_file <- data.frame(x = character(), y = numeric())

file.names <- list.files(path = 'input-topics/')

for(file in file.names){
  command = paste("./palmetto.sh", "C_V", gsub(" ", "", paste("/home/roberto/Documents/DSProjects/Palmetto-from-R/input-topics/", file)))
  result = system(command,intern=TRUE)  
  
  sum <- 0
  for (line in (2:length(result))){
    x <- strsplit(result[line],"\t")  
    x <- as.numeric(x[[1]][2])
    sum <- sum + x
  }
  sum <- sum / (length(result) - 1)  
  output_file <- rbind(output_file, data.frame(x = file, y = sum))
}

write.table(output_file, file = "output.txt", quote = FALSE, col.names = TRUE, row.names = FALSE)
