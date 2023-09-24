rm(list = ls())
file <- list.files("SEV01_RawData")
file <- file[grep("SEV", file, fixed = TRUE)] 
for (f in file) {
  print(f)
  SEV01Data <- read.csv(paste("./SEV01_RawData/", f, sep=""), header=TRUE, sep=",", stringsAsFactors = F, encoding = "UTF-8") 
  if (exists("df.M")) {
    df.M <- rbind(df.M, SEV01Data)
  } else {
    df.M <- SEV01Data
  }
}
write.csv(df.M, file = "SEV01_original.csv", row.names = F)

file <- list.files("SEV02_RawData")
file <- file[grep("SEV", file, fixed = TRUE)] 
for (f in file) {
  print(f)
  SEV02Data <- read.csv(paste("./SEV02_RawData/", f, sep=""), header=TRUE, sep=",", stringsAsFactors = F, encoding = "UTF-8") 
  if (exists("df.M")) {
    df.M <- rbind(df.M, SEV02Data)
  } else {
    df.M <- SEV02Data
  }
}
write.csv(df.M, file = "SEV02_original.csv", row.names = F)