library(fst)
library(data.table)

# Identify files larger than 50MB in the current directory and its subdirectories
fl <- list.files(".", full.names = TRUE, recursive = TRUE)
fl <- sort(fl[file.size(fl)/(1024^2) >= 50])
if (file.exists("./simulation/large_files_indx.csv")) {
    fl <- sort(unique(c(fread("./simulation/large_files_indx.csv")$pths, fl)))
}
fwrite(list(pths = fl), "./simulation/large_files_indx.csv")

# add large files to .gitignore
excl <- readLines("./.gitignore")
for (i in 1:length(fl)) {
  file <- gsub("^./", "", fl[i])
  if (file %in% excl) next
  write(file, file="./.gitignore", append = TRUE)
}

# split the files into 50MB chunks
for (i in 1:length(fl)) {
  file <- fl[i]
    # split the file into 49MB chunks
    if (.Platform$OS.type == "unix") {
      system(paste0("split -b 49m ", file, " ", file, ".chunk"))
    } else if (.Platform$OS.type == "windows") {
      # For windows split and cat are from https://unxutils.sourceforge.net/
      shell(paste0("split -b 49m ", file, " ", file, ".chunk"))
    } else stop("Operating system is not supported.")
    # remove the original file
    file.remove(file)
}

# recombine the chunks of large files
if (file.exists("./simulation/large_files_indx.csv")) {
    fl <- fread("./simulation/large_files_indx.csv")$pths
    for (i in 1:length(fl)) {
        if (file.exists(fl[i])) next
        file <- fl[i]
        # recombine the chunks
        if (.Platform$OS.type == "unix") {
            system(paste0("cat ", file, ".chunk?? > ", file, ""))
        } else if (.Platform$OS.type == "windows") {
            # For windows split and cat are from https://unxutils.sourceforge.net/
            shell(paste0("cat ", file, ".chunk?? > ", file, ""))
        } else {
            stop("Operating system is not supported.")
        }
    }
}






