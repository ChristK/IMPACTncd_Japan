library(arrow)
library(data.table)
library(fs) # for path_rel()

# Define directories
dir1 <- "/mnt/storage_fast4/jpn/outputs/summaries_noscenariobatching/"
dir2 <- "/mnt/storage_fast4/jpn/outputs/summaries/"

# Get relative file paths from dir1
files1 <- list.files(
  dir1,
  pattern = "\\.parquet$",
  full.names = TRUE,
  recursive = TRUE
)
rel_paths <- path_rel(files1, start = dir1)

# Check existence and compare content
for (rel_path in rel_paths) {
  file1 <- file.path(dir1, rel_path)
  file2 <- file.path(dir2, rel_path)

  if (!file.exists(file2)) {
    stop(sprintf("File missing in dir2: %s", rel_path))
  }

  t1 <- as.data.table(read_parquet(file1))
  t2 <- as.data.table(read_parquet(file2))

  if (!all.equal(t1, t2)) {
    message(sprintf("Mismatch found: %s", rel_path))
    break
  }
}

if (exists("t1") && exists("t2") && !all.equal(t1, t2)) {
  message("You can now inspect t1 and t2 for differences.")
} else {
  message("All files are identical.")
}

all.equal(t1, t2)


# t1 <- as.data.table(open_dataset("/mnt/storage_fast4/jpn/outputs/summaries/dis_characteristics_esp/5_dis_characteristics_esp.parquet"))
# t2 <- as.data.table(open_dataset("/mnt/storage_fast4/jpn/outputs/summaries_noscenariobatching/dis_characteristics_esp/"))[mc == 5]
# t2 <- t2[complete.cases(t2), ]
# all.equal(t1$mean_age_incd_htn, t2$mean_age_incd_htn)

