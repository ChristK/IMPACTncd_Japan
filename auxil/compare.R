library(arrow)
library(data.table)
lsf <- sort(list.files(
    "/mnt/storage_fast4/IMPACTncd_Japan/outputs/tables",
    full.names = TRUE
))
lsf2 <- sort(list.files(
    "/home/ckyprid/My_Models/IMPACTncd_Japan-export_csv.gz_outputs/output_for_val/tables",
    full.names = TRUE
))

for (i in 1:length(lsf)) {
    if (basename(lsf[i]) != basename(lsf2[i])) {
        stop(
            "File names do not match: ",
            basename(lsf[i]),
            " vs ",
            basename(lsf2[i])
        )
    }
    x <- fread(lsf[[i]])
    y <- fread(lsf2[[i]])
    k <- sort(names(x))
    k <- grep("%", k, value = TRUE, invert = TRUE)
    setkeyv(x, k)
    setkeyv(y, k)
  if ("disease" %in% names(x) && "disease" %in% names(y)) {
    x <- x[!grepl("^cmsmm", disease)]
    y <- y[!grepl("^cmsmm", disease)] 
  }

    E <- all.equal(x, y)
    if (!identical(TRUE, E)) {
      warning(i, "\n", "Files do not match: ", lsf[i], " vs ", lsf2[i], "\n", E)
    }
}
warnings()

i <- 45 # 45:50, 71:76, 89:94
x <- fread(lsf[[i]])
y <- fread(lsf2[[i]])
k <- sort(names(x))
k <- grep("%", k, value = TRUE, invert = TRUE)
setkeyv(x, k)
setkeyv(y, k)
if (nrow(x) != nrow(y)) {
  if (ncol(x) != ncol(y)) {
    stop("Number of rows and columns do not match: ", nrow(x), " vs ", nrow(y), " and ", ncol(x), " vs ", ncol(y))
  } else {
    for (j in seq_len(ncol(x))) {
      ux <- sort(unique(x[[j]]))
      uy <- sort(unique(y[[j]]))
      if (length(ux) / length(x[[j]]) > 0.5) next
      res <- setdiff(union(ux, uy), intersect(ux, uy))
      if (length(res) > 0) {
        warning(
          "Column name: ",
          names(ux)[j],
          " does not match: ",
          paste(res, colapse = ", "),
          "\n",
          "File: ",
          lsf[i]
        )
      }
    } 
  }
} else { # if number of rows match
    for (ii in seq_len(nrow(x))) {
      E <- all.equal(x[ii, ], y[ii, ])
      if (!identical(TRUE, E)) {
        warning(
          "Row ", ii, " does not match: ", "\n",
          rbind(x[ii, ], y[ii, ]), "\n",
          lsf[i], "\n", E
        )
      }
    }

}
ii <- 23045
rbind(x[ii, ], y[ii, ])
View(x)
View(y)




fpth <- "/mnt/storage_fast4/IMPACTncd_Japan/outputs/summaries/costs_scaled_up"
tt <- as.data.table(open_dataset(fpth))
j
what = "dis_mrtl"
