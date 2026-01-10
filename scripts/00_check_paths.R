# 00_check_paths.R
project_dir <- "C:/Users/pc/Documents/hybrid-geostat-ml-r"
data_dir    <- file.path(project_dir, "data")
out_fig_dir <- file.path(project_dir, "outputs", "figures")
script_dir  <- file.path(project_dir, "scripts")

cat("project_dir:", project_dir, "\n")
cat("data_dir   :", data_dir, "\n")
cat("scripts    :", script_dir, "\n")
cat("out_fig    :", out_fig_dir, "\n\n")

dir.create(data_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(out_fig_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(script_dir, recursive=TRUE, showWarnings=FALSE)

cat("Fichiers dans data/:\n")
print(list.files(data_dir, full.names=TRUE))

data_file <- file.path(data_dir, "TPgroupe2.xls")
cat("\nTPgroupe2.xls existe ? -> ", file.exists(data_file), "\n")
