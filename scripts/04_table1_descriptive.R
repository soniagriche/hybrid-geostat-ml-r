rm(list=ls())

library(readxl)
library(dplyr)

# 1) Projet
project_dir <- "C:/Users/pc/Documents/hybrid-geostat-ml-r"
setwd(project_dir)

file_path <- "data/TPgroupe2.xls"
if(!file.exists(file_path)) file_path <- "data/TPgroupe2.xlsx"
if(!file.exists(file_path)) stop("??? Mets TPgroupe2.xls dans: ", file.path(project_dir,"data"))

dir.create("outputs/tables", recursive=TRUE, showWarnings=FALSE)

# 2) Lecture
df_raw <- read_excel(file_path, .name_repair="unique_quiet")
names(df_raw) <- make.unique(names(df_raw))

cat("??? Fichier lu :", normalizePath(file_path, winslash="/"), "\n\n")
cat("???? Colonnes disponibles:\n")
print(names(df_raw))

# 3) Convertir en numérique (même si virgule)
to_num <- function(x){
  if(is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

df_num <- df_raw %>% mutate(across(everything(), to_num))

# 4) Garder seulement les colonnes vraiment numériques (>= 5 valeurs non-NA)
keep <- sapply(df_num, function(v) sum(is.finite(v)) >= 5)
df_num2 <- df_num[, keep, drop=FALSE]

if(ncol(df_num2)==0) stop("??? Aucune colonne numérique détectée (ou trop de NA).")

# 5) Fonction stats
desc <- function(v){
  v <- v[is.finite(v)]
  c(
    N      = length(v),
    Mean   = mean(v),
    SD     = sd(v),
    Min    = min(v),
    Q25    = as.numeric(quantile(v, 0.25, names=FALSE)),
    Median = as.numeric(quantile(v, 0.50, names=FALSE)),
    Q75    = as.numeric(quantile(v, 0.75, names=FALSE)),
    Max    = max(v)
  )
}

tab <- do.call(rbind, lapply(df_num2, desc)) %>% as.data.frame()
tab$Variable <- rownames(tab); rownames(tab) <- NULL
tab <- tab[, c("Variable","N","Mean","SD","Min","Q25","Median","Q75","Max")]
tab[,2:9] <- lapply(tab[,2:9], \(x) round(x, 3))

# 6) Sauvegarde
out_csv <- "outputs/tables/Descriptive_AllNumeric.csv"
write.csv(tab, out_csv, row.names=FALSE)

cat("\n??? Table descriptive créée : ", normalizePath(out_csv, winslash="/"), "\n", sep="")
print(head(tab, 15))

shell.exec(normalizePath("outputs/tables"))
