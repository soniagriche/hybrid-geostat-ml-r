# ============================================================
# 05_table2_performance_cv.R
# Table 2 - CV performance: OK vs ML vs Hybrid (PHIE1)
# Outputs: outputs/tables/Table2_performance_CV_PHIE1.csv
# ============================================================

rm(list=ls())

pkgs <- c("readxl","dplyr","sp","gstat","caret")
for(p in pkgs){
  if(!requireNamespace(p, quietly=TRUE)) install.packages(p)
  library(p, character.only=TRUE)
}

project_dir <- "C:/Users/pc/Documents/hybrid-geostat-ml-r"
setwd(project_dir)
dir.create("outputs/tables", recursive=TRUE, showWarnings=FALSE)

# ---- Read data
file_path <- "data/TPgroupe2.xls"
if(!file.exists(file_path)) file_path <- "data/TPgroupe2.xlsx"
if(!file.exists(file_path)) stop("??? TPgroupe2 introuvable dans data/")

df_raw <- readxl::read_excel(file_path, .name_repair="unique_quiet")
names(df_raw) <- make.unique(names(df_raw))
nms <- names(df_raw)

as_num <- function(x){
  if(is.numeric(x)) return(x)
  x <- gsub(",", ".", as.character(x))
  suppressWarnings(as.numeric(x))
}
find_col <- function(patterns){
  for(p in patterns){
    hit <- grep(p, nms, ignore.case=TRUE, value=TRUE)
    if(length(hit)>0) return(hit[1])
  }
  stop("Colonne introuvable: ", paste(patterns, collapse=" | "))
}

# ---- Detect columns
colX   <- find_col(c("^x$","utm.*x","easting","coord.*x"))
colY   <- find_col(c("^y$","utm.*y","northing","coord.*y"))
colPHI <- find_col(c("phie1","phi1","phie","phi","poro","poros"))

df <- data.frame(
  X = as_num(df_raw[[colX]]),
  Y = as_num(df_raw[[colY]]),
  PHIE1 = as_num(df_raw[[colPHI]])
)
df <- df[complete.cases(df), ]
stopifnot(nrow(df) >= 20)

# ---- Metrics
metrics <- function(y, yhat){
  rmse <- sqrt(mean((y-yhat)^2))
  mae  <- mean(abs(y-yhat))
  r2   <- 1 - sum((y-yhat)^2)/sum((y-mean(y))^2)
  c(RMSE=rmse, MAE=mae, R2=r2)
}

# ---- 5-fold CV
set.seed(123)
folds <- caret::createFolds(df$PHIE1, k=5)

res_list <- list()

for(i in seq_along(folds)){
  test_idx <- folds[[i]]
  train_df <- df[-test_idx, ]
  test_df  <- df[test_idx, ]
  
  # spatial objects for OK
  train_sp <- train_df
  test_sp  <- test_df
  sp::coordinates(train_sp) <- ~X+Y
  sp::coordinates(test_sp)  <- ~X+Y
  
  # (A) OK: variogram + fit + krige to test points
  vg <- gstat::variogram(PHIE1 ~ 1, data=train_sp, cressie=TRUE)
  vgm_fit <- gstat::fit.variogram(vg, gstat::vgm(model="Exp"))
  ok_test <- gstat::krige(PHIE1 ~ 1, train_sp, test_sp, model=vgm_fit)
  yhat_ok <- ok_test$var1.pred
  
  # (B) ML baseline (RF) using X,Y only
  tr <- as.data.frame(train_df)
  te <- as.data.frame(test_df)
  rf <- caret::train(PHIE1 ~ X+Y, data=tr, method="rf",
                     trControl=caret::trainControl(method="none"))
  yhat_ml <- predict(rf, newdata=te)
  
  # (C) Hybrid: ML with OK predictions as additional feature
  # OK prediction on train points (as feature)
  ok_train <- gstat::krige(PHIE1 ~ 1, train_sp, train_sp, model=vgm_fit)
  tr$OKpred <- ok_train$var1.pred
  te$OKpred <- yhat_ok
  
  rf2 <- caret::train(PHIE1 ~ X+Y+OKpred, data=tr, method="rf",
                      trControl=caret::trainControl(method="none"))
  yhat_hyb <- predict(rf2, newdata=te)
  
  res_list[[i]] <- rbind(
    OK     = metrics(te$PHIE1, yhat_ok),
    ML     = metrics(te$PHIE1, yhat_ml),
    Hybrid = metrics(te$PHIE1, yhat_hyb)
  )
}

tab2 <- Reduce("+", res_list) / length(res_list)
tab2 <- as.data.frame(tab2)
tab2$Method <- rownames(tab2); rownames(tab2) <- NULL
tab2 <- tab2[, c("Method","RMSE","MAE","R2")]
tab2[,2:4] <- lapply(tab2[,2:4], \(x) round(x, 4))

out <- "outputs/tables/Table2_performance_CV_PHIE1.csv"
write.csv(tab2, out, row.names=FALSE)

cat("??? Table 2 saved:", normalizePath(out, winslash="/"), "\n")
print(tab2)
