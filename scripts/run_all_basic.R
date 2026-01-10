# run_all_basic.R (A1-A4 + 1-10 + 20-27)
rm(list=ls())
library(readxl); library(sp); library(gstat); library(viridisLite)

project_dir <- "C:/Users/pc/Documents/hybrid-geostat-ml-r"
data_file   <- file.path(project_dir, "data", "TPgroupe2.xls")
out_dir     <- file.path(project_dir, "outputs", "figures")
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
stopifnot(file.exists(data_file))

df_raw <- read_excel(data_file)
nms <- names(df_raw)

find_col <- function(patterns){
  for(p in patterns){
    hit <- grep(p, nms, ignore.case=TRUE, value=TRUE)
    if(length(hit)>0) return(hit[1])
  }
  stop("Colonne introuvable: ", paste(patterns, collapse=", "))
}

colX   <- find_col(c("^x$","easting","utm.*x","coord.*x"))
colY   <- find_col(c("^y$","northing","utm.*y","coord.*y"))
colPHI <- find_col(c("phie","phi","poro","poros"))
colK   <- find_col(c("^k","perm","perme"))

X   <- suppressWarnings(as.numeric(df_raw[[colX]]))
Y   <- suppressWarnings(as.numeric(df_raw[[colY]]))
PHI <- suppressWarnings(as.numeric(df_raw[[colPHI]]))
K1  <- pmax(suppressWarnings(as.numeric(df_raw[[colK]])), 1e-6)
logK <- log10(K1)

ok <- is.finite(X) & is.finite(Y) & is.finite(PHI) & is.finite(logK)
spdf <- data.frame(X=X[ok], Y=Y[ok], PHI=PHI[ok], logK=logK[ok])
stopifnot(nrow(spdf) >= 10)

if (median(spdf$PHI, na.rm=TRUE) > 1.5) spdf$PHI <- spdf$PHI/100
coordinates(spdf) <- ~X+Y

# grille
grd <- expand.grid(
  X = seq(min(spdf$X), max(spdf$X), length.out=120),
  Y = seq(min(spdf$Y), max(spdf$Y), length.out=120)
)
coordinates(grd) <- ~X+Y
gridded(grd) <- TRUE

# dist
maxdist <- spDists(coordinates(spdf), longlat=FALSE)
cutoff  <- 0.5 * max(maxdist, na.rm=TRUE)
width   <- cutoff/15

fit_safe <- function(vg){
  m <- suppressWarnings(fit.variogram(vg, vgm("Gau")))
  if(any(is.na(m$psill)) || any(m$psill < 0)) m <- suppressWarnings(fit.variogram(vg, vgm("Sph")))
  if(any(is.na(m$psill)) || any(m$psill < 0)) m <- suppressWarnings(fit.variogram(vg, vgm("Exp")))
  m$psill <- pmax(m$psill, 0)
  m
}

# ---- A3/A4 maps (points)
png(file.path(out_dir,"FigA3_phi_points.png"), 900, 650, res=150)
print(spplot(spdf["PHI"], main="Fig. A3 - PHI (points)", col.regions=viridis(100)))
dev.off()

png(file.path(out_dir,"FigA4_logK_points.png"), 900, 650, res=150)
print(spplot(spdf["logK"], main="Fig. A4 - log10(K) (points)", col.regions=viridis(100)))
dev.off()

# ---- Variograms + OK
vg_phi  <- variogram(PHI ~ 1,  spdf, cutoff=cutoff, width=width, cressie=TRUE)
vg_logK <- variogram(logK ~ 1, spdf, cutoff=cutoff, width=width, cressie=TRUE)
m_phi   <- fit_safe(vg_phi)
m_logK  <- fit_safe(vg_logK)

png(file.path(out_dir,"Fig1_PHI_omni.png"), 900, 650, res=150)
plot(vg_phi, main="Fig.1 - PHI variogram (robust)")
dev.off()

png(file.path(out_dir,"Fig5_logK_omni.png"), 900, 650, res=150)
plot(vg_logK, main="Fig.5 - log10(K) variogram (robust)")
dev.off()

kr_phi  <- krige(PHI ~ 1,  spdf, grd, model=m_phi)
kr_logK <- krige(logK ~ 1, spdf, grd, model=m_logK)

png(file.path(out_dir,"Fig4_PHI_OK_pred.png"), 900, 750, res=150)
print(spplot(kr_phi["var1.pred"], main="Fig.4 - PHI OK prediction", col.regions=viridis(100)))
dev.off()

png(file.path(out_dir,"Fig8_logK_OK_pred.png"), 900, 750, res=150)
print(spplot(kr_logK["var1.pred"], main="Fig.8 - log10(K) OK prediction", col.regions=viridis(100)))
dev.off()

# ---- 20-23 SGS-approx (P05/P10)
set.seed(123)
phi_P05  <- krige(PHI ~ 1,  spdf, grd, model=m_phi,  nsim=1, nmax=5,  maxdist=cutoff)
phi_P10  <- krige(PHI ~ 1,  spdf, grd, model=m_phi,  nsim=1, nmax=10, maxdist=cutoff)
logK_P05 <- krige(logK ~ 1, spdf, grd, model=m_logK, nsim=1, nmax=5,  maxdist=cutoff)
logK_P10 <- krige(logK ~ 1, spdf, grd, model=m_logK, nsim=1, nmax=10, maxdist=cutoff)

br_phi  <- pretty(range(c(phi_P05$sim1, phi_P10$sim1), na.rm=TRUE), n=8)
br_logK <- pretty(range(c(logK_P05$sim1, logK_P10$sim1), na.rm=TRUE), n=8)
cols_phi  <- viridis(length(br_phi)-1, option="plasma")
cols_logK <- viridis(length(br_logK)-1, option="plasma")

png(file.path(out_dir,"Fig20_PHI_OKP05.png"), 950, 650, res=150)
print(spplot(phi_P05["sim1"], main="Fig.20 - PHI (OK P05, SGS-approx)",
             at=br_phi, col.regions=cols_phi, colorkey=list(at=br_phi)))
dev.off()

png(file.path(out_dir,"Fig21_PHI_OKP10.png"), 950, 650, res=150)
print(spplot(phi_P10["sim1"], main="Fig.21 - PHI (OK P10, SGS-approx)",
             at=br_phi, col.regions=cols_phi, colorkey=list(at=br_phi)))
dev.off()

png(file.path(out_dir,"Fig22_logK_OKP05.png"), 950, 650, res=150)
print(spplot(logK_P05["sim1"], main="Fig.22 - log10(K) (OK P05, SGS-approx)",
             at=br_logK, col.regions=cols_logK, colorkey=list(at=br_logK)))
dev.off()

png(file.path(out_dir,"Fig23_logK_OKP10.png"), 950, 650, res=150)
print(spplot(logK_P10["sim1"], main="Fig.23 - log10(K) (OK P10, SGS-approx)",
             at=br_logK, col.regions=cols_logK, colorkey=list(at=br_logK)))
dev.off()

# ---- 24-25 variances
png(file.path(out_dir,"Fig24_PHI_variance.png"), 900, 700, res=150)
print(spplot(kr_phi["var1.var"], main="Fig.24 - PHI OK variance", col.regions=viridis(100)))
dev.off()

png(file.path(out_dir,"Fig25_logK_variance.png"), 900, 700, res=150)
print(spplot(kr_logK["var1.var"], main="Fig.25 - log10(K) OK variance", col.regions=viridis(100)))
dev.off()

# ---- 26 scatter
png(file.path(out_dir,"Fig26_scatter_PHI_logK.png"), 900, 700, res=150)
plot(spdf$PHI, spdf$logK, pch=19,
     main="Fig.26 - PHI vs log10(K)", xlab="PHI", ylab="log10(K)")
grid()
dev.off()

# ---- 27 maps
png(file.path(out_dir,"Fig27_maps_PHI_logK_OK.png"), 1400, 700, res=150)
par(mfrow=c(1,2), mar=c(3,3,2,1))
print(spplot(kr_phi["var1.pred"], main="PHI OK pred", col.regions=viridis(100)))
print(spplot(kr_logK["var1.pred"], main="logK OK pred", col.regions=viridis(100)))
dev.off()

cat("\n??? Terminé. Figures dans:", out_dir, "\n")
