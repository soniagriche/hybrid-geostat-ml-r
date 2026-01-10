# 21_fig21_sgs_approx.R
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

X   <- suppressWarnings(as.numeric(df_raw[[colX]]))
Y   <- suppressWarnings(as.numeric(df_raw[[colY]]))
PHI <- suppressWarnings(as.numeric(df_raw[[colPHI]]))

ok <- is.finite(X) & is.finite(Y) & is.finite(PHI)
spdf <- data.frame(X=X[ok], Y=Y[ok], PHI=PHI[ok])
stopifnot(nrow(spdf) >= 10)

if (median(spdf$PHI, na.rm=TRUE) > 1.5) spdf$PHI <- spdf$PHI/100
coordinates(spdf) <- ~X+Y

grd <- expand.grid(
  X = seq(min(spdf$X), max(spdf$X), length.out=120),
  Y = seq(min(spdf$Y), max(spdf$Y), length.out=120)
)
coordinates(grd) <- ~X+Y
gridded(grd) <- TRUE

maxdist <- spDists(coordinates(spdf), longlat=FALSE)
cutoff  <- 0.5 * max(maxdist, na.rm=TRUE)
width   <- cutoff/15

vg_phi <- variogram(PHI ~ 1, spdf, cutoff=cutoff, width=width, cressie=TRUE)

fit_safe <- function(vg){
  m <- suppressWarnings(fit.variogram(vg, vgm("Gau")))
  if(any(is.na(m$psill)) || any(m$psill < 0)) m <- suppressWarnings(fit.variogram(vg, vgm("Sph")))
  if(any(is.na(m$psill)) || any(m$psill < 0)) m <- suppressWarnings(fit.variogram(vg, vgm("Exp")))
  m$psill <- pmax(m$psill, 0)
  m
}
m_phi <- fit_safe(vg_phi)

set.seed(123)
phi_P10 <- krige(PHI ~ 1, spdf, grd, model=m_phi, nsim=1, nmax=10, maxdist=cutoff)

rng  <- range(phi_P10$sim1, na.rm=TRUE)
brks <- pretty(rng, n=8)
cols <- viridis(length(brks)-1, option="plasma")

f21 <- file.path(out_dir, "Fig21_PHI_OKP10_SGSapprox.png")
png(f21, 950, 650, res=150)
print(spplot(phi_P10["sim1"],
             main="Fig. 21 - PHI (OK P10, SGS-approx)",
             at=brks, col.regions=cols, colorkey=list(at=brks)))
dev.off()

cat("??? écrit:", f21, "\n")
