setwd("/mnt/sky/Dropbox/WorkSpace/2025-07-02_fossilsforthewin/")

# attach necessary stuff
library(chronosphere)
library(divDyn)
library(rgplates)

# the bounds of the permian period

# the whole pbdb
dir.create("data", showWarnings=FALSE)
dir.create("data/chronosphere", showWarnings=FALSE)
dir.create("export", showWarnings=FALSE)

# load PBDB data
pbdb <- chronosphere::fetch("pbdb", datadir="data/chronosphere/", ver="20260419")

# An approximate interval for Glossopteris
# the permian early intervals
permian <- c("Roadian",
	"Kungurian",
	"Wuchiapingian",
	"Wordian"        ,
	"Changhsingian",
	"Permian",
	"Sakmarian",
	"Guadalupian",
	"Artinskian",
	"Asselian",
	"Lopingian",
	"Capitanian",
	"Cisuralian")

triassic <- c(
	"Early Triassic",
	"Ladinian",
	"Triassic",
	"Induan",
	"Olenekian",
	"Anisian",
	"Julian"

)

interest <- c(permian, triassic)

################################################################################
# Glossopteris

# The glossopteris subset
focal <- pbdb[which(pbdb$genus=="Glossopteris"),]

# the early intervals
unique(focal$early_interval)

focal <- focal[which(focal$early_interval%in%interest),]

focal$group <- "Glossopteris"

################################################################################
# the bounds of the interval where this thing is? Early Permian to Anisian
data(stages)
bounds<- range(stages[43:54,  c("top", "bottom")])
mid <- mean(bounds)

# Natural Earth Polygons
ne <- chronosphere::fetch("NaturalEarth", datadir="data/chronosphere/")
proj <- "ESRI:54030"

sf <- st_as_sf(focal, coords=c("lng", "lat"), crs="WGS84")
sfProj <- st_transform(sf, proj)

png(paste0("export/glossopteris_modern.png"), width=2200, height=1000, pointsize=20)
	plot(ne$geometry, col="gray", border=NA)
	points(focal[, c("lng", "lat")], pch=16, col="#dd2222bb")
dev.off()

png(paste0("export/glossopteris_modernProj.png"), width=2200, height=1000, pointsize=30)
	plot(st_transform(mapedge(), proj), col="white", border="black", lwd=2)
	plot(st_transform(ne, proj)$geometry, col="darkgray", border=NA, add=TRUE)
	plot(st_transform(sfProj)$geometry, pch=21, col="black", bg="#dd222266", add=TRUE)
	plot(st_transform(mapedge(), proj), col=NA, border="darkgray", lwd=2, add=TRUE)
dev.off()

################################################################################
# Paleo model
################################################################################

# model
MERDITH2021 <- chronosphere::fetch("GPlates",ser="MERDITH2021",  datadir="data/chronosphere/")

# the continents
conts <- reconstruct("continents", age=mid, model=MERDITH2021)
coords <- reconstruct(focal[, c("lng", "lat")], age=mid, model=MERDITH2021)

png(paste0("export/glossopteris_rec.png"), width=2200, height=1000, pointsize=20)
	plot(conts$geometry, col="gray", border=NA)
	points(coords, pch=21, col="black", bg="#dd222266")
dev.off()


################################################################################
mid <- 270
# Scotese model
PALEOMAP <- chronosphere::fetch("paleomap",ser="model",  datadir="data/chronosphere/")

# temperature reconstruction
gmst <- chronosphere::fetch("paleomap",ser="gmst",  datadir="data/chronosphere/")
one <- gmst[as.character(mid)]
crs(one) <- "WGS84"

# contein
contsPM <- reconstruct("static_polygons", age=mid, model=PALEOMAP)
coordsPM <- reconstruct(focal[, c("lng", "lat")], age=mid, model=PALEOMAP)
coordsNoNA <- na.omit(coordsPM)
pmSF<- st_as_sf(as.data.frame(coordsNoNA), coords=c("paleolong", "paleolat"), crs="WGS84")
meProj <- st_transform(mapedge(), proj)

png(paste0("export/glossopteris_rec270.png"), width=2200, height=1000, pointsize=30)
	plot(project(one, proj), axes=FALSE)
	plot(st_transform(contsPM, proj)$geometry, col="#66666666", border="#666666CC", add=TRUE)
	points(st_transform(pmSF, proj)$geometry, pch=21, col="black", bg="#dd222266")
	plot(meProj, col=NA, border="white", lwd=8, add=TRUE)
dev.off()
