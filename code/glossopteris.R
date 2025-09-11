# set working directory
setwd("/mnt/sky/Dropbox/WorkSpace/2025-07-02_fossilsforthewin/")

# attach necessary stuff
library(chronosphere)
library(rgplates)
library(icosa)
library(divDyn)


# Glossopteris
# Lystrosaurus
# Cynognathus
# Mesosaurus

# the bounds of the permian period
data(stages)
permBounds<- range(stages[stages$sys=="P", c("top", "bottom")])
permMid <- mean(permBounds)


# the whole pbdb
pbdb <- chronosphere::fetch("pbdb", datadir="data/chronosphere/", ver="20250630")

# Natural Earth Polygons
ne <- chronosphere::fetch("NaturalEarth", datadir="data/chronosphere/")

# The glossopteris subset
gloss <- pbdb[which(pbdb$genus=="Glossopteris"),]

# the early intervals
unique(gloss$early_interval)

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

permGloss <- gloss[which(gloss$early_interval%in%permian),]


# present-day plotting of occurrenes
plot(ne$geometry, col="gray", border=NA)
points(permGloss[, c("lng", "lat")], pch=16, col="#dd2222bb")

hex<- hexagrid(deg=10, sf=TRUE)

# the modern cell
permGloss$modCell <- locate(hex,permGloss[, c("lng", "lat")])

tMod <- table(permGloss$modCell)


dir.create("export", showWarnings=FALSE)
png("export/modern_glossopteris.png", width=2200, height=1000, pointsize=20)
	plot(hex, tMod, reset=FALSE, border="gray80")
	plot(ne$geometry, col="gray", border=NA, add=TRUE)
	plot(hex, tMod, add=TRUE, border="gray80")
	points(permGloss[, c("lng", "lat")], pch=16, col="#dd2222bb")
dev.off()

# occupancy
length(tMod)


# model
MERDITH2021 <- chronosphere::fetch("GPlates",ser="MERDITH2021",  datadir="data/chronosphere/")

# reconstruct
pCoords<- reconstruct(permGloss[, c("lng","lat")], age=permMid, model=MERDITH2021)
colnames(pCoords)  <- c("plong", "plat")
# the continental plates

conts <- reconstruct("continents", age=permMid, model=MERDITH2021)
plot(conts$geometry, col="gray", border=NA)
points(pCoords[, c("plong", "plat")], pch=16, col="#dd2222bb")


# the paleo occupancy
permGloss$paleoCell <- locate(hex,pCoords)
tPaleo <- table(permGloss$paleoCell)


dir.create("export", showWarnings=FALSE)
png("export/paleo_glossopteris.png", width=2200, height=1000, pointsize=20)
	plot(hex, tPaleo, reset=FALSE, border="gray80")
	plot(conts$geometry, col="gray", border=NA,add=TRUE)
	plot(hex, tPaleo, add=TRUE, border="gray80")
	points(pCoords, pch=16, col="#dd2222bb")
dev.off()




# Genus - period combination compare
# Distribution or range of occurrences in the recontstructed world
# COmpared with present-day coordinates
# Expectation: increase in the spread of occurrenec - tectonics are shuffling occurrences
#
#
