### Some manual cleaning, resulting from experimenting with the data vizzing!

EdgelistClean1 <- gsub("nwobhm", "new wave of british heavy metal", as.matrix(EdgelistClean))
EdgelistClean1 <- gsub("garage punk fusion genre", "garage punk", as.matrix(EdgelistClean))
EdgelistClean1 <- gsub("pub rock united kingdom", "pub rock", as.matrix(EdgelistClean))

EdgelistClean1[EdgelistClean1[, "To"] == "punk rock",]

Punk1 <- EdgelistClean1[EdgelistClean1[, "From"] == "punk rock",]
Punk2 <- EdgelistClean1[EdgelistClean1[, "To"] == "punk rock",]
punkbind <- rbind(Punk1, Punk2)
punkbind

HH1 <- EdgelistClean1[EdgelistClean1[, "From"] == "hip hop",]
HH2 <- EdgelistClean1[EdgelistClean1[, "To"] == "hip hop",]
hhbind <- rbind(HH1, HH2)

hhpunkbind <- rbind(hhbind, punkbind)

EdgelistClean1[EdgelistClean1[, "To"] == "pub rock united kingdom",]
