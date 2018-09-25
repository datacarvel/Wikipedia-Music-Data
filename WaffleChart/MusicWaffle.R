# Now let's try with a totally different display: waffles!
  # My data as communities is stored as the variable "ok" - obviously lacked imagination a few days a go
  # Made sure ggplot2, waffle, mosaic, dplyr are loaded

  # Now a basic example, simply taken from here: https://github.com/hrbrmstr/waffle

parts <- c(80, 30, 20, 10)
waffle(parts, rows = 8)

  # Trying with my data now

ok <- cluster_louvain(EdgelistClean1_igraph)
clusters_list <- ok[1:27]

names(clusters_list) <- c(paste("Group_", letters[1:27], sep = ""))
clusters_mtx <- as.matrix(unlist(clusters_list, recursive = TRUE, use.names = TRUE))
clusters_mtx2 <- cbind(rownames(clusters_mtx), clusters_mtx)
GroupLetter <- gsub('[[:digit:]]+', '', clusters_mtx2[,1])
clusters_mtx3 <- cbind(GroupLetter, clusters_mtx2)
rownames(clusters_mtx3) <- NULL

clusters_df$GroupLetter <- as.data.frame(clusters_mtx3, row.names = NULL)

col_table <- table(col)
col_factor <- factor(col)
col_sorted <- sort(col_table, decreasing = TRUE)
col_sorted27 <- col_sorted[1:28]
col_sorted27_f <- factor(names(col_sorted27))
colours_vector <- names(col_sorted27) 

# For some reason, the colours of the communities all have eight characters, and only one or two is showing up when trying to do my waffle, so having tested the colours in another software, it seems the colours are right, but have all 2 extra characters, so I just remove the last two characters with the following (stringr package):

colours_vector <- str_trunc(colours_vector, width = 7, ellipsis = "")

clusters_df2 <- transform(clusters_df, freq= ave(seq(nrow(clusters_df)), GroupLetter, FUN=length))
clusters_df3 <- clusters_df2[order(-clusters_df2$freq), ]

parts <- c(sort(table(clusters_df3$GroupLetter), decreasing = TRUE))
waffle(parts, rows = 35, keep = TRUE) + scale_fill_manual(values = colours_vector)

# The Waffle package does not permit to use more than one specific glyph, it's only one for all your data, so I exported my waffle and replaced the squares by icons in Illustrator
