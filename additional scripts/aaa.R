
CAM_nodes <- CAMfiles[[1]]
CAM_nodes$text_summarized <- CAM_nodes$text
CAM_nodes$text_summarized_suffremoved <- str_remove(string = CAM_nodes$text_summarized, pattern = "_neutral|_positive|_negative|_ambivalent")


tmp <- CAM_nodes
matout <- tmp %>%
  group_by(text_summarized_suffremoved) %>%
  summarise(N = n(), M = mean(value), SD = sd(value),
            Content = NA)

matout$M <- round(x = matout$M, digits = 2)
matout$SD <- round(x = matout$SD, digits = 2)
colnames(matout)[1] <- "summarized concepts"
matout



#> number of rows
length(unique(CAM_nodes$text_summarized_suffremoved)) * 3 + 2 # N, mean, SD
#> number of columns
length(unique(CAM_nodes$CAM))

## ID vars
hc_dat <- data.frame(CAM = unique(CAM_nodes$CAM), participantCAM = unique(CAM_nodes$participantCAM))
## word vars
for(w in unique(CAM_nodes$text_summarized_suffremoved)){
  varName_w <- str_remove_all(string = str_to_title(string = w, locale = "en"), pattern = " |\\W+")

  hc_dat[[paste0("N_", varName_w)]] <- NA
  hc_dat[[paste0("mean_", varName_w)]] <- NA
  hc_dat[[paste0("SD_", varName_w)]] <- NA
}

dim(hc_dat)


# c <- "01ec8ab0-7648-4c48-96f9-b27cfd90c8fc"
# w <- "acceptability of SAI"
verbose = FALSE


for(c in unique(CAM_nodes$CAM)){
  if(verbose){
    cat("considered CAM: ", c, "\n")
  }
  tmp_CAM_nodes <- CAM_nodes[CAM_nodes$CAM == c, ]

  for(w in unique(CAM_nodes$text_summarized_suffremoved)){
    if(verbose){
      cat("considered concept: ", w, "\n")
    }

    varName_w <- str_remove_all(string = str_to_title(string = w, locale = "en"), pattern = " |\\W+")
    if(verbose){
      cat("   > the freqeuncy, mean, SD are saved with the prefix N_, mean_, SD_ plus
      word without white spaces: ", varName_w, "\n")
    }

    if(sum(tmp_CAM_nodes$text_summarized_suffremoved == w) > 0){
      tmp_CAM_nodes_w <- tmp_CAM_nodes[tmp_CAM_nodes$text_summarized_suffremoved == w, ]

      ## add N
      hc_dat[hc_dat$CAM == c, paste0("N_", varName_w)] <- nrow(tmp_CAM_nodes_w)
      ## add mean
      hc_dat[hc_dat$CAM == c, paste0("mean_", varName_w)] <- mean(x = tmp_CAM_nodes_w$value)
      ## add SD, only if > 1
      hc_dat[hc_dat$CAM == c, paste0("SD_", varName_w)] <- sd(x = tmp_CAM_nodes_w$value)
    }
  }
  if(verbose){
    cat("\n")
  }
}






### data set
hc_df <- hc_dat[, str_subset(string = colnames(hc_dat), pattern = "mean_")]
hc_df_scaled <- scale(hc_df)

### run Hierarchical Clustering
dist.eucl <- dist(hc_df_scaled, method = "euclidean")
round(as.matrix(dist.eucl)[1:3, 1:3], 1)

hc_cluster <- hclust(dist.eucl, method = "ward.D2") # Ward's method
plot(hc_cluster)

# cutting at height
cutHeight <- 10
abline(h = cutHeight, col = "red", lty = 2)
rect.hclust(hc_cluster, h=cutHeight, border = "tomato")

### get average values to interpret cluster
cluster <-cutree(hc_cluster, h=cutHeight)

aggregate(hc_df, by=list(cluster=cluster), mean, na.rm = TRUE)
# aggregate(hc_df_scaled, by=list(cluster=cluster), mean, na.rm = TRUE)



### how many cluster?
dist.eucl_matrix = as.matrix(dist(hc_df, method = "euclidean"))
## Silhouette
plot(2:15, sapply(2:15, function(i) {
  mean(cluster::silhouette(cutree(hc_cluster, i), dmatrix=dist.eucl_matrix)[,"sil_width"]) }),
  xlab="Number of clusters", ylab="Average Silhouette", type="b", pch=20)
