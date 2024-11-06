

centralConcepts <- c("Religion", "Atheismus")


paste0("\nCAMs sliced datasets: .txt files (nodes, connectors, merged) created for central concept: ", centralConcepts[1])

slicedCAMs_combined <- sliceAllCAMs_combined(CAMfilesList = CAMfiles,
                                             drawnCAMs = CAMdrawn,
                                             connectionToRemove = centralConcepts,
                                             nodeToRemove = NULL,
                                             centralConceptsSubgraphs = centralConcepts,
                                             plot = FALSE)


length(unique(slicedCAMs_combined[[1]]$CAM))
length(unique(slicedCAMs_combined[[3]]$CAM.x))


tmp_nodes <- slicedCAMs_combined[[1]][slicedCAMs_combined[[1]]$CAM %in% unique(slicedCAMs_combined[[1]]$CAM)[1],]
tmp_merged <- slicedCAMs_combined[[3]][slicedCAMs_combined[[3]]$CAM.x %in% unique(slicedCAMs_combined[[3]]$CAM.x)[1],]





tmp_c12 <- draw_CAM(dat_merged = tmp_merged,
                    dat_nodes = tmp_nodes, ids_CAMs = "all",
                    plot_CAM = FALSE,
                    useCoordinates = TRUE,
                    relvertexsize = 3,
                    reledgesize = 1)
plot(tmp_c12[[1]])

tmp_merged$id %in% tmp_nodes$id
tmp_merged$idending[!tmp_merged$idending %in% tmp_nodes$id]

tmp_connectors <- slicedCAMs_combined[[2]][slicedCAMs_combined[[2]]$CAM %in% unique(slicedCAMs_combined[[2]]$CAM)[1],]
tmp_connectors$daughterID %in% tmp_nodes$id
# tmp_connectors$idending[!tmp_connectors$idending %in% tmp_nodes$id]



CAMdrawn_sliced <- draw_CAM(dat_merged = slicedCAMs_combined[[3]],
                     dat_nodes = slicedCAMs_combined[[1]],ids_CAMs = "all",
                     plot_CAM = TRUE,
                     useCoordinates = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

plot(CAMdrawn_sliced[[1]])


CAMfiles[[1]]$text[CAMfiles[[1]]$id %in% tmp_merged$idending[!tmp_merged$idending %in% tmp_nodes$id]]


slicedCAMs_seperated <- sliceAllCAMs_seperated(slicedCAMs = slicedCAMs_combined,
                                               centralConceptsSubgraphs = centralConcepts,
                                               plot = FALSE)
names(slicedCAMs_seperated)

dat_merged = slicedCAMs_seperated[[3]]
dat_nodes = slicedCAMs_seperated[[1]]

a <- dat_nodes[dat_nodes$CAM == dat_nodes$CAM[1],]
b <- dat_merged[dat_merged$CAM.x == dat_nodes$CAM[1],]


b$id %in% a$id



nrow(dat_merged)
nrow(dat_nodes)


CAMdrawn_c1 <- draw_CAM(dat_merged = slicedCAMs_seperated[[3]],
                        dat_nodes = slicedCAMs_seperated[[1]], ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)

CAMdrawn_c2 <- draw_CAM(dat_merged = slicedCAMs_seperated[[6]],
                        dat_nodes = slicedCAMs_seperated[[4]], ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)


tmp_merged <- slicedCAMs_combined[[3]][slicedCAMs_combined[[3]]$CAM.x %in% unique(slicedCAMs_combined[[3]]$CAM.x), ]
tmp_nodes <- slicedCAMs_combined[[1]][slicedCAMs_combined[[1]]$CAM %in% unique(slicedCAMs_combined[[1]]$CAM), ]


tmp_c12 <- draw_CAM(dat_merged = tmp_merged,
                    dat_nodes = tmp_nodes, ids_CAMs = "all",
                    plot_CAM = FALSE,
                    useCoordinates = TRUE,
                    relvertexsize = 3,
                    reledgesize = 1)

j <- 3
plot(tmp_c12[[j]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn[[names(tmp_c12)[j]]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)

names(CAMdrawn_c12)




CAMdrawn_c12 <- draw_CAM(dat_merged = slicedCAMs_combined[[3]],
                         dat_nodes = slicedCAMs_combined[[1]], ids_CAMs = "all",
                         plot_CAM = TRUE,
                         useCoordinates = TRUE,
                         relvertexsize = 3,
                         reledgesize = 1)




tmp_merged <- slicedCAMs_seperated[[3]][slicedCAMs_seperated[[3]]$CAM.x %in% unique(slicedCAMs_seperated[[3]]$CAM.x)[1], ]
tmp_nodes <- slicedCAMs_seperated[[1]][slicedCAMs_seperated[[1]]$CAM %in% unique(slicedCAMs_seperated[[1]]$CAM)[1], ]



tmp_merged$id %in% tmp_nodes$id
tmp_merged$idending[!tmp_merged$idending %in% tmp_nodes$id]

tmp_connectors <- slicedCAMs_combined[[2]][slicedCAMs_combined[[2]]$CAM %in% unique(slicedCAMs_combined[[2]]$CAM)[1],]
tmp_connectors$daughterID %in% tmp_nodes$id
tmp_connectors$idending[!tmp_connectors$idending %in% tmp_nodes$id]


CAMdrawn_c1_1 <- draw_CAM(dat_merged = tmp_merged,
                          dat_nodes = tmp_nodes, ids_CAMs = "all",
                          plot_CAM = FALSE,
                          useCoordinates = TRUE,
                          relvertexsize = 3,
                          reledgesize = 1)



CAMdrawn_c1 <- draw_CAM(dat_merged = slicedCAMs_seperated[[3]],
                        dat_nodes = slicedCAMs_seperated[[1]], ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)

CAMdrawn_c2 <- draw_CAM(dat_merged = slicedCAMs_seperated[[6]],
                        dat_nodes = slicedCAMs_seperated[[4]], ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)



plot(CAMdrawn[[names(CAMdrawn_c1)[2]]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn_c12[[2]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn_c1[[2]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn_c2[[2]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)


###########
tmp_microIndicators_c1 <- compute_indicatorsCAM(drawn_CAM = CAMdrawn_c1,
                                                micro_degree = NULL,
                                                micro_valence = NULL,
                                                micro_centr_clo = NULL,
                                                micro_transitivity = NULL,
                                                largestClique = FALSE)


tmp_microIndicators_c2 <- compute_indicatorsCAM(drawn_CAM = CAMdrawn_c2,
                                                micro_degree = NULL,
                                                micro_valence = NULL,
                                                micro_centr_clo = NULL,
                                                micro_transitivity = NULL,
                                                largestClique = FALSE)


getDescriptives(dataset = tmp_microIndicators_c1, nameAPAtable = NULL)
getDescriptives(dataset = tmp_microIndicators_c2, nameAPAtable = NULL)



tmp_microIndicators_c12 <- rbind(tmp_microIndicators_c1, tmp_microIndicators_c2)
tmp_microIndicators_c12$group <- rep(centralConcepts, each = nrow(tmp_microIndicators_c1))

p_meanValence <- ggstatsplot::ggwithinstats(
  data = tmp_microIndicators_c12,
  x = group,
  y = mean_valence_macro,
  type = "parametric", ## type of statistical test
  xlab = "Central Concepts", ## label for the x-axis
  ylab = "Mean Valence", ## label for the y-axis
  title = "Comparison of mean valence between choosen central concepts"
) + ## modifying the plot further
  ggplot2::scale_y_continuous(
    limits = c(-3, 3),
    breaks = -3:3
  )
p_meanValence












##########################


## remove connections
if(nrow(tmp_diff_dat) >= 1){
  for(j in 1:nrow(tmp_diff_dat)){
    print(nrow(CAMfilesList[[3]]))
    if(all(tmp_merged$isBidirectional == 1)){
      print("a")
      # CAMfilesList[[2]] <- CAMfilesList[[2]][!(CAMfilesList[[2]]$motherID %in% tmp_diff_dat$from[j] &
      #                                            CAMfilesList[[2]]$daughterID %in% tmp_diff_dat$to[j]), ]
      # CAMfilesList[[3]] <- CAMfilesList[[3]][!(CAMfilesList[[3]]$idending %in% tmp_diff_dat$from[j]), ]
    }else{
      prin("b")
      # CAMfilesList[[2]] <- CAMfilesList[[2]][!(CAMfilesList[[2]]$motherID %in% tmp_diff_dat$from[j] &
      #                                            CAMfilesList[[2]]$daughterID %in% tmp_diff_dat$to[j]), ]
      # CAMfilesList[[3]] <- CAMfilesList[[3]][!(CAMfilesList[[3]]$id %in% tmp_diff_dat$from[j] &
      #                                            CAMfilesList[[3]]$idending %in% tmp_diff_dat$to[j]), ]
    }
    print(nrow(CAMfilesList[[3]]))
  }
}

