

centralConcepts <- c("negative aspects", "positive aspects")


paste0("\nCAMs sliced datasets: .txt files (nodes, connectors, merged) created for central concept: ", centralConcepts[1])

slicedCAMs_combined <- sliceAllCAMs_combined(CAMfilesList = CAMfiles,
                                             drawnCAMs = CAMdrawn,
                                             connectionToRemove = NULL,
                                             nodeToRemove = "Covid-19",
                                             centralConceptsSubgraphs = centralConcepts,
                                             plot = FALSE)




slicedCAMs_seperated <- sliceAllCAMs_seperated(slicedCAMs = slicedCAMs_combined,
                                               centralConceptsSubgraphs = centralConcepts,
                                               plot = FALSE)
names(slicedCAMs_seperated)



tmp_merged <- slicedCAMs_combined[[3]][slicedCAMs_combined[[3]]$CAM.x %in% unique(slicedCAMs_combined[[3]]$CAM.x)[1], ]
tmp_nodes <- slicedCAMs_combined[[1]][slicedCAMs_combined[[1]]$CAM %in% unique(slicedCAMs_combined[[1]]$CAM)[1], ]


tmp_c12 <- draw_CAM(dat_merged = tmp_merged,
                    dat_nodes = tmp_nodes, ids_CAMs = "all",
                    plot_CAM = FALSE,
                    useCoordinates = TRUE,
                    relvertexsize = 3,
                    reledgesize = 1)

plot(tmp_c12[[1]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn[[names(tmp_c12)[1]]], edge.arrow.size = .3,
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
