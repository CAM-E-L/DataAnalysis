# ==============================================================================
# R-Code - CAM
# date of creation: December 2021
# authors: Julius Fenn
# function: draw_CAM()
# ==============================================================================

############################################################################
##### ggplot APA 7 settings
############################################################################
ggplot_theme <- theme(axis.title.x = element_text(size=14),
                      axis.title.y = element_text(size=14),
                      axis.text.x = element_text(size=12,hjust=0.5,vjust=0.5,face="plain", colour = "black"),
                      axis.text.y = element_text(size=12,face="plain", colour = "black"),
                      panel.border = element_blank(),
                      axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank())


############################################################################
##### save_graphic()
# save graphic object as png file
############################################################################
save_graphic <- function(filename){
  tmp <- paste(filename, ".png", sep = "")
  Cairo::Cairo(file=tmp,
               type="png",
               units="px",
               width=2500,
               height=1700,
               pointsize=44, #text is shrinking by saving graphic
               dpi= "auto",
               bg = "white")
}



############################################################################
##### rename_identicalTerms()
# rename within a CAM identical terms to X_1, X_2, X_N
# > often occurs after summarizing terms
############################################################################
### args:
# dat_nodes = CAMfiles[[1]]
# drawn_CAM = CAMdrawn
# removeSuffix = TRUE
rename_identicalTerms <- function(dat_nodes = CAMfiles[[1]],
                                  drawn_CAM = CAMdrawn,
                                  removeSuffix = FALSE){

  vec_ids <- c(); h = 1
  if(any(colnames(dat_nodes) == "text_summarized")){
    print("text_summarized column identified")
    dat_nodes$text_backup <- dat_nodes$text
    dat_nodes$text <- dat_nodes$text_summarized
  }

  if(removeSuffix){
    dat_nodes$text <- str_remove_all(string = dat_nodes$text, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
  }

  ## rename terms if any terms is > 1x time within one CAM
  for(i in 1:length(unique(dat_nodes$CAM))){
    if(any(table(dat_nodes$text[dat_nodes$CAM == unique(dat_nodes$CAM)[i]]) > 1)){

      vec_ids[h] <- unique(dat_nodes$CAM)[i]
      # print(vec_ids[h])
      h = h + 1

      tmp_err_name <- names(table(dat_nodes$text[dat_nodes$CAM == unique(dat_nodes$CAM)[i]]))[
        table(dat_nodes$text[dat_nodes$CAM == unique(dat_nodes$CAM)[i]]) > 1]
      # print(tmp_err_name)



if(all(names(drawn_CAM) %in% unique(dat_nodes$CAM))){
      ref_CAM_igraph <- drawn_CAM[[c(1:length(drawn_CAM))[names(drawn_CAM) == unique(dat_nodes$CAM)[i]]]]
}else{
      ref_CAM_igraph <- drawn_CAM[[c(1:length(drawn_CAM))[names(drawn_CAM) == unique(dat_nodes$participantCAM)[i]]]]
}


      if(removeSuffix){
        V(ref_CAM_igraph)$label <- str_remove_all(string = V(ref_CAM_igraph)$label, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
      }

      # table(dat_nodes$text[dat_nodes$CAM == unique(dat_nodes$CAM)[i]])

      # cat("CAM: ", unique(dat_nodes$CAM)[i], "i:", i,"\n")
      for(k in 1:length(tmp_err_name)){
        tmp_sel <- cbind(V(ref_CAM_igraph)$label, degree(graph = ref_CAM_igraph))
        tmp_sel <- as.data.frame(tmp_sel); colnames(tmp_sel) <- c("text", "degree")
        tmp_sel$idvertex <- rownames(tmp_sel); rownames(tmp_sel) <- NULL

        tmp_sel <- tmp_sel[tmp_sel$text == tmp_err_name[k],]
        tmp_sel$degree <- as.numeric(as.character(tmp_sel$degree))
        tmp_sel <- tmp_sel[order(tmp_sel$degree, decreasing = TRUE),]
        # tmp_sel

        if(nrow(tmp_sel) > 1){
          for(j in 2:nrow(tmp_sel)){
            dat_nodes$text[dat_nodes$CAM == unique(dat_nodes$CAM)[i] & dat_nodes$id == tmp_sel$idvertex[j]] <-
              paste0(tmp_err_name[k], "_", j)
          }
        }
      }
    }
  }


  ## check correctness of renaming step
  vec_tmp <- c()
  for(i in 1:length(unique(dat_nodes$CAM))){
    tmp <- str_replace_all(string=dat_nodes$text[
      dat_nodes$CAM == unique(dat_nodes$CAM)[i]],
      pattern=" |-", repl="_")
    vec_tmp[i] <- paste0(tmp, collapse = " ")
  }

  tdm <- tm::TermDocumentMatrix(x = VCorpus(x = VectorSource(vec_tmp)))


  if(any(names(table(tdm$v)) != "1")){
    cat("ERROR:", sum(table(tdm$v)[names(table(tdm$v)) != "1"]),
        "CAMs containg an identical term")

    tdm_mat <- as.matrix(tdm)
    for(i in 2:length(unique(tdm$v))){
      cat("\n\n following CAMs containg", i, "times an identical term:\n",
          unique(dat_nodes$CAM)[
            as.numeric(colnames(tdm_mat)[colSums(tdm_mat == i) > 0])])
    }
  }else{
    cat("renaming was successful, terms in the following", length(vec_ids), "CAMs were renamed:",
        vec_ids)
  }

  if(any(colnames(dat_nodes) == "text_summarized")){
    dat_nodes$text_summarized <- dat_nodes$text
    dat_nodes$text <- dat_nodes$text_backup
    dat_nodes$text_backup <- NULL
  }


  return(dat_nodes)
}
