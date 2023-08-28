getDescriptives <- function(dataset = questionnaire, regEx,
                            sorted = TRUE,
                            nameAPAtable = NULL){
  x <- dataset[, str_detect(string = colnames(dataset),
                            pattern = regEx)]


  ## table
  tmp_descriptives <- sapply(x, function(x) c(
    "Mean"= mean(x,na.rm=TRUE),
    "SD" = sd(x,na.rm=TRUE),
    "Median" = median(x,na.rm=TRUE),
    "CoeffofVariation" = sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE),
    "Minimum" = min(x,na.rm=TRUE),
    "Maximun" = max(x,na.rm=TRUE),
    "Lower Quantile" = as.numeric(quantile(x,0,na.rm=TRUE)),
    "Upper Quantile" = as.numeric(quantile(x,1,na.rm=TRUE)),
    "Skewness" = moments::skewness(x = x,na.rm=TRUE),
    "Kurtosis(-3)" = moments::kurtosis(x = x,na.rm=TRUE) -3,
    "KS-Test" = ks.test(x = x, y = "pnorm", mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))$p.value
  )
  )
  tmp_descriptives <- round(x = tmp_descriptives, digits = 2)

  # print(t(tmp_descriptives))


  ## ggplot
  tmp_long <- x %>%
    gather(key="variables", value="value") %>%
    mutate(variables = gsub("\\.", " ",variables)) %>%
    mutate(value = round(as.numeric(value),2))

  if(sorted){
    p <- tmp_long %>%
      mutate(variables = fct_reorder(variables, value)) %>%
      ggplot( aes(x=value, color=variables, fill=variables)) +
      geom_histogram() +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("") +
      ylab("Frequency") +
      facet_wrap(~variables, scales = "free")
  }else{
    p <- tmp_long %>%
      ggplot( aes(x=value, color=variables, fill=variables)) +
      geom_histogram() +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("") +
      ylab("Frequency") +
      facet_wrap(~variables, scales = "free")
  }

  print(p)


  ## round digits
  out_tmp <- round(x = t(tmp_descriptives), digits = 2)

  ## stargazer
  if(!is.null(nameAPAtable)){
    notNeeded <- capture.output(stargazer(out_tmp, type = "html", summary = FALSE,
                                          out = paste0(nameAPAtable, ".html")))
  }




  return(out_tmp)
}


getDescriptives(dataset = tmp_Indicators, regEx = "_macro", sorted = FALSE, nameAPAtable = NULL)
getDescriptives(dataset = tmp_Indicators, regEx = "_micro", sorted = FALSE, nameAPAtable = NULL)
