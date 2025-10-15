#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param theta
##' @param topicNames
##' @param groupingIndex
plot_topic_results <- function(theta, topicNames, groupingIndex) {
  res <- NULL

  rankOneCounts <- table(apply(theta, 1, which.max))
  missingTopics <- setdiff(colnames(theta),names(rankOneCounts))
  if (length(missingTopics)>0) {
    zeroFrequency <- rep(0,1,length(missingTopics))
    names(zeroFrequency) <- missingTopics
    rankOneCounts <- c(rankOneCounts,zeroFrequency)
  }
  rankOneTopic <- as.numeric(prop.table(rankOneCounts))
  DF_soft <- data.frame(Topic=paste0(topicNames,' (',names(topicNames),')'), Probability=colMeans(theta), stringsAsFactors=F)
  DF_hard <- data.frame(Topic=paste0(topicNames,' (',names(topicNames),')'), Proportion=rankOneTopic, stringsAsFactors=F)
  res$soft <- ggplot(DF_soft, aes(x = Probability, y = reorder(Topic, Probability))) + geom_bar(stat="identity") + ylab("Topic")
  res$hard <- ggplot(DF_hard, aes(x = Proportion, y = reorder(Topic, Proportion))) + geom_bar(stat="identity") + ylab("Topic")

  topic_proportion_per_group <- aggregate(theta, by = list(Group = groupingIndex), mean)
  colnames(topic_proportion_per_group)[2:(length(topicNames)+1)] <- paste0('(',names(topicNames),') ',topicNames)
  vizDataFrame <- reshape2::melt(topic_proportion_per_group, id.vars = "Group")
  names(vizDataFrame) <- gsub('variable','Topic',names(vizDataFrame))
  vizDataFrame$Group <- factor(vizDataFrame$Group)
  res$prop <- ggplot(vizDataFrame, aes(x=Group, y=value, fill=Topic)) +
    geom_bar(stat = "identity") + ylab("Proportion")

  topic_proportion_diff <- t(topic_proportion_per_group[1,-1] - topic_proportion_per_group[2,-1])
  diff_numeric <- as.numeric(topic_proportion_diff[,1])
  diff_groups <- rep("Positive",1,length(diff_numeric))
  diff_groups[diff_numeric<0] <- "Negative"
  topic_proportion_diff_DF <- data.frame(Topic=rownames(topic_proportion_diff),
                                         Delta=diff_numeric,Sentiment=diff_groups,stringsAsFactors=F)
  topic_proportion_diff_DF <- topic_proportion_diff_DF[order(topic_proportion_diff_DF$Delta),]
  res$diff <- ggplot(topic_proportion_diff_DF, aes(x=Delta, y=reorder(Topic, Delta), fill=Sentiment)) +
    geom_bar(stat='identity') + ylab("Topic") + xlab("Difference of average class-wise probabilities")

  return(res)
}
