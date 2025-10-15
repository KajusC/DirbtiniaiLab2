#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param DF
##' @param topShops
##' @param textSize
plot_data_overview <- function(DF, topShops, textSize=14) {
  res <- NULL

  # plot counts
  tempDF <- DF
  tempDF$rating <- ordered(tempDF$rating, levels=c(1,2,3,4,5))
  res$shop_counts <- ggplot(tempDF %>% count(shop,rating) %>% dplyr::filter(shop %in% names(topShops)) %>% inner_join(data.frame(topShops),by=c("shop"="Var1")),
         aes(x=shop, y=n, fill=rating)) +
    geom_bar(stat="identity", position=position_stack(reverse = TRUE)) + coord_flip() +
    scale_x_discrete(limits = names(topShops)) +
    geom_text(aes(y=Freq, label=Freq), hjust = -0.1) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size=textSize), axis.text.y = element_text(size=textSize),
          legend.title=element_text(size=textSize), legend.text=element_text(size=textSize))

  # plot balance
  tempDF <- DF
  badness <- tempDF %>% dplyr::filter(shop %in% names(topShops)) %>% group_by(shop) %>% summarise(target=mean(bad)) %>% arrange(target)
  tempDF$bad <- ordered(tempDF$bad, levels=c(1,0))
  res$shop_balance <- ggplot(tempDF %>% dplyr::filter(shop %in% names(topShops)) %>% arrange(bad) %>% mutate(const=1) %>% inner_join(badness),
         aes(y = const, x = shop, fill = bad)) + geom_col(position = position_fill(reverse = FALSE)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = badness$shop) + coord_flip() +
    geom_text(aes(y=0, label=sprintf('%4.2f%%',100*(1-target))), color="black", hjust = 0) +
    geom_text(aes(y=1,label=sprintf('%4.2f%%',100*target)), color="white", hjust = 1) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size=textSize), axis.text.y = element_text(size=textSize),
          legend.title=element_text(size=textSize), legend.text=element_text(size=textSize))

  # plot counts yearly
  tempDF <- DF
  tempDF$bad <- ordered(tempDF$bad, levels=c(1,0))
  year_counts <- tempDF %>% mutate(year=format(as.Date(date),"%Y")) %>% count(year) %>% mutate(pct=100*n/sum(n))
  res$yearly_counts <- ggplot(tempDF %>% mutate(year=format(as.Date(date),"%Y")) %>% count(year,bad) %>% inner_join(year_counts,by="year"),
         aes(x=year, y=n.x, fill=bad)) +
    geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
    geom_text(aes(y=0, label=sprintf('(%4.2f%%)',pct)), vjust=1.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size=textSize), axis.text.y = element_text(size=textSize),
          legend.title=element_text(size=textSize), legend.text=element_text(size=textSize))

  # plot balance yearly
  tempDF <- DF
  tempDF <- tempDF %>% mutate(year=format(as.Date(date),"%Y"))
  badness <- tempDF %>% group_by(year) %>% summarise(target=mean(bad)) %>% arrange(year)
  tempDF$bad <- ordered(tempDF$bad, levels=c(1,0))
  res$yearly_balance <- ggplot(tempDF %>% arrange(bad) %>% mutate(const=1) %>% inner_join(badness),
         aes(y = const, x = year, fill = bad)) + geom_col(position = position_fill(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = badness$year) +
    geom_text(aes(y=1,label=sprintf('%4.2f%%',100*(1-target))), color="black", vjust = 1.5) +
    geom_text(aes(y=0,label=sprintf('%4.2f%%',100*target)), color="white", vjust = -0.8) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size=textSize), axis.text.y = element_text(size=textSize),
          legend.title=element_text(size=textSize), legend.text=element_text(size=textSize))

  return(res)
}
