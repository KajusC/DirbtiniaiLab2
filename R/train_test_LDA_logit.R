#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
##' @param trnIter
##' @param tstIter
##' @param vecObj
##' @param trnTarget
##' @param tstTarget
##' @param nTopics
train_test_LDA_logit <- function(trnIter, tstIter=NULL, vecObj, trnTarget, tstTarget=NULL, nTopics,
                             kFoldsCV=3, alphaSeq=seq(0,1,len=21)^3, lambda_type="lambda.min") {
  modelName <- paste0(nTopics,'D_LSA')
  tfidf_model <- text2vec::TfIdf$new()
  lsa_model <- text2vec::LSA$new(n_topics = nTopics)
  train_embeddings <- lsa_model$fit_transform(tfidf_model$fit_transform(text2vec::create_dtm(trnIter,vecObj)))
  no_cores <- max(1,parallel::detectCores()-1)
  cid <- parallel::makeCluster(no_cores)
  doParallel::registerDoParallel(cid)
  glmnet_model <- glmnetUtils::cva.glmnet(x=train_embeddings, y=trnTarget, family = "binomial",
                                         nfolds = kFoldsCV, alpha = alphaSeq, outerParallel = cid)
  opt.alpha <- glmnet_model$alpha[which.min(sapply(glmnet_model$modlist, function(mod) min(mod$cvm)))]
  parallel::stopCluster(cl=cid)
  if (is.null(tstIter) & is.null(tstTarget)){
    fileNames <- NULL
    fileName <- paste0('data/',modelName,'_vec.rds')
    saveRDS(vecObj, fileName)
    fileNames <- c(fileNames,fileName)
    fileName <- paste0('data/',modelName,'_tfidf.rds')
    saveRDS(tfidf_model, fileName)
    fileNames <- c(fileNames,fileName)
    fileName <- paste0('data/',modelName,'_lsa.rds')
    saveRDS(lsa_model, fileName)
    fileNames <- c(fileNames,fileName)
    fileName <- paste0('data/',modelName,'_logit.rds')
    saveRDS(glmnet_model, fileName)
    fileNames <- c(fileNames,fileName)
    return(fileNames)
  } else {
    test_embeddings <- lsa_model$transform(tfidf_model$transform(text2vec::create_dtm(tstIter,vecObj)))
    score <- predict(glmnet_model, test_embeddings, s=lambda_type, alpha=opt.alpha, type="response")[,1]
    idx <- 1:nrow(test_embeddings)
    DF <- data.frame(index=idx,model=modelName,score,target=tstTarget,stringsAsFactors=F)
    return(DF)
  }
}
