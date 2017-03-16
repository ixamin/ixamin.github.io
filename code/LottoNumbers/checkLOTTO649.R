lottoname <- "LOTTO649"
min_matches <- 2
source("fn_downloadNeeded.R")
if (downloadNeeded()) {
    source("fn_LOTTO649History.R")
    LOTTO649History()
}

previous_draws <- readRDS(paste0(lottoname,".rds"))
last_draw <- previous_draws[.N,c("DRAW.DATE","NUMBER.DRAWN.1","NUMBER.DRAWN.2","NUMBER.DRAWN.3","NUMBER.DRAWN.4","NUMBER.DRAWN.5","NUMBER.DRAWN.6")]
setkey(last_draw,DRAW.DATE)

predictions <- readRDS(paste0("Predictions_",lottoname,".rds"))
last_prediction <- predictions[1,c("DATE","ITM_CNT","NUMBER.1","NUMBER.2","NUMBER.3","NUMBER.4","NUMBER.5","NUMBER.6")]
setkey(last_prediction,DATE)

ITM <- length(intersect(last_prediction[,-c("DATE","ITM_CNT")],last_draw[,-c("DRAW.DATE")]))
if (ITM >= min_matches ) {
    predictions[last_prediction, `:=`(ITM_CNT=ITM)]
    last_prediction[, `:=`(ITM_CNT=ITM)]
    saveRDS(predictions,paste0("Predictions_",lottoname,".rds"))
    post_tweet(paste0("#GoodNewsEveryone LottoMoto's #LOTTO649 ",format(last_prediction[,DATE],"%b %d")," prediction matched ", last_prediction[,ITM_CNT]," numbers in the ",format(last_draw[,DRAW.DATE],"%b %d, %Y")," draw."))
} else {
	writeLines(paste0("\nNo ",last_prediction[,DATE]," #LOTTO649 match. Checked on ",format(Sys.time(),"%b %d, %Y at %I:%M %p"),"\nEnd"))
}
