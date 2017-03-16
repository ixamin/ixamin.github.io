/*** R
lottoname <- "LOTTO649"
*/
#include <Rcpp.h>
#include <vector>       //std::vector
#include <algorithm>    //std::set_intersection
#include <SecretSauce.cpp>
using namespace Rcpp;

#include <Rcpp.h>
NumericMatrix comboScore(double amount_to_pick,
                         double out_of,
                         double min_matches,
                         IntegerMatrix combos,
                         IntegerMatrix previous) {
  //size of previous draws matrix
  int p_nrow = previous.nrow(), p_ncol = previous.ncol();
  //for sizing results matrix and match iterating
  int c_nrow = combos.nrow(), c_ncol = combos.ncol();
  //sizing of results matrix
  NumericMatrix results(c_nrow, c_ncol + 3);
  //create vector to hold the combination to be tested
  std::vector<int> combo;
  combo.reserve(c_ncol);
  //create vector to hold draw to be tested
  std::vector<int> draw;
  draw.reserve(p_ncol);

  for (int cr = 0; cr < c_nrow; ++cr) {
    if(cr % 100 == 0) Rcpp::checkUserInterrupt();
    //reset the vector of drawn numbers
    combo.clear();
    for (int cc = 0; cc < c_ncol; ++cc) {
      //populate the combo vector with the set of numbers to be tested
      combo.push_back(combos(cr,cc));
    }
    //sort the combo vector in prep for set_intersection function
    std::sort(combo.begin(),combo.end());
    //(re)set the cr total score variable
    double comboScore = 0.0;
    //(re)set count of matched draws
    double matchedDraws = 0.0;
    //for every draw's previously drawn numbers
    for (int pr = 0; pr < p_nrow; ++pr) {
      //reset the vector of drawn numbers
      draw.clear();
      //(re)set the combination to draw score variable
      double drawScore = 0.0;
      for (int pc = 0; pc < p_ncol; ++pc) {
        draw.push_back(previous(pr,pc));
      }
      //sort the draw vector in prep for set_intersection function
      std::sort(draw.begin(),draw.end());

      std::vector<int> match;
      //return intersection of combo and draw vectors
      std::set_intersection(combo.begin(),combo.end(),
                            draw.begin(),draw.end(),
                            std::back_inserter(match)
      );
      int match_size = match.size();
      //a secret mix of frequency and quality metrics
      //is used to calculate the score...
      SecretSauce(amount_to_pick,
                  out_of,
                  match_size,
                  min_matches,
                  matchedDraws,
                  drawScore,
                  comboScore);
    }
    //keep only scores greater than zero
    if (comboScore > 0) {
      results(cr,0) = cr;
      results(cr,1) = comboScore;
      results(cr,2) = matchedDraws;
      for (int i = 3; i < combo.size() + 3; ++i) {
        results(cr,i) = combo[i-3];
      }
    }
  }
  return results;
}

/*** R
lottoname <- toupper(lottoname)
setwd("~/Documents/rcode/fun")
source("fn_nextDraw.R")

amount_to_pick <- 6
out_of <- 49
min_matches <- 2

previous_draws <- readRDS(paste0(lottoname,".rds"))
previous_draws <- previous_draws[as.Date(DRAW.DATE) > as.Date("2015-01-31"),] #new LOTTO machine
previous_draws <- unname(as.matrix(previous_draws[, c("DRAW.DATE") := NULL]))

scoreboard <- data.table(comboScore(amount_to_pick,out_of,min_matches,combos,previous_draws))

setnames(scoreboard,c("idx","score","matches","NUMBER.1","NUMBER.2","NUMBER.3","NUMBER.4","NUMBER.5","NUMBER.6"))
setorder(scoreboard,-score,-matches)

prediction <- scoreboard[1,c("NUMBER.1","NUMBER.2","NUMBER.3","NUMBER.4","NUMBER.5","NUMBER.6")]
prediction[, `:=`(DATE = as.Date(nextDraw(lottoname)), ITM_CNT = 0)]

predictions <- readRDS(paste0("Predictions_",lottoname,".rds"))
predictions <- rbindlist(list(predictions,prediction),use.names = TRUE,fill = FALSE,idcol = NULL)
predictions <- unique(predictions)
setorder(predictions,-DATE)
saveRDS(predictions,paste0("Predictions_",lottoname,".rds"))

post_tweet(paste0("LottoMoto's ",format(nextDraw(lottoname),"%b %d, %Y")," #LOTTO649 numbers are ", paste(as.character(scoreboard[1,-c("idx","score","matches")]),collapse = ", ")," #dreaming. Read about LottoMoto at ixamin.com/LottoNumbers.html"))

quit()
*/
