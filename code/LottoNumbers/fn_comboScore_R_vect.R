fn_comboScore_R_vect <- function(min_matches,combos,previous_draws) {
  scoreboard <- apply(previous_draws,1,function(i) apply(combos,1,function(j) length(intersect(i,j))))
  scoreboard[scoreboard < min_matches] <- 0
  scoreboard <- as.data.table(scoreboard)
  scoreboard[,score := as.integer(rowSums(.SD))]
  scoreboard[,idx := .I]
  return(scoreboard[,.(idx,score)])
}
