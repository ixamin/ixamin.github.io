fn_comboScore_R_iter <- function(min_matches,combos,previous_draws) {
  scoreboard <- data.table()
  for (rc in seq.int(nrow(combos))) {
    combo_score <- 0L
    draw_score <- 0L
    for (rp in seq.int(nrow(previous_draws))) {
      picked_score <- 0L
      for (cc in seq.int(ncol(combos))) {
        for (cp in seq.int(ncol(previous_draws))) {
          if (combos[rc,cc] == previous_draws[[rp,cp]]) {
            picked_score <- picked_score + 1L
          }
        }
      }
      if (picked_score >= min_matches) {
        draw_score <- draw_score + picked_score
      }
    }
    combo_score <- combo_score + draw_score
    out <- data.table(score = combo_score, combo_numbers = list(combos[rc,]))
    scoreboard <- rbindlist(list(scoreboard, out))
    combo_score <- 0L
  }
  scoreboard[,idx := .I]
  return(scoreboard[,.(idx,score)])
}
