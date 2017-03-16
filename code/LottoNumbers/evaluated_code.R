library(data.table)
source("code/LottoNumbers/fn_comboScore_R_iter.R")
source("code/LottoNumbers/fn_comboScore_R_vect.R")
Rcpp::sourceCpp("code/LottoNumbers/fn_comboScore_Cpp.cpp")
lotto <- readRDS("code/LottoNumbers/LOTTO649.rds")
combos <- readRDS("code/LottoNumbers/Combinations_LOTTO649.rds")
# str(lotto)

lotto_numbers <- unname(unlist(lotto[,-c("DRAW.DATE")]))
lotto_data <- data.table(numbers = lotto_numbers)
lotto_data <- unique(lotto_data[,count := .N, by=(numbers)])

library(ggplot2)
smile <- rgb(241,219,63,max = 255)
winner_img <- png::readPNG("code/LottoNumbers/winner.png")
winner <- grid::rasterGrob(winner_img, interpolate=TRUE)

draw_distribution <- ggplot() +
  ggtitle("Density distribution of past Lotto 649 draws") +
  stat_density_2d(geom = "point", data = lotto_data,aes(x=numbers, y=count,size = ..density..,stroke= ..density..,alpha = ..density..),
                  inherit.aes = FALSE,
                  shape = 21,
                  fill=smile,
                  stroke=.4,
                  alpha = .9,
                  n = 49,
                  contour = FALSE) +
  scale_x_discrete(limit = seq(1:49)) +
  annotation_custom(winner, xmin=0, xmax=10, ymin=468, ymax=478) +
  theme(legend.position="none",axis.title.x=element_blank(),
        axis.text.x = element_text(size = 6),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_text(size = 6),
        title = element_text(size = 8)) +
  labs(y="Count")
# draw_distribution


previous_draws <- lotto[as.Date(DRAW.DATE) > as.Date("2015-01-31"),]
previous_draws_cpp <- unname(as.matrix(previous_draws[, c("DRAW.DATE") := NULL]))

amount_to_pick <- 6
out_of <- 49
min_matches <- 2

# test_iter_timings <- data.table()
# test_vect_timings <- data.table()
# test_cpp_timings <- data.table()
# sapply(seq(from = 1000, to = 5000, by = 500), function(x) {
#   set.seed(99)
#   combos <- combos[sample.int(nrow(combos),size = x,replace = FALSE),]
#   score_iter_time <- system.time(
#     scoreboard_iter <<- fn_comboScore_R_iter(min_matches,combos,previous_draws)
#   )
#   iter_timings_out <- data.table(num_combos = x, elapsed_minutes = round(score_iter_time[[1]]/60,4))
#   test_iter_timings <<- rbindlist(list(test_iter_timings,iter_timings_out))
#   score_vect_time <- system.time(
#     scoreboard_vect <<- fn_comboScore_R_vect(min_matches,combos,previous_draws)
#   )
#   vect_timings_out <- data.table(num_combos = x, elapsed_minutes = round(score_vect_time[[1]]/60,4))
#   test_vect_timings <<- rbindlist(list(test_vect_timings,vect_timings_out))
#   score_cpp_time <- system.time(
#     scoreboard_cpp <<- as.data.table(fn_comboScore_Cpp(amount_to_pick,out_of,min_matches,combos,previous_draws_cpp))
#   )
#   cpp_timings_out <- data.table(num_combos = x, elapsed_minutes = round(score_cpp_time[[1]]/60,4))
#   test_cpp_timings <<- rbindlist(list(test_cpp_timings,cpp_timings_out))
# })
# saveRDS(scoreboard_iter,"code/LottoNumbers/scoreboard_iter.rds")
# saveRDS(scoreboard_vect,"code/LottoNumbers/scoreboard_vect.rds")
# setnames(scoreboard_cpp,c("idx","score"))
# saveRDS(scoreboard_cpp,"code/LottoNumbers/scoreboard_cpp.rds")
# saveRDS(test_iter_timings,"code/LottoNumbers/test_iter_timings.rds")
# saveRDS(test_vect_timings,"code/LottoNumbers/test_vect_timings.rds")
# saveRDS(test_cpp_timings,"code/LottoNumbers/test_cpp_timings.rds")

scoreboard_iter <- readRDS("code/LottoNumbers/scoreboard_iter.rds")
scoreboard_vect <- readRDS("code/LottoNumbers/scoreboard_vect.rds")
scoreboard_cpp <- readRDS("code/LottoNumbers/scoreboard_cpp.rds")
test_iter_timings <- readRDS("code/LottoNumbers/test_iter_timings.rds")
test_vect_timings <- readRDS("code/LottoNumbers/test_vect_timings.rds")
test_cpp_timings <- readRDS("code/LottoNumbers/test_cpp_timings.rds")

timings_iter_model <- lm(elapsed_minutes ~ num_combos,data = test_iter_timings)
timings_vect_model <- lm(elapsed_minutes ~ num_combos,data = test_vect_timings)
timings_cpp_model <- lm(elapsed_minutes ~ num_combos,data = test_cpp_timings)

predict(timings_iter_model,newdata = data.frame(num_combos=choose(49,6)))
all_combos_iter_score_time <- round(predict(timings_iter_model,newdata = data.frame(num_combos=choose(49,6)))/60/24,1)
writeLines(paste0("\nThe predicted elapsed time to score all ",prettyNum(choose(49,6),big.mark = ",") ," combinations against\nthe current 649 results dataset with ",nrow(previous_draws)," records using the 'iter' model is ",all_combos_iter_score_time," days."))
predict(timings_vect_model,newdata = data.frame(num_combos=choose(49,6)))
all_combos_vect_score_time <- round(predict(timings_vect_model,newdata = data.frame(num_combos=choose(49,6)))/60/24,1)
writeLines(paste0("\nThe predicted elapsed time to score all ",prettyNum(choose(49,6),big.mark = ",") ," combinations against\nthe current 649 results dataset with ",nrow(previous_draws)," records using the 'vect' model is ",all_combos_vect_score_time," days."))
writeLines(paste0("\nAre the two resulting scoreboards the same? ",ifelse(identical(scoreboard_iter,scoreboard_vect),"Yes!","No.")))
all_combos_cpp_score_time <- round(predict(timings_cpp_model,newdata = data.frame(num_combos=choose(49,6)))/60/24,6)
writeLines(paste0("\nThe predicted elapsed time to score all ",prettyNum(choose(49,6),big.mark = ",") ," combinations against\nthe current 649 results dataset with ",nrow(previous_draws)," records using the 'vect' model is ",all_combos_cpp_score_time," days."))
writeLines(paste0("\nAre the two resulting scoreboards the same? ",ifelse(identical(scoreboard_vect,scoreboard_cpp),"Yes!","No.")))
