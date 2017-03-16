library(utils)
if(!is.element("XML", utils::installed.packages()[,1])) install.packages("XML")
library(XML)
if(!is.element("data.table", utils::installed.packages()[,1])) {install.packages("data.table")} # CRAN version
library(data.table)
if(!is.element("Rcpp", utils::installed.packages()[,1])) {install.packages("Rcpp")}
library(Rcpp)
if(!is.element("rtweet", utils::installed.packages()[,1])) {install.packages("rtweet")}
library(rtweet)
if(!is.element("ggplot2", utils::installed.packages()[,1])) {install.packages("ggplot2")}
library(ggplot2)
