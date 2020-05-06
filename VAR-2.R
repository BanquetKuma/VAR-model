library(vars)
library(tseries)
library(CADFtest)
library(tidyverse)
library(ggsci)
library(magrittr)
library(vars)
# 偏グレンジャー因果用のスクリプト読み込み
source('https://raw.githubusercontent.com/cran/FIAR/master/R/partGranger.R')
# install.packages("dplyr", dependencies = TRUE)
library(dplyr)
# 全ての古いパッケージをアップデートする
# update.packages()
#packageVersion("dplyr")

df <- read.csv("C:\\Users\\SHO\\OneDrive\\ドキュメント\\R Script\\Input\\macrodata.csv")

str(df)

summary(df)

# 選択列のベクトル
columnList <- c("id","realgdp", "realcons", "realinv")

df[, columnList]

df_long =  df[, columnList] %>%
  tidyr::pivot_longer(c(-id), names_to = "Name", values_to = "Value")

df_long

g <- ggplot(df_long, aes(x = id, y = Value, color = Name))
g <- g + geom_line()
g <- g + scale_color_nejm()
plot(g)

# ADF検定を行う
CADFtest(df$realgdp,type=c("trend"),max.lag.y = 10)
CADFtest(df$realcons,type=c("trend"),max.lag.y = 10)
CADFtest(df$realinv,type=c("trend"),max.lag.y = 10)

# 1階差分を取る
diff_realgdp <- diff(df[, columnList]$realgdp)
diff_realcons <- diff(df[, columnList]$realcons)
diff_realinv <- diff(df[, columnList]$realinv)

# データフレームにまとめる
df_diff <- data.frame(id = c(0:201),
                      realgdp = diff_realgdp, 
                      realcons = diff_realcons, 
                      realinv = diff_realinv)

df_diff_long =  df_diff %>%
  tidyr::pivot_longer(col=-id,names_to = "Name", values_to = "Value")

g <- ggplot(df_diff_long, aes(x = id, y = Value, color = Name))
g <- g + geom_line()
g <- g + scale_color_nejm()
plot(g)

# ADF検定を行う
CADFtest(df_diff$realgdp,type=c("trend"),max.lag.y = 10)
CADFtest(df_diff$realcons,type=c("trend"),max.lag.y = 10)
CADFtest(df_diff$realinv,type=c("trend"),max.lag.y = 10)

# 2階差分を取る
diff2_realgdp <- diff(diff(df[, columnList]$realgdp))
diff2_realcons <- diff(diff(df[, columnList]$realcons))
diff2_realinv <- diff(diff(df[, columnList]$realinv))

# データフレームにまとめる
df_diff_diff <- data.frame(id = c(0:200),
                      realgdp = diff2_realgdp, 
                      realcons = diff2_realcons, 
                      realinv = diff2_realinv)

df_diff_diff_long =  df_diff_diff %>%
  tidyr::pivot_longer(col=-id,names_to = "Name", values_to = "Value")

g <- ggplot(df_diff_diff_long, aes(x = id, y = Value, color = Name))
g <- g + geom_line()
g <- g + scale_color_nejm()
plot(g)

# ADF検定を行う
CADFtest(df_diff_diff$realgdp,type=c("trend"),max.lag.y = 10)
CADFtest(df_diff_diff$realcons,type=c("trend"),max.lag.y = 10)
CADFtest(df_diff_diff$realinv,type=c("trend"),max.lag.y = 10)

# ラグの選択
VARselect(df_diff_diff,lag.max=10)

# df_diff_diffからid列を削除する
df_diff_diff <- df_diff_diff[, colnames(df_diff_diff) != "id"]


#VARモデルの推定(AIC基準で選ぶ)
df_var <- VAR(df_diff_diff,p=VARselect(df_diff_diff,lag.max=10)$selection[1])

# 結果の表示
coef(df_var)

summary(df_var)

plot(df_var)

# Varモデルに基づいて予測する
df_var.pred<-predict(df_var,n.ahead=20,ci=0.95)

plot(df_var.pred)

#グレンジャー因果性検定
causality(df_var,cause="realgdp")
causality(df_var,cause="realcons")
causality(df_var,cause="realinv")

#偏グレンジャー因果性検定
partGranger(df_diff_diff[,c("realgdp","realcons")],nx=1,ny=1,order=9)
partGranger(df_diff_diff[,c("realgdp","realinv")],nx=1,ny=1,order=9)
partGranger(df_diff_diff[,c("realcons","realgdp")],nx=1,ny=1,order=9)
partGranger(df_diff_diff[,c("realcons","realinv")],nx=1,ny=1,order=9)
partGranger(df_diff_diff[,c("realinv","realgdp")],nx=1,ny=1,order=9)
partGranger(df_diff_diff[,c("realinv","realcons")],nx=1,ny=1,order=9)

#インパルス応答関数
df_irf<-irf(df_var,n.ahead=20,ci=0.95)
plot(df_irf)

# 分散分解
df_var_fevd <- fevd(df_var, n.ahead = 10)
plot(df_var_fevd)

