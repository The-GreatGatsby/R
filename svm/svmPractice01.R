# svm練習
# https://logics-of-blue.com/svm-concept/

install.packages("kernlab")
library(kernlab)

# -------------------------------------------------------
# 線形サポートベクトル分類
# -------------------------------------------------------

# サンプルデータ
bird <- data.frame(
  wing = c(12, 10, 13, 10, 13, 12),
  body = c(15, 20, 23, 30, 36, 39),
  type = c("A","A", "A", "B", "B", "B")
)


# 図示
# 「type="n"」としてplot関数を使うと、プロット（〇点）を消せる。
# そのうえで、text関数を使って、〇点の代わりに、データの行番号をグラフに書き込みました。
plot(
  wing ~ body, 
  data=bird, 
  type="n",
  main="鳥の羽と体の大きさ"
)
text(
  wing ~ body, 
  data=bird, 
  rownames(bird),
  col=c(1,2)[bird$type],
  cex=2
)



# 線形のSV分類
# 説明変数（wing,body）を使って、typeを分類。
# 線形データへ適用させる場合は『vanilladot』を指定。
svm_bird <- ksvm(
  type ~ wing + body, 
  data=bird,
  type="C-svc",
  kernel="vanilladot"
)

# 結果
# 『parameter : cost C = 1 』は、パラメタCは（勝手に）１を設定。
# もちろん自分で設定することも可能。
# 『Training error : 0 』は、誤判別が1つも無かったということ。
# 『Number of Support Vectors : 4 』は、サポートベクトル、すなわち「分類に使われたデータ数」は4つだけということ。
svm_bird

# サポートベクトル
# 何番目のデータが該当するのかは、『SVindex』という関数で分かる。
# 逆に言えば１，６番目のデータは、あってもなくても分類境界の作成に関与していない。
SVindex(svm_bird)


# 分類境界の図示
# 青色がプラスで赤色がマイナスです。正負の符合が切り替わる０の部分（白色）が境界線。
# ちゃんと、少し斜めに線が引かれていることに注目。
plot(svm_bird, data=bird)







# -------------------------------------------------------
# 非線形サポートベクトル分類
# -------------------------------------------------------

bird_2 <- data.frame(
  wing = c(12, 12, 10, 13, 10, 13, 12, 12, 12, 12, 11),
  body = c(10, 15, 20, 22, 34, 36, 39, 37, 25, 29, 27),
  type = c("A", "A", "A", "A", "A", "A","A", "A", "B", "B", "B")
)

# 図示
plot(
  wing ~ body, 
  data=bird_2, 
  type="n",
  main="鳥の羽と体の大きさ(非線形)"
)
text(
  wing ~ body, 
  data=bird_2, 
  rownames(bird_2),
  col=c(1,2)[bird_2$type],
  cex=2
)



# 参考
# 線形のSV分類。これだとうまくいかない。今回のデータは線形（一直線）では分類できない。
# kernel="vanilladot"で線形と分かる。
svm_bird_dame <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="vanilladot",
  C=25
)

# 分類境界の図示
# 全てのデータが真っ赤　＝　全部Aに分類されてる。
plot(svm_bird_dame, data=bird_2)
# 分類結果
fitted(svm_bird_dame)



## 非線形にも対応するイメージ
# 非線形なデータに対しては、データを変換してから分類境界を作るというのがサポートベクトルマシンで使われている方法。
# 非線形変換の考え方
# （あくまで参考。SVMの実際の計算とは異なります）
new_val <- (bird_2$wing + bird_2$body -39)^2
# 2乗した結果を新たに変数として使えば、分類できそう
plot(
  new_val ~ bird_2$body, 
  col=c(1,2)[bird_2$type], 
  pch=16,
  main="非線形変換をした例"
)


# 非線形のSV分類
# polydot（多項式カーネル）
# 先ほどの変換をして作った『new_val』が大きければA種、小さければB種とみなすことができそう。
# このようにデータをいったん変形してから分類をするという技術を使えば、非線形なデータにも対応が可能です。
# 『kpar = list(degree=2)』とすると2乗の多項式を使うことができます。
# またパラメタCは25にしておきました。
svm_bird_2 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="polydot",
  kpar = list(degree=2),
  C=25
)

# 結果
svm_bird_2

# 分類境界の図示
plot(svm_bird_2, data=bird_2)




# 非線形のSV分類
################################ rbfdot（ガウシアンカーネル）
# sigmaが大きければ大きいほど、より「グネグネした」複雑な境界線が引かれる傾向があります。
# でも大きすぎると過学習になりやすい。
svm_bird_3 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=1),
  C=25
)

# 結果
svm_bird_3

# サポートベクトル
SVindex(svm_bird_3)

# 分類境界の図示
plot(svm_bird_3, data=bird_2)





# 非線形のSV分類
# rbfdot（ガウシアンカーネル）
# sigmaを増やした
svm_bird_4 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=20),
  C=25
)
# 分類境界の図示
# 分類境界線が曖昧になりすぎ。
# タイプBが3個あってその周りもきっとタイプBであるはずなのに、、このプロットを見る限り、
# 青2個と青1個の間は赤くなっているので、タイプAに誤判別されてまう（過学習）。
plot(svm_bird_4, data=bird_2)




# 非線形のSV分類
# rbfdot（ガウシアンカーネル）
# Cを減らした。Cを下げると汎化性能が上がるが、下げすぎると、誤判別される。
svm_bird_5 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=1),
  C=0.1
)
# 分類境界の図示
plot(svm_bird_5, data=bird_2)
# 結果
fitted(svm_bird_5)




# -------------------------------------------------------
# サポートベクトル回帰
# -------------------------------------------------------
# サポートベクトルマシンは分類と回帰とで計算の方法が大きく変わります。
#しかし、共に「明らかに正しく予測できているデータに関しては、どーでもいいので無視して、
# データを予測するのに必要となるサポートベクトルのみを使う」という発想で予測します。
# 回帰問題の場合は、予測誤差を工夫します。
# それが「ε-不感損失関数」です
# 横軸に「予測値 － 実測値」の残差を置き、縦軸に「予測誤差の大きさ」すなわち損失を載せています。
# 残差が-ε～+εまでの範囲内では、損失は0とみなされます。
# 残差がεを超えたら、損失として計上します。
# なお、残差がεと同じ、あるいはεを超えてしまうデータをサポートベクトルとみなします。
# このε‐不感損失関数が小さくなるようにモデルを推定します。
# そのため、ε‐不感損失関数に影響を及ぼさないデータ（サポートベクトルではないデータ）は予測に使われません。
# εの値は、予め決めておく必要があります。
# サポートベクトル分類で設定するパラメタ（コストCや、ガウシアンカーネルの場合はsigmaなど）もそのまま使う（解釈は変わりません）ので、チューニングが必要です。
# 考え方としてはこれだけです。
# 非線形なデータに対してはカーネル関数を使うことにより対応ができます。

# 予測モデルの作成
svm_air <- ksvm(
  Ozone ~ Temp,
  data=airquality,
  epsilon=0.25,
  type="C-svc",
  kernel="rbfdot",
  kpar=list(sigma=1),
  C=2
)

# 予測
new <- data.frame(
  Temp=seq(min(airquality$Temp), max(airquality$Temp), 0.1)
)
svm_air_pred <- predict(svm_air, new)
# 予測結果の図示
plot(
  airquality$Ozone ~ airquality$Temp,
  xlab="Temp", 
  ylab="Ozone"
)
lines(svm_air_pred ~ as.matrix(new), col=2, lwd=2)



# -------------------------------------------------------
# ハイパーパラメタのチューニング
# -------------------------------------------------------

# install.packages("caret")
# install.packages("e1071")
library(caret)
library(e1071)

# 並列化演算を行う
# install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# 鳥のデータ使ってチューニング
set.seed(0)
tuned_svm <- train(
  type ~ wing + body, 
  data=bird_2,
  method = "svmRadial", 
  tuneGrid = expand.grid(C=c(1:30), sigma=seq(0.1, 2, 0.1)),
  preProcess = c('center', 'scale')
)

# チューニングの結果
tuned_svm



# 最も「よい」パラメタを使って境界線を図示
best_svm <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=0.8),
  C=3
)

plot(best_svm, data=bird_2)









