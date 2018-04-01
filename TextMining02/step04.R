

# クロス集計表の準備
cross.tab <- matrix(c(96, 54, 52, 48), nrow = 2, ncol = 2, byrow = TRUE)
rownames(cross.tab) <- c("Male", "Female")
colnames(cross.tab) <- c("Jotai", "Keitai")
cross.tab

# クロス集計表での有意差があるかを見るには、フィッシャーとカイ二乗。
# フィッシャーの正確確率検定
fisher.test(cross.tab)
# カイ自乗検定。集計表に小さい値が含まれてたら結果が不正確になる
# correct=FALSEは、「イェーツの連続補正を行わない」という意味。これはクロス集計表の値が小さい時に使う（賛否両論あり）。
chisq.test(cross.tab, correct = FALSE)

# 2×3のクロス集計表の準備
cross.tab.2 <- matrix(c(805, 414, 226, 99, 38, 12), nrow = 2, ncol = 3, byrow = TRUE)
rownames(cross.tab.2) <- c("Correct", "Error")
colnames(cross.tab.2) <- c("Level 1", "Level 2", "Level 3")
cross.tab.2
# 2×3のクロス集計表にフィッシャーの正確確率検定を実行
fisher.test(cross.tab.2)
# フィッシャー検定では、差があることだけしか分からない。表のどこに差があるかまでは分からない。
# だから、2x2以上のクロス表で、どこに違いがあるかを知るには、多重比較という分析する。


# 多重比較とは、全ての組み合わせをフィッシャーすること。
# 1列目と2列目を検定
fisher.test(cross.tab.2[, c(1, 2)])
# 1列目と3列目を検定
fisher.test(cross.tab.2[, c(1, 3)])
# 2列目と3列目を検定
fisher.test(cross.tab.2[, c(2, 3)])
# 同じクロス表で3回フィッシャー検定行うから、有意水準も3で割る。（0.05/3=0.017以下で帰無仮説を棄却。）
# ただし、データ数が多くなると有意水準が低なりすぎて、有意差を見落としてしまう。
# ボンフェロー二補正という。

# 表中の数値を全て10倍
cross.tab.3 <- cross.tab * 10
# 10倍したデータの確認
cross.tab.3
# フィッシャーの正確確率検定
fisher.test(cross.tab.3)


# テキストマイニングでは、サンプルサイズが大きいためp値は小さくなる。
# だから、サンプルサイズに影響を受けない「効果量（effect size）」という指標を使う。
# 一般的なのが、オッズ比。
# オッズ比の計算
(cross.tab[1, 1] / cross.tab[2, 1]) / (cross.tab[1, 2] / cross.tab[2, 2])
# 10倍したデータでオッズ比を計算。サンプルサイズに影響されない。
(cross.tab.3[1, 1] / cross.tab.3[2, 1]) / (cross.tab.3[1, 2] / cross.tab.3[2, 2])



# 追加パッケージのインストール（初回のみ）
# install.packages("vcd", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(vcd)
# オッズ比の計算
oddsratio(cross.tab, log = FALSE)
# オッズ比の信頼区間（下限値，上限値）の計算
confint(oddsratio(cross.tab, log = FALSE))

# オッズ比は2x2限定。
# それ以上は「クラメールのV」
# クラメールのVの計算
V <- assocstats(cross.tab.3)
V

# 追加パッケージのインストール（初回のみ）
# install.packages("RVAideMemoire", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(RVAideMemoire)
# クラメールのVの信頼区間（下限値，上限値）の計算
cramer.test(cross.tab.3)




# 追加パッケージのインストール（初回のみ）
# install.packages("corpora", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(corpora)
# データセットの準備
data(BNCbiber)
head(BNCbiber, 5)
# 相関係数の計算
cor(BNCbiber[, 2], BNCbiber[, 4])
# 散布図の描画
plot(BNCbiber[, 2], BNCbiber[, 4], xlab = "past tense", ylab = "present tense")
# 無相関検定
cor.test(BNCbiber[, 2], BNCbiber[, 4])
# スピアマンの順位相関係数(外れ値の影響を緩和)の計算
cor(BNCbiber[, 2], BNCbiber[, 4], method = "spearman")
# ピアソンの積率相関係数（一般的だが、外れ値の影響を受けやすい）
cor(BNCbiber[, 2 : 4])
# スピアマンの順位相関係数の計算
cor(BNCbiber[, 2 : 4], method = "spearman")





# 追加パッケージのインストール（初回のみ）
# install.packages("psych", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(psych)
# 相関係数が表示された散布図行列の作成
pairs.panels(BNCbiber[, 2 : 4])
# 単回帰分析
lm.result <- lm(BNCbiber[, 2] ~ BNCbiber[, 4])
# 結果の確認
lm.result

# 回帰式の可視化
plot(BNCbiber[, 4], BNCbiber[, 2], xlab = "present tense", ylab = "past tense", pch = 16, col = "grey")
# 回帰直線
abline(lm.result)

# 重回帰分析
lm.result.2 <- lm(BNCbiber[, 2] ~ BNCbiber[, 3] + BNCbiber[, 4])
lm.result.2
# 重回帰分析の注意点
# 変数が多すぎると、多重共線性が起きる。
# 多重共線性とは、2つの説明変数の間に強い相関関係がある時に発生し、偏回帰係数の正負が逆になる場合がある。
# step関数で説明変数を減らす。
# 