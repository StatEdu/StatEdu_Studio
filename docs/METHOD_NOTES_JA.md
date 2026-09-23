# 方法論ノート — StatEdu Studio 1.3.0

既存の方法論ノートを拡張し、推定原理、仮定、数式、診断と解釈を説明します。

## 目次

- [1. 測定水準](#method-1)
- [2. 記述統計と度数](#method-2)
- [3. クロス集計](#method-3)
- [4. t 検定と ANOVA](#method-4)
- [5. ノンパラメトリック検定](#method-5)
- [6. 対応と反復測定](#method-6)
- [7. 相関](#method-7)
- [8. 信頼性と評価者間一致](#method-8)
- [9. 探索的因子分析](#method-9)
- [10. 主成分分析](#method-10)
- [11. 線形回帰](#method-11)
- [12. 階層的回帰](#method-12)
- [13. 媒介・調整効果](#method-13)
- [14. ロジスティック回帰](#method-14)
- [15. 一般化線形モデル](#method-15)
- [16. 正則化回帰](#method-16)
- [17. 縦断・パネルモデル](#method-17)
- [18. 複雑標本](#method-18)
- [19. 閾値の読み方](#method-19)
- [20. 警告と実行除外](#method-20)
- [21. 保存結果の解釈](#method-21)
- [22. 方法の記載と引用](#method-22)
- [23. 参考文献](#method-23)
- [24. 標本数・検出力・効果量](#method-24)
- [25. CFA・SEM・PLS-SEM の方法論](#method-25)
- [26. 生存時間分析の推定対象](#method-26)
- [27. 外部検証の解釈](#method-27)
- [28. 重要度–遂行度分析（IPA）](#method-28)

<a id="method-1"></a>

## 1. 測定水準

数値のカテゴリコードを連続測定値と誤認しないでください。欠測処理と参照カテゴリを先に決め、結果比較では標本の違いを考慮してください。

<a id="method-2"></a>

## 2. 記述統計と度数

百分率の分母と欠測値の扱いを確認してください。小さい期待度数と独立性を点検し、統計的有意性と関連の強さを区別してください。

<a id="method-3"></a>

## 3. クロス集計

クロス集計は期待度数と独立性を点検し、疎な表では正確検定の適用範囲を確認します。

<a id="method-4"></a>

## 4. t 検定と ANOVA

独立性、残差分布、分散を確認してください。ANCOVA では共変量と結果の関係、傾きの適切性を検討します。ノンパラメトリック検定が常に中央値だけを比較するわけではありません。

$$
F=\frac{MS_{\mathrm{between}}}{MS_{\mathrm{within}}}
$$

<a id="method-5"></a>

## 5. ノンパラメトリック検定

Mann–Whitney、Kruskal–Wallis、Wilcoxon、Friedman はデータの順位と計画に応じて使います。独立群と対応群を区別し、同順位、ゼロ差、正確・漸近検定、連続性補正を確認します。分布形が異なると位置や中央値だけの差とは解釈できません。多重比較には指定した補正を適用します。

<a id="method-6"></a>

## 6. 対応と反復測定

同じ対象の測定を正しく対応付けてください。球面性など計画固有の仮定、補正、交互作用、多重比較を確認します。処置反復測定専用メニューは公開インストーラーから除外されています。

<a id="method-7"></a>

## 7. 相関

相関は因果関係や一致を意味しません。高い内的一貫性だけで一次元性を判断しないでください。ICC のモデル、単一・平均測定、一致・一貫性を明記します。

$$
z=\frac{1}{2}\log\frac{1+r}{1-r}
$$

<a id="method-8"></a>

## 8. 信頼性と評価者間一致

α と ω は信頼性の指標であり、一次元性や妥当性の証明ではありません。逆転項目と項目間共分散を先に確認します。

$$
\alpha=\frac{k}{k-1}\left(1-\frac{\sum_i\sigma_i^2}{\sigma_{\mathrm{total}}^2}\right)
$$

<a id="method-9"></a>

## 9. 探索的因子分析

EFA は共通因子を推定します。因子数、抽出法、回転法を示し、交差負荷と独自性も確認します。

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Psi
$$

<a id="method-10"></a>

## 10. 主成分分析

PCA は観測分散を成分へ要約します。共通因子モデルとは異なり、変数の尺度が異なる場合は標準化が結果を左右します。

<a id="method-11"></a>

## 11. 線形回帰

階層的比較には最終モデルの共通完全ケースを使います。OLS の変化検定、HC3 Wald 検定、ブートストラップ ΔR² 区間を区別します。共線性、影響点、残差を確認し、注釈には表示した統計量だけを説明します。

$$
\hat\beta=(X^\prime X)^{-1}X^\prime y
$$

<a id="method-12"></a>

## 12. 階層的回帰

階層的回帰は最大4ブロックに対応します。各段階では、それまでのブロックの変数を保持して次のブロックを追加します。

- モデル 1: ブロック 1.
- モデル 2: ブロック 1 + ブロック 2.
- モデル 3: ブロック 1 + ブロック 2 + ブロック 3.
- モデル 4: ブロック 1 + ブロック 2 + ブロック 3 + ブロック 4.

同じ完全ケースで縮小モデルと拡張モデルを比較します。ΔR²=R²full−R²reduced は追加ブロックの説明力です。通常の OLS F 変化検定と HC3 Wald 検定は異なり、頑健標準誤差へ置き換えるだけで F 変化検定が頑健になるわけではありません。ブートストラップでは同じ再標本で両モデルを適合させます。

<a id="method-13"></a>

## 13. 媒介・調整効果

単純媒介では M=aX+eM、Y=c′X+bM+eY と表し、間接効果は ab です。総効果が非有意でも ab が有意な場合があります。調整モデル Y=b0+b1X+b2W+b3XW+e の条件付き X 効果は b1+b3W です。中心化は W=0 の意味を変えますが交互作用の存在を保証しません。観測範囲内の単純傾斜や Johnson–Neyman 区間を検討します。BC は BCa の加速度補正を含みません。役割、経路、共変量を入れた式、反復数、有効率、区間法を報告します。

<a id="method-14"></a>

## 14. ロジスティック回帰

ロジスティック回帰の exp(B) はオッズ比です。参照カテゴリ、完全分離、疎なセルを確認し、リスク比と混同しないでください。

$$
\log\frac{p}{1-p}=X\beta,\qquad OR_j=\exp(\beta_j)
$$

<a id="method-15"></a>

## 15. 一般化線形モデル

GLM では応答分布とリンク関数を指定します。過分散、オフセット、残差を確認し、係数をリンク尺度に沿って解釈します。

$$
g(\mu_i)=x_i^\prime\beta+\mathrm{offset}_i
$$

<a id="method-16"></a>

## 16. 正則化回帰

正則化回帰は残差による損失に係数の大きさを抑える項を加えます。Ridge は L2、LASSO は L1、Elastic Net は両者を組み合わせます。λ が強度、α が L1 と L2 の配分を制御します。説明変数の標準化、同一の交差検証分割、λ.min と λ.1se の選択を記載します。変数選択後の係数を事前指定 OLS と同じ p 値で評価せず、選択安定性と外部予測を検討します。

$$
\frac{1}{2n}\|y-X\beta\|_2^2+\lambda\left[\frac{1-\alpha}{2}\|\beta\|_2^2+\alpha\|\beta\|_1\right]
$$

<a id="method-17"></a>

## 17. 縦断・パネルモデル

反復観測を独立ケースとして扱わないでください。母集団平均効果と個体条件付き効果を区別し、相関・ランダム効果構造と標本計画を明記します。構造方程式メニューがすべての計画に対応するわけではありません。

<a id="method-18"></a>

## 18. 複雑標本

層、PSU、重み、必要に応じた有限母集団補正を設計に指定します。Taylor 線形化と複製重み法は同じものではなく、公開データの推奨法を使います。部分集団は設計情報を保ったドメインとして推定し、単に行を除いて標準誤差を計算しないでください。設計自由度と孤立 PSU の処理を報告します。

<a id="method-19"></a>

## 19. 閾値の読み方

.05、.30、.70、VIF の 5・10 などは単独の合否判定ではありません。アプリの警告条件と文献上の経験則を区別し、標本数、効果量、信頼区間、残差図、研究目的を併せて検討します。小さい p は大きい効果や臨床的重要性を意味しません。

<a id="method-20"></a>

## 20. 警告と実行除外

推定不能、非収束、特異な設計行列、無効な再標本は非有意結果ではありません。原因を解決できなければ、除外した分析・経路と理由を報告します。回帰の有効反復率は 80%以上が Adequate、50%以上80%未満が Caution、50%未満または20回未満では区間と p を保留します。PLS の推論には全再標本の80%以上の有効性が必要です。

<a id="method-21"></a>

## 21. 保存結果の解釈

背景は透明、非対応形式では白を使います。非有意表示が有効なら有効な p ≥ .05 の経路を破線にし、p のない経路と区別します。表紙は UI 言語に従い、Free はロゴと StatEdu, Institute of Statistics を表示します。保存後に表の向き、折返し、注釈、残差図の組を確認します。

<a id="method-22"></a>

## 22. 方法の記載と引用

推定法と仮定に対応する文献を本文で引用し、使用ソフト・バージョン、標本、欠測処理、符号化、効果の尺度、区間法を記載します。数値の一致を引用の代わりにせず、複数の手法を使った場合は各手法の出典を示します。

<a id="method-23"></a>

## 23. 参考文献

The implementation and method notes are aligned with standard references and package documentation commonly used for applied statistical reporting:

| Reference | Connected method note |
|---|---|
| Agresti (2013) | Cross-tabulation, logistic regression, categorical GLMM interpretation |
| Bartlett (1954) | Bartlett test in factor analysis and PCA diagnostics |
| Breusch & Pagan (1979) | Regression residual homoscedasticity diagnostics |
| Cronbach (1951) | Cronbach's alpha |
| Curran, West, & Finch (1996) | Normality screening for t-test / ANOVA, correlation, reliability, and factor analysis |
| Durbin & Watson (1950, 1951) | Regression serial-correlation diagnostics |
| Efron & Tibshirani (1993) | Bootstrap inference |
| Fisher (1935) | t-test / ANOVA and general linear model background |
| Friedman, Hastie, & Tibshirani (2010) | `glmnet` regularization path |
| Kaiser (1974) | KMO in factor analysis and PCA |
| Levene (1960) | Equal-variance screening |
| MacKinnon & White (1985) | HC3 robust standard errors |
| Mardia (1970) | Multivariate normality |
| McDonald (1999) | Omega reliability |
| O'Brien (2007) | VIF interpretation |
| Shapiro & Wilk (1965) | Shapiro-Wilk normality test |
| Tibshirani (1996) | LASSO |
| White (1980) | Heteroskedasticity-consistent covariance |
| Zou & Hastie (2005) | Elastic Net |

- Agresti, A. (2013). *Categorical Data Analysis*.
- Altman, D. G., & Bland, J. M. (1983). Measurement in medicine: the analysis of method comparison studies.
- Bland, J. M., & Altman, D. G. (1986). Statistical methods for assessing agreement between two methods of clinical measurement.
- Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences*.
- Faul, F., Erdfelder, E., Lang, A. G., & Buchner, A. (2007). G*Power 3.
- Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*.
- Harrell, F. E. (2015). *Regression Modeling Strategies*.
- Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of sample size calculation for linear and logistic regression.
- Kline, R. B. (2016). *Principles and Practice of Structural Equation Modeling*.
- Kraemer, H. C., & Thiemann, S. (1987). *How Many Subjects?*
- McHugh, M. L. (2012). Interrater reliability: the kappa statistic.
- McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here.
- Mundry, R., & Nunn, C. L. (2009). Stepwise model fitting and statistical inference.
- Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models.
- Rosner, B. (2015). *Fundamentals of Biostatistics*.
- Tabachnick, B. G., & Fidell, L. S. (2019). *Using Multivariate Statistics*.
- West, S. G., Finch, J. F., & Curran, P. J. (1995). Structural equation models with nonnormal variables.
- Wilcox, R. R. (2017). *Introduction to Robust Estimation and Hypothesis Testing*.

<a id="method-24"></a>

## 24. 標本数・検出力・効果量

α は第一種過誤率、1−β は検出力、n は標本数、ρ は相関、σ は標準偏差、Δ は検出したい差です。以下は既存ノートの計算式を保持したものです。効果、配分、脱落率、相関構造を事前に定めます。GEE・LMM・GLMM や SEM/CFA の計画近似は、実データを生成して毎回モデルを再適合する完全なシミュレーションとは区別します。複雑な設計では感度分析を併用します。

### 24.1 共通記号

$$
n_{\mathrm{recruit}}=\left\lceil \frac{n}{1-\mathrm{dropout}}\right\rceil
$$

### 24.2 t 検定

$$
d=\frac{\bar{x}_1-\bar{x}_2}{s_p}
$$

$$
s_p=\sqrt{\frac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}}
$$

$$
g=Jd,\qquad J=1-\frac{3}{4df-1}
$$

$$
d_z=\frac{\bar{x}_D}{s_D}
$$

### 24.3 比率

$$
RD=p_1-p_2
$$

$$
RR=\frac{p_1}{p_2}
$$

$$
OR=\frac{p_1/(1-p_1)}{p_2/(1-p_2)}
$$

$$
h=2\arcsin(\sqrt{p_1})-2\arcsin(\sqrt{p_2})
$$

### 24.4 カイ二乗

$$
w=\sqrt{\sum_i \frac{(p_i-p_{0i})^2}{p_{0i}}}
$$

$$
\lambda=Nw^2
$$

$$
\phi=\sqrt{\frac{\chi^2}{N}}
$$

$$
V=\sqrt{\frac{\chi^2}{N\min(r-1,c-1)}}
$$

### 24.5 相関

$$
r=\operatorname{sign}(t)\sqrt{\frac{t^2}{t^2+df}}
$$

$$
r=\sqrt{\frac{F}{F+df_{\mathrm{error}}}}
$$

$$
z_r=\operatorname{atanh}(r)=\frac{1}{2}\log\left(\frac{1+r}{1-r}\right)
$$

$$
SE_z=\frac{1}{\sqrt{n-3}}
$$

$$
q=z_{r1}-z_{r2}
$$

### 24.6 ANOVA

$$
f=\sqrt{\frac{\eta^2}{1-\eta^2}}
$$

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

$$
\eta_p^2=\frac{F\,df_{\mathrm{effect}}}{F\,df_{\mathrm{effect}}+df_{\mathrm{error}}}
$$

$$
\omega_p^2\approx
\frac{F\,df_{\mathrm{effect}}-df_{\mathrm{effect}}}
     {F\,df_{\mathrm{effect}}+df_{\mathrm{error}}+1}
$$

### 24.7 ANCOVA / MANOVA

$$
f_{\mathrm{adjusted}}=\frac{f}{\sqrt{1-R^2_{\mathrm{covariates}}}}
$$

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

$$
f^2=\frac{V}{1-V},\qquad f=\sqrt{f^2}
$$

$$
\eta^2=1-\Lambda^{1/s},\qquad
f^2=\frac{\eta^2}{1-\eta^2}
$$

### 24.8 ノンパラメトリック

$$
r_{\mathrm{rb}}=1-\frac{2U}{n_1n_2}
$$

$$
\delta=P(X>Y)-P(X<Y)
$$

$$
\varepsilon^2=\frac{H(N+1)}{N^2-1}
$$

$$
W=\frac{\chi^2_F}{N(k-1)}
$$

### 24.9 McNemar

$$
OR=\frac{p_{01}}{p_{10}}
$$

$$
OR=\frac{b}{c},\qquad \log(OR)=\log(b)-\log(c)
$$

$$
g=\left|p_{01}-p_{10}\right|
$$

### 24.10 回帰

$$
f^2=\frac{R^2}{1-R^2}
$$

$$
f^2=\frac{R^2_{\mathrm{full}}-R^2_{\mathrm{reduced}}}{1-R^2_{\mathrm{full}}}
$$

$$
f^2=\frac{\Delta R^2}{1-\Delta R^2}
$$

$$
d\approx\frac{\log(OR)\sqrt{3}}{\pi}
$$

$$
ab=\beta_a\beta_b
$$

### 24.11 GEE

$$
OR_{\mathrm{GEE}}=\exp(B)
$$

$$
IRR_{\mathrm{GEE}}=\exp(B)
$$

$$
h=2\arcsin(\sqrt{p_1})-2\arcsin(\sqrt{p_2})
$$

### 24.12 LMM

$$
d_{\mathrm{LMM}}=\frac{B}{SD_{\mathrm{residual}}}
$$

$$
\eta_p^2=
\frac{F\cdot df_{\mathrm{effect}}}
     {F\cdot df_{\mathrm{effect}}+df_{\mathrm{error}}}
$$

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

$$
d_z=\frac{\bar{x}_1-\bar{x}_2}
{\sqrt{s_1^2+s_2^2-2\,\mathrm{cov}_{12}}}
$$

$$
y_{gij}=d_{\mathrm{LMM}}g_it_j+b_i+\epsilon_{gij}
$$

$$
\widehat{\mathrm{power}}=\frac{1}{S}\sum_{s=1}^{S}I(p_s<\alpha)
$$

### 24.13 GLMM

$$
OR_{\mathrm{GLMM}}=\exp(B),\qquad B=\log(OR_{\mathrm{GLMM}})
$$

$$
d_{\mathrm{latent}}\approx\frac{B\sqrt{3}}{\pi}
=\frac{\log(OR)\sqrt{3}}{\pi}
$$

$$
IRR_{\mathrm{GLMM}}=\exp(B),\qquad B=\log(IRR_{\mathrm{GLMM}})
$$

$$
d_{\mathrm{GLMM}}=\frac{B}{SD_{\mathrm{residual}}}
$$

### 24.14 生存時間 / Cox

$$
\log(HR)
$$

$$
E\approx
\frac{(z_{1-\alpha/2}+z_{1-\beta})^2}
     {p_1p_2[\log(HR)]^2}
$$

### 24.15 同等性・非劣性

$$
D_{\mathrm{equiv}}=\Delta-|\hat{\theta}|
$$

$$
D_{\mathrm{NI}}=\Delta+\hat{\theta}
$$

### 24.16 ROC AUC と診断精度

$$
\Delta_{\mathrm{AUC}}=AUC-AUC_0
$$

$$
d\approx\sqrt{2}\,\Phi^{-1}(AUC)
$$

### 24.17 件数・率の回帰

$$
IRR=\exp(B),\qquad B=\log(IRR)
$$

$$
\operatorname{Var}(Y)=\mu
$$

$$
\operatorname{Var}(Y)=\mu+\alpha\mu^2
$$

$$
RR_{\mathrm{rate}}=\frac{\lambda_1}{\lambda_2}
$$

### 24.18 クラスタ試験

$$
DE=1+(m-1)ICC
$$

$$
n_{\mathrm{clustered}}\approx n_{\mathrm{individual}}DE
$$

### 24.19 精度・信頼区間

$$
n=\left(\frac{z\,SD}{h}\right)^2
$$

$$
n=\frac{z^2p(1-p)}{h^2}
$$

$$
z_r=\operatorname{atanh}(r),\qquad SE_z=\frac{1}{\sqrt{n-3}}
$$

### 24.20 信頼性・一致度

$$
\log(1-\alpha)
$$

$$
n\ge items+1
$$

### 24.21 SEM / CFA

$$
\lambda=(N-1)\,df\,RMSEA^2
$$

$$
\Delta_{\mathrm{RMSEA}}=RMSEA_A-RMSEA_0
$$

$$
z=\operatorname{atanh}(\theta)
$$

$$
M_{\mathrm{obs}}=\frac{p(p+1)}{2}
$$

$$
q\approx 2p+k+s
$$

$$
df\approx M_{\mathrm{obs}}-q
$$

<a id="method-25"></a>

## 25. CFA・SEM・PLS-SEM の方法論

測定水準に合う推定法を使います。収束、負の分散、大きな因子間相関、局所的不適合を先に検討します。部分不変性の制約の自動解除や完全自動の項目パーセリングを想定しないでください。

現在の構造モデルは独立した横断データを対象とします。マルチレベル・複雑標本・縦断 SEM と解釈しないでください。群間経路比較の前に測定不変性を検討します。調整モデルの多母集団分析には制限があります。

形成型モデルに PLSc を適用しないでください。完全な再標本の有効率が 80% 未満では推論を制限します。効果推論と厳密適合度ブートストラップを区別し、MICOM/MGA、交差検証の条件と警告を確認します。

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Theta
$$

<a id="method-26"></a>

## 26. 生存時間分析の推定対象

S(t) は時点 t までイベントがない確率、RMST は指定した τ までの S(t) の積分です。Cox の exp(β) はハザード比で生存確率比ではありません。打切りの独立性、比例ハザード性、時間原点を確認します。競合リスクの CIF、原因別 HR、部分分布 SHR は推定対象が異なります。

<a id="method-27"></a>

## 27. 外部検証の解釈

一部指標の一致は全分析・データ・推論の同等性を意味しません。データ、符号化、推定法、欠測処理、許容差を揃えて比較します。AMOS は比較可能な 50 モデル中 49 が一致し、SPSS LMM は 2,895 項目中 41 に差が残りました。詳細な原資料や内部実行記録は公開文書に含めません。

一般分析・回帰・縦断・生存時間分析を SPSS、CFA・SEM を AMOS、PLS-SEM・PLSc と CB-SEM を SmartPLS と比較しました。1.3.0 に反映した累積検証で、条件と残る差は検証ページに要約します。

現在の結果を確認し、結果追加で蓄積します。HTML・PDF・Word・Excel に保存できます。HWPX は韓国語 UI の累積結果画面のみで提供し、Word や Hancom を介さず直接作成します。Word/HWPX は主表・付録表・説明・図を選択でき、初期選択は主表です。保存時に再分析せず表示結果を使用します。画像は Free 300 dpi、開発者版 600 dpi です。HTML は表紙と表一覧リンクを含みます。

<a id="method-28"></a>

## 28. 重要度–遂行度分析（IPA）

導出重要度は他の遂行度属性を調整した Pearson 偏相関です。修正版の対数変換は正値を要求し、負の符号を保持します。群内共通完全例または時点間共通対応標本を用います。平均差は Welch または対応 t 検定で、差は後者−前者です。直接得点の表示された有限 p 値に Holm 補正を適用し、95%区間は個別区間です。導出重要度と差は百分位 bootstrap を使い、区間表示には有効反復80%以上かつ50回以上が必要です。偏相関差に t 検定 p 値は付けません。共通基準は回答者数で重み付けした座標平均で、併合標本の偏相関ではありません。群間比較は IPA 内で指定し、データ分割による別々の分析と区別します。
