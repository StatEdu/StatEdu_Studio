## 1. 测量水平

不要将数字类别代码误当作连续测量值。分析前确定缺失值处理及参考类别；比较结果时应考虑样本差异。

## 2. 描述统计与频数

检查百分比的分母及缺失值处理。检查稀疏期望频数和独立性，区分统计显著性与关联强度。

## 3. 交叉表

列联表应检查期望频数及观测独立性；稀疏表需核实精确检验的适用条件。

## 4. t 检验与 ANOVA

检查独立性、残差分布及方差。ANCOVA 需检查协变量与结果的关系及斜率是否适当。非参数检验并不总是仅比较中位数。

$$
F=\frac{MS_{\mathrm{between}}}{MS_{\mathrm{within}}}
$$

## 5. 非参数检验

根据秩与研究设计使用 Mann–Whitney、Kruskal–Wallis、Wilcoxon 或 Friedman。区分独立与配对数据，检查并列秩、零差值、精确或渐近检验及连续性校正。分布形状不同时不能只解释为位置或中位数差异。多重比较使用指定校正。

## 6. 配对与重复测量

正确配对同一对象的观测。检查球形性等设计假设、校正、交互作用和多重比较。处理重复测量专用菜单不在公开安装包中。

## 7. 相关

相关不代表因果或一致性。不能仅凭高内部一致性认定单维性。应注明 ICC 模型、单次或平均测量、绝对一致或一致性定义。

$$
z=\frac{1}{2}\log\frac{1+r}{1-r}
$$

## 8. 信度与评定者一致性

α 和 ω 衡量信度，不能证明单维性或效度。先检查反向计分及题项间协方差。

$$
\alpha=\frac{k}{k-1}\left(1-\frac{\sum_i\sigma_i^2}{\sigma_{\mathrm{total}}^2}\right)
$$

## 9. 探索性因子分析

EFA 估计共同因子。报告因子数、提取方法和旋转方法，并检查交叉载荷及独特性。

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Psi
$$

## 10. 主成分分析

PCA 将观测方差概括为成分，不等于共同因子模型。变量量纲不同时，标准化会影响结果。

## 11. 线性回归

分层比较使用最终模型的共同完整个案。区分 OLS 变化检验、HC3 Wald 检验及自助法 ΔR² 区间。检查共线性、影响点和残差；注释仅解释已输出的指标。

$$
\hat\beta=(X^\prime X)^{-1}X^\prime y
$$

## 12. 分层回归

分层回归最多支持4个区块。每一步保留前面区块的变量，并加入下一个区块。

- 模型 1: 区块 1.
- 模型 2: 区块 1 + 区块 2.
- 模型 3: 区块 1 + 区块 2 + 区块 3.
- 模型 4: 区块 1 + 区块 2 + 区块 3 + 区块 4.

用相同完整个案比较简约与扩展模型。ΔR²=R²full−R²reduced 表示新增区块的解释力。普通 OLS 的 F 变化检验不同于 HC3 Wald 检验，替换稳健标准误并不会自动使 F 变化检验稳健。自助法应在每个相同重抽样中拟合两个模型。

## 13. 中介与调节效应

简单中介可写为 M=aX+eM、Y=c′X+bM+eY，间接效应为 ab。总效应不显著时 ab 仍可能显著。调节模型 Y=b0+b1X+b2W+b3XW+e 中，X 的条件效应为 b1+b3W。中心化改变 W=0 的含义，并不保证交互作用成立。应在观测范围内解释简单斜率或 Johnson–Neyman 区间。BC 不包含 BCa 的加速度校正。报告变量角色、路径、协变量所在方程、重抽样次数、有效比例和区间方法。

## 14. 逻辑回归

逻辑回归的 exp(B) 为优势比。检查参照类别、完全分离及稀疏单元，勿与风险比混淆。

$$
\log\frac{p}{1-p}=X\beta,\qquad OR_j=\exp(\beta_j)
$$

## 15. 广义线性模型

GLM 需要指定响应分布和链接函数。检查过度离散、偏置项及残差，并按链接尺度解释系数。

$$
g(\mu_i)=x_i^\prime\beta+\mathrm{offset}_i
$$

## 16. 正则化回归

正则化回归在残差损失中加入约束系数大小的项。Ridge 使用 L2，LASSO 使用 L1，Elastic Net 结合两者。λ 控制强度，α 控制 L1 与 L2 的配比。需报告预测变量标准化、相同交叉验证划分、λ.min 或 λ.1se 的选择。变量选择后的系数不能套用预先指定 OLS 的 p 值解释；应检查选择稳定性和外部预测表现。

$$
\frac{1}{2n}\|y-X\beta\|_2^2+\lambda\left[\frac{1-\alpha}{2}\|\beta\|_2^2+\alpha\|\beta\|_1\right]
$$

## 17. 纵向与面板模型

不要将重复观测当作独立个案。区分总体平均效应与个体条件效应，注明相关或随机效应结构及抽样设计。结构方程菜单并不支持上述所有设计。

## 18. 复杂抽样

指定分层、PSU、权重及适用的有限总体校正。Taylor 线性化与重复权重法不同，应遵循数据提供方指南。子群估计应保留抽样设计信息，不能简单删行后计算标准误。报告设计自由度及单 PSU 层的处理。

## 19. 阈值的解释

.05、.30、.70 以及 VIF 的 5、10 不能单独作为通过标准。区分程序警告条件与文献经验规则，并结合样本量、效应量、置信区间、残差图及研究目的。小 p 值不表示效应大或具有临床重要性。

## 20. 警告与跳过结果

不可估计、不收敛、奇异设计矩阵和无效重抽样不等同于不显著。无法解决时应报告被排除的分析、路径及原因。回归重抽样有效比例至少80%为 Adequate，50%至不足80%为 Caution，低于50%或有效次数少于20时暂停区间与 p 值。PLS 推断要求至少80%的完整重抽样有效。

## 21. 保存结果的解释

背景使用透明，不支持透明的格式使用白色。开启非显著路径显示时，有效 p ≥ .05 的路径为虚线，并与无 p 值路径区分。封面跟随 UI 语言；Free 显示徽标及 StatEdu, Institute of Statistics。导出后检查表格方向、换行、注释及成对残差图。

## 22. 方法报告与引用

在正文引用与估计方法及假设相符的文献，报告软件与版本、样本、缺失处理、编码、效应尺度及区间方法。不能以数值一致替代方法引用；使用多个方法时应分别说明来源。

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

## 24. 样本量、功效与效应量

α 为第一类错误率，1−β 为功效，n 为样本量，ρ 为相关，σ 为标准差，Δ 为目标差异。以下保留既有方法说明的计算公式。预先确定效应、分配、脱落率和相关结构。GEE、LMM、GLMM 或 SEM/CFA 的规划近似不等于生成完整数据并逐次重新拟合模型的仿真；复杂设计应进行敏感性分析。

### 24.1 共同符号

$$
n_{\mathrm{recruit}}=\left\lceil \frac{n}{1-\mathrm{dropout}}\right\rceil
$$

### 24.2 t 检验

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

### 24.3 比例

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

### 24.4 卡方

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

### 24.5 相关

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

### 24.8 非参数

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

### 24.10 回归

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

### 24.14 生存 / Cox

$$
\log(HR)
$$

$$
E\approx
\frac{(z_{1-\alpha/2}+z_{1-\beta})^2}
     {p_1p_2[\log(HR)]^2}
$$

### 24.15 等效与非劣效

$$
D_{\mathrm{equiv}}=\Delta-|\hat{\theta}|
$$

$$
D_{\mathrm{NI}}=\Delta+\hat{\theta}
$$

### 24.16 ROC AUC 与诊断准确性

$$
\Delta_{\mathrm{AUC}}=AUC-AUC_0
$$

$$
d\approx\sqrt{2}\,\Phi^{-1}(AUC)
$$

### 24.17 计数与发生率回归

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

### 24.18 整群试验

$$
DE=1+(m-1)ICC
$$

$$
n_{\mathrm{clustered}}\approx n_{\mathrm{individual}}DE
$$

### 24.19 精度与置信区间

$$
n=\left(\frac{z\,SD}{h}\right)^2
$$

$$
n=\frac{z^2p(1-p)}{h^2}
$$

$$
z_r=\operatorname{atanh}(r),\qquad SE_z=\frac{1}{\sqrt{n-3}}
$$

### 24.20 信度与一致性

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

## 25. CFA、SEM、PLS-SEM 方法

根据测量水平选择估计方法。先检查收敛、负方差、过高因子相关及局部失配。不要假定支持部分不变性约束自动释放或全自动题项打包。

目前结构模型适用于独立横断面数据。不要将其解释为多层、复杂抽样或纵向 SEM。组间路径比较前检查测量不变性；调节模型的多组分析存在支持限制。

不要对形成式模型应用 PLSc。完整重抽样有效比例低于 80% 时限制自助法推断。区分效应推断与精确拟合度自助法，检查 MICOM/MGA、交叉验证的条件与警告。

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Theta
$$

## 26. 生存分析的估计目标

S(t) 为到 t 时尚未发生事件的概率，RMST 是到指定 τ 的生存曲线积分。Cox 的 exp(β) 为风险比而非生存概率比。检查独立删失、比例风险及时间起点。竞争风险 CIF、原因别 HR 和亚分布 SHR 的估计目标不同。

## 27. 外部验证的解释

部分指标一致不代表所有分析、数据与推断等价。需统一数据、编码、估计方法、缺失处理及容差。例如 AMOS 的 50 个可比较模型中 49 个一致；SPSS LMM 的 2,895 项中仍有 41 项差异。详细原始数据及内部运行记录不属于公开文档。

一般分析、回归、纵向与生存分析与 SPSS 比较；CFA、SEM 与 AMOS 比较；PLS-SEM、PLSc 及 CB-SEM 与 SmartPLS 比较。这些是纳入 1.3.0 的累计验证，条件及剩余差异汇总于验证页面。

查看当前结果并添加到结果集中。支持 HTML、PDF、Word、Excel；HWPX 仅在韩语界面的累积结果页提供，直接生成而不经 Word 或 Hancom 转换。Word/HWPX 可选择主表、附录表、说明和图形，默认主表。保存使用已捕获的显示结果，不重新计算。Free 图像为300 dpi，开发者版为600 dpi。HTML 包含封面和表目录链接。

## 28. 重要性–表现分析（IPA）

推导重要性是控制其他表现属性的 Pearson 偏相关。修订 IPA 对数变换要求正值，保留负偏相关符号。使用组内共同完整个案或时间间共同配对样本。均值差用 Welch 或配对 t 检验，差为第二−第一。对显示的直接评分有限 p 值做 Holm 校正，95%区间仍为逐项区间。推导重要性与差异使用百分位 bootstrap；有效重复须同时达到80%和50次才显示区间。偏相关差不附 t 检验 p 值。共同参考为按受访人数加权的坐标均值，不是合并样本偏相关。组间比较在 IPA 内设置；拆分文件会分别分析。
