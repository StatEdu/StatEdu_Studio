# Ghi chú phương pháp — StatEdu Studio 1.3.0

Mở rộng ghi chú phương pháp gốc: nguyên lý ước lượng, giả định, công thức, chẩn đoán và diễn giải.

## Mục lục

- [1. Mức đo](#method-1)
- [2. Thống kê mô tả và tần số](#method-2)
- [3. Bảng chéo](#method-3)
- [4. Kiểm định t và ANOVA](#method-4)
- [5. Kiểm định phi tham số](#method-5)
- [6. Dữ liệu ghép cặp và đo lặp](#method-6)
- [7. Tương quan](#method-7)
- [8. Độ tin cậy và đồng thuận](#method-8)
- [9. Phân tích nhân tố khám phá](#method-9)
- [10. Thành phần chính](#method-10)
- [11. Hồi quy tuyến tính](#method-11)
- [12. Hồi quy phân cấp](#method-12)
- [13. Trung gian và điều tiết](#method-13)
- [14. Hồi quy logistic](#method-14)
- [15. Mô hình tuyến tính tổng quát](#method-15)
- [16. Hồi quy điều chuẩn](#method-16)
- [17. Mô hình dọc và bảng](#method-17)
- [18. Khảo sát phức tạp](#method-18)
- [19. Diễn giải ngưỡng](#method-19)
- [20. Cảnh báo và kết quả bỏ qua](#method-20)
- [21. Diễn giải kết quả lưu](#method-21)
- [22. Báo cáo phương pháp và trích dẫn](#method-22)
- [23. Tài liệu tham khảo](#method-23)
- [24. Cỡ mẫu, lực kiểm định và hiệu ứng](#method-24)
- [25. Phương pháp CFA, SEM và PLS-SEM](#method-25)
- [26. Đại lượng đích sống còn](#method-26)
- [27. Diễn giải xác minh bên ngoài](#method-27)
- [28. Phân tích tầm quan trọng–mức thực hiện (IPA)](#method-28)

<a id="method-1"></a>

## 1. Mức đo

Không nhầm mã nhóm dạng số với số đo liên tục. Xác định cách xử lý thiếu và nhóm tham chiếu trước phân tích; cân nhắc khác biệt mẫu khi so sánh kết quả.

<a id="method-2"></a>

## 2. Thống kê mô tả và tần số

Kiểm tra mẫu số của tỷ lệ và cách xử lý thiếu. Xem tần số kỳ vọng nhỏ và tính độc lập; phân biệt ý nghĩa thống kê với độ mạnh liên hệ.

<a id="method-3"></a>

## 3. Bảng chéo

Với bảng chéo, kiểm tra tần số kỳ vọng và tính độc lập; bảng thưa cần xem điều kiện áp dụng kiểm định chính xác.

<a id="method-4"></a>

## 4. Kiểm định t và ANOVA

Kiểm tra tính độc lập, phân phối phần dư và phương sai. ANCOVA cần quan hệ đồng biến–kết quả và độ dốc phù hợp. Kiểm định phi tham số không phải lúc nào cũng chỉ so sánh trung vị.

$$
F=\frac{MS_{\mathrm{between}}}{MS_{\mathrm{within}}}
$$

<a id="method-5"></a>

## 5. Kiểm định phi tham số

Chọn Mann–Whitney, Kruskal–Wallis, Wilcoxon hoặc Friedman theo thứ hạng và thiết kế. Phân biệt nhóm độc lập, ghép cặp; kiểm tra hạng bằng nhau, chênh lệch bằng không, kiểm định chính xác/tiệm cận và hiệu chỉnh liên tục. Khi hình dạng phân phối khác nhau, không chỉ diễn giải khác biệt trung vị. Dùng hiệu chỉnh đã chọn cho so sánh nhiều lần.

<a id="method-6"></a>

## 6. Dữ liệu ghép cặp và đo lặp

Ghép đúng quan sát của cùng một người. Kiểm tra giả định theo thiết kế, gồm tính cầu, hiệu chỉnh, tương tác và so sánh nhiều lần. Menu riêng về can thiệp đo lặp không có trong bộ cài công khai.

<a id="method-7"></a>

## 7. Tương quan

Tương quan không chứng minh nhân quả hay đồng thuận. Nhất quán nội tại cao không tự chứng minh tính đơn hướng. Nêu mô hình ICC, phép đo đơn/trung bình và đồng thuận/nhất quán.

$$
z=\frac{1}{2}\log\frac{1+r}{1-r}
$$

<a id="method-8"></a>

## 8. Độ tin cậy và đồng thuận

α và ω đo độ tin cậy, không chứng minh tính đơn hướng hay giá trị đo lường. Kiểm tra câu đảo chiều và hiệp phương sai giữa các mục.

$$
\alpha=\frac{k}{k-1}\left(1-\frac{\sum_i\sigma_i^2}{\sigma_{\mathrm{total}}^2}\right)
$$

<a id="method-9"></a>

## 9. Phân tích nhân tố khám phá

EFA ước lượng nhân tố chung. Báo cáo số nhân tố, phương pháp trích và xoay; kiểm tra tải chéo và phương sai riêng.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Psi
$$

<a id="method-10"></a>

## 10. Thành phần chính

PCA tóm tắt phương sai quan sát thành các thành phần, khác mô hình nhân tố chung. Chuẩn hóa ảnh hưởng kết quả khi thang đo khác nhau.

<a id="method-11"></a>

## 11. Hồi quy tuyến tính

So sánh phân cấp dùng các trường hợp đầy đủ chung của mô hình cuối. Phân biệt kiểm định thay đổi OLS, Wald HC3 và khoảng bootstrap ΔR². Xem đa cộng tuyến, quan sát ảnh hưởng và phần dư; chú thích chỉ giải thích chỉ số được hiển thị.

$$
\hat\beta=(X^\prime X)^{-1}X^\prime y
$$

<a id="method-12"></a>

## 12. Hồi quy phân cấp

Hồi quy phân cấp hỗ trợ tối đa bốn khối. Mỗi bước giữ lại các biến từ những khối trước và thêm khối tiếp theo.

- Mô hình 1: Khối 1.
- Mô hình 2: Khối 1 + Khối 2.
- Mô hình 3: Khối 1 + Khối 2 + Khối 3.
- Mô hình 4: Khối 1 + Khối 2 + Khối 3 + Khối 4.

So sánh mô hình rút gọn và đầy đủ trên cùng trường hợp đầy đủ. ΔR²=R²full−R²reduced là đóng góp khối thêm. Kiểm định F thay đổi OLS khác Wald HC3; chỉ thay sai số chuẩn vững không làm F thay đổi trở nên vững. Bootstrap cần khớp cả hai mô hình trên cùng mỗi mẫu lặp.

<a id="method-13"></a>

## 13. Trung gian và điều tiết

Trung gian đơn giản là M=aX+eM, Y=c′X+bM+eY; hiệu ứng gián tiếp ab có thể có ý nghĩa khi tổng hiệu ứng không có. Trong Y=b0+b1X+b2W+b3XW+e, hiệu ứng X có điều kiện là b1+b3W. Trừ trung bình thay đổi ý nghĩa W=0 nhưng không bảo đảm tương tác. Diễn giải độ dốc đơn hoặc Johnson–Neyman trong phạm vi quan sát. BC không gồm hiệu chỉnh gia tốc BCa. Báo cáo vai trò, đường dẫn, phương trình có đồng biến, số lần yêu cầu/hợp lệ và phương pháp khoảng.

<a id="method-14"></a>

## 14. Hồi quy logistic

Trong hồi quy logistic, exp(B) là tỷ số odds. Kiểm tra nhóm tham chiếu, phân tách và ô thưa; không nhầm với tỷ số nguy cơ.

$$
\log\frac{p}{1-p}=X\beta,\qquad OR_j=\exp(\beta_j)
$$

<a id="method-15"></a>

## 15. Mô hình tuyến tính tổng quát

GLM cần phân phối đáp ứng và hàm liên kết. Kiểm tra quá phân tán, offset và phần dư; diễn giải hệ số trên thang liên kết.

$$
g(\mu_i)=x_i^\prime\beta+\mathrm{offset}_i
$$

<a id="method-16"></a>

## 16. Hồi quy điều chuẩn

Hồi quy điều chuẩn thêm vào hàm mất mát phần dư một thành phần giới hạn hệ số. Ridge dùng L2, LASSO dùng L1, Elastic Net kết hợp cả hai. λ kiểm soát mức độ, α kiểm soát tỷ lệ phối hợp. Báo cáo chuẩn hóa, cùng phân chia kiểm định chéo và lựa chọn λ.min/λ.1se. Sau chọn biến, không tự áp dụng p thông thường của OLS định trước; kiểm tra độ ổn định chọn biến và dự báo bên ngoài.

$$
\frac{1}{2n}\|y-X\beta\|_2^2+\lambda\left[\frac{1-\alpha}{2}\|\beta\|_2^2+\alpha\|\beta\|_1\right]
$$

<a id="method-17"></a>

## 17. Mô hình dọc và bảng

Không coi quan sát lặp là trường hợp độc lập. Phân biệt hiệu ứng trung bình quần thể với hiệu ứng có điều kiện theo cá thể; nêu cấu trúc tương quan/hiệu ứng ngẫu nhiên và thiết kế. Menu phương trình cấu trúc không hỗ trợ mọi thiết kế này.

<a id="method-18"></a>

## 18. Khảo sát phức tạp

Chỉ định tầng, PSU, trọng số và hiệu chỉnh quần thể hữu hạn nếu phù hợp. Tuyến tính hóa Taylor khác trọng số lặp; theo hướng dẫn nhà cung cấp. Ước lượng nhóm con như miền giữ thông tin thiết kế, không chỉ xóa dòng trước tính sai số chuẩn. Báo cáo bậc tự do thiết kế và xử lý tầng chỉ có một PSU.

<a id="method-19"></a>

## 19. Diễn giải ngưỡng

.05, .30, .70 và VIF 5, 10 không phải ngưỡng đạt chung. Phân biệt cảnh báo phần mềm với quy tắc kinh nghiệm; xem cỡ mẫu, hiệu ứng, khoảng, phần dư và mục tiêu nghiên cứu. p nhỏ không có nghĩa hiệu ứng lớn hay quan trọng lâm sàng.

<a id="method-20"></a>

## 20. Cảnh báo và kết quả bỏ qua

Không ước lượng được, không hội tụ, thiết kế suy biến và mẫu lặp không hợp lệ không có nghĩa không có ý nghĩa. Báo cáo phân tích/đường bị bỏ và lý do nếu chưa giải quyết. Trong hồi quy, ≥80% hợp lệ là Adequate, 50–<80% là Caution; <50% hoặc dưới20 lần hợp lệ thì không xuất khoảng và p. PLS yêu cầu ≥80% mẫu lặp đầy đủ hợp lệ.

<a id="method-21"></a>

## 21. Diễn giải kết quả lưu

Nền trong suốt, hoặc trắng nếu định dạng không hỗ trợ. Khi bật hiển thị không có ý nghĩa, đường có p hợp lệ ≥ .05 dùng nét đứt, khác đường không có p. Bìa theo ngôn ngữ UI; Free có logo và StatEdu, Institute of Statistics. Kiểm tra hướng bảng, xuống dòng, chú thích và cặp hình phần dư sau khi lưu.

<a id="method-22"></a>

## 22. Báo cáo phương pháp và trích dẫn

Trích dẫn nguồn phù hợp ước lượng và giả định; nêu phần mềm/phiên bản, mẫu, thiếu, mã hóa, thang hiệu ứng và khoảng. Khớp số không thay thế nguồn phương pháp. Trích dẫn riêng từng phương pháp đã dùng.

<a id="method-23"></a>

## 23. Tài liệu tham khảo

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

## 24. Cỡ mẫu, lực kiểm định và hiệu ứng

α là sai lầm loại I, 1−β là lực kiểm định, n là cỡ mẫu, ρ là tương quan, σ là độ lệch chuẩn, Δ là chênh lệch mục tiêu. Bên dưới giữ các công thức từ ghi chú trước. Định trước hiệu ứng, phân bổ, bỏ cuộc và tương quan. Xấp xỉ hoạch định GEE/LMM/GLMM hoặc SEM/CFA không phải mô phỏng dữ liệu đầy đủ rồi khớp lại từng mô hình; thiết kế phức tạp cần phân tích độ nhạy.

### 24.1 Ký hiệu chung

$$
n_{\mathrm{recruit}}=\left\lceil \frac{n}{1-\mathrm{dropout}}\right\rceil
$$

### 24.2 Kiểm định t

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

### 24.3 Tỷ lệ

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

### 24.4 Chi bình phương

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

### 24.5 Tương quan

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

### 24.8 Phi tham số

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

### 24.10 Hồi quy

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

### 24.14 Sống còn / Cox

$$
\log(HR)
$$

$$
E\approx
\frac{(z_{1-\alpha/2}+z_{1-\beta})^2}
     {p_1p_2[\log(HR)]^2}
$$

### 24.15 Tương đương và không kém hơn

$$
D_{\mathrm{equiv}}=\Delta-|\hat{\theta}|
$$

$$
D_{\mathrm{NI}}=\Delta+\hat{\theta}
$$

### 24.16 ROC AUC và độ chính xác chẩn đoán

$$
\Delta_{\mathrm{AUC}}=AUC-AUC_0
$$

$$
d\approx\sqrt{2}\,\Phi^{-1}(AUC)
$$

### 24.17 Hồi quy số đếm và tỷ suất

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

### 24.18 Thử nghiệm cụm

$$
DE=1+(m-1)ICC
$$

$$
n_{\mathrm{clustered}}\approx n_{\mathrm{individual}}DE
$$

### 24.19 Độ chính xác và khoảng

$$
n=\left(\frac{z\,SD}{h}\right)^2
$$

$$
n=\frac{z^2p(1-p)}{h^2}
$$

$$
z_r=\operatorname{atanh}(r),\qquad SE_z=\frac{1}{\sqrt{n-3}}
$$

### 24.20 Độ tin cậy và đồng thuận

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

## 25. Phương pháp CFA, SEM và PLS-SEM

Chọn ước lượng theo mức đo. Kiểm tra hội tụ, phương sai âm, tương quan nhân tố lớn và sai lệch cục bộ trước. Không mặc định có tự động bỏ ràng buộc bất biến từng phần hay tự động gộp mục hoàn toàn.

Mô hình cấu trúc hiện tại dùng dữ liệu cắt ngang độc lập. Không diễn giải thành SEM đa cấp, khảo sát phức tạp hay dọc. Kiểm tra bất biến đo lường trước so sánh đường dẫn giữa nhóm; mô hình điều tiết đa nhóm có giới hạn hỗ trợ.

Không áp dụng PLSc cho mô hình hình thành. Suy luận bootstrap bị hạn chế khi dưới 80% mẫu lặp đầy đủ hợp lệ. Phân biệt suy luận hiệu ứng với bootstrap độ phù hợp chính xác; kiểm tra điều kiện, cảnh báo MICOM/MGA và kiểm định chéo.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Theta
$$

<a id="method-26"></a>

## 26. Đại lượng đích sống còn

S(t) là xác suất chưa có sự kiện đến t; RMST tích phân S(t) đến τ. exp(β) của Cox là tỷ số nguy cơ tức thời, không phải tỷ số xác suất sống. Kiểm tra kiểm duyệt độc lập, nguy cơ tỷ lệ và gốc thời gian. CIF, HR theo nguyên nhân và SHR phân phối phụ có đích ước lượng khác nhau.

<a id="method-27"></a>

## 27. Diễn giải xác minh bên ngoài

Khớp một số chỉ số không chứng minh tương đương mọi phân tích. Cần thống nhất dữ liệu, mã hóa, ước lượng, xử lý thiếu và dung sai. Ví dụ 49 trong 50 mô hình AMOS có thể so sánh khớp; 41 trong 2,895 giá trị LMM của SPSS còn khác biệt. Dữ liệu gốc chi tiết và nhật ký nội bộ không thuộc tài liệu công khai.

Đã so sánh phân tích tổng quát, hồi quy, dọc và sống còn với SPSS; CFA/SEM với AMOS; PLS-SEM/PLSc và CB-SEM với SmartPLS. Đây là xác minh tích lũy được đưa vào 1.3.0; trang Xác minh tóm tắt điều kiện và khác biệt còn lại.

Xem kết quả và thêm vào bộ kết quả tích lũy. Có HTML, PDF, Word và Excel. HWPX chỉ có ở màn hình kết quả tích lũy với giao diện Hàn và được ghi trực tiếp, không qua Word/Hancom. Word/HWPX cho chọn bảng chính, phụ lục, giải thích và hình; mặc định là bảng chính. Lưu bản hiển thị đã ghi nhận, không tính lại. Free dùng 300 dpi, bản phát triển 600 dpi. HTML có trang bìa và danh sách bảng liên kết.

<a id="method-28"></a>

## 28. Phân tích tầm quan trọng–mức thực hiện (IPA)

Tầm quan trọng suy ra là tương quan riêng Pearson kiểm soát các thuộc tính thực hiện khác. Logarit IPA sửa đổi yêu cầu giá trị dương và giữ dấu âm. Dùng các trường hợp đầy đủ chung trong nhóm hoặc mẫu cặp chung qua thời điểm. Chênh lệch thứ hai trừ thứ nhất dùng Welch hoặc t cặp. Holm điều chỉnh p hữu hạn hiển thị của điểm trực tiếp; khoảng 95% vẫn riêng lẻ. Tầm quan trọng suy ra và chênh lệch dùng bootstrap phân vị, cần ít nhất 80% và 50 lần hợp lệ; không gán p kiểm định t cho chênh lệch tương quan riêng. Tham chiếu chung là trung bình tọa độ theo trọng số số người, không phải tương quan riêng mẫu gộp. Chọn nhóm trong IPA để so sánh; chia tệp tạo phân tích riêng.
