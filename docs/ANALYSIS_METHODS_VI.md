# Phân tích — StatEdu Studio 1.3.0

Phạm vi phân tích và đầu ra của StatEdu Studio 1.3.0 bản công khai.

## Mục lục

1. [Phạm vi công khai của 1.3.0](#scope)
2. [Chuẩn bị dữ liệu](#data)
3. [Tần số, mô tả và bảng chéo](#descriptive)
4. [So sánh nhóm và ANCOVA](#group)
5. [Đo ghép cặp và đo lặp hỗn hợp](#paired)
6. [Tương quan, độ tin cậy và đồng thuận](#correlation)
7. [Phân tích nhân tố khám phá và PCA](#factor)
8. [Phân tích tầm quan trọng–mức thực hiện (IPA)](#ipa)
9. [Hồi quy và hồi quy phân cấp](#regression)
10. [Hiệu ứng trung gian và điều tiết](#mediation)
11. [Hồi quy logistic, GLM và có phạt](#generalized)
12. [Dữ liệu dọc, bảng và khảo sát](#longitudinal)
13. [Phân tích sống còn](#survival)
14. [Phân tích nhân tố khẳng định (CFA)](#cfa)
15. [Mô hình phương trình cấu trúc (SEM)](#sem)
16. [PLS-SEM và PLSc](#pls)
17. [Cỡ mẫu, lực kiểm định và cỡ hiệu ứng](#planning)
18. [Bảng, hình và xuất kết quả](#reporting)
19. [Xác minh và giới hạn báo cáo](#validation)

<a id="scope"></a>

## 1. Phạm vi công khai của 1.3.0

CFA, SEM, PLS-SEM và hiệu ứng trung gian/điều tiết có trong menu công khai. Bộ cài công khai không gồm phân tích tổng hợp và ANOVA các can thiệp đo lặp trên cùng đối tượng. Pro được dự kiến cho phiên bản sau.

<a id="data"></a>

## 2. Chuẩn bị dữ liệu

Nhập SPSS, SAS, Stata, Excel, CSV và DAT; kiểm tra tên, nhãn, mức đo, nhóm và giá trị tham chiếu. Xem cỡ mẫu thực dùng và cách xử lý dữ liệu thiếu của từng phân tích.

<a id="descriptive"></a>

## 3. Tần số, mô tả và bảng chéo

Xuất tần số, tỷ lệ phần trăm, thống kê vị trí và độ phân tán, bảng liên hợp. Các tùy chọn cung cấp kiểm định liên hệ, cỡ hiệu ứng và chẩn đoán.

<a id="group"></a>

## 4. So sánh nhóm và ANCOVA

Dùng kiểm định t độc lập, ANOVA, ANCOVA và so sánh nhóm phi tham số. Tùy chọn hỗ trợ phương pháp theo phương sai, so sánh hậu nghiệm và cỡ hiệu ứng.

<a id="paired"></a>

## 5. Đo ghép cặp và đo lặp hỗn hợp

Dùng phân tích ghép cặp, đo lặp, ANOVA hỗn hợp và phân tích ghép cặp phi tham số. Xuất hiệu ứng thời gian, nhóm và so sánh theo thiết kế đã chọn.

<a id="correlation"></a>

## 6. Tương quan, độ tin cậy và đồng thuận

Phân tích tương quan, độ tin cậy thang đo và đồng thuận giữa người đánh giá. Chọn phương pháp và chỉ số phù hợp loại biến và thiết kế đánh giá.

<a id="factor"></a>

## 7. Phân tích nhân tố khám phá và PCA

Xem số nhân tố/thành phần, trích xuất, xoay, tải và phương sai giải thích trong EFA và PCA. CFA được mô tả riêng.

<a id="ipa"></a>

## 8. Phân tích tầm quan trọng–mức thực hiện (IPA)

Mở Phân tích → IPA, chọn đánh giá trực tiếp hoặc tầm quan trọng suy ra. Ghép biến quan trọng và thực hiện theo cùng thứ tự thuộc tính, hoặc chọn thực hiện và hài lòng chung. Chọn toàn bộ, nhóm độc lập hoặc cặp trước/sau. Dùng cột WIDE tương ứng hoặc ID/thời gian LONG và hai giá trị thời gian. Chọn đường tham chiếu, biểu đồ; kiểm tra số mẫu, tọa độ, khoảng và chênh lệch. Lưu Word/HWPX từ kết quả tích lũy sau khi thêm.

<a id="regression"></a>

## 9. Hồi quy và hồi quy phân cấp

Dùng OLS, suy luận vững HC3 và hồi quy bootstrap. Phân tích phân cấp so sánh các khối liên tiếp và thay đổi phương sai giải thích. Đầu ra tùy chọn gồm sr², f², đa cộng tuyến và chẩn đoán phần dư. Hồi quy phân cấp hỗ trợ tối đa bốn khối. Mỗi bước giữ lại các biến từ những khối trước và thêm khối tiếp theo.

<a id="mediation"></a>

## 10. Hiệu ứng trung gian và điều tiết

Gán vai trò biến dự báo, kết quả, trung gian, điều tiết và đồng biến, rồi vẽ đường dẫn để ước lượng hiệu ứng trực tiếp, gián tiếp, tổng và có điều kiện. Không chọn theo số mô hình. Cấu trúc không hỗ trợ được kiểm tra trước khi chạy.

<a id="generalized"></a>

## 11. Hồi quy logistic, GLM và có phạt

Dùng mô hình logistic, tuyến tính tổng quát, Ridge, LASSO và Elastic Net. Báo cáo hệ số, hiệu năng cùng thang kết quả, họ phân phối, hàm liên kết và thiết lập kiểm định chéo.

<a id="longitudinal"></a>

## 12. Dữ liệu dọc, bảng và khảo sát

Thiết kế hỗ trợ gồm GEE, LMM, GLMM, hiệu ứng cố định/ngẫu nhiên và mẫu khảo sát phức tạp. Kiểm tra mã cá thể/cụm, thời gian, trọng số, tầng và cụm theo phân tích.

<a id="survival"></a>

## 13. Phân tích sống còn

Dùng Kaplan–Meier, log-rank, RMST, Cox và phân tích nguy cơ cạnh tranh được hỗ trợ. Kiểm tra định dạng và mã sự kiện trước; tổ hợp không hỗ trợ bị hạn chế.

<a id="cfa"></a>

## 14. Phân tích nhân tố khẳng định (CFA)

Dùng ML, MLR và WLSMV/DWLS cho dữ liệu thứ bậc; xem tải, độ phù hợp, độ tin cậy, AVE, HTMT và so sánh bất biến đo lường được hỗ trợ. Normal/Wishart chỉ áp dụng cho thiết lập ML phù hợp.

<a id="sem"></a>

## 15. Mô hình phương trình cấu trúc (SEM)

Báo cáo đường dẫn đo lường/cấu trúc, độ phù hợp và hiệu ứng trực tiếp, gián tiếp, có điều kiện được hỗ trợ. Bootstrap mặc định 5,000 lần; kết quả phản ánh khoảng BC hoặc phân vị và các tùy chọn đã chọn.

<a id="pls"></a>

## 16. PLS-SEM và PLSc

Dùng Mode A phản ánh, Mode B hình thành, PLSc phản ánh, chẩn đoán đường dẫn/đo lường và dự báo/so sánh nhóm được hỗ trợ. Thiếu được thay bằng trung bình chỉ báo qua seminr::mean_replacement, tính lại trong từng mẫu bootstrap. Mặc định 5,000 lần.

<a id="planning"></a>

## 17. Cỡ mẫu, lực kiểm định và cỡ hiệu ứng

Tính cỡ mẫu, lực kiểm định và cỡ hiệu ứng cho kiểm định được hỗ trợ. Ghi alpha, hiệu ứng giả định, phân bổ nhóm và hướng kiểm định.

<a id="reporting"></a>

## 18. Bảng, hình và xuất kết quả

Bản công khai 1.3.0 lưu HTML/hình và PDF/Word/Excel. Báo cáo HTML/PDF của trung gian/điều tiết, CFA, SEM, PLS-SEM thêm hình mô hình kết quả ở cuối. Hình giữ bố cục đang hiển thị; Free dùng 300 dpi, phát triển/Pro dùng 600 dpi. Xem kết quả và thêm vào bộ kết quả tích lũy. Có HTML, PDF, Word và Excel. HWPX chỉ có ở màn hình kết quả tích lũy với giao diện Hàn và được ghi trực tiếp, không qua Word/Hancom. Word/HWPX cho chọn bảng chính, phụ lục, giải thích và hình; mặc định là bảng chính. Lưu bản hiển thị đã ghi nhận, không tính lại. Free dùng 300 dpi, bản phát triển 600 dpi. HTML có trang bìa và danh sách bảng liên kết.

<a id="validation"></a>

## 19. Xác minh và giới hạn báo cáo

Đã so sánh phân tích tổng quát, hồi quy, dọc và sống còn với SPSS; CFA/SEM với AMOS; PLS-SEM/PLSc và CB-SEM với SmartPLS. Đây là xác minh tích lũy được đưa vào 1.3.0; trang Xác minh tóm tắt điều kiện và khác biệt còn lại.
