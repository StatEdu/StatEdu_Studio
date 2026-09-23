# Tổng quan — StatEdu Studio 1.3.0

StatEdu Studio là ứng dụng thống kê trên Windows để chuẩn bị dữ liệu, phân tích, trực quan hóa mô hình, lập kế hoạch cỡ mẫu và báo cáo kết quả. Chọn biến và tùy chọn trên giao diện, đồng thời xem phương pháp, giả định, chẩn đoán và diễn giải. Tổng quan này giới thiệu toàn bộ ứng dụng hiện tại, không chỉ tính năng mới của 1.3.0.

## Biên tập dữ liệu, phạm vi và công cụ tính

Nhập SPSS, SAS, Stata, Excel, CSV và DAT; kiểm tra tên, nhãn, mức đo, nhóm và giá trị tham chiếu. Xem cỡ mẫu thực dùng và cách xử lý dữ liệu thiếu của từng phân tích.

Quản lý tên, nhãn, mức đo và danh mục; mã hóa lại, tính biến, xử lý thiếu, ghép dữ liệu, tổng hợp theo ID và chuyển WIDE–LONG. Chọn trường hợp và chia dữ liệu xác định phạm vi phân tích. Có công cụ EQ-5D, HINT-8, Framingham, ASCVD và các chỉ số chuyển hóa.

## Phân tích được hỗ trợ

### Tần số, mô tả và bảng chéo

Xuất tần số, tỷ lệ phần trăm, thống kê vị trí và độ phân tán, bảng liên hợp. Các tùy chọn cung cấp kiểm định liên hệ, cỡ hiệu ứng và chẩn đoán.

### So sánh nhóm và ANCOVA

Dùng kiểm định t độc lập, ANOVA, ANCOVA và so sánh nhóm phi tham số. Tùy chọn hỗ trợ phương pháp theo phương sai, so sánh hậu nghiệm và cỡ hiệu ứng.

### Đo ghép cặp và đo lặp hỗn hợp

Dùng phân tích ghép cặp, đo lặp, ANOVA hỗn hợp và phân tích ghép cặp phi tham số. Xuất hiệu ứng thời gian, nhóm và so sánh theo thiết kế đã chọn.

### Tương quan, độ tin cậy và đồng thuận

Phân tích tương quan, độ tin cậy thang đo và đồng thuận giữa người đánh giá. Chọn phương pháp và chỉ số phù hợp loại biến và thiết kế đánh giá.

### Phân tích nhân tố khám phá và PCA

Xem số nhân tố/thành phần, trích xuất, xoay, tải và phương sai giải thích trong EFA và PCA. CFA được mô tả riêng.

### Phân tích tầm quan trọng–mức thực hiện (IPA)

Mở Phân tích → IPA, chọn đánh giá trực tiếp hoặc tầm quan trọng suy ra. Ghép biến quan trọng và thực hiện theo cùng thứ tự thuộc tính, hoặc chọn thực hiện và hài lòng chung. Chọn toàn bộ, nhóm độc lập hoặc cặp trước/sau. Dùng cột WIDE tương ứng hoặc ID/thời gian LONG và hai giá trị thời gian. Chọn đường tham chiếu, biểu đồ; kiểm tra số mẫu, tọa độ, khoảng và chênh lệch. Lưu Word/HWPX từ kết quả tích lũy sau khi thêm.

### Hồi quy và hồi quy phân cấp

Dùng OLS, suy luận vững HC3 và hồi quy bootstrap. Phân tích phân cấp so sánh các khối liên tiếp và thay đổi phương sai giải thích. Đầu ra tùy chọn gồm sr², f², đa cộng tuyến và chẩn đoán phần dư. Hồi quy phân cấp hỗ trợ tối đa bốn khối. Mỗi bước giữ lại các biến từ những khối trước và thêm khối tiếp theo.

### Hiệu ứng trung gian và điều tiết

Gán vai trò biến dự báo, kết quả, trung gian, điều tiết và đồng biến, rồi vẽ đường dẫn để ước lượng hiệu ứng trực tiếp, gián tiếp, tổng và có điều kiện. Không chọn theo số mô hình. Cấu trúc không hỗ trợ được kiểm tra trước khi chạy.

### Hồi quy logistic, GLM và có phạt

Dùng mô hình logistic, tuyến tính tổng quát, Ridge, LASSO và Elastic Net. Báo cáo hệ số, hiệu năng cùng thang kết quả, họ phân phối, hàm liên kết và thiết lập kiểm định chéo.

### Dữ liệu dọc, bảng và khảo sát

Thiết kế hỗ trợ gồm GEE, LMM, GLMM, hiệu ứng cố định/ngẫu nhiên và mẫu khảo sát phức tạp. Kiểm tra mã cá thể/cụm, thời gian, trọng số, tầng và cụm theo phân tích.

### Phân tích sống còn

Dùng Kaplan–Meier, log-rank, RMST, Cox và phân tích nguy cơ cạnh tranh được hỗ trợ. Kiểm tra định dạng và mã sự kiện trước; tổ hợp không hỗ trợ bị hạn chế.

### Phân tích nhân tố khẳng định (CFA)

Dùng ML, MLR và WLSMV/DWLS cho dữ liệu thứ bậc; xem tải, độ phù hợp, độ tin cậy, AVE, HTMT và so sánh bất biến đo lường được hỗ trợ. Normal/Wishart chỉ áp dụng cho thiết lập ML phù hợp.

### Mô hình phương trình cấu trúc (SEM)

Báo cáo đường dẫn đo lường/cấu trúc, độ phù hợp và hiệu ứng trực tiếp, gián tiếp, có điều kiện được hỗ trợ. Bootstrap mặc định 5,000 lần; kết quả phản ánh khoảng BC hoặc phân vị và các tùy chọn đã chọn.

### PLS-SEM và PLSc

Dùng Mode A phản ánh, Mode B hình thành, PLSc phản ánh, chẩn đoán đường dẫn/đo lường và dự báo/so sánh nhóm được hỗ trợ. Thiếu được thay bằng trung bình chỉ báo qua seminr::mean_replacement, tính lại trong từng mẫu bootstrap. Mặc định 5,000 lần.

### Cỡ mẫu, lực kiểm định và cỡ hiệu ứng

Tính cỡ mẫu, lực kiểm định và cỡ hiệu ứng cho kiểm định được hỗ trợ. Ghi alpha, hiệu ứng giả định, phân bổ nhóm và hướng kiểm định.

## Xem, tích lũy và lưu kết quả

Xem kết quả và thêm vào bộ kết quả tích lũy. Có HTML, PDF, Word và Excel. HWPX chỉ có ở màn hình kết quả tích lũy với giao diện Hàn và được ghi trực tiếp, không qua Word/Hancom. Word/HWPX cho chọn bảng chính, phụ lục, giải thích và hình; mặc định là bảng chính. Lưu bản hiển thị đã ghi nhận, không tính lại. Free dùng 300 dpi, bản phát triển 600 dpi. HTML có trang bìa và danh sách bảng liên kết.

## Tài liệu và phạm vi

Mở Tổng quan, Hướng dẫn, Phân tích, Ghi chú phương pháp, Kiểm chứng và Lịch sử trong menu thông tin. Bảng thống kê chính giữ tiếng Anh; menu và giải thích theo ngôn ngữ giao diện. Kiểm chứng chỉ áp dụng cho dữ liệu và tùy chọn đã nêu, không phải mọi tổ hợp hay rà soát bởi người bản ngữ.

Bộ cài phát triển là StatEdu Studio Dev 1.3.0-dev, có tên ứng dụng riêng và giữ các menu phát triển. Bản công khai không gồm phân tích tổng hợp và ANOVA điều trị lặp lại trên cùng đối tượng; vẫn có ANOVA hỗn hợp đo lặp và kiểm định cặp.
