# Hướng dẫn sử dụng — StatEdu Studio 1.3.0

Phiên bản 1.3.0 bổ sung CFA, SEM và PLS-SEM/PLSc vào phạm vi công khai, đồng thời cung cấp RMST và nguy cơ cạnh tranh trong phân tích sống còn. Các chức năng này đi cùng khung trung gian/điều tiết, phân tích chung, mô hình dọc/bảng, mẫu phức tạp và công cụ lập kế hoạch. Lịch sử phân biệt tính năng mới công khai với cải tiến tính năng cũ.

Bộ cài phát triển là StatEdu Studio Dev 1.3.0-dev, có tên ứng dụng riêng và giữ các menu phát triển. Bản công khai không gồm phân tích tổng hợp và ANOVA điều trị lặp lại trên cùng đối tượng; vẫn có ANOVA hỗn hợp đo lặp và kiểm định cặp.

## 1. Khởi động và chuẩn bị dữ liệu

Sau khi cài, mở StatEdu Studio Dev từ menu Start. Nạp dữ liệu, kiểm tra tên biến, mức đo, giá trị thiếu và mã nhóm. Kiểm tra chọn trường hợp và chia dữ liệu đang bật, gán biến, chọn tùy chọn rồi chạy. Đổi ngôn ngữ giữ nguyên tên biến và giá trị người dùng.

## 2. CFA

Gán chỉ báo quan sát cho nhân tố tiềm ẩn và xác định quan hệ. Chọn phương pháp ước lượng, xử lý thiếu, chuẩn hóa và bootstrap được hỗ trợ. Xem cảnh báo nhận dạng, hội tụ và nghiệm không hợp lệ trước tải, độ phù hợp, độ tin cậy, AVE và HTMT. Khi so sánh nhóm, kiểm tra bất biến đo lường và ràng buộc.

## 3. SEM

Dựng mô hình đo lường và đường cấu trúc trên khung SEM, gồm đường cho hiệu ứng trực tiếp và gián tiếp. Chọn ước lượng và bootstrap rồi chạy. Báo cáo hệ số, khoảng tin cậy, độ phù hợp và số lần lặp hợp lệ cùng nhau. Sửa đường hoặc nút làm kết quả cũ mất hiệu lực; cần chạy lại. Có thể lưu bố cục và tệp mô hình.

## 4. PLS-SEM / PLSc

Gán cấu trúc và chỉ báo; chọn Mode A phản ánh hoặc Mode B cấu tạo. PLSc dùng cho mô hình phản ánh được hỗ trợ. Kiểm tra đường, bootstrap, dự báo và so sánh nhóm. Xem tải/trọng số, đa cộng tuyến, độ tin cậy, giá trị đo lường và đường cấu trúc. Phân biệt số lặp yêu cầu và hợp lệ; không bỏ qua cảnh báo từ chối ước lượng hoặc thiếu lần lặp.

## 5. Phân tích sống còn

Chọn biến thời gian, biến sự kiện, mã sự kiện/kiểm duyệt, đơn vị thời gian, nhóm và biến dự báo. Xem khuyến nghị và điều kiện trước Kaplan–Meier, bảng sống, RMST, Cox hoặc nguy cơ cạnh tranh. Kiểm tra thời điểm giới hạn τ của RMST và giả định nguy cơ tỷ lệ của Cox. Phân biệt sự kiện quan tâm và cạnh tranh; CIF, HR theo nguyên nhân và SHR Fine–Gray không tương đương.

## 6. Xem, tích lũy và lưu kết quả

Xem kết quả và thêm vào bộ kết quả tích lũy. Có HTML, PDF, Word và Excel. HWPX chỉ có ở màn hình kết quả tích lũy với giao diện Hàn và được ghi trực tiếp, không qua Word/Hancom. Word/HWPX cho chọn bảng chính, phụ lục, giải thích và hình; mặc định là bảng chính. Lưu bản hiển thị đã ghi nhận, không tính lại. Free dùng 300 dpi, bản phát triển 600 dpi. HTML có trang bìa và danh sách bảng liên kết.

## 7. Tài liệu và phạm vi

Mở Tổng quan, Hướng dẫn, Phân tích, Ghi chú phương pháp, Kiểm chứng và Lịch sử trong menu thông tin. Bảng thống kê chính giữ tiếng Anh; menu và giải thích theo ngôn ngữ giao diện. Kiểm chứng chỉ áp dụng cho dữ liệu và tùy chọn đã nêu, không phải mọi tổ hợp hay rà soát bởi người bản ngữ.

## Phân tích tầm quan trọng–mức thực hiện (IPA)

Mở Phân tích → IPA, chọn đánh giá trực tiếp hoặc tầm quan trọng suy ra. Ghép biến quan trọng và thực hiện theo cùng thứ tự thuộc tính, hoặc chọn thực hiện và hài lòng chung. Chọn toàn bộ, nhóm độc lập hoặc cặp trước/sau. Dùng cột WIDE tương ứng hoặc ID/thời gian LONG và hai giá trị thời gian. Chọn đường tham chiếu, biểu đồ; kiểm tra số mẫu, tọa độ, khoảng và chênh lệch. Lưu Word/HWPX từ kết quả tích lũy sau khi thêm.
