# Lịch sử phiên bản

## v1.2.0 - 2026-08-06

### Bổ sung

- Bổ sung quy trình Độ đồng thuận giữa người đánh giá vào phạm vi phân tích công khai; đầu ra ưu tiên chỉ số đồng thuận được khuyến nghị đồng thời giữ các chỉ số hỗ trợ.
- Bổ sung ANOVA đo lặp hỗn hợp cho so sánh nhóm trước–sau và nhiều thời điểm, với phân tích PP/ITT, tóm tắt hiệu chỉnh đồng biến, kiểm tra giả định, so sánh hậu nghiệm và xuất HTML/PDF/Excel.
- Bổ sung vùng vẽ Mô hình tùy chỉnh Trung gian / Điều tiết vào nhóm quy trình Hồi quy / Mô hình công khai.

### Thay đổi

- Đưa các thay đổi ổn định sau 1.1.3 vào siêu dữ liệu phát hành chính thức `1.2.0`.
- Tích hợp ANOVA đo lặp vào menu So sánh nhóm; thống nhất bố cục thiết lập, tùy chọn kiểm tra giả định, kiểm tra tính cầu/Levene, hướng dẫn tính chuẩn và lời khuyến nghị với quy trình phân tích có hướng dẫn của StatEdu Studio.
- Mở rộng kiểm định thống kê cho tương quan, độ tin cậy, đồng thuận giữa người đánh giá, phân tích nhân tố / PCA, kiểm định t / ANOVA, đo lặp ghép cặp, hồi quy, logistic, dọc / bảng, hồi quy phạt, cỡ mẫu, cỡ hiệu ứng, trình sửa dữ liệu, mô hình tùy chỉnh và đo lặp hỗn hợp.
- Đổi nhãn menu mô hình tùy chỉnh bằng tiếng Hàn để điều hướng bản công khai rõ ràng hơn.

### Sửa lỗi

- Sửa trình khởi chạy bản Electron công khai để vùng vẽ Mô hình tùy chỉnh Trung gian / Điều tiết được bật mặc định trong bộ cài 1.2.0.
- Loại bỏ các hàng kết quả trùng khi dùng lại kết quả mô hình tùy chỉnh đã khớp.

## v1.1.3 - 2026-07-12

### Thay đổi

- Đưa bản dành cho nhà phát triển hiện đã ổn định thành bộ cài chính thức `1.1.3`.
- Giữ phạm vi bản công khai theo quy tắc đóng gói 1.1.1: loại tài liệu, kiểm thử, ví dụ, mã nguồn và nội dung không cần lúc chạy khỏi môi trường R đi kèm.
- Bổ sung đa ngôn ngữ cho nhãn thư mục dự án phân tích tiềm ẩn mới, chỗ giữ chỗ tệp dữ liệu, lựa chọn dấu phân cách DAT và điều khiển phân tích dùng chung.

### Sửa lỗi

- Sửa bước kiểm tra nhập Excel để tệp đã chọn vẫn tương tác được và tải qua quy trình kiểm tra trang tính/ô bắt đầu.
- Giới hạn bản xem trước “Xem dữ liệu đã chọn” dùng chung ở các biến đã chọn và 15 hàng.
- Sửa xác định thư mục dự án/đầu ra Latent Mplus để tạo đầu ra cạnh tệp dữ liệu ban đầu khi còn đường dẫn gốc.
- Đổi lưu thiết lập Latent Mplus sang mở hộp thoại lưu để chọn tên tệp.
- Căn trái phần hiển thị thư mục dự án/đầu ra Latent Mplus.
- Chuyển các nhãn giao diện trực tiếp còn lại qua bảng i18n dùng chung để lớp ngôn ngữ dịch được các tùy chọn mới về dữ liệu, hồi quy, trung gian, điều tiết và phân tích tiềm ẩn.
- Tiếp tục loại vùng vẽ mô hình trung gian/điều tiết tùy chỉnh khỏi bản phát hành chính thức.

## v1.1.1 - 2026-07-07

### Sửa lỗi

- Phát hành bộ cài vá để nâng cấp Windows thay thế các tệp desktop đã đóng gói thay vì dùng lại bản cài cùng phiên bản đã cũ.
- Tăng độ chắc chắn của chẩn đoán khởi động Electron bằng thời gian chờ Shiny dài hơn và ghi đầu ra tiến trình R khi khởi động thất bại.
- Sửa tải tệp dữ liệu desktop, chọn tiếng Hàn lúc khởi động, xử lý tệp thiết kế mẫu phức tạp và bảo toàn nhãn trống.

## v1.1.0 - 2026-07-06

### Bổ sung

- Đưa bản công khai 1.1.0 vào phát hành với mọi quy trình phân tích trừ vùng vẽ Mô hình tùy chỉnh Trung gian / Điều tiết.
- Bổ sung giới hạn lưu của bản công khai: lưu HTML vẫn bật mặc định; các điều khiển hình, PDF, Excel và Thêm kết quả vẫn hiện nhưng bị vô hiệu hóa.
- Giữ kiểm định t / ANOVA là ngoại lệ xuất công khai, bật HTML, hình, PDF, Excel và Thêm kết quả; bộ sưu tập kết quả kiểm định t / ANOVA có thể xuất Excel và Word.

### Thay đổi

- Cập nhật hồ sơ phát hành Electron để các phiên bản ngữ nghĩa chính thức được xây dựng thành bộ cài StatEdu Studio công khai.

## v1.0.1 - 2026-06-28

### Thay đổi

- Ổn định chuyển giao diện Hàn/Anh ở menu chính, màn hình thiết lập, công cụ tính, tài liệu và thông báo; giữ bảng kết quả bằng tiếng Anh.
- Cập nhật Trình sửa dữ liệu và menu phân tích với các danh mục nhóm, nhãn tiếng Hàn đã sửa, bố cục nút và thẻ được căn chỉnh.
- Cải thiện ngưỡng tính chuẩn của kiểm định t / ANOVA, tóm tắt ký hiệu hậu nghiệm có thứ tự và bố cục/xuất bảng chéo.
- Bổ sung điều khiển tên biến đầu ra của công cụ tính và cải thiện bố cục các bảng EQ-5D, ASCVD10.
- Bổ sung kiểm tra cập nhật, siêu dữ liệu đóng gói liên kết tệp `.studio` và biểu tượng tệp `.studio`.
- Bổ sung menu Trợ giúp cho báo lỗi, yêu cầu tính năng, yêu cầu phân tích, hỏi đáp và kiểm tra cập nhật.
- Liên kết các mục yêu cầu Trợ giúp tới biểu mẫu website StatEdu Studio và định tuyến hỏi đáp theo ngôn ngữ giao diện.
- Cập nhật ảnh hướng dẫn sử dụng 1.0 và tài nguyên tài liệu song ngữ.

## v1.0.0 - 2026-06-25

### Thay đổi

- Đưa nhánh phát hành ổn định sang siêu dữ liệu phiên bản công khai 1.0.0.
- Đổi siêu dữ liệu gói Electron từ tên beta sang tên phát hành StatEdu Studio chính thức.
- Giữ nêu rõ các tuyên bố công khai 1.0 bị hoãn, xác minh DOI, xác minh website và điều kiện QA của bản đóng gói trong tài liệu phát hành.
- Cập nhật ký hiệu hậu nghiệm có thứ tự để chỉ hiện so sánh có ý nghĩa trực tiếp theo thứ tự trung bình, tránh chuỗi bắc cầu như `b>a>c` khi có một cặp không có ý nghĩa.
- Bổ sung hướng PDF bảng chéo theo độ rộng: bảng chính rộng in ngang, bảng hẹp và bảng hỗ trợ vẫn in dọc.
- Bổ sung ngưỡng tính chuẩn độ lệch/độ nhọn có thể chọn cho kiểm định t / ANOVA: 2/5, 2/7 và 3/7; mặc định vẫn là 2/7.
- Hộp thoại lưu thiết lập nay mở tại thư mục tệp dữ liệu đã tải khi có đường dẫn tệp.
- Ngăn bảng kiểm tra nhập Excel xuất hiện lúc khởi động nếu không có đường dẫn Excel chờ hợp lệ.

## v0.9.42 - 2026-06-23

### Bổ sung

- Bổ sung Trình sửa dữ liệu > Rộng sang dài để chuyển các cột đo lặp thành dữ liệu dạng dài trước phân tích dọc / bảng.
- Bổ sung kiểm định chuyển rộng sang dài và mở rộng kiểm tra mã hóa lại trong trình sửa dữ liệu.
- Bổ sung kiểm tra nhanh khởi động Shiny và phát hành Electron để xác minh bản ứng viên phát hành.
- Bổ sung kiểm tra UTF-8 của tài liệu được quản lý phiên bản cho bản ứng viên phát hành.

### Thay đổi

- Chuẩn hóa bảng, nút, khoảng cách và trình xem dữ liệu của Trình sửa dữ liệu theo mẫu hộp thoại kiểm định t / ANOVA.
- Cập nhật xử lý giá trị thiếu để hỗ trợ cả đánh dấu thiếu do người dùng định nghĩa và chuyển sang NA hệ thống.
- Cải thiện mã hóa lại và đổi tên bằng xóa biến trong hàng đợi, căn chỉnh bảng đích và thiết lập quy tắc rõ hơn.
- Đơn giản hóa tệp thiết lập sang định dạng `.studio` và cập nhật hộp thoại lưu/tải.
- Cải thiện bố cục mô hình dọc / bảng và báo cáo nhóm tham chiếu cho biến dự báo phân loại.
- Siết chặt kiểm tra vệ sinh phát hành đối với tệp chỉ dùng cục bộ, sản phẩm sinh ra, siêu dữ liệu phiên bản và tài liệu phát hành.
- Khôi phục tài liệu kế hoạch sản phẩm tiếng Hàn với các ưu tiên ổn định 1.0 hiện tại.
- Cập nhật tham chiếu trong tài liệu người dùng và phương pháp tiếng Hàn hiện hành sang 0.9.42, đồng thời bổ sung kiểm tra tham chiếu phiên bản hiện tại đã lỗi thời.
- Đánh dấu kế hoạch phân phối, giấy phép và cập nhật 1.0 đã được xem xét trong giai đoạn ổn định 0.9.42.
- Bổ sung kiểm tra vệ sinh phát hành vào bộ kiểm định ổn định cốt lõi để tránh vô tình quản lý phiên bản các tệp staging Electron sinh ra và sản phẩm cục bộ.
- Bổ sung theo dõi và xác minh trạng thái sẵn sàng phát hành cho đóng gói, DOI, website và các quyết định hoãn của 1.0.
- Bổ sung tập lệnh kiểm tra trước phát hành chạy toàn bộ kiểm định ổn định, kiểm tra nhanh khởi động Shiny và phát hành Electron.
- Bổ sung nhật ký quyết định 1.0 cho đóng gói, DOI, website, giới hạn phiên bản, giấy phép, cập nhật và ghi chú phát hành công khai.
- Siết chặt quy ước bố cục và kiểm định vị trí nút chuẩn của Trình sửa dữ liệu ba khối.
- Bổ sung quy trình QA thủ công cho kiểm tra trực quan, dữ liệu, phân tích, xuất và Electron đóng gói của bản ứng viên phát hành.
- Liên kết quy trình QA thủ công vào kiểm tra nhanh phát hành Electron.
- Cập nhật trạng thái sẵn sàng phát hành để ghi nhận đã vượt qua kiểm tra trước phát hành.
- Siết chặt xác minh siêu dữ liệu phát hành đối với các trở ngại công khai 1.0 chưa giải quyết.
- Bổ sung mẫu ghi nhận QA thủ công và kiểm định bằng chứng QA của bản ứng viên phát hành.
- Loại sản phẩm so sánh sinh ra đã được quản lý phiên bản khỏi `outputs/` và chặn sản phẩm đầu ra tại thư mục gốc trong kiểm tra vệ sinh phát hành.
- Ghi lại đầy đủ lệnh kiểm tra trước Electron với đầu ra đóng gói trong README và xác minh siêu dữ liệu phiên bản.
- Làm rõ tài liệu điều kiện phát hành: URL trang đích DOI là `https://studio.statedu.com`.
- Làm rõ kế hoạch phân phối, giấy phép và cập nhật 1.0 chỉ là tài liệu kế hoạch, không khẳng định đã triển khai các phiên bản có giới hạn, kích hoạt giấy phép, trình cập nhật hay hạ tầng bộ cài công khai.
- Bổ sung cảnh báo trong README và trạng thái sẵn sàng phát hành rằng DOI dự kiến phải truy cập được trước mọi thông báo trích dẫn công khai 1.0.
- Siết chặt kiểm tra thông báo mã nguồn/giấy phép về khả năng cung cấp mã nguồn bản công khai, thông báo bên thứ ba, báo cáo giấy phép và tham chiếu văn bản giấy phép đi kèm.
- Thống nhất danh sách kiểm tra phát hành, README và hướng dẫn QA thủ công để lưu bằng chứng QA hoàn tất cùng ghi chú phát hành và sản phẩm kiểm định.
- Ghi lại điều kiện thay tên gói Electron beta 0.9.x trước khi tạo bộ cài công khai 1.0.
- Thống nhất hộp thoại thiết lập Latent Mplus với quy ước tệp thiết lập chỉ dùng `.studio`.
- Cập nhật kế hoạch phân phối/giấy phép/cập nhật 1.0 để ghi chú beta lịch sử không bị hiểu là nền tảng phát hành hiện tại.
- Bổ sung điều kiện QA thủ công: ghi chú phát hành công khai và văn bản người dùng thấy không được khẳng định đã có phiên bản giới hạn, kích hoạt giấy phép, cập nhật trong ứng dụng hoặc hạ tầng bộ cài công khai chưa triển khai.
- Thay thông báo giữ chỗ của trình tạo Latent Mplus bằng thông báo rõ tính năng chưa được bật trong bản phát hành và bổ sung kiểm định.
- Viết lại phụ đề dự phòng của cỡ hiệu ứng để các công cụ tính chưa có không bị hiểu là cam kết tính năng tương lai.
- Loại hàm trợ giúp thẻ Phân tích giữ chỗ không sử dụng khỏi mã lắp ráp menu.
- Làm rõ danh sách kiểm tra phát hành: định dạng thiết lập cũ không xuất hiện trong hộp thoại công khai, nhưng mã định danh tương thích nội bộ vẫn được ghi chép.

## v0.9.41 - 2026-06-20

### Thay đổi

- Nhóm menu Phân tích, Cỡ mẫu và Cỡ hiệu ứng thành các danh mục thống kê cấp đầu nhất quán.
- Sửa điều khiển chuyển biến bảng chéo để biến cột hoặc hàng đã chọn quay lại danh sách biến có sẵn một cách tin cậy.
- Điều chỉnh vị trí nút chuyển ở bảng đích cột và hàng của bảng chéo.


## v0.9.40 - 2026-06-20

### Thay đổi

- Tăng siêu dữ liệu phát triển sau bản beta 0.9.39.
- Đổi nhận diện sản phẩm thành **StatEdu Studio**, gồm đầu trang ứng dụng, phần giới thiệu, tên trình khởi chạy, siêu dữ liệu bộ cài, favicon, tài nguyên logo và tên tệp xuất mặc định.
- Bổ sung kiểm tra tương thích thương hiệu, ghi rõ mã định danh cũ nào được giữ vì DOI, môi trường, đường dẫn hoặc truy xuất dữ liệu tương thích ngược.
- Bảo vệ lời gọi đầu vào Shiny khi máy khách khởi động để tránh lỗi `Shiny.setInputValue` trước khi liên kết máy khách Shiny sẵn sàng.
- Cập nhật siêu dữ liệu khóa kiểm toán gói Electron để phụ thuộc phát triển bắc cầu `undici` được phân giải mà không có phát hiện npm audit.
- Bổ sung siêu dữ liệu tác giả gói Electron và bỏ qua sản phẩm phát hành `StatEdu_Studio_*.zip` khi phát triển cục bộ và staging Electron.

## v0.9.39 - 2026-06-18

### Bổ sung

- Bổ sung quy trình riêng `Analysis > Longitudinal / Panel Models` cho GEE, LMM, GLMM, mô hình bảng hiệu ứng cố định và hiệu ứng ngẫu nhiên.
- Bổ sung kiểm tra giả định theo mô hình, phương án thay thế khuyến nghị, so sánh độ nhạy tự động, ước lượng sẵn sàng công bố, văn bản bản thảo, danh sách báo cáo SCI và phiên bản phần mềm cho kết quả mô hình dọc / bảng.
- Bổ sung thẻ Giá trị thiếu của mô hình dọc / bảng với xử lý thiếu chính, các công cụ độ nhạy MI/IPW/WGEE thực sự và theo dõi phương pháp xử lý thiếu trong báo cáo.
- Bổ sung trọng số phân tích dọc với một ô đích biến trọng số, loại trọng số lấy mẫu/dọc/IPW/kết hợp, cắt ngọn, trọng số cuối chuẩn hóa và báo cáo cỡ mẫu hiệu dụng.
- Bổ sung xử lý phơi nhiễm / offset tùy chọn cho mô hình đếm/tỷ suất dọc, gồm offset `log(exposure)` trong khớp mô hình chính và độ nhạy.
- Bổ sung chi tiết sàng lọc lạm phát số không trong mô hình đếm, so sánh tỷ lệ số không quan sát và kỳ vọng Poisson.
- Bổ sung quy trình hoạt động `Analysis > GLM` cho GLM Gaussian, logistic nhị phân, Gamma và đếm; tái dùng sàng lọc họ đếm của GEE để chọn Poisson hay nhị thức âm.
- Bổ sung chi tiết báo cáo GLM theo SCI về xử lý ca đầy đủ, chọn Poisson/nhị thức âm, sàng lọc EPV/phân tách/ô thưa trong logistic, kiểm tra quan sát độc lập, chẩn đoán ảnh hưởng, chú thích bảng công bố, danh sách báo cáo và văn bản bản thảo gợi ý.
- Bổ sung tùy chọn GLM dạng thẻ, trong đó thẻ Giá trị thiếu hỗ trợ ca đầy đủ, bù đa lần và trọng số xác suất nghịch đảo.
- Bổ sung tài liệu GLM trong Hướng dẫn sử dụng, Phương pháp phân tích và Ghi chú phương pháp tiếng Hàn về chọn họ/hàm liên kết, độ nhạy với thiếu dữ liệu, SE vững, quá phân tán dữ liệu đếm và yêu cầu báo cáo SCI.
- Bổ sung HTML, PDF, Excel và bộ sưu tập Kết quả đã lưu cho đầu ra GLM, gồm ghi chú công bố, danh sách SCI, văn bản bản thảo và trang phiên bản phần mềm.
- Bổ sung HTML, PDF, Excel và bộ sưu tập Kết quả đã lưu cho đầu ra mô hình dọc / bảng.
- Bổ sung kiểm định khớp mô hình dọc / bảng, GLM, cấu trúc giao diện thiết lập, danh mục kiểm tra giả định, xuất HTML/Excel, so sánh độ nhạy và phần báo cáo SCI.

### Thay đổi

- Thống nhất màn hình thiết lập Mô hình dọc / bảng với bố cục chuyển biến của kiểm định t / ANOVA và chỉ hiện tùy chọn liên quan tới mô hình.
- Gộp tùy chọn Mô hình và Thành phần của mô hình dọc / bảng để đặt loại mô hình, thành phần thời gian cố định và hiệu ứng ngẫu nhiên trong một thẻ.
- Đổi tương quan làm việc GEE mặc định sang hoán đổi; khớp AR(1) nay truyền thứ tự đối tượng/lần đo theo thời gian tới `geepack::geeglm`.
- Đổi nhãn khớp đếm nhị thức âm của GEE thành GLM nhị thức âm biên với SE vững theo cụm đối tượng, vì geepack không cung cấp GEE nhị thức âm nguyên bản.
- Làm rõ ID cụm tùy chọn là biến nhóm bổ sung cho hệ số chặn ngẫu nhiên của LMM/GLMM, không áp dụng vào mô hình GEE/bảng chính đã chọn.
- Làm rõ xử lý thiếu của LMM/GLMM là phân tích MAR dựa trên hợp lý dùng các phép đo lặp có sẵn; MI/IPW là phân tích độ nhạy, không phải khớp chính mặc định.
- Không đưa mô hình đếm lạm phát số không và hurdle vào mô-đun dọc / bảng mặc định để tránh phụ thuộc gói tùy chọn nặng; phát hiện dư số không được báo cáo như hướng dẫn sàng lọc.
- Thay khung mô hình tổng quát cũ bằng thiết lập GLM hoạt động, bộ xử lý chạy, bảng hệ số, thống kê độ phù hợp, SE vững, kiểm tra quá phân tán và VIF tùy chọn.
- Thống nhất `run_app.R` với `R/app_bootstrap.R` để cài gói lúc khởi chạy dùng cùng danh sách gói bắt buộc như lúc chạy ứng dụng.
- Cố định Electron ở 39.8.6 và cập nhật tệp khóa sau kiểm toán gói.
- Củng cố tập lệnh dựng Electron beta để tìm Rscript ngoài PATH và không thất bại vì mô-đun Latent Mplus tùy chọn bị loại khỏi ứng dụng đóng gói.
- Cập nhật README, Hướng dẫn sử dụng, Phương pháp phân tích và Ghi chú phương pháp tiếng Hàn cho quy trình dọc / bảng và GLM mới.

## v0.9.38 - 2026-06-15

### Bổ sung

- Bổ sung ước lượng cỡ mẫu trung gian theo bảng thực nghiệm Fritz & MacKinnon (2007) với lực kiểm định .80.
- Bổ sung tài liệu tham khảo theo phương pháp cho tính cỡ mẫu trung gian Fritz & MacKinnon, Monte Carlo, bootstrap và Sobel.

### Thay đổi

- Mở rộng khối kết quả Cỡ mẫu trên cửa sổ lớn nhưng giữ bố cục ba khối hiện tại ở màn hình rộng 1280px.

## v0.9.37 - 2026-06-12

### Bổ sung

- Bổ sung từ điển nhận diện Likert 4 mức tiếng Hàn và tiếng Anh tương ứng với các bộ nhận diện 5 mức hiện có.
- Bổ sung hướng dẫn bằng lớp phủ thao tác động trong hướng dẫn sử dụng tiếng Hàn của ứng dụng, dùng ảnh hướng dẫn đi kèm.

### Thay đổi

- Cải thiện giao diện từ điển nhận diện Likert tùy chỉnh: mở từ điển đã đăng ký bằng nút, hiện trong hộp danh sách và hiển thị chi tiết đã chọn ở bảng bên.
- Ưu tiên khớp chính xác mức Likert trước khớp tập cha tương thích để câu trả lời 4 mức được nhận diện đúng là thang 4 mức.
- Điều chỉnh thời gian và vị trí hộp phủ hướng dẫn cho tải dữ liệu, kiểm định t / ANOVA và xem kết quả.

### Sửa lỗi

- Giữ vị trí cuộn khi mở hoặc chọn từ điển nhận diện Likert đã đăng ký.
- Cải thiện độ rộng cột chọn và tên nhận diện trong bảng nhận diện Likert.


## v0.9.36 - 2026-06-11

### Bổ sung

- Bổ sung trình quản lý từ điển nhận diện Likert tùy chỉnh có thể chỉnh sửa, gồm xem danh sách đăng ký, chi tiết, chỉnh sửa và xóa.
- Mở rộng kiểm định cho phân tích logistic, mã hóa lại trong trình sửa dữ liệu, nhân tố/PCA, tương quan, kiểm định ghép cặp, nhập/xuất dữ liệu và lịch sử kết quả.

### Thay đổi

- Cải thiện hiển thị bảng kết quả theo khổ B5 cho hồi quy logistic, phân tích nhân tố, PCA, độ tin cậy, kiểm định ghép cặp/đo lặp, tương quan và kết quả đã lưu.
- Cải thiện kiểm tra nhập Excel bằng cách chuyển bản xem trước trang tính vào bảng kiểm tra chính và đơn giản hóa điều khiển nhập.
- Cập nhật chuyển đổi Likert để biến sau chuyển đổi mang đúng loại đo lường được yêu cầu.

### Sửa lỗi

- Sửa sự di chuyển của nút chọn trong danh sách biến của trình sửa dữ liệu sau nhập Excel.
- Sửa đường cập nhật nhận diện giá trị thiếu tự động và nhận diện Likert sau chuyển đổi/nhập.
- Sửa vị trí bảng, ô tham chiếu, hiển thị VIF và xử lý tùy chọn khoảng tin cậy trong hồi quy logistic phân cấp.
- Sửa thứ tự cột tải nhân tố/PCA và tiêu đề bảng gọn cho đầu ra B5 dọc.

## v0.9.35 - 2026-06-10

### Bổ sung

- Bổ sung quy trình Latent Mplus dành cho nhà phát triển dưới dạng mô-đun EasyFlow tùy chọn với các bước Dữ liệu, Thiết lập và Kết quả.
- Bổ sung lưu/tải vai trò tiềm ẩn, xử lý điều kiện tập con, giữ thứ tự chọn và xem thông báo tiến độ trong Kết quả.
- Định tuyến đầu ra tiềm ẩn dưới thư mục tệp dữ liệu đã tải, gồm đầu ra, tệp tạm Mplus, nhật ký chạy, bảng Excel và hình 600 dpi.
- Bổ sung hiển thị biểu đồ Mplus nguyên bản đã chọn và các biến thể màu của biểu đồ hồ sơ chỉ báo.

### Thay đổi

- Thống nhất bảng và hình kết quả tiềm ẩn theo khung B5, đầu ra căn trái, bảng gọn và hình co giãn theo B5 dọc.
- Cải thiện thời điểm tải/thiết lập/đặt lại trong thẻ Dữ liệu và hoãn đăng ký máy chủ tiềm ẩn tới khi mở thẻ tiềm ẩn.
- Ẩn bảng chỉ dành cho LCA trong đầu ra LPA và loại bảng khóa lớp BCH nội bộ khỏi hiển thị Kết quả.
- Cập nhật đóng gói Electron beta để loại mô-đun Latent Mplus chỉ dành cho nhà phát triển khỏi staging beta công khai.

### Sửa lỗi

- Sửa bảng kiểm tra giả định và tổng quan mô hình kiểm định t / ANOVA để có đầy đủ đầu ra kiểm tra giả định.
- Đặt lại vai trò/kết quả tiềm ẩn khi tải tệp dữ liệu mới, nhưng giữ thiết lập YAML được khôi phục tường minh.
- Giữ vị trí cuộn bảng biến tiềm ẩn khi gán vai trò.
- Chuyển thông báo tiến độ chạy phân tích tiềm ẩn từ Thiết lập sang Kết quả.

## v0.9.34 - 2026-06-09

### Thay đổi

- Cải thiện bảng kết quả ghép cặp và đo lặp về tùy chọn tóm tắt, căn thống kê, nhãn cỡ hiệu ứng, cảnh báo và kiểm tra giả định.
- Sửa thứ tự biến đo lặp ghép cặp để các cặp hiển thị theo thứ tự người dùng chọn.
- Giữ các thẻ tùy chọn thiết lập ghép cặp hiện hữu, chỉ bật nhãn biến lặp khi có ít nhất ba phép đo lặp.
- Làm rõ ghi chú phương pháp đo lặp để không báo cáo Wilks' lambda và Greenhouse-Geisser như một phương pháp kết hợp.
- Cập nhật siêu dữ liệu trích dẫn để dùng DOI EasyFlow Statistics đã đăng ký.

## v0.9.33 - 2026-06-06

### Thay đổi

- Mở rộng chẩn đoán giả định ANCOVA: Levene mặc định để kiểm tra phương sai, Brown-Forsythe / Breusch-Pagan / White tùy chọn, bảng chi tiết đồng nhất độ dốc, báo cáo ca đầy đủ, đồ thị tuyến tính phần dư và phân tích độ nhạy ảnh hưởng.
- Bổ sung điều khiển phương pháp tự động của ANCOVA để giữ lựa chọn tự động hoặc báo cảnh báo giả định trong khi vẫn dùng mô hình ANCOVA chuẩn.
- Cải thiện hiển thị bảng dùng chung: chữ 9 pt, độ rộng cố định dọc/ngang, xem trước màn hình phóng 1,5 lần, ký hiệu hậu nghiệm chung, nhãn cột ES và tiêu đề hậu nghiệm hai dòng.
- Sắp lại tùy chọn Giả định / Mô hình / Đầu ra ANCOVA, thống nhất khoảng cách, thụt lề và ghi chú kết quả.

## v0.9.32 - 2026-06-03

### Thay đổi

- Cải thiện trình bày ANCOVA, gồm kiểm soát độ rộng bảng chặt hơn và căn phải thống kê kiểm định.
- Bổ sung chuyển đổi cỡ hiệu ứng LMM kiểu SPSS: eta bình phương riêng phần từ F/df tổng thể và dz cặp dựa trên hiệp phương sai.
- Bổ sung chuyển đổi cỡ hiệu ứng GLMM cho logit nhị phân, đếm với liên kết log và đầu ra hiệu ứng cố định Gaussian.
- Cập nhật Hướng dẫn sử dụng, Phương pháp phân tích và Ghi chú phương pháp tiếng Hàn cho quy trình cỡ hiệu ứng ANCOVA, LMM, GEE và GLMM mới.
- Cải thiện đầu vào Cỡ hiệu ứng để tính tổng thể và từng cặp LMM từ một trong hai bộ đầu vào có sẵn.

## v0.9.31 - 2026-06-02

### Thay đổi

- Bổ sung lưu/mở lịch sử Kết quả với dấu loại tệp riêng `.efs-result`.
- Tách tệp thiết lập đã lưu thành `.efs-settings` có xác minh loại.
- Đổi Thêm kết quả để giữ ảnh chụp trạng thái kết quả đang hiển thị thay vì dựng lại đầu ra.
- Chuẩn hóa độ rộng bảng và quy tắc xuất ngang trên đầu ra phân tích, lịch sử Kết quả, HTML, PDF và Word.
- Cải thiện bố cục bảng Tổng quan mô hình và cảnh báo cho tương quan, kiểm định ghép cặp, phân tích nhân tố, độ tin cậy và hồi quy logistic.
- Loại trang bìa xuất Word để tài liệu Kết quả đã lưu bắt đầu ngay bằng phương pháp và kết quả.
- Bổ sung ANCOVA tự chọn mô hình chuẩn, HC3 vững, dựa trên hạng hoặc tương tác, cùng xuất HTML, PDF, Excel và lịch sử Kết quả.


## v0.9.30 - 2026-06-02

### Thay đổi

- Tinh chỉnh công cụ tính cỡ mẫu/cỡ hiệu ứng bằng cách loại quy trình không thuộc cỡ hiệu ứng khỏi menu cỡ hiệu ứng.
- Bổ sung tính cỡ mẫu nền có thể dừng và báo tiến độ.
- Bổ sung đầu vào tương quan phi cấu trúc LMM và ước lượng bậc tự do SEM/CFA bằng đếm thành phần mô hình.
- Chuẩn hóa cách nhấn mạnh cỡ mẫu cần thiết bằng nhãn `n` rõ ràng và lực kiểm định mặc định 0.95.
- Mở rộng Hướng dẫn sử dụng, Phương pháp phân tích và Ghi chú phương pháp tiếng Hàn về cỡ mẫu, lực kiểm định và cỡ hiệu ứng với công thức và tài liệu tham khảo.
- Cập nhật tài liệu Phương pháp phân tích tiếng Hàn cho đầu ra 0.9.30, gồm Tổng quan mô hình của kiểm định t/ANOVA, ghép cặp, ghép cặp phi tham số và tương quan.
- Đóng gói tài nguyên MathJax cục bộ để hiển thị công thức ngoại tuyến trong Ghi chú phương pháp.


## v0.9.29 - 2026-06-01

### Bổ sung

- Bổ sung menu cấp cao riêng Cỡ mẫu và Cỡ hiệu ứng sau Phân tích.
- Bổ sung công cụ tính cỡ mẫu, lực kiểm định và cỡ hiệu ứng có tài liệu tham khảo cho kiểm định t, ANOVA / ANCOVA, GEE, LMM, phi tham số, tỷ lệ, chi bình phương, McNemar, hồi quy, sống còn và các quy trình lập kế hoạch khác.
- Bổ sung kiểm định tập trung cho các hàm bao tính cỡ mẫu, lực kiểm định đạt được và cỡ hiệu ứng.

### Thay đổi

- Thiết kế lại màn hình cỡ mẫu và cỡ hiệu ứng theo quy trình ba khối dùng chung với các bảng thiết lập phân tích.
- Sắp lại thứ tự menu Cỡ mẫu và Cỡ hiệu ứng theo nhóm thiết kế nghiên cứu.
- Cập nhật đầu ra cỡ hiệu ứng kiểm định t để nhấn mạnh phương pháp đã chọn và hiện các cỡ hiệu ứng có thể quy đổi, bỏ các giá trị trung gian không phải cỡ hiệu ứng.

## v0.9.28 - 2026-05-30

### Sửa lỗi

- Đưa hộp thoại mở dữ liệu/thiết lập Windows lên trước bằng cửa sổ chủ WinForms luôn trên cùng thay cho bộ chọn tệp R nguyên bản.
- Chuyển bảng tải của Phân tích nhân tố và PCA ngay sau bảng tổng quan trên màn hình, HTML/PDF và Excel.
- Sửa Thông tin > Giấy phép nguồn mở để lấy thông báo bên thứ ba từ đường dẫn ứng dụng đóng gói và nhóm siêu dữ liệu giấy phép theo gói EFS trực tiếp, phụ thuộc đi kèm, gói R cơ sở/khuyến nghị và môi trường R.
- Thay bước dọn cổng của trình khởi chạy Windows bằng `netstat` / `taskkill` để tránh treo khi đóng tiến trình ứng dụng đang dùng cổng 7894.


## v0.9.27 - 2026-05-29

### Thay đổi

- Rút tên tệp xuất mặc định người dùng thấy từ `EasyFlow_Statistics_...` sang tiền tố `EFS_...` cho tệp kết quả, dữ liệu, thiết lập và các tệp xuất sinh ra.

### Bổ sung

- Bổ sung Thông tin > Lịch sử phiên bản để xem nhật ký thay đổi đi kèm ngay trong ứng dụng desktop.


## v0.9.26 - 2026-05-29

### Bổ sung

- Bổ sung nhập Excel hai bước với chọn trang tính, ô bắt đầu kiểu A1, điều khiển hàng tiêu đề và xem trước trước khi tải bộ dữ liệu.
- Giữ tùy chọn nhập Excel đã chọn trong thiết lập lưu để mở lại tệp Excel.


## v0.9.25 - 2026-05-29

### Sửa lỗi

- Thống nhất Thêm kết quả / xuất Word của Hồi quy với bảng hệ số trên màn hình bằng cách giữ hàng tham chiếu phân loại, nhãn giá trị và tóm tắt độ phù hợp mô hình một dòng.
- Dùng phần Word nằm ngang cho bảng hệ số hồi quy rộng để đầu ra Word sát bảng kết quả đang hiển thị hơn.


## v0.9.24 - 2026-05-29

### Sửa lỗi

- Sửa hiển thị bảng kiểm định t / ANOVA và phi tham số để p như `.008` và cỡ hiệu ứng như `.022` giữ đủ ba chữ số thập phân khi ký hiệu chú thích có cùng chữ số cuối.


## v0.9.23 - 2026-05-29

### Sửa lỗi

- Thay bộ chọn tệp dữ liệu desktop dựa trên PowerShell bằng hộp thoại Windows nguyên bản `choose.files()` của R để Mở tệp dữ liệu xuất hiện tin cậy từ ứng dụng Electron đã cài.


## v0.9.22 - 2026-05-29

### Sửa lỗi

- Chuyển bộ chọn tệp dữ liệu desktop sang hộp thoại Windows nguyên bản trước khi dự phòng Tcl/Tk, giảm tình trạng hộp thoại mở sau Electron hoặc không xuất hiện.
- Giữ bộ lọc Excel, SAS, Stata, CSV, DAT và SPSS hiện trong bộ chọn tệp dữ liệu.


## v0.9.21 - 2026-05-29

### Bổ sung

- Bổ sung nhập dữ liệu cho Excel cũ `.xls`, SAS `.sas7bdat` / `.xpt` và Stata `.dta`.
- Cập nhật bộ chọn tệp dữ liệu, nội dung thẻ Dữ liệu và kiểm định nhập/xuất cho các định dạng nhập mở rộng.


## v0.9.20 - 2026-05-29

### Thay đổi

- Giảm tải trang Shiny ban đầu bằng cách chỉ dựng nội dung thẻ Trình sửa dữ liệu, Công cụ tính, Phân tích và Thông tin khi mở thẻ.

## v0.9.19 - 2026-05-29

### Thay đổi

- Giảm thời gian khởi động desktop đã cài bằng cách chỉ gắn Shiny và DT lúc khởi động đầu tiên.
- Bỏ quét gói môi trường đi kèm dư thừa lúc Electron khởi động; khả năng có gói vẫn được kiểm tra trong dựng bản và kiểm tra nhanh phát hành.
- Rút ngắn khoảng thăm dò Shiny sẵn sàng của Electron và thêm chẩn đoán thời gian tải BrowserWindow riêng.

## v0.9.18 - 2026-05-29

### Bổ sung

- Bổ sung hàng điều khiển lưu chuẩn năm vị trí vào kết quả Hồi quy logistic.
- Bổ sung xuất Hồi quy logistic sang HTML, PDF, Excel và bộ sưu tập Kết quả đã lưu.

## v0.9.17 - 2026-05-29

### Sửa lỗi

- Đổi Tương quan > Tương quan nâng cao để tương quan biến tiềm ẩn thay thế bộ phương pháp chính, thay vì tạo một bộ kết quả riêng bị trùng.
- Cặp liên tục–thứ bậc/nhị phân đủ điều kiện nay hiện Polyserial trực tiếp trong bảng Phương pháp chính khi bật tương quan biến tiềm ẩn.

## v0.9.16 - 2026-05-29

### Sửa lỗi

- Sửa xuống dòng chú thích p và cỡ hiệu ứng trong bảng kiểm định t / ANOVA và phi tham số độc lập.

## v0.9.15 - 2026-05-29

### Sửa lỗi

- Sửa kiểu ký hiệu chú thích trong dòng của kiểm định t / ANOVA để cỡ hiệu ứng vẫn thẳng hàng và tiếp tục bỏ số 0 đầu.

## v0.9.14 - 2026-05-29

### Thay đổi

- Bổ sung giấy phép ứng dụng GPL, văn bản cung cấp mã nguồn và trang Thông tin cho thông báo nguồn/giấy phép desktop đi kèm.
- Bổ sung thông báo OSS được tạo, báo cáo giấy phép, tập hợp văn bản giấy phép đi kèm và kiểm tra nhanh phát hành cho bộ cài Electron/R.
- Bổ sung báo cáo cắt gọn môi trường R đi kèm và cố định chính xác phiên bản Electron/electron-builder.
- Giảm chi phí khởi động desktop đã cài và bổ sung chẩn đoán thời gian khởi động.
- Loại cơ chế chặn đóng phiên lúc Shiny khởi động có thể khiến ứng dụng desktop mắc ở màn hình xám vô hiệu hóa.
- Dựng lại bộ cài Windows beta cho phiên bản 0.9.14.

## v0.9.13 - 2026-05-28

### Thay đổi

- Bật xuất kết quả Word được hỗ trợ và chuẩn hóa quy tắc xuất giữa Word, PDF và Excel.
- Bổ sung đầu ra Word hướng công bố với bìa, trang phương pháp, chỉ chọn bảng chính, ghi chú bảng, ký hiệu chỉ số trên và B5 dọc mặc định; chỉ bảng rộng dùng ngang.
- Cải thiện độ rộng bảng, tiêu đề, cột hậu nghiệm và thống kê cuối bảng cho ghép cặp, đo lặp, kiểm định t/ANOVA, tương quan, hồi quy, hồi quy phân cấp và logistic khi xuất PDF/Word.
- Cải thiện xuất Excel để tiêu đề, đầu bảng hai cấp, đường viền, ghi chú gộp và độ rộng cột cố định giữ được cấu trúc bảng đang hiển thị.
- Tăng kích thước cửa sổ khởi động Electron và ổn định căn hàng thao tác thiết lập hồi quy sau khi dựng kết quả.
- Dựng lại bộ cài Windows beta cho phiên bản 0.9.13.

## v0.9.12 - 2026-05-27

### Thay đổi

- Cải thiện bố cục PDF và kết quả công bố của ghép cặp, đo lặp ghép cặp, ghép cặp phi tham số, phân tích nhân tố, PCA, hồi quy logistic và kiểm định t / ANOVA.
- Cải thiện khoảng cách bảng, căn tổng quan mô hình và kích thước dấu chìm beta trong báo cáo xuất.
- Tinh gọn lời diễn giải để tóm tắt tập trung vào phương pháp, N, giả định và ghi chú quyết định ngắn gọn.
- Chuẩn hóa xuất PDF khổ A4 và Word khổ B5; bảng vừa chiều rộng in được nhưng giữ quy tắc căn chỉnh đang hiển thị.
- Mở rộng điều chỉnh bảng PDF ngang cho cột cỡ hiệu ứng của kiểm định đo lặp ghép cặp và bảng hệ số hồi quy phân cấp.
- Chuẩn hóa quy tắc bảng Excel để đầu bảng hai cấp, hàng tiêu đề, đường viền, độ rộng cột cố định và hàng ghi chú gộp khớp bố cục bảng hiển thị.
- Bật nút xuất Word ở thẻ Kết quả cho các phiên bản hỗ trợ xuất kết quả.
- Thống nhất phông đầu/thân bảng Word, bật kiểu nút lưu Word hiển thị, dùng hai chữ số thập phân cho trung bình/độ lệch chuẩn trong tóm tắt ghép cặp và gộp bảng tổng quan/giả định/chẩn đoán kiểm định ghép cặp hỗn hợp.
- Giữ đầu bảng hai cấp khi xuất Word, loại logo báo cáo khỏi thân Word, thêm phần ngang cho bảng đo lặp/phân cấp rộng và giảm lề/độ rộng cột PDF cho bảng ghép cặp, đo lặp và phân cấp.
- Đổi tổng quan mô hình kiểm định t / ANOVA để N, phương pháp và lý do thành các cột trực tiếp; tổng quan hồi quy nhiều mô hình dùng đầu bảng hai cấp biến phụ thuộc/mô hình.
- Buộc bảng PDF đo lặp ghép cặp vừa chiều rộng trang ngang, thêm đường kẻ đầu bảng cấp một cho bảng phân cấp và giới hạn phần ngang Word để tài liệu mặc định vẫn B5 dọc.
- Mở rộng cột hậu nghiệm và tolerance, tăng bìa PDF, nhóm cỡ hiệu ứng ghép cặp dưới đầu bảng hai cấp, khôi phục ký hiệu chú thích trên và ghi chú bảng Word, gộp hàng cuối hồi quy bị lặp trong Word.
- Giảm cỡ chữ ghi chú bảng Word, đưa toàn bộ ghi chú đã hiển thị vào Word, đưa ký hiệu đầu mô hình phân cấp lên chỉ số trên và gộp thống kê cuối bảng phân cấp một lần mỗi mô hình.
- Gắn nhãn các lần chỉ chạy Block 1 từ màn hình hồi quy phân cấp là hồi quy thông thường khi thêm vào bộ sưu tập Kết quả đã lưu.
- Đổi xuất Word để chỉ gồm bảng chính sẵn sàng công bố, mỗi bảng bắt đầu trang riêng, giữ kích thước hình đang hiển thị không phóng lớn; chỉ dùng ngang cho bảng ghép cặp/phân cấp rộng và ma trận tương quan từ 10 biến.
- Căn giữa thống kê cuối bảng hồi quy trong Word và thêm đường trên đậm phía trên F(p); đồng nhất phương sai phần dư là một mục cuối bảng x²(p).
- Tăng cửa sổ khởi động Electron, thêm bìa Word và trang phương pháp phân tích, đặt hai hình hồi quy mỗi trang và loại bảng chi tiết hậu nghiệm kiểm định t/ANOVA khỏi xuất bảng công bố Word.
- Cải thiện khoảng cách bảng Word, độ rộng cột tần số/mô tả, hàng tóm tắt hồi quy, chuyển phần ngang và kích thước hình để giảm xuống dòng, khoảng trắng thừa và trang trắng.
- Ổn định căn hàng thao tác hồi quy sau khi dựng kết quả, tăng lọc bảng hậu nghiệm Word, mở rộng cột n(%)/M±SD kết hợp và IQR, thu gọn bảng tương quan rộng.

## v0.9.11 - 2026-05-27

### Thay đổi

- Bổ sung tóm tắt tổng quan mô hình và kiểm tra giả định gọn cho ghép cặp, kiểm định t / ANOVA, hồi quy và hồi quy logistic.
- Chuyển chẩn đoán giả định chi tiết vào bảng kiểm tra riêng, giữ tổng quan tập trung vào N, phương pháp và lý do ngắn gọn.
- Bổ sung bảng tùy chọn dạng thẻ cho phân tích nhân tố, PCA và kiểm định t / ANOVA, giữ trạng thái thẻ và cải thiện khoảng cách.
- Đổi mặc định cỡ hiệu ứng hồi quy để hiện f2 và không chọn sr2.


## v0.9.10 - 2026-05-27

### Thay đổi

- Bổ sung quy trình đóng gói Electron beta với môi trường R đi kèm, trình khởi chạy cửa sổ desktop, siêu dữ liệu bộ cài và biểu tượng EasyFlow.
- Cải thiện nhập CSV/Excel tiếng Hàn bằng thử các bảng mã thông dụng và chuẩn hóa tên, giá trị ký tự đã nhập.
- Giữ loại đo lường nhị phân, phân loại và thứ bậc đã kiểm tra khi lưu/tải thiết lập để menu Phân tích dùng cùng loại biến như bước xem Dữ liệu.
- Giới hạn cột kết quả Tần số / Thống kê mô tả ở các thống kê phù hợp loại biến đã chọn.
- Tinh chỉnh dấu chìm beta và chỗ giữ chỗ xuất Word trong xuất kết quả.


## v0.9.9 - 2026-05-27

### Thay đổi

- Thiết kế lại mã hóa lại trên cùng biến quanh bước xếp hàng `Add` và bước cuối `Apply` để xem quy tắc trước khi thay đổi dữ liệu.
- Bổ sung quy tắc mã hóa lại trong hàng đợi có thể sửa, với chọn hàng, xóa, loại biến mặc định tự động và suy luận tự động loại đo lường đầu ra.
- Bổ sung mã hóa lại giá trị đơn cho giá trị phân loại quan sát, dấu giá trị thiếu, thông báo không khớp và đổi giá trị sang hoặc từ `NA`.
- Cải thiện điều khiển mã hóa lại thành nhóm, toán tử khoảng, căn bảng, vị trí nút thao tác và khoảng cách bố cục Mã hóa lại biến.


## v0.9.8 - 2026-05-26

### Thay đổi

- Bổ sung menu Thông tin với Tổng quan, Hướng dẫn sử dụng, Phương pháp phân tích, Ghi chú phương pháp và thông tin ứng dụng.
- Mở rộng tài liệu tiếng Hàn về hướng dẫn người dùng, phương pháp đã triển khai, ghi chú phương pháp, tổng quan gói/môi trường chạy, tiêu chí và tài liệu tham khảo.
- Làm rõ thuật ngữ đồng nhất phương sai phần dư hồi quy và nhãn phương pháp xu hướng bảng chéo.
- Thêm 20.000 vào tùy chọn số lần tái lấy mẫu bootstrap được ghi chép; giữ 50.000 là khuyến nghị.
- Chuẩn hóa tên trong tài liệu để **EasyFlow Statistics** luôn được viết đầy đủ và nhấn mạnh nhất quán.

## v0.9.7 - 2026-05-26

### Thay đổi

- Bổ sung chọn phương pháp tương quan tự động: Pearson cho cặp liên tục chuẩn, Spearman cho cặp không chuẩn hoặc thứ bậc.
- Bổ sung điều kiện bảo vệ để tương quan, ghép cặp, kiểm định t / ANOVA, hồi quy và logistic bỏ qua biến/mô hình không hợp lệ thay vì dừng cả phân tích.
- Bổ sung cảnh báo và đầu ra bỏ qua cho cỡ mẫu nhỏ, phương sai bằng không, toàn bộ đồng hạng, ô thưa, nguy cơ phân tách, thiếu hạng và ngưỡng VIF.
- Bổ sung tùy chọn ma trận Pearson / polychoric cho phân tích nhân tố và PCA, cùng hướng dẫn dữ liệu thứ bậc và cảnh báo cỡ mẫu.
- Hợp nhất hàm hỗ trợ cảnh báo và đầu ra bị bỏ qua giữa màn hình kết quả và xuất Excel.


## v0.9.6 - 2026-05-25

### Thay đổi

- Bổ sung menu Kiểm định ghép cặp phi tham số riêng dùng Wilcoxon hạng có dấu và Friedman.
- Bổ sung tùy chọn hậu nghiệm ghép cặp Bonferroni và Holm-Bonferroni, mặc định Bonferroni cho menu ghép cặp.
- Bổ sung trung vị, Q1~Q3 và ghi chú cỡ hiệu ứng Wilcoxon cho kết quả ghép cặp phi tham số.
- Thống nhất đầu bảng, ký hiệu chú thích, đầu ra xuất và bố cục nút thao tác giữa ghép cặp và ghép cặp phi tham số.


## v0.9.5 - 2026-05-25

### Thay đổi

- Bổ sung menu Kiểm định phi tham số độc lập dùng Mann-Whitney U và Kruskal-Wallis.
- Bổ sung tóm tắt trung vị và tứ phân vị cho kiểm định phi tham số độc lập.
- Bổ sung cỡ hiệu ứng Cliff's delta cho Mann-Whitney U.
- Sửa ký hiệu chữ hậu nghiệm gọn để các nhóm chung không khác biệt có ý nghĩa nhận tổ hợp chữ.
- Hiển thị ký hiệu chú thích p và cỡ hiệu ứng trong cột hẹp liền kề để bảng căn ổn định.
- Cải thiện khoảng cách bảng tùy chọn Kiểm định phi tham số và Kiểm định ghép cặp.


## v0.9.4 - 2026-05-25

### Thay đổi

- Cải thiện dấu chìm HTML/PDF chỉ dành cho phát triển với nhận diện EasyFlow và StatEdu ngang.
- Bật xuất hình tương quan cho ma trận biểu đồ phân tán và bản đồ nhiệt tương quan.


## v0.9.3 - 2026-05-25

### Thay đổi

- Thống nhất bảng tải PCA với kiểu bảng phân tích nhân tố, gồm h², độ phức tạp, trị riêng, phương sai, phương sai tích lũy và hàng chẩn đoán KMO / Bartlett, nhưng bỏ cột độ tin cậy.
- Cải thiện thiết lập PCA cho chọn ma trận, chọn thành phần theo phương sai tích lũy và căn trường số chọn thành phần.
- Cải thiện kiểu bảng chẩn đoán nhân tố và hoàn thiện vị trí hàng tóm tắt KMO / Bartlett.
- Giữ cả năm điều khiển lưu phân tích hiện trong bản phát triển, bổ sung PDF / Thêm kết quả cho các mô-đun còn lại.
- Loại trang trí bìa PDF và nhãn in tên tệp/ngày nội bộ, thêm số trang ở dưới bên phải.
- Bổ sung xử lý nhận diện bìa PDF theo phiên bản, gồm logo StatEdu và tên StatEdu Statistical Research Institute trong bản phát triển.
- Thêm ngày xuất PDF dưới ngày lưu trên bìa báo cáo.
- Thêm dấu chìm chỉ dành cho phát triển vào báo cáo HTML và PDF xuất ra.


## v0.9.1 - 2026-05-24

### Thay đổi

- Cải thiện phân tích nhân tố khám phá với ma trận tải sắp xếp, lọc tải nhỏ tùy chọn, đánh dấu giá trị có vấn đề, phương sai chung, độ phức tạp, tóm tắt trị riêng/phương sai và ma trận cấu trúc xoay xiên.
- Bổ sung tóm tắt độ tin cậy nhân tố con tùy chọn ngay cạnh ma trận tải nhân tố.
- Cải thiện chẩn đoán nhân tố về chọn phương pháp trích theo tính chuẩn, số nhân tố cố định cao, giá trị thiếu/vô hạn và vấn đề độ tin cậy cấp mục.
- Thu gọn bảng tùy chọn nhân tố để mọi tùy chọn vừa khối thiết lập ba cột chuẩn.


## v0.9.0 - 2026-05-24

### Thay đổi

- Bổ sung tích lũy ở thẻ Kết quả để thu thập đầu ra hỗ trợ theo thứ tự qua Thêm kết quả.
- Bổ sung xuất bộ sưu tập Kết quả sang HTML, PDF, Excel và Word.
- Chưa đưa phân tích nhân tố và PCA vào Thêm kết quả cho đến khi quyết định định dạng bảng Kết quả cuối cùng.


## v0.8.12

### Thay đổi

- Bổ sung phân tích nhân tố khám phá với trích trục chính và hợp lý cực đại, xoay Varimax/Oblimin, chọn theo trị riêng hoặc số nhân tố cố định, chọn phương pháp theo tính chuẩn, chẩn đoán KMO / Bartlett, biểu đồ scree và xuất.
- Bổ sung PCA với đầu vào ma trận tương quan hoặc hiệp phương sai, chọn thành phần theo trị riêng, số cố định hoặc phương sai tích lũy, xoay tùy chọn, biểu đồ scree/thành phần, chẩn đoán và xuất.
- Bổ sung kiểm định tính toán và xuất phân tích nhân tố/PCA.


## v0.8.11

### Thay đổi

- Khôi phục nhận diện thanh điều hướng bằng ảnh logo EasyFlow Statistics ngang thay vì ghép biểu tượng với chữ HTML.

## v0.8.10

### Thay đổi

- Sửa tương phản nhận diện thanh điều hướng để chữ logo EasyFlow Statistics và phiên bản vẫn thấy trên nền đầu trang sáng.
- Chuẩn hóa ký hiệu ý nghĩa hậu nghiệm có thứ tự để các mẫu so sánh chung hiển thị nhất quán, gồm `3, 2>1` và `3>2, 1`.
- Cải thiện chuyển biến kiểm định t / ANOVA khi đưa biến trở lại từ danh sách phụ thuộc hoặc độc lập.
- Cải thiện suy luận loại đo lường tự động để biến số có phần thập phân không bị phân loại thành biến phân loại chỉ vì ít giá trị duy nhất.
- Giới hạn dọn dẹp trình khởi chạy ở cổng ứng dụng trước khi bắt đầu phiên EasyFlow Statistics mới.
- Bổ sung nhận diện giá trị thiếu tự động với bước xem xét chuyển sang `NA`.
- Bổ sung biến đổi theo công thức để tạo biến mới từ biểu thức số, văn bản, thống kê, ngày và điều kiện.
- Sắp lại lệnh Trình sửa dữ liệu, hợp nhất mã hóa lại vào một quy trình Mã hóa lại biến với đích cùng biến hoặc biến mới.

## v0.8.7

### Thay đổi

- Bổ sung tự nhận diện văn bản Likert và chuyển đổi hàng loạt dữ liệu khảo sát nhập vào.
- Bổ sung điều khiển xem xét Likert theo nhóm cho nội dung mục, nhãn gốc, giá trị số, đảo mã và loại biến sau chuyển đổi.
- Cải thiện xử lý thiếu một phần mức Likert để mục không quan sát đủ mức trả lời vẫn khớp thang đầy đủ đã nhận diện.
- Thu hẹp các cột thống kê gọn của hồi quy phân cấp để dễ đọc bảng.


## v0.8.6

### Thay đổi

- Bổ sung xem xét biến Bước 3 với dạng Nhãn / Biến và quy trình Áp dụng thống nhất cho nhãn giá trị, nhãn biến và thay đổi loại đo lường.
- Đảm bảo thay đổi loại đo lường Bước 3 được truyền tới menu phân tích sau khi áp dụng.
- Giữ điều khiển xem xét Bước 3 nhất quán với bố cục quy trình Dữ liệu hiện tại.


## v0.8.4

### Thay đổi

- Cải thiện bảng tần số, ghi chú kiểm định t / ANOVA, p/CI tương quan, phân tích mục độ tin cậy, khoảng cách Durbin-Watson hồi quy, chú giải phương pháp hồi quy phân cấp và đầu ra logistic không ổn định.
- Bổ sung sửa hàng loạt loại đo lường ở Bước 2 cho các biến được đánh dấu trên trang Dữ liệu hiện tại.
- Giữ mức đo lường nguồn khi đảo mã tự động tạo biến mới hoặc ghi đè biến có sẵn.


## v0.8.3

### Thay đổi

- Bổ sung quy trình Trình sửa dữ liệu để kiểm tra lỗi mã hóa, đảo mã tự động, mã hóa lại thành biến khác và tính biến theo hàng.
- Bổ sung điều khiển áp dụng sửa lỗi, xem trước biến sinh ra, lưu dữ liệu sau tạo biến và kiểm định mã hóa lại, đọc CSV / DAT đã sao chép.
- Chuẩn hóa bố cục thiết lập/kết quả giữa Trình sửa dữ liệu, Công cụ tính và Phân tích, gồm vị trí nút chung và hành vi dự phòng của trình xem dữ liệu đã chọn.
- Cập nhật mặc định tùy chọn và điều khiển hậu nghiệm phi tham số, gồm mặc định alpha thứ bậc trong độ tin cậy và khoảng cách kiểm định t / ANOVA.
- Cải thiện tệp dữ liệu đồng bộ đám mây bằng cách sao chép SAV, CSV và DAT tới nơi đọc tạm trước nhập.
- Khôi phục chọn nhiều trong danh sách chuyển bằng Ctrl / Shift / Ctrl+A, giữ đồng bộ đầu vào Shiny ổn định.


## v0.8.2

### Thay đổi

- Bật mặc định tùy chọn thường dùng ở thiết lập ghép cặp, tần số, tương quan, độ tin cậy, logistic và kiểm định t / ANOVA.
- Bổ sung lựa chọn hiệu chỉnh hậu nghiệm phi tham số độc lập cho so sánh tiếp theo Kruskal-Wallis, mặc định Bonferroni và có Holm Bonferroni.
- Cải thiện khoảng cách bảng tùy chọn kiểm định t / ANOVA để điều khiển hậu nghiệm và cỡ hiệu ứng vừa trong bảng thiết lập.
- Bổ sung mã hóa lại trên cùng biến trong Trình sửa dữ liệu.

## v0.8.1

### Thay đổi

- Gộp Kiểm định ghép cặp (2) và (3+) vào một thiết lập tự chọn phân tích phù hợp theo số lần đo lặp.
- Đổi tên quy trình hồi quy phân cấp thành Hồi quy và bỏ menu hồi quy riêng, vẫn giữ hồi quy một khối và phân cấp nhiều khối.
- Bổ sung tiến độ và điều khiển dừng bootstrap vào quy trình hồi quy thống nhất.
- Cập nhật kích thước bố cục ghép cặp và nhận diện thẻ Dữ liệu.


## v0.8.0

### Thay đổi

- Bổ sung thiết lập và kết quả Logistic cho biến phụ thuộc nhị phân, thứ bậc và đa thức, gồm mô hình khối phân cấp, OR / CI, tùy chọn pseudo R2, VIF, hàng độ phù hợp và cảnh báo.
- Bổ sung điều khiển Đặt lại thiết lập chung trên màn hình phân tích, chỉ bật khi khối gán phân tích có biến.
- Chuẩn hóa truy cập trình xem dữ liệu chọn, nhấp đúp xóa khỏi danh sách chuyển và khoảng cách lưới ba bảng trong menu phân tích.
- Cập nhật xử lý khối hồi quy và hồi quy phân cấp để thu gọn các khối đầu trống trước khi chạy.


## v0.7.11

### Thay đổi

- Đổi thiết lập Bảng chéo để biến cột được gán trên biến hàng, với kích thước bảng cột/hàng phù hợp số biến dự kiến.
- Bổ sung xuất PDF bảng chéo và bật mặc định mọi thao tác lưu cho bản phát triển.
- Chuẩn hóa bảng chéo với thống kê căn trên, tiêu đề cột căn giữa, giá trị hàng căn trái và ghi chú cỡ hiệu ứng đánh số.
- Chuẩn hóa cỡ hiệu ứng trên mọi đầu ra thành ba chữ số thập phân, không có số 0 đầu.
- Bổ sung ghi chú p, cỡ hiệu ứng và xu hướng được đánh số cho kiểm định t / ANOVA, hiển thị ký hiệu dưới dạng chỉ số trên.
- Bổ sung kiểm định hiển thị ghi chú kiểm định t / ANOVA và mở rộng kiểm định bảng chéo.


## v0.7.10

### Thay đổi

- Bổ sung Phân tích bảng chéo cho biến nhị phân, thứ bậc và phân loại, với chi bình phương Pearson, Fisher chính xác / Monte Carlo dự phòng và phân tích xu hướng.
- Bổ sung gán nhiều biến hàng/cột có sắp thứ tự, bảng nhóm theo cột, tùy chọn phần trăm hàng/cột/tổng và ô n/phần trăm tách tùy chọn.
- Bổ sung chú thích phương pháp p, p cho xu hướng với ghi chú theo phương pháp, ghi chú cỡ hiệu ứng và xuất HTML / Excel cho bảng chéo.
- Bổ sung kiểm định thống kê, hiển thị, thứ tự biến và hàm hỗ trợ xuất bảng chéo.


## v0.7.9

### Thay đổi

- Thu gọn và căn khoảng cách các bảng công cụ tính EQ-5D, hội chứng chuyển hóa và mức độ chuyển hóa.
- Ẩn bảng tiêu chí mặc định hội chứng chuyển hóa khi chọn tiêu chí Tùy chỉnh.
- Làm lại bảng Công thức mức độ chuyển hóa theo các bảng tham khảo công cụ tính khác và tách phần Đầu ra.
- Bổ sung kiểm định công cụ tính HINT8, EQ-5D, hội chứng chuyển hóa, FRS, ASCVD10 và mức độ chuyển hóa.


## v0.7.7

### Thay đổi

- Đổi hiển thị giá trị ban đầu HINT8 thành ma trận mục theo mức gọn.
- Giữ bảng thiết lập HINT8 hiện sau khi tải dữ liệu kể cả không có biến thứ bậc.
- Thu gọn khoảng cách bảng giá trị ban đầu HINT8.

## v0.7.6

### Thay đổi

- Đổi giá trị ban đầu EQ-5D từ danh sách tham khảo dài thành ma trận chiều theo mức gọn.

## v0.7.5

### Thay đổi

- Sửa áp dụng nhãn ở Dữ liệu Bước 3 để nhãn biến, nhãn giá trị và loại đo lường được áp dụng bằng một lần nhấp và lưu trong thiết lập.
- Bổ sung giới hạn xuất trả phí cho PDF, Excel và Thêm kết quả, giữ HTML và xuất hình trong chế độ miễn phí.
- Bổ sung xuất PDF hồi quy và hồi quy phân cấp với bìa, bố cục in dọc/ngang hỗn hợp, bảng rộng co giãn và trang biểu đồ hai cột.
- Cải thiện HTML đã lưu thành trình xem có cuộn bảng ngang, giữ bố cục gốc trên màn hình.
- Chuẩn hóa bố cục nút lưu hồi quy/hồi quy phân cấp, bật mặc định sr2, f2 và VIF.
- Sửa hộp thoại lưu hỏi lặp sau khi hủy và giảm lỗi kích hoạt thẻ menu phân tích.

## v0.7.4

### Thay đổi

- Sắp lại điều hướng trên thành Dữ liệu, Trình sửa dữ liệu, Công cụ tính, Phân tích, Kết quả và Thông tin.
- Bổ sung nhóm menu Trình sửa dữ liệu và Phân tích, gồm menu ghép cặp và hồi quy lồng nhau.
- Cải thiện menu lồng nhau để menu con Phân tích và Công cụ tính dùng kích hoạt thẻ Shiny thông thường.
- Giảm chậm thiết lập bằng tránh đẩy bảng Dữ liệu Bước 3 không cần thiết và lặp tóm tắt thông tin biến khi chuyển menu.

## v0.7.3

### Thay đổi

- Bổ sung mô-đun công cụ tính HINT8, EQ5D, Metabolic Syndrome, Metabolic Severity, FRS và ASCVD10.
- Đưa đầu ra công cụ tính trở lại dữ liệu đang tải để dùng được trong menu phân tích.
- Loại phần còn sót của Dữ liệu Bước 4/5 cũ và hoàn thiện sửa nhãn biến ở Bước 3.
- Chuẩn hóa ghi chú bảng kết quả để độ rộng ghi chú khớp bảng trên các phân tích.


## v0.7.2

### Thay đổi

- Bổ sung khối nhân tố con Độ tin cậy với hàng tổng, phân tích mục kết hợp và chẩn đoán loại từng mục trên toàn bộ mục.
- Tinh chỉnh tùy chọn Độ tin cậy để chỉ hiện và kiểm định thống kê omega khi bật tùy chọn omega.
- Điều chỉnh kích thước danh sách, độ rộng bảng và căn tiêu đề Độ tin cậy.


## v0.7.1

### Thay đổi

- Cải thiện nhãn đo lặp, tiêu đề kết quả nhóm, ghi chú hậu nghiệm và chú giải cỡ hiệu ứng của Kiểm định ghép cặp (3+).
- Chuẩn hóa chiều cao danh sách chuyển và căn nút chuyển trong Độ tin cậy, Tần số, Ghép cặp, kiểm định t/ANOVA, Tương quan, Hồi quy và Phân cấp.

## v0.7.0

### Bổ sung

- Bổ sung thẻ Ghép cặp (3+) cho ít nhất ba lần đo, định tuyến RM ANOVA, Friedman và Cochran's Q với kiểm tra giả định và so sánh hậu nghiệm.
- Bổ sung cỡ hiệu ứng đo lặp: eta bình phương riêng phần, Kendall's W, Hedges' g và Wilcoxon r.

### Thay đổi

- Cải thiện bảng ghép cặp, ký hiệu hậu nghiệm, vị trí cỡ hiệu ứng và bố cục xuất HTML/Excel.
- Điều chỉnh bảng chọn ghép cặp để dùng hàng đo lặp theo nhóm và chiều cao danh sách đích gọn hơn.


## v0.6.8

### Bổ sung

- Bổ sung thẻ Ghép cặp cho hai lần đo, định tuyến t ghép cặp/Wilcoxon, McNemar/McNemar chính xác cho cặp nhị phân, Stuart-Maxwell/Bowker cho cặp phân loại.
- Bổ sung kiểm tra giả định chênh lệch cặp tùy chọn bằng Shapiro-Wilk hoặc độ lệch/độ nhọn, cùng sàng lọc ngoại lệ 3*IQR.
- Bổ sung xuất HTML và Excel cho bảng ghép cặp và ghi chú kiểm tra giả định.


## v0.6.7

### Thay đổi

- Căn giữa lại nút chuyển phân tích bằng bỏ độ lệch xuống chung và căn bố cục hai nút hồi quy/kiểm định t với hàng khối đích.


## v0.6.6

### Thay đổi

- Hồi quy và hồi quy phân cấp nay hiện hệ số phân loại dưới dạng `variable:level` và gồm hàng tham chiếu mặc định ngay cả khi không đặt tham chiếu rõ trong thẻ Dữ liệu.


## v0.6.5

### Thay đổi

- Khôi phục căn nút chuyển phân tích theo hình học thiết lập 0.5.7 và áp dụng vào thẻ Độ tin cậy mới.
- Khôi phục nút lưu hồi quy phân cấp về vị trí hàng thao tác 0.5.7.


## v0.6.4

### Thay đổi

- Áp dụng dấu sao mức ý nghĩa cho ma trận hệ số tương quan khi chọn tùy chọn mức ý nghĩa.


## v0.6.3

### Thay đổi

- Sửa thay đổi mức đo lường Bước 3 để chuyển sang thẻ phân tích đẩy cả lựa chọn đo lường hiện tại cùng nhãn.
- Đưa bộ chọn đo lường của nhãn phân loại Bước 3 vào đường thu thập đầu vào trực tiếp phía máy chủ.
- Chuyển khối nút lưu hồi quy phân cấp trở lại dưới khối thiết lập thứ ba.


## v0.6.2

### Thay đổi

- Sửa lọc biến phụ thuộc trong hồi quy và hồi quy phân cấp để tôn trọng mức đo lường ghi đè ở Bước 3 khi chọn biến phụ thuộc liên tục.
- Căn lại nút chuyển biến sau thay đổi hình học bảng thiết lập dùng chung.


## v0.6.1

### Thay đổi

- Sửa truyền mức đo lường để thay đổi loại biến Bước 3 áp dụng ngay vào danh sách thiết lập Độ tin cậy, Tần số, kiểm định t/ANOVA, Tương quan, Hồi quy và Phân cấp.


## v0.6.0

### Thay đổi

- Bổ sung phân tích Độ tin cậy với chọn mục cùng mức đo, tự chọn KR-20/Cronbach's alpha/omega, hỗ trợ alpha/omega thứ bậc, chẩn đoán mục và ghi chú phương pháp theo tính chuẩn.
- Chuẩn hóa điều khiển lưu phân tích giữa các thẻ kết quả với nút HTML/hình/Excel/thêm kết quả theo phiên bản.
- Cải thiện xuất HTML và Excel để lưu ghi chú đúng độ rộng bảng và độ rộng cột Excel dễ đọc.
- Bổ sung `psych` làm bộ máy phân tích alpha, omega và hệ số thứ bậc dựa trên polychoric.


## v0.5.7

### Thay đổi

- Làm lại thiết lập hồi quy phân cấp để hiện Biến phụ thuộc và một Khối đang hoạt động, chuyển khối trước/sau mà vẫn giữ trạng thái biến Block 1/2/3.
- Điều chỉnh chiều cao bảng, cỡ danh sách và vị trí chuyển khối của thiết lập phân cấp cho bố cục gọn, thẳng hàng hơn.
- Cải thiện đường phân cách giữa hàng hệ số và hàng độ phù hợp mô hình trong hồi quy phân cấp.
- Điều chỉnh độ rộng cột và khoảng đệm bảng hồi quy phân cấp cho đầu ra ba mô hình rộng có cột cỡ hiệu ứng.


## v0.5.6

### Thay đổi

- Bổ sung xuất HTML dùng chung giữa các thẻ kết quả, thống nhất bảng HTML lưu với kiểu hồi quy trong ứng dụng.
- Mở rộng tương quan với tự chọn phương pháp theo mức đo, tương quan tiềm ẩn tùy chọn, ma trận phương pháp/lý do, ma trận p và CI 95%, hình phân tán/bản đồ nhiệt lớn hơn.
- Cải thiện xuất Excel/HTML cho hồi quy và phân cấp, gồm đầu bảng hai cấp, căn số, ghi chú và hành vi hộp thoại lưu.
- Ổn định điều khiển tùy chọn hồi quy và phân cấp trong quy trình bootstrap.
- Chuẩn hóa hình học khối thiết lập cho các thẻ phân tích không phân cấp.


## v0.5.5

### Thay đổi

- Triển khai chạy Tương quan với tương quan từng cặp, kiểm tra tính chuẩn tùy chọn, p, khoảng tin cậy, ký hiệu ý nghĩa, ma trận và đồ thị.
- Bổ sung xuất bảng Excel cho kiểm định t / ANOVA.
- Bổ sung xuất bảng Excel và hình chẩn đoán phần dư cho hồi quy phân cấp.
- Cập nhật yêu cầu gói chạy cục bộ cho các phụ thuộc phân tích mới.


## v0.5.4

### Thay đổi

- Bổ sung cỡ hiệu ứng, xu hướng, ký hiệu ý nghĩa có thứ tự và hậu nghiệm mở rộng cho kiểm định t / ANOVA.
- Cải thiện hành vi tùy chọn tính chuẩn, nhãn tổng quan, nhãn thống kê, ghi chú p và bảng kiểm định t / ANOVA.
- Bổ sung kiểm định đa khoảng Duncan qua agricolae và cập nhật tải gói bắt buộc.
- Sửa cột thống kê tùy chọn Tần số / Mô tả, điều chỉnh bảng về độ rộng gọn kiểu hồi quy.
- Cải thiện khoảng cách, đường phân cách và nhãn chi bình phương của bảng hồi quy phân cấp.


## v0.5.2

### Thay đổi

- Bổ sung số mẫu bootstrap và giá trị seed vào tổng quan khi dùng hồi quy bootstrap.
- Ổn định chuyển biến hồi quy khi Shift chọn phần tử đầu và giảm đặt lại cuộn do chọn.
- Tăng chiều cao danh sách biến có sẵn của hồi quy để hiện 20 biến.
- Bổ sung và cải thiện tài nguyên SVG ý tưởng logo EasyFlow Statistics.


## v0.5.1

### Thay đổi

- Ổn định chuyển biến hồi quy, gồm chọn nhiều, Ctrl+A, hướng di chuyển và giữ thứ tự.
- Cải thiện bố cục hồi quy, chiều cao danh sách cố định, vị trí nút chuyển và giữ trạng thái ô tùy chọn.
- Bổ sung hành vi xuất bảng/hình dùng chung cho đầu ra phân tích.
- Bổ sung khung thiết lập và đầu ra Tần số / Mô tả với giao diện chuyển biến dùng chung.
- Cập nhật siêu dữ liệu trích dẫn EasyFlow Statistics.


## v0.5.0

### Bổ sung

- Bổ sung khung thẻ Phân cấp cho hồi quy bội phân cấp với một biến phụ thuộc và tổ chức dự báo Block 1/2/3.
- Bổ sung chuyển biến Block 2 sang Block 3 cho thiết lập hồi quy phân cấp tương lai.
- Bổ sung khung thẻ Tổng quát cho các mô hình hồi quy tổng quát tương lai.

### Thay đổi

- Cập nhật tùy chọn Tổng quát cho mô hình kiểu GLM bằng bỏ bootstrap và sr2/f2 chỉ dành cho OLS.
- Nhóm mô hình đếm thành Poisson / Negative binomial / Zero-inflated, giữ Gamma cho kết quả liên tục dương.
- Cập nhật tùy chọn báo cáo Tổng quát để dùng exp(B) dưới dạng IRR / tỷ số.

## v0.4.1

### Thay đổi

- Đổi tên thẻ hồi quy và tiêu đề trang từ EasyFlow Statistics sang Hồi quy.

### Sửa lỗi

- Sửa xử lý cảnh báo VIF trống có thể hiện `missing value where TRUE/FALSE needed`.

## v0.4.0

### Bổ sung

- Bổ sung hộp thoại lưu Windows nguyên bản cho xuất bảng Excel và chọn thư mục hình.
- Bổ sung xuất sổ Excel kiểu bảng tạp chí với bảng hệ số, hàng độ phù hợp mô hình, chẩn đoán và ghi chú.
- Bổ sung cảnh báo đa cộng tuyến dựa trên VIF, hướng dẫn khi VIF nghiêm trọng.
- Bổ sung Ridge, LASSO và Elastic Net với kiểm định chéo cho đa cộng tuyến nghiêm trọng.
- Bổ sung bảng hồi quy phạt theo SCI cho hiệu năng mô hình, so sánh hệ số OLS/phạt và biến dự báo được giữ.

### Thay đổi

- Cải thiện Tổng quan mô hình trong Excel với gộp ô biến độc lập chung, xuống dòng và độ rộng gọn.
- Ẩn chẩn đoán phần dư và Durbin-Watson khi hiện kết quả hồi quy phạt.
- Cập nhật tên trang tính kết quả hồi quy để dùng nhãn hoặc tên biến phụ thuộc.

### Sửa lỗi

- Sửa lỗi lưu thiết lập khi không chọn biến phân loại.
- Ngăn lưu tên đo lường trống trong các giá trị ghi đè đo lường.

## v0.3.1

### Thay đổi

- Gộp Tổng quan mô hình của nhiều biến phụ thuộc vào một bảng.
- Sắp lại hồi quy để hiện toàn bộ bảng hệ số trước, sau đó đồ thị chẩn đoán.
- Gộp kiểm tra giả định và Durbin-Watson thành mỗi loại một bảng cho các biến phụ thuộc.
- Chỉ dùng nhãn biến phụ thuộc khi có nhãn, nếu không dùng tên biến.
- Hiện hướng dẫn cỡ hiệu ứng một lần sau các bảng hệ số.

## v0.2.0

### Bổ sung

- Bổ sung đầu ra hồi quy tuần tự cho nhiều biến phụ thuộc.
- Bổ sung tiến độ và điều khiển dừng bootstrap trong bảng thiết lập hồi quy.
- Bổ sung đầu ra tùy chọn sr2, f2 và chẩn đoán VIF/đa cộng tuyến.
- Bổ sung tham khảo hướng dẫn cỡ hiệu ứng sr2 và Cohen's f2.
- Bổ sung đồ thị chẩn đoán phần dư đặt cạnh nhau.

### Thay đổi

- Thiết kế lại hồi quy với Biến, Biến phụ thuộc, Biến độc lập và điều khiển bootstrap.
- Cập nhật Tổng quan mô hình để báo biến phụ thuộc, biến độc lập, N, R2(adj. R2), F(p) và phương pháp đã chọn.
- Chuẩn hóa đồ thị đồng nhất phương sai phần dư và hiển thị ranh giới ngoại lệ.
- Cải thiện ký hiệu chỉ số trên/dưới trong đầu ra hồi quy.

### Sửa lỗi

- Sửa chỉnh nhãn để văn bản không bị đặt lại sau mỗi ký tự gõ.
- Sửa tải thiết lập và truyền nhãn biến, mức đo, tham chiếu, nhãn giá trị giữa các bước.
- Sửa xử lý dừng bootstrap và vị trí hiển thị tiến độ.

## v0.1.2

### Bổ sung

- Bổ sung nút Lên/Xuống dưới Biến phụ thuộc trong thiết lập hồi quy.
- Giữ thứ tự biến phụ thuộc trong thiết lập lưu và hiển thị tóm tắt.

## v0.1.1

### Sửa lỗi

- Bật nút tiêu đề `selected` ở Bước 3 để chọn hoặc bỏ chọn mọi biến hiện của vai trò đang hoạt động.
- Đổi nút áp dụng Bước 3 để gửi trạng thái ô chọn DataTables hiện tại.
- Giữ và đồng bộ chỉnh sửa `var_label`, `reference`, `value` và `label` khi DataTables vẽ lại.

## v0.1.0

### Bổ sung

- Nguyên mẫu ứng dụng Shiny đầu tiên.
- Tải lên CSV và chọn biến.
- Phân tích hồi quy bội.
- Kiểm định tính chuẩn phần dư Kolmogorov-Smirnov có hiệu chỉnh Lilliefors.
- Kiểm định đồng nhất phương sai Breusch-Pagan.
- Sai số chuẩn vững HC3.
- Khoảng tin cậy bootstrap.
- Tra dL/dU Durbin-Watson bằng `C:/StatEdu/easyflow_statistics/easyflow_statistics_3.0.xlsx`.

