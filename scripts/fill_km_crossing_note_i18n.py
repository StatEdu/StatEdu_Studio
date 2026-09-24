import json, re
from pathlib import Path
source = 'This descriptive screen detects changes in the relative ordering of Kaplan–Meier step functions. When curves cross, do not summarize the comparison with a single log-rank p-value alone; also review a prespecified RMST or time-specific effects. Do not select a weighted test post hoc to obtain the smallest p-value.'
translations = {
'ja': 'この記述的な確認では、Kaplan–Meier階段関数の相対的な順序の変化を検出します。曲線が交差する場合、単一のログランクp値だけで比較を要約せず、事前に指定したRMSTや時点別の効果も検討してください。最小のp値を得るために、重み付き検定を事後的に選択しないでください。',
'zh': '此描述性筛查检测Kaplan–Meier阶梯函数相对顺序的变化。当曲线交叉时，不要仅用单个log-rank p值概括比较；还应审查预先指定的RMST或特定时间点的效应。不要为了获得最小p值而事后选择加权检验。',
'es': 'Esta evaluación descriptiva detecta cambios en el orden relativo de las funciones escalonadas de Kaplan–Meier. Cuando las curvas se cruzan, no resuma la comparación únicamente con un valor p de log-rank; examine también un RMST preespecificado o efectos en tiempos específicos. No seleccione una prueba ponderada a posteriori para obtener el menor valor p.',
'fr': 'Ce dépistage descriptif détecte les changements dans l’ordre relatif des fonctions en escalier de Kaplan–Meier. Lorsque les courbes se croisent, ne résumez pas la comparaison par une seule valeur p du log-rank ; examinez aussi un RMST prédéfini ou des effets à des temps précis. Ne choisissez pas a posteriori un test pondéré pour obtenir la plus petite valeur p.',
'de': 'Diese deskriptive Prüfung erkennt Änderungen in der relativen Reihenfolge der Kaplan–Meier-Treppenfunktionen. Bei sich kreuzenden Kurven sollte der Vergleich nicht allein durch einen Log-Rank-p-Wert zusammengefasst werden; prüfen Sie auch eine vorab festgelegte RMST oder zeitpunktspezifische Effekte. Wählen Sie nicht nachträglich einen gewichteten Test aus, um den kleinsten p-Wert zu erhalten.',
'vi': 'Sàng lọc mô tả này phát hiện thay đổi trong thứ tự tương đối của các hàm bậc thang Kaplan–Meier. Khi các đường cong giao nhau, không nên tóm tắt so sánh chỉ bằng một giá trị p log-rank; hãy xem xét thêm RMST được xác định trước hoặc hiệu ứng tại các thời điểm cụ thể. Không chọn kiểm định có trọng số sau khi xem kết quả chỉ để thu được giá trị p nhỏ nhất.'}
for lang,value in translations.items():
    p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
    data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=value
    p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
