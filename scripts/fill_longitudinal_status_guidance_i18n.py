"""Missing-data status and experimental GEE guidance."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['Not needed','필요하지 않음','不要','不需要','No necesario','Non nécessaire','Nicht erforderlich','Không cần thiết'],
 ['Strategy','방법','方法','方法','Estrategia','Stratégie','Strategie','Chiến lược'],
 ['R package warnings','R 패키지 경고','Rパッケージの警告','R包警告','Advertencias de paquetes de R','Avertissements des packages R','Warnungen von R-Paketen','Cảnh báo từ các gói R'],
 ['Experimental SPSS compatibility mode: custom GEE estimator, not the geepack estimator. This mode is not the default analysis.',
  '실험적 SPSS 호환 모드: geepack 추정량이 아닌 자체 GEE 추정량을 사용합니다. 이 모드는 기본 분석이 아닙니다.',
  '実験的なSPSS互換モード：geepackの推定量ではなく独自のGEE推定量を使用します。このモードは既定の分析ではありません。',
  '实验性SPSS兼容模式：使用自定义GEE估计量，而非geepack估计量。此模式不是默认分析。',
  'Modo experimental de compatibilidad con SPSS: utiliza un estimador GEE propio, no el de geepack. Este modo no es el análisis predeterminado.',
  'Mode expérimental de compatibilité SPSS : utilise un estimateur GEE personnalisé, et non celui de geepack. Ce mode n’est pas l’analyse par défaut.',
  'Experimenteller SPSS-Kompatibilitätsmodus: verwendet einen eigenen GEE-Schätzer statt des geepack-Schätzers. Dieser Modus ist nicht die Standardanalyse.',
  'Chế độ tương thích SPSS thử nghiệm: sử dụng bộ ước lượng GEE tùy chỉnh, không phải bộ ước lượng của geepack. Đây không phải chế độ phân tích mặc định.'],
 ['Unstructured GEE with parameter-count correlation correction (SPSS ADJUSTCORR=YES), Pearson scale divided by N-p, and robust sandwich standard errors. Observation weights are not supported in this mode.',
  '모수 개수에 따른 상관 보정(SPSS ADJUSTCORR=YES), N-p로 나눈 Pearson 척도 및 강건 샌드위치 표준오차를 사용하는 비구조화 GEE입니다. 이 모드에서는 관측 가중치를 지원하지 않습니다.',
  'パラメータ数による相関補正（SPSS ADJUSTCORR=YES）、N-pで除したPearson尺度、およびロバストなサンドイッチ標準誤差を使用する非構造化GEEです。このモードでは観測重みを使用できません。',
  '非结构化GEE采用基于参数个数的相关校正（SPSS ADJUSTCORR=YES）、除以N-p的Pearson尺度以及稳健夹心标准误。此模式不支持观测权重。',
  'GEE no estructurada con corrección de la correlación según el número de parámetros (SPSS ADJUSTCORR=YES), escala de Pearson dividida por N-p y errores estándar sándwich robustos. Este modo no admite pesos de observación.',
  'GEE non structurée avec correction de la corrélation selon le nombre de paramètres (SPSS ADJUSTCORR=YES), échelle de Pearson divisée par N-p et erreurs-types sandwich robustes. Ce mode ne prend pas en charge les poids d’observation.',
  'Unstrukturierte GEE mit Korrelationskorrektur anhand der Parameteranzahl (SPSS ADJUSTCORR=YES), durch N-p dividierter Pearson-Skala und robusten Sandwich-Standardfehlern. Beobachtungsgewichte werden in diesem Modus nicht unterstützt.',
  'GEE phi cấu trúc với hiệu chỉnh tương quan theo số tham số (SPSS ADJUSTCORR=YES), thang Pearson chia cho N-p và sai số chuẩn sandwich vững. Chế độ này không hỗ trợ trọng số quan sát.'],
]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
