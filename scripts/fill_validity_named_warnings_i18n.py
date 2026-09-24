import json,re
from pathlib import Path
rows=[[
 'Caution: missing exogenous latent covariance paths (%s) are fixed to zero.',
 '주의: 누락된 외생 잠재변수 공분산 경로(%s)는 0으로 고정됩니다.',
 '注意：省略された外生潜在変数の共分散パス（%s）は0に固定されます。',
 '注意：缺失的外生潜变量协方差路径（%s）固定为0。',
 'Precaución: las covarianzas latentes exógenas omitidas (%s) se fijan en cero.',
 'Attention : les chemins de covariance latente exogène omis (%s) sont fixés à zéro.',
 'Achtung: Fehlende exogene latente Kovarianzpfade (%s) werden auf null fixiert.',
 'Chú ý: các đường hiệp phương sai tiềm ẩn ngoại sinh bị thiếu (%s) được cố định bằng 0.'
 ],[
 'Caution: single-indicator factor indicators (%s) were automatically identified by fixing their residual variances to zero, which is a perfect-measurement assumption.',
 '주의: 단일지표 요인 지표(%s)는 오차분산 0으로 자동 고정되어 완전측정 가정으로 식별되었습니다.',
 '注意：単一指標因子の指標（%s）は残差分散を0に固定することで自動識別されました。これは完全測定を仮定します。',
 '注意：单指标因子的指标（%s）通过将残差方差固定为0自动识别，这意味着假定完全测量。',
 'Precaución: los indicadores de factores de indicador único (%s) se identificaron automáticamente fijando su varianza residual en cero, lo que supone medición perfecta.',
 'Attention : les indicateurs des facteurs à indicateur unique (%s) ont été identifiés automatiquement en fixant leur variance résiduelle à zéro, ce qui suppose une mesure parfaite.',
 'Achtung: Die Indikatoren von Ein-Indikator-Faktoren (%s) wurden durch Fixierung ihrer Residualvarianzen auf null automatisch identifiziert. Dies setzt fehlerfreie Messung voraus.',
 'Chú ý: các chỉ báo của nhân tố một chỉ báo (%s) được nhận dạng tự động bằng cách cố định phương sai phần dư bằng 0, tức giả định đo lường hoàn hảo.'
 ],[
 'Caution: cross-loaded indicators (%s) appear in more than one factor. Factor-specific AVE/CR remain descriptive, while simple-structure discriminant-validity interpretations require particular caution.',
 '주의: 교차적재 지표(%s)가 둘 이상의 요인에 포함되어 있습니다. 요인별 AVE/CR은 기술적 지표로만 보며, 단순구조 기반 판별타당도 해석에는 특별한 주의가 필요합니다.',
 '注意：交差負荷指標（%s）は複数の因子に含まれます。因子別AVE/CRは記述的指標にとどまり、単純構造に基づく弁別的妥当性の解釈には特に注意が必要です。',
 '注意：交叉载荷指标（%s）出现在多个因子中。因子特定AVE/CR仍是描述性指标，基于简单结构的区分效度解释须格外谨慎。',
 'Precaución: los indicadores con cargas cruzadas (%s) aparecen en más de un factor. AVE/CR por factor son descriptivos; la interpretación de validez discriminante basada en estructura simple requiere especial cautela.',
 'Attention : les indicateurs à saturations croisées (%s) figurent dans plusieurs facteurs. Les AVE/CR par facteur restent descriptifs ; l’interprétation de validité discriminante fondée sur une structure simple exige une prudence particulière.',
 'Achtung: Indikatoren mit Kreuzladungen (%s) gehören zu mehreren Faktoren. Faktorspezifische AVE/CR bleiben deskriptiv; Aussagen zur diskriminanten Validität auf Basis einer Einfachstruktur erfordern besondere Vorsicht.',
 'Chú ý: các chỉ báo tải chéo (%s) xuất hiện ở nhiều nhân tố. AVE/CR theo nhân tố chỉ mang tính mô tả; diễn giải giá trị phân biệt dựa trên cấu trúc đơn giản cần đặc biệt thận trọng.'
 ]]
p=Path('R/setup_custom_model_canvas_structural_render_validity_notes.R');s=p.read_text(encoding='utf-8')
for row,variable in zip(rows,['missing_covariances','auto_single_residuals','cross_loaded_indicators']):
 pattern=r'paste0\(if \(ko\) "[^"\n]*" else "[^"\n]*", paste\('+variable+r', collapse = ", "\), if \(ko\) "[^"\n]*" else "[^"\n]*"\)'
 replacement='sprintf(statedu_localized_text(statedu_current_language(app_language_fn), '+json.dumps(row[0],ensure_ascii=False)+', '+json.dumps(row[1],ensure_ascii=False)+'), paste('+variable+', collapse = ", "))'
 s,n=re.subn(pattern,lambda m:replacement,s)
 assert n==1 or row[0] in s
p.write_text(s,encoding='utf-8')
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],2):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows:data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',row[0].lower()).strip('_')]=row[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
