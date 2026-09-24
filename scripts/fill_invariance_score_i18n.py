import json, re
from pathlib import Path

rows = '''Configural invariance|配置不変性|形态不变性|Invariancia configural|Invariance configurale|Konfigurale Invarianz|Bất biến cấu hình
Metric invariance|測定単位不変性|度量不变性|Invariancia métrica|Invariance métrique|Metrische Invarianz|Bất biến đo lường
Threshold invariance|閾値不変性|阈值不变性|Invariancia de umbrales|Invariance des seuils|Schwellenwertinvarianz|Bất biến ngưỡng
Scalar invariance|切片不変性|截距不变性|Invariancia escalar|Invariance scalaire|Skalare Invarianz|Bất biến hệ số chặn
Scalar invariance (thresholds + loadings)|切片不変性（閾値＋因子負荷量）|截距不变性（阈值＋载荷）|Invariancia escalar (umbrales + cargas)|Invariance scalaire (seuils + saturations)|Skalare Invarianz (Schwellenwerte + Ladungen)|Bất biến hệ số chặn (ngưỡng + tải nhân tố)
Strict invariance|厳密不変性|严格不变性|Invariancia estricta|Invariance stricte|Strikte Invarianz|Bất biến nghiêm ngặt
Constraint|等値制約|等值约束|Restricción de igualdad|Contrainte d’égalité|Gleichheitsrestriktion|Ràng buộc bằng nhau
Score χ²|スコアχ²|得分χ²|χ² de puntuación|χ² du score|Score-χ²|χ² điểm số
Max !standardized EPC!|最大 !標準化EPC!|最大!标准化EPC!|Máx. !EPC estandarizado!|Max. !EPC standardisé!|Max. !standardisierter EPC!|!EPC chuẩn hóa! tối đa
Raw χ²|未補正χ²|未校正χ²|χ² sin corregir|χ² non corrigé|Unkorrigiertes χ²|χ² chưa hiệu chỉnh
Raw p|未補正p値|未校正p值|p sin corregir|p non corrigé|Unkorrigiertes p|p chưa hiệu chỉnh
Raw BH-adjusted p|未補正検定のBH補正p値|未校正检验的BH校正p值|p ajustado por BH de la prueba sin corregir|p ajusté par BH du test non corrigé|BH-korrigiertes p des unkorrigierten Tests|p hiệu chỉnh BH của kiểm định chưa hiệu chỉnh'''
for i, lang in enumerate(['ja','zh','es','fr','de','vi'],1):
    path=Path('i18n')/(lang+'.json'); data=json.loads(path.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        fields=[x.replace('!', '|') for x in row.split('|')]; assert len(fields)==7
        key='analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')
        data['translations'][key]=fields[i]
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
