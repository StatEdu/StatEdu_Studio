import json, re
from pathlib import Path

rows = [
 ['Max |standardized residual|','最大 |標準化残差|','最大|标准化残差|','Máx. |residuo estandarizado|','Max. |résidu standardisé|','Max. |standardisiertes Residuum|','|Phần dư chuẩn hóa| tối đa'],
 ['Flagged residuals','基準超過の残差数','超出阈值的残差数','Número de residuos que superan el umbral','Nombre de résidus dépassant le seuil','Anzahl der Residuen über dem Grenzwert','Số phần dư vượt ngưỡng']
]
for i, lang in enumerate(['ja','zh','es','fr','de','vi'],1):
    path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
    for fields in rows:
        key='analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')
        data['translations'][key]=fields[i]
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
