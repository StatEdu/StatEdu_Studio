import json,re
from pathlib import Path
rows='''ANCOVA requires at least four complete cases.|ANCOVAには少なくとも4件の完全ケースが必要です。|ANCOVA至少需要4个完整案例。|ANCOVA requiere al menos cuatro casos completos.|L’ANCOVA nécessite au moins quatre cas complets.|ANCOVA erfordert mindestens vier vollständige Fälle.|ANCOVA cần ít nhất bốn trường hợp đầy đủ.
Grouping variable must have at least two observed levels.|群分け変数には少なくとも2つの観測水準が必要です。|分组变量至少需要两个观测水平。|La variable de agrupación debe tener al menos dos niveles observados.|La variable de groupe doit avoir au moins deux niveaux observés.|Die Gruppierungsvariable muss mindestens zwei beobachtete Ausprägungen haben.|Biến phân nhóm phải có ít nhất hai mức được quan sát.
Select at least one covariate.|共変量を少なくとも1つ選択してください。|请至少选择一个协变量。|Seleccione al menos una covariable.|Sélectionnez au moins une covariable.|Wählen Sie mindestens eine Kovariate aus.|Hãy chọn ít nhất một hiệp biến.
No data frame is available for ANCOVA.|ANCOVAに使用できるデータがありません。|没有可用于ANCOVA的数据。|No hay datos disponibles para ANCOVA.|Aucune donnée n’est disponible pour l’ANCOVA.|Für ANCOVA sind keine Daten verfügbar.|Không có dữ liệu để thực hiện ANCOVA.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
