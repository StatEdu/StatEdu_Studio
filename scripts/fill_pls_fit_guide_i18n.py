import json,re
from pathlib import Path
rows = '''Supplementary Table 3: Structural effect guide indices|補助表3：構造効果の参考指標|辅助表3：结构效应参考指标|Tabla suplementaria 3: índices orientativos del efecto estructural|Tableau complémentaire 3 : indices indicatifs des effets structurels|Ergänzungstabelle 3: Orientierungswerte für strukturelle Effekte|Bảng bổ sung 3: chỉ số tham khảo tác động cấu trúc
Descriptive f² references are .02 (small), .15 (medium), and .35 (large); f² is computed from an estimator-consistent reduced model. Inner VIF is not a standalone pass criterion.|f²の記述的な参考値は.02（小）、.15（中）、.35（大）です。f²は推定量と整合する縮小モデルから計算します。Inner VIFも単独の合格判定基準ではありません。|f²的描述性参考值为.02（小）、.15（中）、.35（大）；f²由与估计量一致的简化模型计算。Inner VIF也不是独立的通过标准。|Los valores descriptivos de referencia de f² son .02 (pequeño), .15 (mediano) y .35 (grande); f² se calcula con un modelo reducido coherente con el estimador. Inner VIF no es un criterio independiente de aprobación.|Les valeurs descriptives de référence de f² sont .02 (faible), .15 (moyen) et .35 (élevé) ; f² est calculé à partir d’un modèle réduit cohérent avec l’estimateur. Inner VIF n’est pas un critère autonome de validation.|Deskriptive Referenzwerte für f² sind .02 (klein), .15 (mittel) und .35 (groß); f² wird mit einem zum Schätzer konsistenten reduzierten Modell berechnet. Inner VIF ist kein eigenständiges Bestehenskriterium.|Giá trị tham khảo mô tả của f² là .02 (nhỏ), .15 (vừa) và .35 (lớn); f² được tính từ mô hình rút gọn nhất quán với phương pháp ước lượng. Inner VIF không phải tiêu chí đạt độc lập.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
