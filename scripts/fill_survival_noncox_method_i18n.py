import json,re
from pathlib import Path
rows='''cumulative incidence functions|累積発生関数|累积发生函数|funciones de incidencia acumulada|fonctions d’incidence cumulée|kumulative Inzidenzfunktionen|hàm tỷ lệ mới mắc tích lũy
Gray's test|Gray検定|Gray检验|prueba de Gray|test de Gray|Gray-Test|kiểm định Gray
cause-specific Cox regression|原因別Cox回帰|原因别Cox回归|regresión de Cox por causa específica|régression de Cox par cause|ursachenspezifische Cox-Regression|hồi quy Cox theo nguyên nhân
Fine–Gray regression|Fine–Gray回帰|Fine–Gray回归|regresión de Fine–Gray|régression de Fine–Gray|Fine–Gray-Regression|hồi quy Fine–Gray
with censoring distributions estimated separately within %s|（検閲分布を%s内で別々に推定）|（在%s内分别估计删失分布）|con distribuciones de censura estimadas por separado dentro de %s|avec distributions de censure estimées séparément au sein de %s|mit innerhalb von %s getrennt geschätzten Zensierungsverteilungen|với phân phối kiểm duyệt được ước lượng riêng trong %s
the actuarial life-table method|生命表法|寿命表法|el método actuarial de tabla de vida|la méthode actuarielle des tables de survie|die aktuarielle Sterbetafelmethode|phương pháp bảng sống
the Kaplan–Meier method with delayed entry|遅延エントリーを考慮したKaplan–Meier法|考虑延迟进入的Kaplan–Meier法|el método de Kaplan–Meier con entrada tardía|la méthode de Kaplan–Meier avec entrée différée|die Kaplan–Meier-Methode mit verzögertem Eintritt|phương pháp Kaplan–Meier có vào nghiên cứu muộn
the Kaplan–Meier method|Kaplan–Meier法|Kaplan–Meier法|el método de Kaplan–Meier|la méthode de Kaplan–Meier|die Kaplan–Meier-Methode|phương pháp Kaplan–Meier'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
