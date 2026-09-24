"""Cluster effect-size method prose; continuous note reuses its existing key."""
import json
from pathlib import Path
expr = ['h / sqrt(1 + (m - 1)ICC)', 'periods / (periods - 1)']
rows = {
'en': ["Binary cluster effect uses Cohen's h; planning effect = {0}.", 'Stepped-wedge planning effect applies a simple cluster design effect multiplied by {1}.'],
'ko': ['이분형 군집 효과크기는 Cohen의 h를 사용하며, 계획용 효과크기 = {0}입니다.', '단계적 도입 군집설계의 계획용 효과크기에는 단순 군집 설계효과에 {1}을 곱한 보정을 적용합니다.'],
'ja': ['二値アウトカムのクラスター効果量にはCohenのhを用い、計画用効果量 = {0}です。', 'ステップドウェッジの計画用効果量には、単純なクラスターのデザイン効果に{1}を掛けた補正を適用します。'],
'zh': ['二元结局的整群效应量使用Cohen h；规划效应量 = {0}。', '阶梯楔形设计的规划效应量采用简单整群设计效应乘以{1}的调整。'],
'es': ['El efecto por conglomerados para resultados binarios utiliza h de Cohen; efecto para planificación = {0}.', 'El efecto para planificación del diseño escalonado aplica un efecto de diseño simple por conglomerados multiplicado por {1}.'],
'fr': ['L’effet en grappes pour les résultats binaires utilise le h de Cohen; effet de planification = {0}.', 'L’effet de planification du plan en escalier applique un effet de plan simple en grappes multiplié par {1}.'],
'de': ['Der Clustereffekt für binäre Ergebnisse verwendet Cohens h; Planungseffekt = {0}.', 'Der Planungseffekt beim Stepped-Wedge-Design verwendet einen einfachen Cluster-Designeffekt multipliziert mit {1}.'],
'vi': ['Hiệu ứng cụm cho kết quả nhị phân sử dụng h của Cohen; hiệu ứng lập kế hoạch = {0}.', 'Hiệu ứng lập kế hoạch của thiết kế bậc thang áp dụng hiệu ứng thiết kế cụm đơn giản nhân với {1}.']
}
for lang, templates in rows.items():
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'sample_size.result.'+key:value.format(*expr) for key,value in zip(['note_cluster_binary','note_cluster_stepped'],templates)})
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
