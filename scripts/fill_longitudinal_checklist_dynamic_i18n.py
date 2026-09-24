import json
from pathlib import Path
rows={
'en':['Analysis weight summary was not generated.','For GEE, compare working correlation structures when clinically plausible; current structure is %s.'],
'ko':['분석 가중치 요약을 생성하지 않았습니다.','임상적으로 타당하면 GEE 작업상관 구조를 비교하십시오. 현재 구조는 %s입니다.'],
'ja':['分析重みの要約は生成されませんでした。','臨床的に妥当な場合はGEEの作業相関構造を比較してください。現在の構造は%sです。'],
'zh':['未生成分析权重摘要。','若临床上合理，请比较 GEE 工作相关结构；当前结构为 %s。'],
'es':['No se generó el resumen de pesos de análisis.','Para GEE, compare estructuras de correlación de trabajo cuando sea clínicamente plausible; la estructura actual es %s.'],
'fr':['Le résumé des poids d’analyse n’a pas été généré.','Pour GEE, comparez les structures de corrélation de travail lorsque cela est cliniquement plausible ; la structure actuelle est %s.'],
'de':['Es wurde keine Zusammenfassung der Analysegewichte erstellt.','Vergleichen Sie bei GEE die Arbeitskorrelationsstrukturen, wenn dies klinisch plausibel ist; die aktuelle Struktur ist %s.'],
'vi':['Chưa tạo bản tóm tắt trọng số phân tích.','Đối với GEE, hãy so sánh các cấu trúc tương quan làm việc khi hợp lý về lâm sàng; cấu trúc hiện tại là %s.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.checklist.'+k:v for k,v in zip(['no_weights','gee_compare'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
