import json,re
from pathlib import Path
rows='''Select an event variable to display its observed-value map.|イベント変数を選択すると観測値の対応表が表示されます。|选择事件变量以显示其观测值映射表。|Seleccione una variable de evento para mostrar la correspondencia de sus valores observados.|Sélectionnez une variable d’événement pour afficher la correspondance de ses valeurs observées.|Wählen Sie eine Ereignisvariable aus, um die Zuordnung ihrer beobachteten Werte anzuzeigen.|Chọn biến biến cố để hiển thị bảng ánh xạ các giá trị quan sát.
Observed event-code mapping|観測イベントコードの対応付け|观测事件代码映射|Correspondencia de códigos de evento observados|Correspondance des codes d’événement observés|Zuordnung beobachteter Ereigniscodes|Ánh xạ mã biến cố quan sát
Raw value: %s|元の値: %s|原始值：%s|Valor original: %s|Valeur brute : %s|Rohwert: %s|Giá trị gốc: %s
Event label|イベントラベル|事件标签|Etiqueta del evento|Libellé de l’événement|Ereignisbezeichnung|Nhãn biến cố
I confirmed the meaning of every event code.|すべてのイベントコードの意味を確認しました。|我已确认每个事件代码的含义。|He confirmado el significado de cada código de evento.|J’ai confirmé la signification de chaque code d’événement.|Ich habe die Bedeutung jedes Ereigniscodes bestätigt.|Tôi đã xác nhận ý nghĩa của từng mã biến cố.
The 0/1 roles are initial suggestions only; analysis is blocked until confirmed.|0/1の役割は初期提案にすぎません。確認するまで分析は実行できません。|0/1角色仅为初始建议；确认前无法进行分析。|Los roles de 0/1 son solo sugerencias iniciales; el análisis está bloqueado hasta confirmarlos.|Les rôles de 0/1 ne sont que des suggestions initiales ; l’analyse est bloquée jusqu’à leur confirmation.|Die Rollen von 0/1 sind nur erste Vorschläge; die Analyse bleibt bis zur Bestätigung gesperrt.|Vai trò của 0/1 chỉ là gợi ý ban đầu; phân tích bị chặn cho đến khi được xác nhận.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
