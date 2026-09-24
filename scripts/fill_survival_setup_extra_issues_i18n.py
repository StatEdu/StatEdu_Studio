import json
from pathlib import Path
rows='''missing_columns_prefix|変数が見つかりません: |找不到变量：|Variables no encontradas: |Variables introuvables : |Variablen nicht gefunden: |Không tìm thấy biến: 
extra.invalid_time_encoding|時間の役割に指定する値は数値の期間である必要があります。日時フィールドは分析前に経過期間に変換してください。|时间角色值必须为数值型时长；日期时间字段须在分析前转换为经过时长。|Los valores de tiempo deben ser duraciones numéricas; convierta las fechas y horas en duraciones transcurridas antes del análisis.|Les valeurs temporelles doivent être des durées numériques ; convertissez les dates et heures en durées écoulées avant l’analyse.|Zeitwerte müssen numerische Dauern sein; Datums- und Zeitfelder vor der Analyse in verstrichene Dauern umwandeln.|Giá trị thời gian phải là khoảng thời gian dạng số; chuyển trường ngày giờ thành thời gian đã trôi qua trước khi phân tích.
extra.competing_event_requires_competing_risk|競合イベントコードは、通常のKaplan–MeierまたはCox分析で一般的な打ち切りとして扱うことはできません。|竞争事件代码不能在标准Kaplan–Meier或Cox分析中作为普通删失处理。|Los códigos de eventos competitivos no pueden tratarse como censura ordinaria en el análisis estándar de Kaplan–Meier o Cox.|Les codes d’événements concurrents ne peuvent pas être traités comme une censure ordinaire dans une analyse standard de Kaplan–Meier ou de Cox.|Konkurrierende Ereigniscodes dürfen in der standardmäßigen Kaplan–Meier- oder Cox-Analyse nicht als gewöhnliche Zensierung behandelt werden.|Không thể xử lý mã biến cố cạnh tranh như kiểm duyệt thông thường trong phân tích Kaplan–Meier hoặc Cox tiêu chuẩn.
extra.unsupported_other_state|other_stateに割り当てられたイベントコードには多状態生存モデルが必要であり、現行の生存分析では未対応です。|指定为other_state的事件代码需要多状态生存模型，当前生存分析版本不支持。|Los códigos asignados a other_state requieren un modelo de supervivencia multiestado, no disponible en la versión actual.|Les codes affectés à other_state nécessitent un modèle de survie multiétat, non pris en charge dans la version actuelle.|Als other_state zugeordnete Ereigniscodes benötigen ein Mehrzustands-Überlebensmodell, das derzeit nicht unterstützt wird.|Mã biến cố được gán other_state cần mô hình sống còn đa trạng thái, hiện chưa được hỗ trợ.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');d=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  d['translations']['survival.setup.'+f[0]]=f[i]
 p.write_text(json.dumps(d,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
