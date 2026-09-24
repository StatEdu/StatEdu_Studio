import json,re
from pathlib import Path
rows='''%s analysis completed.|%sの分析が完了しました。|%s分析已完成。|Se completó el análisis %s.|L’analyse %s est terminée.|Die Analyse %s ist abgeschlossen.|Đã hoàn tất phân tích %s.
%s package is required.|%sパッケージが必要です。|需要%s软件包。|Se requiere el paquete %s.|Le package %s est requis.|Das Paket %s wird benötigt.|Cần có gói %s.
Heywood-constrained reanalysis|Heywood制約による再分析|Heywood约束重新分析|Reanálisis con restricciones de Heywood|Réanalyse avec contraintes de Heywood|Neuanalyse mit Heywood-Restriktionen|Phân tích lại với ràng buộc Heywood
Fix each negative residual variance to a small positive percentage of that variable's observed variance.|各負の残差分散を、その変数の観測分散の小さな正の割合に固定します。|将每个负残差方差固定为该变量观测方差的一个较小正百分比。|Fije cada varianza residual negativa en un pequeño porcentaje positivo de la varianza observada de esa variable.|Fixez chaque variance résiduelle négative à un faible pourcentage positif de la variance observée de cette variable.|Fixieren Sie jede negative Residualvarianz auf einen kleinen positiven Prozentsatz der beobachteten Varianz dieser Variablen.|Cố định mỗi phương sai phần dư âm ở một tỷ lệ phần trăm dương nhỏ của phương sai quan sát của biến đó.
Observed-variance percentage|観測分散の割合（%）|观测方差百分比|Porcentaje de la varianza observada|Pourcentage de la variance observée|Prozentsatz der beobachteten Varianz|Tỷ lệ phần trăm phương sai quan sát
Recommended starting value: 0.1%. This is a sensitivity analysis, not an automatic correction of model misspecification.|推奨開始値：0.1%。これは感度分析であり、モデルの誤指定を自動的に修正する手続きではありません。|建议初始值：0.1%。这是敏感性分析，不会自动纠正模型设定错误。|Valor inicial recomendado: 0.1%. Es un análisis de sensibilidad, no una corrección automática de una especificación incorrecta del modelo.|Valeur initiale recommandée : 0.1%. Il s’agit d’une analyse de sensibilité, pas d’une correction automatique d’une mauvaise spécification du modèle.|Empfohlener Startwert: 0.1%. Dies ist eine Sensitivitätsanalyse, keine automatische Korrektur einer falschen Modellspezifikation.|Giá trị khởi đầu khuyến nghị: 0.1%. Đây là phân tích độ nhạy, không phải cách tự động sửa lỗi đặc tả mô hình.
Run constrained model|制約モデルを実行|运行约束模型|Ejecutar modelo restringido|Exécuter le modèle contraint|Modell mit Restriktionen ausführen|Chạy mô hình có ràng buộc
The constrained model fixed %s to %s%% of observed variance.|制約モデルでは%sの残差分散を観測分散の%s%%に固定しました。|约束模型已将%s的残差方差固定为观测方差的%s%%。|El modelo restringido fijó la varianza residual de %s en el %s%% de la varianza observada.|Le modèle contraint a fixé la variance résiduelle de %s à %s%% de la variance observée.|Das Modell mit Restriktionen fixierte die Residualvarianz von %s auf %s%% der beobachteten Varianz.|Mô hình có ràng buộc đã cố định phương sai phần dư của %s ở %s%% phương sai quan sát.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  assert f[0].count('%s')==f[i].count('%s')
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
