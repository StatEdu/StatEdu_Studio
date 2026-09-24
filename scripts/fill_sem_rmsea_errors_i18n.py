"""SEM RMSEA and degrees-of-freedom validation."""
import json
from pathlib import Path
rows={
'en':['Model degrees of freedom must be at least 1.','Estimated model degrees of freedom must be at least 1. Add measured variables or simplify the model.','For the close-fit test, alternative RMSEA must be greater than null RMSEA.','For the not-close-fit test, alternative RMSEA must be less than null RMSEA.'],
'ko':['모형 자유도는 1 이상이어야 합니다.','추정 모형 자유도는 1 이상이어야 합니다. 관측변수를 추가하거나 모형을 단순화하세요.','근접적합 검정에서는 대립가설 RMSEA가 귀무가설 RMSEA보다 커야 합니다.','비근접적합 검정에서는 대립가설 RMSEA가 귀무가설 RMSEA보다 작아야 합니다.'],
'ja':['モデルの自由度は1以上である必要があります。','推定モデル自由度は1以上である必要があります。観測変数を追加するか、モデルを簡略化してください。','近似適合検定では、対立仮説のRMSEAが帰無仮説のRMSEAより大きい必要があります。','非近似適合検定では、対立仮説のRMSEAが帰無仮説のRMSEAより小さい必要があります。'],
'zh':['模型自由度必须至少为1。','估计模型自由度必须至少为1。请增加观测变量或简化模型。','近似拟合检验中，备择假设RMSEA必须大于原假设RMSEA。','非近似拟合检验中，备择假设RMSEA必须小于原假设RMSEA。'],
'es':['Los grados de libertad del modelo deben ser al menos 1.','Los grados de libertad estimados del modelo deben ser al menos 1. Añada variables observadas o simplifique el modelo.','Para la prueba de ajuste cercano, el RMSEA alternativo debe ser mayor que el RMSEA nulo.','Para la prueba de ajuste no cercano, el RMSEA alternativo debe ser menor que el RMSEA nulo.'],
'fr':['Le modèle doit avoir au moins 1 degré de liberté.','Le nombre estimé de degrés de liberté du modèle doit être au moins égal à 1. Ajoutez des variables observées ou simplifiez le modèle.','Pour le test d’ajustement proche, le RMSEA alternatif doit être supérieur au RMSEA nul.','Pour le test d’ajustement non proche, le RMSEA alternatif doit être inférieur au RMSEA nul.'],
'de':['Die Modellfreiheitsgrade müssen mindestens 1 betragen.','Die geschätzten Modellfreiheitsgrade müssen mindestens 1 betragen. Fügen Sie beobachtete Variablen hinzu oder vereinfachen Sie das Modell.','Beim Test auf enge Anpassung muss der alternative RMSEA größer als der RMSEA unter der Nullhypothese sein.','Beim Test auf nicht enge Anpassung muss der alternative RMSEA kleiner als der RMSEA unter der Nullhypothese sein.'],
'vi':['Bậc tự do của mô hình phải ít nhất là 1.','Bậc tự do ước tính của mô hình phải ít nhất là 1. Hãy thêm biến quan sát hoặc đơn giản hóa mô hình.','Đối với kiểm định độ phù hợp gần, RMSEA theo giả thuyết đối phải lớn hơn RMSEA theo giả thuyết không.','Đối với kiểm định độ phù hợp không gần, RMSEA theo giả thuyết đối phải nhỏ hơn RMSEA theo giả thuyết không.'],
}
keys=['error_sem_df','error_sem_estimated_df','error_rmsea_close','error_rmsea_not_close']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
