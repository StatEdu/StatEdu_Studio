"""Matched-pair count and probability-sum validation."""
import json
from pathlib import Path
rows={
'en':['b discordant count must be nonnegative.','c discordant count must be nonnegative.','At least one discordant pair is required.','p01 + p10 must not exceed 1.'],
'ko':['불일치 빈도 b는 0 이상이어야 합니다.','불일치 빈도 c는 0 이상이어야 합니다.','불일치 쌍이 하나 이상 필요합니다.','p01 + p10은 1을 초과할 수 없습니다.'],
'ja':['不一致度数bは0以上である必要があります。','不一致度数cは0以上である必要があります。','少なくとも1組の不一致ペアが必要です。','p01 + p10は1を超えてはいけません。'],
'zh':['不一致频数b必须非负。','不一致频数c必须非负。','至少需要一对不一致配对。','p01 + p10不能超过1。'],
'es':['La frecuencia discordante b debe ser no negativa.','La frecuencia discordante c debe ser no negativa.','Se requiere al menos un par discordante.','p01 + p10 no debe superar 1.'],
'fr':['L’effectif discordant b doit être non négatif.','L’effectif discordant c doit être non négatif.','Au moins une paire discordante est nécessaire.','p01 + p10 ne doit pas dépasser 1.'],
'de':['Die diskordante Häufigkeit b muss nichtnegativ sein.','Die diskordante Häufigkeit c muss nichtnegativ sein.','Mindestens ein diskordantes Paar ist erforderlich.','p01 + p10 darf 1 nicht überschreiten.'],
'vi':['Tần số bất đồng b phải không âm.','Tần số bất đồng c phải không âm.','Cần ít nhất một cặp bất đồng.','p01 + p10 không được vượt quá 1.'],
}
keys=['error_mcnemar_b','error_mcnemar_c','error_mcnemar_no_pairs','error_mcnemar_probability_sum']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
