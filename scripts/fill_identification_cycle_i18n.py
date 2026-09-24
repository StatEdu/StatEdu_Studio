import json,re
from pathlib import Path
source='The current automatic identification scheme does not support reciprocal or cyclic structural paths.'
values={
'ja':'現在の自動識別方式では、相互回帰または循環する構造経路をサポートしていません。',
'zh':'当前自动识别方案不支持互反或循环结构路径。',
'es':'El esquema actual de identificación automática no admite trayectorias estructurales recíprocas o cíclicas.',
'fr':'Le schéma actuel d’identification automatique ne prend pas en charge les chemins structurels réciproques ou cycliques.',
'de':'Das aktuelle automatische Identifikationsverfahren unterstützt keine reziproken oder zyklischen Strukturpfade.',
'vi':'Sơ đồ định danh tự động hiện tại không hỗ trợ đường dẫn cấu trúc đối ứng hoặc có chu trình.'}
for lang,value in values.items():
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=value
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
