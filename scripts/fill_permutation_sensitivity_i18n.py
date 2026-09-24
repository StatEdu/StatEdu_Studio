import json, re
from pathlib import Path

rows = '''Path difference|経路差|路径差异|Diferencia de rutas|Différence de chemins|Pfaddifferenz|Chênh lệch đường dẫn
Permutation p|置換検定のp値|置换检验p值|p de permutación|p de permutation|Permutations-p|p hoán vị
MGA permutation adequate|MGA置換の有効性基準を充足|MGA置换有效性达标|Validez de permutaciones MGA suficiente|Validité des permutations MGA suffisante|MGA-Permutationsgültigkeit ausreichend|Đạt tiêu chí tính hợp lệ của hoán vị MGA'''
for i, lang in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        fields = row.split('|')
        assert len(fields) == 7
        key = 'analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', fields[0].lower()).strip('_')
        data['translations'][key] = fields[i]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
