import json
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
values = '''Binary event for %s is %s; reference is %s.|%s의 사건 범주는 %s이고 기준 범주는 %s입니다.|%sのイベントカテゴリは%s、参照カテゴリは%sです。|%s的事件类别为%s，参考类别为%s。|La categoría de evento de %s es %s; la referencia es %s.|La catégorie d’événement de %s est %s ; la référence est %s.|Die Ereigniskategorie für %s ist %s; die Referenzkategorie ist %s.|Nhóm biến cố của %s là %s; nhóm tham chiếu là %s.'''.split('|')
assert len(values) == len(languages)
for language, value in zip(languages, values):
    assert value.count('%s') == 3
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.ui.binary_event_for_s_is_s_reference_is_s'] = value
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
