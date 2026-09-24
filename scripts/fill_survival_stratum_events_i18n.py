"""Localize the generated event-count suffix without translating stratum names."""
import json
from pathlib import Path

translations = {
    'en': '%s: events = %s', 'ko': '%s: 사건 수 = %s',
    'ja': '%s: イベント数 = %s', 'zh': '%s: 事件数 = %s',
    'es': '%s: eventos = %s', 'fr': '%s: événements = %s',
    'de': '%s: Ereignisse = %s', 'vi': '%s: số biến cố = %s',
}
for language, text in translations.items():
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.ui.s_events_s'] = text
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
