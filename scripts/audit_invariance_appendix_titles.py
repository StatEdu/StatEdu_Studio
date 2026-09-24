import json, re
from pathlib import Path

source = Path('R/setup_custom_model_canvas_structural_render_invariance.R').read_text(encoding='utf-8')
source = source[source.index('structural_canvas_invariance_appendix_ui <-'):]
titles = re.findall(r'appendix_table\(\s*"[^"]+",\s*"([^"]+)"', source)
assert len(titles) >= 10
missing = {}
for language in ['ja', 'zh', 'es', 'fr', 'de', 'vi']:
    translations = json.loads((Path('i18n') / (language + '.json')).read_text(encoding='utf-8'))['translations']
    missing[language] = [title for title in titles if not translations.get('analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', title.lower()).strip('_'))]
print(json.dumps({'titles_checked': len(titles), 'missing': missing}, ensure_ascii=True, indent=2))
assert not any(missing.values())
