import collections
import csv
import json
from pathlib import Path
out=Path('tmp/multilingual-program-audit')
history=json.loads((out/'three-day-evidence.json').read_text(encoding='utf-8'))
panels=list(csv.DictReader((out/'all-panels.csv').open(encoding='utf-8-sig')))
fallback=list(csv.DictReader((out/'visible-fallback-candidates.csv').open(encoding='utf-8-sig')))
confirmed={
 'Create data','ID conditional statistic','Number of missing values','Complete case flag',
 'Draw paths on the shared canvas. The design variables set under Complex Samples are applied automatically.'}
findings=[row for row in fallback if row['english'] in confirmed]
with (out/'confirmed-english-findings.csv').open('w',encoding='utf-8-sig',newline='') as f:
    writer=csv.DictWriter(f,fieldnames=['panel','language','english']);writer.writeheader();writer.writerows(findings)
def area(panel):
    for prefix in ('analysis','data_editor','calculator','about','help','latent'):
        if panel.startswith('lazy_'+prefix):return prefix
    return 'other'
summary={
 'evidence_reports_by_date':dict(collections.Counter(r['date'] for r in history)),
 'evidence_count':len(history),
 'registered_panels_by_area':dict(collections.Counter(area(r['panel']) for r in panels if r['language']=='en')),
 'render_checks':len(panels),'render_failures':sum(r['status']!='rendered' for r in panels),
 'visible_korean_label_candidates':sum(int(r['korean_label_candidates']) for r in panels),
 'confirmed_english_phrases':len(set(r['english'] for r in findings)),
 'confirmed_phrase_locale_occurrences':len(findings),
 'confirmed_panels':sorted(set(r['panel'] for r in findings)),
 'raw_fallback_candidates':len(fallback),
 'warning':'Registered panels include legacy and feature-gated panels; render success is not complete interaction coverage.'}
(out/'summary.json').write_text(json.dumps(summary,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps(summary,ensure_ascii=False,indent=2))
