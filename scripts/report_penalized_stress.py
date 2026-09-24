from pathlib import Path
import csv, math
base=Path('tmp/penalized-stress')
rows=list(csv.DictReader((base/'summary.csv').open(encoding='utf-8')))
names={'null':'무효과 (n=100, p=10)','collinear':'공선성·상쇄 효과 (n=100, p=12, ρ=.95)','small':'작은 표본 (n=24, p=8)','high_dimensional':'다수 변수 (n=60, p=100)'}
def pct(v):
 try:return f'{float(v)*100:.1f}%'
 except:return '—'
lines=['# 선택 후 검정: 모의실험 점검 결과','',
'라소·엘라스틱넷의 반복 표본 분할 검정을 점검했습니다. 조건별 독립 자료 50개, 자료마다 방법별 50회 분할을 사용했습니다. 총 400개 분석입니다. 기준은 p<.05이며, 각 자료에서 참 무효과 변수 중 하나라도 기각하면 가족단위 거짓 양성으로 계산했습니다.','',
'아래 구간은 모의실험 반복 횟수에 따른 정확 이항 95% 신뢰구간입니다. 개별 회귀계수의 신뢰구간이 아닙니다. 검정력은 참 효과 중 검출한 비율의 반복 평균입니다.','',
'| 조건 | 방법 | 거짓 양성률 (95% 구간) | 검정력 | 모든 참 효과를 선택한 분할 | 실패 분할 |',
'|---|---|---|---|---|---|']
for r in rows:
 lines.append(f"| {names[r['scenario']]} | {r['method']} | {pct(r['FWER'])} ({pct(r['FWER_low'])}–{pct(r['FWER_high'])}) | {pct(r['power'])} | {pct(r['screening']) if r['scenario']!='null' else '—'} | {pct(r['failed_splits'])} |")
lines+=['','해석 시 주의할 점:','',
'- 50회에서 거짓 양성이 0회여도 95% 구간 상한은 약 7.1%입니다. 따라서 이 결과만으로 5% 오류율 통제를 입증하지 않습니다.',
'- 공선성 조건은 상관 .95인 두 변수에 계수 +1, −1을 주어 효과가 상쇄됩니다. 낮은 검정력을 공선성 자체만의 영향으로 일반화할 수 없습니다.',
'- 모든 참 효과를 선택한 분할 비율은 실제 모의실험에서만 알 수 있습니다. 실제 자료에서는 참 효과를 모르므로 이 비율을 계산하거나 검정의 가정 충족을 보장할 수 없습니다.',
'- 독립적인 정규·등분산 오차를 사용했습니다. 비정규성, 이분산성, 종속 관측 및 다른 효과 크기는 이번 점검 대상이 아닙니다.',
'- 실패 분할은 삭제하거나 재추출하지 않았으며, 원래 절차대로 p=1을 통합 분모에 유지했습니다. 빈 선택 집합은 계산 실패와 구분했습니다.','',
'실행 스크립트: `scripts/validate_penalized_stress.R`. 원시 반복 결과: `replicates.csv`. 수치 요약: `summary.csv`. 자료 생성 시드와 분할 시드는 스크립트에 고정되어 있습니다.']
(base/'report.md').write_text('\n'.join(lines),encoding='utf-8')
print('Report written:',base/'report.md')
