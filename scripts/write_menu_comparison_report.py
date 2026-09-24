import csv
import html
from pathlib import Path

root = Path('output/menu-before-after-20260915')
def read(name):
    with (root/name).open(encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))
rows = read('summary.csv')
boot = read('bootstrap-summary.csv')
survey = {int(x['id']): x for x in read('survey-values-summary.csv')}
names = ['빈도·기술통계','교차분석','t-test / ANOVA','ANCOVA','대응표본 검정','반복측정 분산분석','대응 비모수 검정','상관분석: Pearson','신뢰도','평가자 간 일치도: ICC','탐색적 요인분석','주성분분석: Varimax','선형 회귀','로지스틱 회귀','일반화 선형모형: Gaussian','단일집단 반복측정 ANOVA','비모수 검정: Mann–Whitney','상관분석: Spearman','Kaplan–Meier','Cox 회귀','경쟁사건 분석','종단모형: GEE','종단모형: LMM','복합표본 빈도·기술통계','복합표본 교차분석','복합표본 t-test / ANOVA','복합표본 상관분석','복합표본 회귀','복합표본 로지스틱','CFA: ML 적합','SEM: ML 적합','PLS-SEM: 기본 적합','매개효과: 5,000회','조절효과: 5,000회','사용자 정의 매개: 5,000회','Ridge·LASSO·Elastic Net: 선택 부트스트랩 30회','메타분석: 랜덤효과 DL','복합표본 매개·조절']
def verdict(r):
    i = int(r['id'])
    if i == 12: return '차이: 회전 정밀도 변경'
    if i == 22: return '차이: 수렴 기준 변경'
    if i == 17: return '차이: 연속성 보정 변경'
    if i == 37: return '8월 기준에 없음; 별도 비교 아래 참조'
    if i in (32, 36): return '공통 수치값 일치; 자료형·추가 항목 차이'
    if i in survey and survey[i]['raw_calls_equal'] == 'TRUE': return '계측한 반올림 전 추정값 일치'
    if r['canonical_equal'] == 'TRUE': return '반환 결과 일치¹'
    return '공통 수치 일치; 객체 차이²'
def ms(s):
    return '—' if s in ('NA','') else f'{float(s)*1000:.2f}'
def change(s):
    if s in ('NA',''): return '—'
    v=float(s)
    return f'{abs(v):.1f}% '+('단축' if v>=0 else '증가')
lines = ['# 분석 메뉴별 최적화 전·후 비교 — 2026-09-15', '',
'**모든 결과가 동일하거나 모든 메뉴가 빨라진 것은 아니다.** 일반 SEM 5,000회는 기존 결과표의 21개 열이 정확히 일치하면서 약 70% 단축됐다. PCA·GEE·Mann–Whitney·PLS 부트스트랩에서는 두 버전 사이의 통계 설정/유효성 정책 변경에 따른 실제 차이를 확인했다. 이들을 순수한 속도 최적화 효과로 계산하면 안 된다.', '',
'## 비교 기준과 측정 범위', '',
'- 이전: 기존 종합 감사의 2026-08-23 커밋 `f74d2996ee7570c81f93b9feb9571b248c87f83c` 전체 소스. 이후: 2026-09-15 작업트리의 동결 사본. 같은 PC·번들 R 4.5.3·같은 패키지 라이브러리 사용. 당시 배포본의 별도 R/패키지 환경을 재현한 비교는 아니다.',
'- 기준 커밋 이후에는 성능 변경 외에 기능·통계 정책·표시 변경도 있다. 따라서 아래 시간은 두 코드 버전의 실행시간 차이이며, 변경 전체를 최적화만의 효과로 단정하지 않는다.',
'- 주 표는 38개 대표 실행 경로다. 각 버전의 독립 프로세스 2개에서 준비 실행 후 3회씩 측정했다. 표의 시간은 각 프로세스 내 3회 중앙값 두 개의 평균이다. 두 번째 프로세스는 메뉴 순서를 반대로 실행했다. 오류 종료 시간은 성능으로 집계하지 않았다.',
'- 일반 자료 180행, 상관 3변수, EFA/PCA 6문항, 종단 60명×3시점, 복합표본 30 PSU·180행. 세부 자료·옵션·seed는 재현 스크립트에 고정했다. 메뉴별 대표 조건이며 모든 하위 옵션/자료 크기의 검증은 아니다.',
'- 대부분은 계산 함수 및 진단/표준출력 수집 시간이다. 복합표본 빈도/교차/t-test/상관/회귀/로지스틱은 반환 함수가 표 구성까지 수행하므로 계산+표 구성 시간이다. 모듈 로딩, 메뉴 클릭 대기, 그래프 렌더링, 파일 내보내기, 실제 설치 시간은 제외한다.',
'- 수 ms 구간의 작은 차이는 OS·JIT·GC 변동 영향을 받는다. 백분율이 크더라도 절대 차이가 작은 항목을 확정적인 성능 개선/저하로 일반화하지 않는다. 전체 프로그램 평균 개선율은 계산하지 않았다.', '',
'## 메뉴별 기본 실행 시간', '',
'단위: **밀리초(ms)**. 단축/증가는 계산된 관측 차이다. 동일성 판정은 아래 주석의 비교 범위를 따른다.', '',
'| 메뉴·측정 조건 | 이전 ms | 이후 ms | 시간 변화 | 결과 비교 |',
'| --- | ---: | ---: | --- | --- |']
for r in rows:
    lines.append(f"| {names[int(r['id'])-1]} | {ms(r['before_seconds'])} | {ms(r['after_seconds'])} | {change(r['reduction_percent'])} | {verdict(r)} |")
lines += ['',
'¹ 반환 결과 일치는 환경 참조·외부 포인터와 `timing`/`timings`를 제외하고 함수/언어 객체를 텍스트로 표현한 뒤 허용오차 없이 비교한 것이다. 원시 직렬화 전체의 바이트 일치를 뜻하지 않는다. 수치값은 `identical(..., num.eq=FALSE)`로 비교했다.',
'² 공통 수치는 같은 결과 경로에서 양쪽에 존재하는 숫자 필드의 값·자료형·속성이 일치한다는 뜻이다. 추가 필드·문자 설명·내부 호출 정보 등 차이가 남아 전체 반환 객체 동일로 집계하지 않았다. `summary.csv`에 공통/추가/누락 숫자 필드 수와 데이터프레임 비교 결과를 남겼다.',
'- 성공한 기본 비교 37개에서 경고·메시지와 표준출력·부모 RNG 상태는 일치했다. 두 버전 각각의 반복 실행은 메타분석의 이전 버전 오류 상태를 포함한 38개 경로 모두 재현됐다(76개 정규화 비교). 오류 재현은 분석 성공으로 세지 않았다.',
'- 복합표본 6개 메뉴는 실제 survey 함수의 반올림 전 반환 추정값을 별도로 계측했다. 이전/이후 호출 수는 17/2/16/9/5/5개로 같았고 계측값과 그 속성이 정확히 일치했다. 계측 자체가 결과·진단·RNG를 바꾸지 않았음도 확인했다.',
'- 복합표본의 늘어난 시간은 약 8~15ms(매개·조절 약 15ms) 수준이다. 여러 메뉴에 설계·결측 설명표가 추가되어 표 구성량도 달라졌다. 전체 차이를 수치 계산 자체의 저하로 단정하지 않는다.', '',
'## 긴 부트스트랩 비교', '',
'각 행은 별도 새 프로세스에서 실행한 두 번의 원시 시간이다. 버전 순서는 이전→이후, 이후→이전으로 교차했다. 준비/메뉴 렌더링 시간과 구분한다.', '',
'| 경로 | 이전 1 / 2 (초) | 이후 1 / 2 (초) | 관측 시간 변화 | 결과 비교 |',
'| --- | ---: | ---: | --- | --- |']
boot_notes = {'SEM 5000':'기존 21개 열 정확 일치; 추론 출처 열 추가','CFA 1000':'timings 제외 전체 반환값 일치; 유효 997/1,000','PLS 5000':'동일하지 않음: 유효성 판정·p 계산 방식 변경'}
for kind in ('SEM 5000','CFA 1000','PLS 5000'):
    t=sorted([r for r in boot if r['kind']==kind],key=lambda r:int(r['iteration']))
    if len(t)!=2: raise RuntimeError(f'Missing repeated results: {kind}')
    b=[float(r['before_seconds']) for r in t];a=[float(r['after_seconds']) for r in t]
    reduction=100*(sum(b)-sum(a))/sum(b)
    lines.append(f"| {kind} | {b[0]:.2f} / {b[1]:.2f} | {a[0]:.2f} / {a[1]:.2f} | {change(str(reduction))} | {boot_notes[kind]} |")
lines += ['',
'SEM: 240행·3잠재요인·9지표, 완전자료 ML/FIML 설정, seed 20260826, 12작업자·청크250. 두 비교 모두 4,998/5,000 유효, fallback 0. 부트스트랩 작업 시작부터 결과 읽기까지의 시간이다. 새 `inference_source` 열과 `bootstrap_component_status` 속성 때문에 객체 전체는 같지 않으나 기존 21개 열(6행)의 값·자료형·속성은 모두 정확히 일치했다. 이 작업 반환물에는 원시 재표집 인덱스와 모든 draw가 없으므로 이번 측정으로 그 바이트 동일성까지 주장하지 않는다.',
'CFA: 180행·1요인·3지표, ML/listwise, 신뢰도 1,000회, seed 20260826, 8작업자·청크250, isolated worker/metadata fast path 활성, 원시 estimates 반환. 두 비교 모두 유효 997회와 전체 반환값(timings 제외)이 일치했다. 시간은 1차 30.9% 증가, 2차 0.5% 증가로 편차가 있어 일관된 14% 회귀로 일반화하지 않는다. 이번 조건에서 개선은 입증하지 못했다.',
'PLS: 180행·2잠재요인·6지표, 5,000회, seed 20260826. 현재 작업자 옵션은 8개이며 이전 엔진은 당시 자체 실행 정책을 사용한다. 두 버전에서 총 요청 반복 수는 같지만 유효 반복 판정과 추론식이 달라 동일한 통계 작업의 순수 가속 비교로 해석하지 않는다.',
'세 부트스트랩의 버전별 독립 반복 결과는 정규화 후 재현됐다. 이전/이후 진단·표준출력(SEM은 수집 조건)·부모 RNG 상태도 일치했다.', '',
'## 확인된 수치 차이', '',
'| 경로 | 실제 차이 | 확인한 원인·추가 검증 |',
'| --- | --- | --- |',
'| PCA Varimax | 적재량 최대 차이 0.0004217271, 점수 최대 차이 0.0016963485 | 현재 `stats::varimax(..., eps=1e-12)` 사용. 이전 함수 사본에 이 설정만 맞추자 적재량·점수 정확 일치. |',
'| GEE | 계수 최대 차이 2.767045e-9, p 최대 차이 1.277766e-9 | 현재 geese 수렴 기준 epsilon=1e-10/maxit=100. 이전 함수 사본에 맞추자 계수표 전체와 모형 계수 정확 일치. |',
'| Mann–Whitney | p 0.0021730801347390784 → 0.0021627078548880217 | 이전은 wilcox.test 기본 연속성 보정, 현재는 correct=FALSE. 실제 호출 계측으로 확인. U=2,978·z·Cliff’s delta는 같고 표시 p는 둘 다 .002이나 반올림 전 p는 다르다. 기술통계도 평균/SD에서 중앙값/IQR로 변경됐다. |',
'| PLS 5,000회 | 유효 5,000 → 4,917; 경로 CI [0.4962493, 0.7158077] → [0.4984229, 0.7159229]; p 0 → 0.0004066694 | 현재 inadmissible 83회 제외 및 plus-one 양측 경험적 p 적용. 원래 경로 추정치는 양쪽 모두 약 0.6123912. 속도만 바꾼 동등 결과로 집계하지 않는다. |',
'| PLS 기본 적합 / 벌점회귀 | 설정값·CV folds의 integer/double 자료형 차이 | 해당 공통 숫자의 실제 값은 같다. 추가/누락 출력도 있어 전체 객체 동일과 구분한다. |', '',
'PCA·GEE의 설정 정렬은 원인 검증용 사본에서만 수행했다. 제품 코드나 위 시간 측정의 이전 소스를 수정하지 않았다. Mann–Whitney·PLS의 정책을 임의로 되돌리지 않았다.', '',
'## 메타분석: 별도 보존 기준과 비교', '',
'8월 기준에는 메타분석 함수가 없어 공통 버전 비교가 성립하지 않는다. 대신 최초 3수준 성능 개선 전 보존 소스 `output/meta-three-level-reuse-20260914/baseline.R`와 현재 동결 소스를 새로 비교했다. 기준 SHA256: `1C5C9C9EB03062E57A3791B3185FA4CF83023F4EC32367569ABF27C1BF5A4148`.', '',
'12연구·24효과값: 입력 정규화, REML 기본 모형, 3수준, 강건 추정, 조절효과, 민감도, 연구별 제외, trim-and-fill, 한영 결과표 묶음. 두 프로세스에서 순서를 교차했다. 모든 반환값·조건·stdout·RNG를 정규화 없이 허용오차 0으로 비교했고 프로세스 간에도 일치했다.', '',
'| 효과 유형 | 이전 1 / 2 (초) | 이후 1 / 2 (초) | 평균 시간 변화 | 결과 |',
'| --- | ---: | ---: | --- | --- |']
mt=read('meta/timing-1.csv')+read('meta/timing-2.csv')
for family,label in [('g','표준화 평균차 g'),('r','상관계수'),('or','오즈비')]:
    b=[float(x['elapsed']) for x in mt if x['family']==family and x['version']=='old']
    a=[float(x['elapsed']) for x in mt if x['family']==family and x['version']=='current']
    lines.append(f'| {label} | {b[0]:.2f} / {b[1]:.2f} | {a[0]:.2f} / {a[1]:.2f} | {change(str(100*(sum(b)-sum(a))/sum(b)))} | 전체 정확 일치 |')
lines += ['', '## 결과표 생성 시간', '',
'아래는 계산을 마친 결과 객체를 UI로 구성하고 HTML 문자열로 변환하는 시간이다. 화면 캡처/그림/파일 변환은 포함하지 않는다. 표·주석·구성의 기능 변경도 포함되므로 순수 최적화 효과가 아니다.', '',
'| 메뉴 | 이전 ms | 이후 ms |', '| --- | ---: | ---: |']
for r in rows:
    if r['before_render'] not in ('NA','') and r['after_render'] not in ('NA',''):
        lines.append(f"| {names[int(r['id'])-1]} | {ms(r['before_render'])} | {ms(r['after_render'])} |")
lines += ['', '## 범위와 재현 자료', '',
'분석 설정·복합표본 설계 설정·분석 추천 메뉴는 자체 통계 결과를 계산하는 메뉴가 아니므로 위 계산 시간 집계에서 제외했다. 앱 로딩/메뉴 클릭/모든 내보내기 형식/실제 설치 업그레이드는 이번 비교 대상이 아니다. 첫 실행 시간이나 모든 데이터 조건의 성능 보증으로 확대하지 않는다.',
'비교 도구 초기 실행에서 복합표본의 구버전 미지원 language 인자와 현재 사본의 보조 data/i18n/settings 누락을 보완한 뒤 해당 측정을 다시 실행했다. 보고서의 최종 CSV/RDS는 보완 후 결과만 사용한다. 수치 검증 중 허용오차를 완화하거나 제품 코드를 변경하지 않았다.',
'- `output/menu-before-after-20260915/summary.csv`: 38경로의 시간·오류·정확성·진단·RNG·표 비교.',
'- `before/`, `after/`, `before.zip`, `after-R-hashes.json`: 보존 소스 및 해시.',
'- `before/after-1/2-*.rds`, `*-times.csv`: 메뉴별 전체 결과·HTML과 반복 측정 범위.',
'- `numeric-differences.csv`, `numeric-difference-types.csv`, `canonical-difference-details.rds`: 차이 위치와 종류.',
'- `survey-values-summary.csv`, `survey-audit-*.rds`, `display-detail-*.txt`: 비모수/복합표본 반올림 전 계측 및 표시 내용.',
'- `precision-aligned-reference.rds`: PCA/GEE 설정 정렬 검증(네 검사 통과).',
'- `bootstrap-summary.csv`, `sem-long-*.rds`, `extra-boot-*.rds`, `final-evidence.rds`: 긴 부트스트랩 원시 결과와 비교.',
'- `meta/`: 별도 기준의 메타분석 누적 전후 비교.',
'- 재현 진입점: `scripts/compare_menu_versions.R`, `compare_menu_extra_cases.R`, `compare_menu_sem_long.R`, `compare_menu_cfa_pls_bootstrap.R`, `compare_menu_meta.R`, `diagnose_menu_precision.R`, `audit_menu_survey_values.R`, `summarize_menu_versions.R`, `summarize_menu_supplements.R`, `finalize_menu_evidence.R`. R 실행은 저장소 루트에서 번들 R 및 UTF-8 로캘 사용.', '',
'이번 작업은 비교·검증과 보고서 작성이며 새 최적화, 제품 수정, 설치 파일 재빌드 또는 배포는 하지 않았다.']
md='\n'.join(lines)+'\n'
Path('docs/PERFORMANCE_MENU_BEFORE_AFTER_20260915.md').write_text(md,encoding='utf-8')
# Small self-contained HTML report; no external script, font, or network dependency.
parts=[];table=False
for line in lines:
    if line.startswith('|'):
        cells=[x.strip() for x in line.strip('|').split('|')]
        if all(set(x)<=set(' -:') for x in cells): continue
        if not table: parts.append('<div class="table"><table>');table=True
        parts.append('<tr>'+''.join('<td>'+html.escape(x)+'</td>' for x in cells)+'</tr>')
        continue
    if table:parts.append('</table></div>');table=False
    if line.startswith('# '):parts.append('<h1>'+html.escape(line[2:])+'</h1>')
    elif line.startswith('## '):parts.append('<h2>'+html.escape(line[3:])+'</h2>')
    elif line:parts.append('<p>'+html.escape(line).replace('**','').replace('`','')+'</p>')
if table:parts.append('</table></div>')
page='<!doctype html><html lang="ko"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>분석 메뉴별 전후 비교</title><style>body{font:16px/1.7 system-ui,sans-serif;color:#152436;background:#f4f6fa;margin:0}main{max-width:1180px;margin:auto;padding:32px;background:white}h1{font-size:28px}h2{margin-top:40px;font-size:22px}.table{overflow:auto}table{border-collapse:collapse;width:100%;font-size:14px}td{padding:9px 12px;border-bottom:1px solid #dce2ea}tr:first-child{background:#173b60;color:white;font-weight:bold}tr:nth-child(even){background:#f0f5fa}p{overflow-wrap:anywhere}@media print{main{padding:0}table{font-size:10px}}</style><main>'+''.join(parts)+'</main></html>'
(root/'report.html').write_text(page,encoding='utf-8')
assert len(rows)==38 and len(boot)==6 and len(mt)==12
print('Wrote 38-route report, 6 long-bootstrap comparisons, and 12 meta timing observations')
