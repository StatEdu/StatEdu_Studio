from pathlib import Path
from html.parser import HTMLParser
import html,json,re,hashlib,os

root=Path(__file__).resolve().parents[1]; outputs=root/'outputs'
dest=outputs/'spss_consolidated_20260906_07';dest.mkdir(exist_ok=True)
reports=[f for d in outputs.glob('spss*') if d.is_dir() and re.search(r'2026090[67]$',d.name) for f in d.glob('*보고서.html')]
def key(f):
 m=re.search(r'spss_phase(\d+)_',f.parent.name)
 return (int(m[1])+4 if m else {'spss_replication_20260906':0,'spss_archive_replication_20260906':1,'spss_fix_20260906':2,'spss_final_20260906':3}.get(f.parent.name,100),f.name)
reports.sort(key=key);assert len(reports)==46,len(reports)
def rel(f):return Path(os.path.relpath(f,dest)).as_posix()
def link(f,label):return f'<a href="{html.escape(rel(f),quote=True)}">{html.escape(label)}</a>'
class Readable(HTMLParser):
 def __init__(self,folder):super().__init__(convert_charrefs=True);self.out=[];self.skip=0;self.folder=folder
 def handle_starttag(self,tag,attrs):
  if tag in ('script','style','head','title'):self.skip+=1;return
  if self.skip:return
  a=dict(attrs)
  if tag=='img':
   src=a.get('src','');label=a.get('alt') or '원문 그림'
   if src and not src.startswith('data:'):self.out.append('<p>'+self.anchor(src,label+' 보기')+'</p>')
   return
  if tag not in ('p','div','section','article','h1','h2','h3','h4','h5','h6','table','thead','tbody','tr','th','td','ul','ol','li','b','strong','em','i','code','pre','br','hr','a','blockquote','details','summary'):return
  tag={'h1':'h3','h2':'h4'}.get(tag,tag)
  attributes=''
  if tag=='a' and a.get('href'):attributes=' href="'+html.escape(self.target(a['href']),quote=True)+'"'
  for k in ('colspan','rowspan'):
   if tag in ('td','th') and a.get(k,'').isdigit():attributes+=f' {k}="{a[k]}"'
  self.out.append('<'+tag+attributes+'>')
 def target(self,url):
  if re.match(r'^(?:[a-zA-Z][\w+.-]*:|#)',url):return url
  return rel((self.folder/url).resolve())
 def anchor(self,url,label):return '<a href="'+html.escape(self.target(url),quote=True)+'">'+html.escape(label)+'</a>'
 def handle_endtag(self,tag):
  if tag in ('script','style','head','title'):
   self.skip=max(0,self.skip-1);return
  if self.skip:return
  if tag in ('p','div','section','article','h1','h2','h3','h4','h5','h6','table','thead','tbody','tr','th','td','ul','ol','li','b','strong','em','i','code','pre','a','blockquote','details','summary'):self.out.append('</'+{'h1':'h3','h2':'h4'}.get(tag,tag)+'>')
 def handle_data(self,data):
  if not self.skip:self.out.append(html.escape(data))

manifest=[];appendices=[];index=[]
for n,f in enumerate(reports,1):
 raw=f.read_text(encoding='utf-8-sig');m=re.search(r'<h1[^>]*>(.*?)</h1>',raw,re.S|re.I) or re.search(r'<title[^>]*>(.*?)</title>',raw,re.S|re.I)
 title=html.unescape(re.sub('<[^>]+>','',m[1])) if m else f.stem
 title=' '.join(title.split());parser=Readable(f.parent);parser.feed(raw)
 phase=re.search(r'phase(\d+)_',f.parent.name);phase=int(phase[1]) if phase else None
 status='당시 검증 기록 — 최신 결론은 본문 기준'
 if phase and 4<=phase<=10:status='정정 대상 — 범주형 설계 관련 집계는 11·12차로 대체'
 if phase==15:status='정책 변경 이력 — PDF 표당 1쪽·긴 표 축소는 18차 이후 정책으로 대체'
 if phase==42:status='대표 사례 실제 저장의 최종 확인 기록'
 index.append(f'<tr><td>{n:02d}</td><td><a href="#r{n}">{html.escape(title)}</a></td><td>{f.parent.name[-8:]}</td><td>{status}</td></tr>')
 appendices.append(f'<details id="r{n}" class="source"><summary>{n:02d}. {html.escape(title)}</summary><p class="notice">{status}. {link(f,"원본 보고서 열기")}</p><div class="sourcebody">'+''.join(parser.out)+'</div></details>')
 manifest.append(dict(index=n,title=title,path=str(f.relative_to(root)).replace('\\','/'),sha256=hashlib.sha256(f.read_bytes()).hexdigest(),bytes=f.stat().st_size,status=status))

agg=json.loads((outputs/'spss_phase40_20260907/aggregate_summary.json').read_text(encoding='utf8'))
latest=json.loads((outputs/'spss_phase41_20260907/summary.json').read_text(encoding='utf8'))
actual=json.loads((outputs/'spss_phase42_20260907/actual_four_format_checks.json').read_text(encoding='utf8'))
assert latest['cases']==sum(a['cases'] for a in agg['analyses'])==179
assert latest['tables_per_format']==sum(a['tables'] for a in agg['analyses'])==1080
rows=''.join(f'<tr><td>{a["phase"]}</td><td>{html.escape(a["name"])}</td><td>{a["cases"]}</td><td>{a["files"]}</td><td>{a["tables"]}</td><td>{a["figures"]}</td><td>통과</td></tr>' for a in agg['analyses'])
body='''<header><p class="eyebrow">StatEdu Studio · 2026년 9월 6–7일</p><h1>SPSS 재현성 검토와 분석 결과 내보내기 종합 보고서</h1><p>자료 조사, 차이 원인 검토, 프로그램 수정, 설정 저장·복원, 4형식 내보내기 검증을 한 문서에 통합했다.</p></header>
<nav><a href="#conclusion">최종 판단</a><a href="#stats">SPSS 비교</a><a href="#fixes">수정·구현</a><a href="#exports">내보내기</a><a href="#remaining">남은 과제</a><a href="#archive">46개 원문</a></nav>
<section id="conclusion"><h2>1. 현재 결론</h2><p><b>분석 결과 내보내기의 핵심 구현과 22개 분석군 자동 회귀 검증, 대표 사례의 실제 4형식 저장 검증을 완료했다.</b> SPSS 재현성은 비교한 설계·자료·항목 범위에서 판단해야 하며, 보관된 모든 프로젝트나 SPSS 전체 기능의 완전한 동등성을 선언하지 않는다.</p>
<div class="metrics"><div><b>46</b>통합한 원본 보고서</div><div><b>22 / 179</b>분석군 / 회귀 사례</div><div><b>716</b>최종 코드 재생성 파일</div><div><b>4</b>실제 앱 저장 확인 형식</div></div>
<p>검토 소스는 1.2.6-dev다. 초기에 언급된 1.2.5-dev 기능 평가와 현재 소스 검증을 구분한다. 42차 실제 앱은 최신 소스를 참조하는 별도 Electron 실행 환경이며, 최신 설치 프로그램의 설치·배포 인증은 아니다. 15차의 QA unpacked 빌드 기록도 최종 수정본 설치 인증으로 간주하지 않는다.</p>
<p>검토 대상은 실제 보관 자료 경로 sample_spss와 관련 검증 자료다. AMOS·SmartPLS의 CFA·SEM·PLS-SEM은 제외했다. SPSS PCA·Varimax와 Studio EFA/PCA 출력 검증은 이 제외 항목과 구분한다.</p></section>
<section><h2>2. 집계 원칙과 정정 사항</h2><ul><li>과거 저장 출력과 현재 자료의 비교, 동일 자료 통제 재실행, 독립 수학 검산, 내보내기 파일 검사는 서로 다른 증거다. 겹치는 셀·모형·재실행 수를 합쳐 전체 통과율을 만들지 않았다.</li><li>4~10차 GEE/LMM 검증 파서가 줄바꿈 뒤 BY 선언을 놓쳤다. LMM 90개 중 63개, GEE 80개 중 36개 구문에 영향이 확인됐다. 원래 범주형 설계 기준의 최종 수치는 11·12차를 사용한다.</li><li>19~40차의 179개 사례와 41차의 179개 사례는 같은 회귀 사례군이다. 358개 고유 사례가 아니다. 42차는 대표 사례 1건의 실제 저장을 추가 확인한 단계다.</li><li>15차의 PDF 표당 한 페이지·긴 표 축소 정책은 이후 변경됐다. 최종 규칙은 세로 표 연속 배치, 긴 표 이어짐, 가로 표 독립 배치다.</li><li>일치는 각 검사 허용오차 내의 일치다. 주요 SPSS 통제 비교는 max(1e-6, |참조값|×1e-8), PCA 적재량 진단은 1e-4 등을 적용했다. 원본 셀별 기준을 우선한다.</li></ul></section>
<section id="stats"><h2>3. SPSS 프로젝트 조사와 수치 비교</h2><h3>보관 자료 조사</h3><p>159개 프로젝트 폴더(비어 있지 않은 폴더 131개), 원본 파일 2,979개, SPV 398개, SAV 397개를 조사했다. 수치 비교가 이뤄진 프로젝트는 72개다. 원본 파일 SHA-256 동일성을 수정 검증에서 재확인했다.</p>
<table><tr><th>비교 구분</th><th>범위</th><th>일치</th><th>차이·한계</th></tr><tr><td>과거 SPSS 저장 출력</td><td>1,396절차 / 82,369셀</td><td>1,057절차 / 67,923셀</td><td>339절차 / 14,446셀 차이. 당시 자료·필터·가중치·분할 설정의 불확실성 포함.</td></tr><tr><td>일반 분석 동일 자료 통제 비교</td><td>314실행 / 13,815셀</td><td>13,815셀</td><td>기존 통과 296실행과 수정 후 회귀 18실행의 결합. Cox는 SPSS 수렴 기준 강화 조건.</td></tr><tr><td>2차 사후검정·ANCOVA·비모수</td><td>349실행 / 14,352셀</td><td>14,352셀</td><td>사후검정은 쌍별 보정 p값 중심. 전체 사후검정 표 인증 아님.</td></tr><tr><td>3차 혼합 반복측정·Friedman·Wilcoxon</td><td>72실행 / 1,755셀</td><td>1,754셀</td><td>Wilcoxon p값 1셀은 기본 연속성 보정 차이 유지.</td></tr><tr><td>GEE — 11차 정정 기준</td><td>80모형 시도 / 71성공 / 2,344항목</td><td>2,344항목</td><td>9모형 실패. SPSS 상관 보정 대응 옵션 및 정밀 수렴 조건.</td></tr><tr><td>LMM — 11·12차 최종 기준</td><td>90모형 / 2,895항목</td><td>2,854항목</td><td>원시 차이 41개 유지. 별도 검산은 Studio 계산을 지지.</td></tr></table>
<p>최초 식별 절차는 4,533개였고 당시 미검증 절차는 3,137개였다. 후속 단계는 구문 중복 정리와 다른 비교 단위를 사용했으므로 추가 실행 수를 3,137에서 단순 차감한 ‘현재 미검증 총수’를 제시하지 않는다. 절차에는 자료 조작·그래프도 포함돼 미구현 분석 기법 수와 같지 않다.</p>
<h3>차이를 유지하거나 별도로 설명한 항목</h3><ul><li><b>PCA:</b> 고유값·공통성·KMO/Bartlett 7,500셀 일치. 기본 Varimax 적재량 10,887셀 중 8,843셀 차이. 엄격한 공통 재회전은 49/50개에서 일치했으나 기본 출력 차이가 제거된 것은 아니다. 나머지 1개는 서로 다른 국소해를 유지했다.</li><li><b>Cox:</b> SPSS 기본 수렴 조건에서는 17셀 차이가 남고, 강화한 수렴 기준에서 66셀 모두 일치했다. Studio 계산을 SPSS 기본 출력에 강제로 맞추지 않았다.</li><li><b>Wilcoxon:</b> SPSS p=0.47407315519583, Studio 기본 p=0.47785980344940. 연속성 보정을 끈 진단값은 일치하지만 기본값은 유지했다.</li><li><b>LMM:</b> 차이 41개 중 31개는 닫힌 해 검산, 10개는 60자리 정밀도의 독립 자유도 산술 검산이 Studio를 지지했다. 이를 SPSS 원시 일치로 재분류하지 않는다. AR1 3모형·90항목은 모두 일치한다.</li></ul></section>
<section id="fixes"><h2>4. 주요 수정과 구현</h2><table><tr><th>영역</th><th>조치와 검증 결과</th></tr><tr><td>로지스틱 / SAV</td><td>로지스틱 IRLS 정밀도 강화로 90개 차이 해소. haven 실패 시 foreign 대체 판독과 라벨·사용자 결측 변환 추가. 실패 SAV 12개·1,410열 비교 통과.</td></tr><tr><td>Mann–Whitney</td><td>표시 z와 p의 연속성 보정 규약을 통일해 5개 p값 차이 해소.</td></tr><tr><td>혼합 반복측정</td><td>집단 평균 효과를 제거한 잔차 공분산으로 구형성을 계산하고 GG/HF 및 오차 자유도 규칙 수정. 1,610셀 일치.</td></tr><tr><td>GEE 안정성·상관 보정</td><td>실패의 프로세스 격리·입력/특이 상관 검증과 SPSS 모수 수 보정 옵션 구현. 최종 정정 기준 71성공, 9실패. 가중치 결합 지원으로 확대 해석하지 않음.</td></tr><tr><td>반복측정 LMM</td><td>REML+UN/AR1, Newton 정밀 보정, Satterthwaite 자유도 경로를 분석·UI·설명·런타임에 연결. 기존 ML 랜덤효과 모형과 구분.</td></tr><tr><td>설정 저장·복원</td><td>종단 변수 역할 7개와 설정 15개, 가정 옵션 보존. UN/AR1 재분석 계수표 identical 통과. .studio는 외부 데이터 경로와 설정을 참조하며 과거 적합 결과의 완전 보관 파일은 아님.</td></tr><tr><td>화면·보고 출력</td><td>재바인딩 반복, REML 설명, 상세 df 누락, 저장 복원 반응형 접근 문제 수정. 분석별 누락 표·그림, 표와 그림 순서, 스타일 전달 수정.</td></tr><tr><td>생존확률 표시</td><td>첫 확률이 여러 행에 반복되는 표 생성 문제 수정. 4개 KM 사례의 224행 중 잘못 표시된 203개 확률 셀 교정. 계산 엔진 값과 표시 값을 구분해 검증.</td></tr></table><p>각 단계의 수정 건수는 공통 함수 변경과 후속 보완이 겹치므로 단순 합산하지 않았다. 초기 수정 보고서의 88줄 추가·2줄 삭제는 당시 범위이며 이틀 전체 변경량이 아니다. 전체 변경 라인 수는 이틀 시작·종료의 고정된 스냅샷 없이는 확정하지 않는다.</p></section>
<section id="exports"><h2>5. 최종 내보내기 규칙과 전체 분석군 검증</h2><table><tr><th>형식</th><th>최종 방식</th></tr><tr><td>HTML</td><td>화면 출력 HTML/CSS를 보존하고 표·그래프 순서를 따른다.</td></tr><tr><td>PDF</td><td>B5 세로 표는 순서대로 연속 배치. 짧은 표는 가능한 한 묶어 유지. 긴 표는 다음 쪽에 이어지며 머리글 반복. 가로 표는 독립 가로 페이지, 긴 가로 표도 이어짐. 이후 세로로 복귀하며 높이 축소로 한 쪽에 강제 맞추지 않는다. 표는 텍스트로 보존한다.</td></tr><tr><td>Word</td><td>편집 가능한 네이티브 표. 표와 그림 순서를 보존한다. 그림·모형도는 이미지일 수 있으며 모든 요소를 편집 가능한 벡터로 저장한다는 뜻은 아니다.</td></tr><tr><td>Excel</td><td>화면의 표시 문자열을 편집 가능한 셀로 저장하고 순서·병합·기본 스타일·방향을 반영한다. 표시 문자열 보존이 목적이므로 원시 숫자·수식 내보내기와 구분한다. 표/그림별 시트와 긴 표 인쇄 축소의 가독성 한계가 있다.</td></tr></table>
<p>19~40차에서 아래 사례군을 단계별 점검했고, 41차에는 동일한 179개 사례를 최종 공통 소스로 다시 생성해 716개 파일을 검사했다. 형식별 표 1,080개·그림 128개다. 검사 전후 R 소스·스타일·VERSION 해시가 동일했다.</p><table><tr><th>차수</th><th>분석군</th><th>사례</th><th>4형식 파일</th><th>표/형식</th><th>그림/형식</th><th>41차</th></tr>'''+rows+'''</table><p>검사는 화면/HTML 셀, Word 셀·병합·그림 수, Excel 셀 문자열·시트 순서·방향, PDF 텍스트 포함·그림 제목/수·빈 페이지와 표/그림 순서를 포함했다. PDF 문자열 포함만으로 중복 값의 횟수나 모든 페이지의 시각적 동일성이 보장되지는 않는다. 41차에서 모든 Office 파일을 다시 열어 인쇄하거나 모든 페이지를 육안 비교한 것은 아니다.</p><p>별도 14쪽 배치 시험에서 세로 110행·가로 95행, 총 205행의 누락 없는 순서, 반복 머리글, 짧은 표 연속 배치, 가로 독립 배치와 세로 복귀를 확인했다.</p>
<h3>42차 실제 앱 저장</h3><p>사용자가 네이티브 저장 창에서 저장을 완료한 4개 파일을 확인했다. 72건 age 기술통계 표 1개와 히스토그램 1개이며, 머리글·값 20개가 HTML·PDF·Word·Excel에서 일치했다. Word는 표 1개, Excel은 표와 그래프의 2개 시트다. PDF는 B5 가로 표 1쪽과 세로 그래프 1쪽으로 구성되며 표는 텍스트, 히스토그램은 이미지다. 실제 저장 조작은 협업으로 확인했고 취소·덮어쓰기 실제 조작은 미검증이다.</p></section>
<section id="remaining"><h2>6. 남은 과제와 배포 판단</h2><table><tr><th>우선순위</th><th>작업</th><th>완료 기준</th></tr><tr><td>배포 전</td><td>최신 설치 패키지 빌드·설치·실행, 실제 취소/덮어쓰기 확인</td><td>최종 소스와 번들 일치, 새 설치 환경에서 분석·저장·취소·기존 파일 보호 확인</td></tr><tr><td>가독성</td><td>긴 Excel 표 인쇄 축소, 일부 그래프/표 줄바꿈</td><td>작은 글씨와 IQR 닫는 괄호 단독 줄바꿈 등을 화면 스타일 정책 안에서 개선</td></tr><tr><td>재현성 추가</td><td>GEE 실패 9개, LMM 미연결 구문 11개</td><td>자료 적합성·특이 상관·수렴·변수 연결을 개별 확인. 실패를 임의 성공으로 대체하지 않음</td></tr><tr><td>범위 확대</td><td>복합표본, 미검증 GLM/GLMM 설계, EMMEANS·다자유도 검정·PROCESS 등</td><td>실제 자료·옵션·추정 규약별 SPSS 수치 비교. 내보내기 통과를 수치 재현성 인증으로 대체하지 않음</td></tr><tr><td>계산 규약 설명</td><td>PCA 회전·Cox 기본 수렴·Wilcoxon 보정 차이</td><td>기본값 차이와 검산 근거를 사용자 문서에 명확히 유지</td></tr></table><p><b>판단: 핵심 내보내기 기능과 검증된 계산·저장 경로는 완료됐으며, 배포 전 최종 점검과 남은 재현성 범위 검토가 필요하다.</b></p></section>
<section id="archive"><h2>7. 전체 원본 보고서 본문</h2><p>46개 HTML 보고서의 본문과 표를 아래에 모두 수록했다. 원문의 과거 판단은 역사적 기록이며 위 종합 판단과 정정 안내를 우선한다. 통합 문서는 본문 열람에 원본 파일을 요구하지 않는다. 원본 대형 그림과 CSV·로그 등 별도 근거 자료는 링크로 연결했다. 원본 문서의 개별 디자인·그림 자체는 합본에 복제하지 않았다.</p><button onclick="document.querySelectorAll('details.source').forEach(d=>d.open=true)">원문 모두 펼치기</button> <button onclick="document.querySelectorAll('details.source').forEach(d=>d.open=false)">원문 모두 접기</button><table><tr><th>번호</th><th>보고서</th><th>기록일</th><th>현재 해석</th></tr>'''+''.join(index)+'''</table>'''+''.join(appendices)+'''</section><footer>통합 문서 작성은 기존 분석 결과·검증 기록의 재정리다. 이번 문서 작성으로 분석 엔진을 추가 수정하거나 SPSS 비교를 새로 실행하지 않았다.</footer>'''
css='''body{font-family:"Malgun Gothic",Arial,sans-serif;max-width:1160px;margin:36px auto;padding:0 22px;color:#203347;line-height:1.8}header{border-bottom:4px solid #17756d;padding-bottom:20px}h1{font-size:30px;line-height:1.4}h2{margin-top:46px;font-size:24px}h3{margin-top:28px}h4{font-size:18px}.eyebrow{color:#17756d}nav{display:flex;gap:20px;flex-wrap:wrap;padding:18px;background:#eef5f5;position:sticky;top:0}a{color:#12685f}table{border-collapse:collapse;width:100%;margin:18px 0;font-size:14px}th,td{border:1px solid #ccd6df;padding:9px;text-align:left;vertical-align:top}th{background:#edf3f7}.metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:14px}.metrics div{background:#eef5f5;padding:20px}.metrics b{display:block;font-size:30px}details{border:1px solid #ccd6df;border-radius:6px;margin:14px 0;padding:16px}summary{cursor:pointer;font-weight:bold}.notice{background:#fff2d8;padding:12px}.sourcebody{overflow:auto}code,pre{white-space:pre-wrap;overflow-wrap:anywhere}button{padding:10px 18px;cursor:pointer}footer{border-top:1px solid #ccc;margin-top:40px;padding:20px;color:#526171}@media(max-width:760px){.metrics{grid-template-columns:repeat(2,1fr)}table{display:block;overflow:auto}nav{position:static}}@media print{nav,button{display:none}body{max-width:none;margin:0}details{break-before:page}th{background:#eee}a{color:inherit}}'''
filename=dest/'StatEdu_Studio_이틀간_작업_종합결과보고서.html'
filename.write_text('<!doctype html><html lang="ko"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>StatEdu Studio 9월 6–7일 종합 결과보고서</title><style>'+css+'</style>'+body+'</html>',encoding='utf8')
(dest/'source_manifest.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2),encoding='utf8')
assert sum(1 for _ in re.finditer(r'<details id="r\d+"',filename.read_text(encoding='utf8')))==46
print(json.dumps(dict(reports=len(reports),bytes=filename.stat().st_size,analyses=len(agg['analyses']),cases=latest['cases'],file=str(filename)),ensure_ascii=True))
