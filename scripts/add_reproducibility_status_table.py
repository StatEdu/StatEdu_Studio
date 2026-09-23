from pathlib import Path
import json,csv,html
root=Path(__file__).resolve().parents[1]
out=root/'outputs/spss_consolidated_20260906_07'
# Counts are SPSS comparison cells, never export file/table counts.
rows=[
('빈도분석·기술통계','빈도 28회, 기술통계 31회','503/503; 3,141/3,141','0','검증 범위 일치','식별 가능한 빈도·퍼센트·N·평균·SD 등.','spss_final_20260906'),
('신뢰도','40회','692/692','0','검증 범위 일치','Cronbach α 및 비교 가능한 문항 통계.','spss_final_20260906'),
('상관분석','Pearson 8회','1,662/1,662','0','Pearson 범위 일치','Spearman·Kendall·편상관 등 전체로 확대하지 않음.','spss_final_20260906'),
('교차분석','21회','228/228','0','검증 범위 일치','Pearson χ²·df·점근 p 등. 정확검정 전체 제외.','spss_final_20260906'),
('t검정·일원 분산분석','t검정은 독립·대응 합계 42회; ANOVA 115회; 사후검정 284회','t검정 합계 3,496/3,496; ANOVA 3,100/3,100; 사후 13,184/13,184','0','검증 범위 일치','t검정 집계에는 다음 대응표본 행이 포함됨. 사후검정은 Scheffé·Games–Howell 보정 p값.','spss_phase2_20260906'),
('공분산분석','55회','1,090/1,090','0','제한 설계 일치','단일 요인 가법 ANCOVA, Type III. 일반 GLM 전체 아님.','spss_phase2_20260906'),
('대응표본','독립·대응 t검정 합계 42회에 포함','위 t검정 합계에 포함','합계 기준 0','검증 범위 일치','현재 요약 근거에 대응 t검정만의 별도 분리 집계 없음. 중복 합산 금지.','spss_final_20260906'),
('대응·반복측정','별도 UI 분석군 집계 없음','—','—','별도 검증 집계 없음','대응 t검정·제한적 반복측정 검증을 이 분석군의 모든 옵션 검증으로 간주하지 않음.','spss_final_20260906'),
('단일집단 반복측정 ANOVA','2회','36/36','0','제한 설계 일치','단일 집단·단일 피험자내 요인, 비교한 구형성/보정 항목 범위.','spss_final_20260906'),
('혼합 반복측정 ANOVA','35회','1,610/1,610','0','수정 후 일치','공변량 없는 단일 집단요인×시간요인. 구형성·GG/HF 수정 포함.','spss_phase3_20260906'),
('비모수','Mann–Whitney 5회; Kruskal–Wallis 5회','24/24; 54/54','0','검증 범위 일치','U·z·점근 p, H·df·p. 정확검정·사후검정 전체 아님.','spss_phase2_20260906'),
('대응 비모수','Friedman 36회; Wilcoxon 1회','144/144; 0/1','Wilcoxon 1','일치 및 규약 차이','Friedman N·χ²·df·p 일치. Wilcoxon은 연속성 보정 차이, 기본값 유지.','spss_phase3_20260906'),
('선형회귀','9회','436/436','0','검증 범위 일치','비교 가능한 모형 요약·ANOVA·계수 표.','spss_final_20260906'),
('위계적 회귀','별도 블록별 검증 집계 없음','—','—','별도 검증 집계 없음','일반 선형회귀의 일치를 ΔR²·F 변화·블록 비교 전체로 확대하지 않음.','spss_final_20260906'),
('로지스틱 회귀','이항 7회','455/455','0','정밀도 수정 후 일치','이항 모형 계수·SE·Wald·p·OR·CI. 다항/순서형 전체 아님.','spss_final_20260906'),
('탐색적 요인분석','공통요인 EFA 별도 검증 집계 없음','—','—','SPSS 재현성 미확인','PC 추출·Varimax 검증은 아래 PCA에 해당. EFA 저장 검사 통과와 구분.','spss_final_20260906'),
('주성분분석','50개 분석','기초 항목 7,500/7,500; 회전 2,044/10,887','회전 8,843','기초 일치·회전 차이','공통 엄격 재회전 49/50 일치는 진단 결과. 실제 기본 적재량 차이 해소 아님.','spss_final_20260906'),
('매개·조절','별도 SPSS/PROCESS 수치 검증 집계 없음','—','—','SPSS 재현성 미확인','저장·자체 회귀 테스트를 PROCESS 계수·bootstrap CI 대조 완료로 간주하지 않음.','spss_final_20260906'),
('일반화 모형','이 분석군 전체의 별도 SPSS 집계 없음','—','—','SPSS 재현성 미확인','일반화 GLM 전체 검증과 종단 GEE 검증을 구분.','spss_final_20260906'),
('종단 모형','GEE 80시도/72성공; 반복측정 LMM 90성공','GEE 2,388/2,388; LMM 2,854/2,895','LMM 41','GEE 조건부 일치·LMM 검산 근거','GEE는 43차 수치 안정성 수정 후 실패 8개. LMM은 11·12차 정정/검산 기준: 차이 31개는 닫힌 해, 10개는 고정밀 df 검산이 Studio를 지지. GLMM·패널 전체 아님.','spss_phase43_20260907'),
('생존분석','Cox 11회','66/66','강화 수렴 0; SPSS 기본 17','Cox 조건부 일치','동일 Breslow 동률 처리 및 SPSS 수렴 강화. KM·경쟁위험·Fine–Gray 전체 SPSS 대조 아님.','spss_final_20260906'),
('평가자간 일치도','별도 SPSS 수치 검증 집계 없음','—','—','SPSS 재현성 미확인','Kappa·ICC 저장/자체 검증과 SPSS 직접 대조를 구분.','spss_final_20260906')]
with (out/'분석기법별_SPSS_재현성_현황.csv').open('w',encoding='utf-8-sig',newline='') as f:
 w=csv.writer(f);w.writerow(['분석군','비교 범위','일치/비교 셀','차이 셀','판정','범위와 주의','근거 폴더']);w.writerows(rows)
table='<section id="repro-by-method"><h2>분석 기법별 SPSS 수치 재현성 결과</h2><p><b>이 표는 SPSS와 Studio 계산 결과의 비교다. HTML·PDF·Excel·Word 저장 검사는 아래 별도 표에서 다룬다.</b> 22개 분석군에 대응하되, 직접 비교가 없는 기법은 미확인으로 표시했다. ‘미확인’은 기능 미구현이나 계산 오류 판정이 아니다. 숫자는 비교한 수치 셀이며 독립 검정 수가 아니다.</p><table><tr><th>분석군</th><th>비교 범위</th><th>일치 / 비교 셀</th><th>차이 셀</th><th>판정</th><th>범위와 주의</th></tr>'
for row in rows:
 table+='<tr>'+''.join('<td>'+html.escape(c)+'</td>' for c in row[:6])+'</tr>'
table+='</table><p>독립·대응 t검정은 합산된 원본 집계를 공유하므로 두 행을 합산하지 않는다. GEE·LMM은 11·12차 정정 이후 수치를 사용한다. PCA 기본 회전, Cox 기본 수렴, Wilcoxon 기본 보정의 차이를 진단값으로 덮어쓰지 않았다. 과거 출력 전체 비교와 동일 자료 통제 비교도 합산하지 않는다.</p><p>근거: <a href="../spss_final_20260906/분석별_동일자료_재검증.csv">일반 분석 셀 집계</a> · <a href="../spss_phase2_20260906/2차_검증_결과보고서.html">2차</a> · <a href="../spss_phase3_20260906/3차_검증_결과보고서.html">3차</a> · <a href="../spss_phase11_20260906/11차_검증_정정보고서.html">11차 정정</a> · <a href="../spss_phase12_20260906/12차_LMM_통합보고서.html">12차 검산</a> · <a href="분석기법별_SPSS_재현성_현황.csv">이 표 CSV</a></p></section>'
(out/'reproducibility_table.html').write_text(table,encoding='utf8')
f=out/'StatEdu_Studio_이틀간_작업_종합결과보고서.html';doc=f.read_text(encoding='utf8')
if 'id="repro-by-method"' not in doc:doc=doc.replace('<section id="fixes">',table+'<section id="fixes">')
else:
 start=doc.index('<section id="repro-by-method">');end=doc.index('<section id="fixes">',start)
 doc=doc[:start]+table+doc[end:]
before_archive,archive=doc.split('<section id="archive">',1)
before_archive=before_archive.replace('<th>41차</th>','<th>41차 저장 검사</th>').replace('<td>통과</td></tr>','<td>저장 검사 통과</td></tr>')
doc=before_archive+'<section id="archive">'+archive
doc=doc.replace('<a href="#stats">SPSS 비교</a>','<a href="#stats">SPSS 비교</a><a href="#repro-by-method">기법별 재현성</a>')
f.write_text(doc,encoding='utf8')
print('Added SPSS reproducibility table: 22 analysis families; export labels clarified.')
