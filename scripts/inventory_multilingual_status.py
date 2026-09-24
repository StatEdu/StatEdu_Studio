"""Menu coverage inventory. Evidence categories are not completion percentages."""
import csv,re
from pathlib import Path
text=Path('R/analysis_menu_ui.R').read_text(encoding='utf-8')
ids=list(dict.fromkeys(re.findall(r'"lazy_analysis_([a-z_]+)"',text)))
names=['빈도/기술통계','교차표','t 검정/분산분석','대응표본','ANCOVA','일원 반복측정','혼합 반복측정','비모수 독립표본','비모수 대응표본','상관','신뢰도','평가자 간 일치도','요인분석','주성분분석','회귀','매개/조절 캔버스','GLM','릿지/라소/엘라스틱넷','로지스틱 회귀','생존분석 설정','Kaplan–Meier','Cox 회귀','경쟁위험','종단/패널','복합표본 설계','복합표본 빈도','복합표본 교차표','복합표본 집단 비교','복합표본 상관','복합표본 회귀','복합표본 로지스틱','복합표본 사용자 모형','CFA','공분산 SEM','PLS-SEM','구조방정식 자동화','메타분석']
assert len(ids)==len(names)==37
representative={'frequencies','crosstabs','ttest_anova','correlation','reliability','interrater_agreement','pca','factor_analysis','one_group_rm_anova','paired','ancova','mixed_rm_anova','hierarchical','generalized','logistic','survival_km','longitudinal','meta','penalized'}
representative.update({'nonparametric','nonparametric_paired'})
representative.add('complex_frequencies')
representative.add('complex_crosstabs')
representative.add('complex_ttest_anova')
representative.add('complex_correlation')
representative.update({'complex_regression','complex_logistic'})
partial={'custom_model_canvas','survival_setup','survival_cox','survival_competing','structural_cfa','structural_cbsem','structural_plssem'}
partial.add('complex_design')
representative.add('complex_custom_model')  # Eight-language actual results and current/accumulated HWPX acceptance pass.
partial.add('structural_automation')  # Recommendation-only UI; actual observer routing checked.
out=Path('docs/MULTILINGUAL_STATUS_INVENTORY_20260916.csv')
with out.open('w',encoding='utf-8-sig',newline='') as f:
 w=csv.writer(f);w.writerow(['menu_id','analysis','evidence_level','remaining','complete'])
 for key,name in zip(ids,names):
  status='대표 결과 검증 기록 있음' if key in representative else '공통/설정/부분 fixture 검증 기록 있음' if key in partial else '전용 다국어 결과 검증 근거 추가 확보 필요'
  remaining='전체 옵션·경고·언어 왕복·저장 최종 확인' if key in representative else '실제 결과·보조표·옵션·오류·저장 검증'
  w.writerow([key,name,status,remaining,'아니오'])
print('Menus:',len(ids),'Representative:',len(representative),'Partial:',len(partial),'Dedicated evidence needed:',len(ids)-len(representative)-len(partial))
