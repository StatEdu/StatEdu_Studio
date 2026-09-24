import json,re
from pathlib import Path
rows='''Guide for Table 2: Additional fit indices not shown in Table 2	표 2 가이드: 표 2에 직접 표시하지 않은 추가 적합도 통계량	表2のガイド：表2に表示していない追加適合度指標	表2指南：表2未列出的其他拟合指标	Guía de la tabla 2: índices de ajuste adicionales no mostrados en la tabla 2	Guide du tableau 2 : indices d’ajustement supplémentaires non affichés dans le tableau 2	Hinweise zu Tabelle 2: Zusätzliche, in Tabelle 2 nicht dargestellte Fit-Indizes	Hướng dẫn bảng 2: chỉ số phù hợp bổ sung không có trong bảng 2
Model statistics	모형 기본 통계	モデルの基本統計	模型基本统计	Estadísticos del modelo	Statistiques du modèle	Modellstatistiken	Thống kê mô hình
Baseline model	기저모형 통계	ベースラインモデル	基线模型	Modelo de referencia	Modèle de référence	Basismodell	Mô hình cơ sở
Incremental fit	증분 적합도	増分適合度	增值拟合	Ajuste incremental	Ajustement incrémental	Inkrementeller Fit	Độ phù hợp gia tăng
RMSEA family	RMSEA 계열	RMSEA関連指標	RMSEA系列	Familia RMSEA	Famille RMSEA	RMSEA-Familie	Nhóm RMSEA
Residual and absolute fit	잔차/절대 적합도	残差・絶対適合度	残差与绝对拟合	Ajuste residual y absoluto	Ajustement résiduel et absolu	Residualer und absoluter Fit	Độ phù hợp phần dư và tuyệt đối
Likelihood and information	우도/정보기준	尤度・情報量規準	似然与信息准则	Verosimilitud e información	Vraisemblance et information	Likelihood und Information	Hợp lý và thông tin
Other indices	기타 지표	その他の指標	其他指标	Otros índices	Autres indices	Weitere Indizes	Chỉ số khác
scaled df	보정 df	補正df	缩放df	gl escalados	ddl corrigés	Skalierte df	df hiệu chỉnh tỷ lệ
χ² scale	χ² 보정계수	χ²補正係数	χ²缩放因子	Factor de escala χ²	Facteur de correction χ²	χ²-Skalierungsfaktor	Hệ số hiệu chỉnh χ²
base χ²	기저 χ²	基準χ²	基线χ²	χ² de referencia	χ² de référence	Basis-χ²	χ² cơ sở
base df	기저 df	基準df	基线df	gl de referencia	ddl de référence	Basis-df	df cơ sở
base p	기저 p	基準p	基线p	p de referencia	p de référence	Basis-p	p cơ sở
base scaled χ²	기저 보정 χ²	基準補正χ²	基线缩放χ²	χ² de referencia escalado	χ² de référence corrigé	Skaliertes Basis-χ²	χ² cơ sở hiệu chỉnh tỷ lệ
base scaled df	기저 보정 df	基準補正df	基线缩放df	gl de referencia escalados	ddl de référence corrigés	Skalierte Basis-df	df cơ sở hiệu chỉnh tỷ lệ
base scaled p	기저 보정 p	基準補正p	基线缩放p	p de referencia escalado	p de référence corrigé	Skaliertes Basis-p	p cơ sở hiệu chỉnh tỷ lệ
unrestricted logL	비제약 logL	非制約logL	无约束logL	logL sin restricciones	logL non restreinte	Unbeschränktes logL	logL không ràng buộc
CI level	신뢰수준	信頼水準	置信水平	Nivel de confianza	Niveau de confiance	Konfidenzniveau	Mức tin cậy
close H0	근접 적합 H0	近似適合H0	近似良好拟合H0	H0 de ajuste cercano	H0 d’ajustement proche	H0 für engen Fit	H0 độ phù hợp gần
not-close H0	비근접 적합 H0	近似不適合H0	近似拟合较差H0	H0 de ajuste no cercano	H0 d’ajustement non proche	H0 für nicht engen Fit	H0 độ phù hợp không gần
robust RMSEA	강건 RMSEA	頑健RMSEA	稳健RMSEA	RMSEA robusto	RMSEA robuste	Robustes RMSEA	RMSEA vững
CI lower	신뢰구간 하한	信頼区間下限	置信区间下限	Límite inferior del IC	Borne inférieure de l’IC	KI-Untergrenze	Cận dưới khoảng tin cậy
CI upper	신뢰구간 상한	信頼区間上限	置信区间上限	Límite superior del IC	Borne supérieure de l’IC	KI-Obergrenze	Cận trên khoảng tin cậy
robust CI lower	강건 신뢰구간 하한	頑健信頼区間下限	稳健置信区间下限	Límite inferior del IC robusto	Borne inférieure de l’IC robuste	Robuste KI-Untergrenze	Cận dưới khoảng tin cậy vững
robust CI upper	강건 신뢰구간 상한	頑健信頼区間上限	稳健置信区间上限	Límite superior del IC robusto	Borne supérieure de l’IC robuste	Robuste KI-Obergrenze	Cận trên khoảng tin cậy vững
robust p	강건 p	頑健p	稳健p	p robusto	p robuste	Robustes p	p vững
robust not-close p	강건 비근접 적합 p	頑健近似不適合p	稳健近似拟合较差p	p robusto de ajuste no cercano	p robuste d’ajustement non proche	Robustes p für nicht engen Fit	p vững độ phù hợp không gần
SRMR Bentler no mean	평균 제외 SRMR Bentler	平均なしSRMR Bentler	不含均值SRMR Bentler	SRMR Bentler sin medias	SRMR Bentler sans moyennes	SRMR Bentler ohne Mittelwerte	SRMR Bentler không có trung bình
CRMR no mean	평균 제외 CRMR	平均なしCRMR	不含均值CRMR	CRMR sin medias	CRMR sans moyennes	CRMR ohne Mittelwerte	CRMR không có trung bình
SRMR Mplus no mean	평균 제외 SRMR Mplus	平均なしSRMR Mplus	不含均值SRMR Mplus	SRMR Mplus sin medias	SRMR Mplus sans moyennes	SRMR Mplus ohne Mittelwerte	SRMR Mplus không có trung bình'''
rows+='''
RMSEA NOTCLOSE PVALUE	RMSEA 비근접 적합 p	RMSEA近似不適合p	RMSEA近似拟合较差p	p RMSEA de ajuste no cercano	p RMSEA d’ajustement non proche	RMSEA-p für nicht engen Fit	p RMSEA độ phù hợp không gần
RMR_NOMEAN	평균 제외 RMR	平均なしRMR	不含均值RMR	RMR sin medias	RMR sans moyennes	RMR ohne Mittelwerte	RMR không có trung bình
SCALING FACTOR H1	H1 보정계수	H1補正係数	H1缩放因子	Factor de escala H1	Facteur de correction H1	H1-Skalierungsfaktor	Hệ số hiệu chỉnh H1
SCALING FACTOR H0	H0 보정계수	H0補正係数	H0缩放因子	Factor de escala H0	Facteur de correction H0	H0-Skalierungsfaktor	Hệ số hiệu chỉnh H0
BASELINE CHISQ SCALING FACTOR	기저 χ² 보정계수	基準χ²補正係数	基线χ²缩放因子	Factor de escala χ² de referencia	Facteur de correction χ² de référence	Basis-χ²-Skalierungsfaktor	Hệ số hiệu chỉnh χ² cơ sở
RMSEA CI LOWER SCALED	보정 RMSEA 신뢰구간 하한	補正RMSEA信頼区間下限	缩放RMSEA置信区间下限	Límite inferior del IC RMSEA escalado	Borne inférieure de l’IC RMSEA corrigé	Skalierte RMSEA-KI-Untergrenze	Cận dưới khoảng tin cậy RMSEA hiệu chỉnh tỷ lệ
RMSEA CI UPPER SCALED	보정 RMSEA 신뢰구간 상한	補正RMSEA信頼区間上限	缩放RMSEA置信区间上限	Límite superior del IC RMSEA escalado	Borne supérieure de l’IC RMSEA corrigé	Skalierte RMSEA-KI-Obergrenze	Cận trên khoảng tin cậy RMSEA hiệu chỉnh tỷ lệ
RMSEA PVALUE SCALED	보정 RMSEA p	補正RMSEA p	缩放RMSEA p	p RMSEA escalado	p RMSEA corrigé	Skaliertes RMSEA-p	p RMSEA hiệu chỉnh tỷ lệ
RMSEA NOTCLOSE PVALUE SCALED	보정 RMSEA 비근접 적합 p	補正RMSEA近似不適合p	缩放RMSEA近似拟合较差p	p RMSEA escalado de ajuste no cercano	p RMSEA corrigé d’ajustement non proche	Skaliertes RMSEA-p für nicht engen Fit	p RMSEA hiệu chỉnh tỷ lệ cho độ phù hợp không gần'''
for metric in ['CFI','TLI','NNFI','RFI','NFI','PNFI','IFI','RNI','RMSEA']:
 rows+='\n'+'\t'.join([metric+' SCALED',f'보정 {metric}',f'補正{metric}',f'缩放{metric}',f'{metric} escalado',f'{metric} corrigé',f'Skaliertes {metric}',f'{metric} hiệu chỉnh tỷ lệ'])
for metric in ['NNFI','RNI']:
 rows+='\n'+'\t'.join([metric+' ROBUST',f'강건 {metric}',f'頑健{metric}',f'稳健{metric}',f'{metric} robusto',f'{metric} robuste',f'Robustes {metric}',f'{metric} vững'])
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  fields=line.split('\t');assert len(fields)==8
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')]=fields[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
