import json,re
from pathlib import Path
rows='''Inference and bootstrap details: Structural paths|推論とブートストラップの詳細：構造パス|推断与自助法详情：结构路径|Detalles de inferencia y bootstrap: rutas estructurales|Détails d’inférence et de bootstrap : chemins structurels|Inferenz- und Bootstrap-Details: strukturelle Pfade|Chi tiết suy luận và bootstrap: đường dẫn cấu trúc
Inference and bootstrap details: Specific indirect effects|推論とブートストラップの詳細：特定間接効果|推断与自助法详情：特定间接效应|Detalles de inferencia y bootstrap: efectos indirectos específicos|Détails d’inférence et de bootstrap : effets indirects spécifiques|Inferenz- und Bootstrap-Details: spezifische indirekte Effekte|Chi tiết suy luận và bootstrap: tác động gián tiếp riêng
Inference and bootstrap details: Direct, indirect, total effects|推論とブートストラップの詳細：直接・間接・総効果|推断与自助法详情：直接、间接、总效应|Detalles de inferencia y bootstrap: efectos directos, indirectos y totales|Détails d’inférence et de bootstrap : effets directs, indirects et totaux|Inferenz- und Bootstrap-Details: direkte, indirekte und Gesamteffekte|Chi tiết suy luận và bootstrap: tác động trực tiếp, gián tiếp và tổng tác động
B CI source|B CI算出根拠|B CI来源|Fuente del IC de B|Source de l’IC de B|Quelle des B-KI|Nguồn CI của B
beta CI source|beta CI算出根拠|beta CI来源|Fuente del IC de beta|Source de l’IC de beta|Quelle des beta-KI|Nguồn CI của beta
Direct structural paths|直接構造パス|直接结构路径|Rutas estructurales directas|Chemins structurels directs|Direkte strukturelle Pfade|Đường dẫn cấu trúc trực tiếp
Specific indirect effects|特定間接効果|特定间接效应|Efectos indirectos específicos|Effets indirects spécifiques|Spezifische indirekte Effekte|Tác động gián tiếp riêng
Other indirect and total effects|その他の間接効果と総効果|其他间接效应和总效应|Otros efectos indirectos y totales|Autres effets indirects et totaux|Weitere indirekte und Gesamteffekte|Tác động gián tiếp khác và tổng tác động
Fixed parameter (not tested)|固定パラメータ（検定対象外）|固定参数（未检验）|Parámetro fijo (no probado)|Paramètre fixé (non testé)|Fester Parameter (nicht getestet)|Tham số cố định (không kiểm định)
Fixed effect (not tested)|固定効果（検定対象外）|固定效应（未检验）|Efecto fijo (no probado)|Effet fixé (non testé)|Fester Effekt (nicht getestet)|Tác động cố định (không kiểm định)'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
