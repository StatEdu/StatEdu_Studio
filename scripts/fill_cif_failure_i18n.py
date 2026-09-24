import json,re
from pathlib import Path
rows='''CIF integrity failed for groups:|CIF整合性検査に不合格の集団:|CIF完整性检查未通过的组别：|Grupos que no superaron la comprobación de integridad de CIF:|Groupes ayant échoué au contrôle d’intégrité des CIF :|Gruppen mit fehlgeschlagener CIF-Integritätsprüfung:|Các nhóm không đạt kiểm tra tính toàn vẹn của CIF:
Do not report the CIF curves or estimates because a finiteness, bounds, or cause-sum integrity check failed; review event coding and engine output.|有限性、範囲、または原因別合計の整合性検査に不合格のため、CIF曲線や推定値を報告せず、イベントのコーディングと計算結果を確認してください。|由于有限性、取值范围或原因别总和的完整性检查未通过，请勿报告CIF曲线或估计值；应检查事件编码与计算结果。|No informe las curvas ni las estimaciones de CIF porque falló una comprobación de finitud, límites o suma por causas; revise la codificación de eventos y los resultados del motor de cálculo.|Ne présentez pas les courbes ni les estimations de CIF, car un contrôle de finitude, de bornes ou de somme par cause a échoué ; vérifiez le codage des événements et les résultats du moteur de calcul.|Berichten Sie keine CIF-Kurven oder Schätzwerte, da eine Prüfung auf Endlichkeit, Grenzen oder Ursachensumme fehlgeschlagen ist; prüfen Sie die Ereigniskodierung und die Berechnungsergebnisse.|Không báo cáo đường cong hoặc ước lượng CIF vì kiểm tra tính hữu hạn, giới hạn hoặc tổng theo nguyên nhân không đạt; hãy xem lại mã hóa biến cố và kết quả tính toán.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
