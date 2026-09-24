import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = '''Subjects with complete group/covariate values and at least one observed repeated outcome.|집단·공변량 값이 완전하고 반복 결과가 하나 이상 관측된 대상자입니다.|群・共変量の値に欠測がなく、反復測定の結果が少なくとも1つ観測されている対象者です。|组别和协变量值完整，且至少有一次重复测量结果的受试者。|Sujetos con valores completos de grupo y covariables y al menos un resultado observado de las medidas repetidas.|Sujets avec des valeurs complètes pour le groupe et les covariables, et au moins un résultat observé parmi les mesures répétées.|Personen mit vollständigen Gruppen- und Kovariatenwerten und mindestens einem beobachteten Messwiederholungsergebnis.|Các đối tượng có đầy đủ giá trị nhóm và biến đồng biến, cùng ít nhất một kết quả đo lặp được quan sát.
Long-format observed outcome rows used by the ITT mixed-model path.|ITT 혼합모형 분석에 사용한 장형 관측 결과 행입니다.|ITT混合モデル分析に使用した、縦長形式の観測結果の行です。|ITT混合模型分析使用的长格式观测结果行。|Filas de resultados observados en formato largo utilizadas en el análisis ITT mediante modelo mixto.|Lignes de résultats observés au format long utilisées dans l’analyse ITT par modèle mixte.|Zeilen beobachteter Ergebnisse im Langformat, die für die ITT-Analyse mit einem gemischten Modell verwendet wurden.|Các hàng kết quả quan sát ở định dạng dài được sử dụng trong phân tích ITT bằng mô hình hỗn hợp.
Complete-case RM ANOVA was not produced.|완전 사례 반복측정 분산분석 결과를 생성하지 못했습니다.|完全ケースによる反復測定分散分析の結果は生成されませんでした。|未生成完整案例重复测量方差分析结果。|No se generó el ANOVA de medidas repetidas con casos completos.|L’ANOVA à mesures répétées sur les cas complets n’a pas été produite.|Es wurde keine Messwiederholungs-ANOVA für vollständige Fälle erstellt.|Không tạo được kết quả ANOVA đo lặp cho các trường hợp đầy đủ.
Complete-case RM ANOVA could not be computed for the selected variables.|선택한 변수로 완전 사례 반복측정 분산분석을 계산할 수 없습니다.|選択した変数では、完全ケースによる反復測定分散分析を計算できませんでした。|无法使用所选变量计算完整案例重复测量方差分析。|No se pudo calcular el ANOVA de medidas repetidas con casos completos para las variables seleccionadas.|L’ANOVA à mesures répétées sur les cas complets n’a pas pu être calculée pour les variables sélectionnées.|Für die ausgewählten Variablen konnte keine Messwiederholungs-ANOVA für vollständige Fälle berechnet werden.|Không thể tính ANOVA đo lặp cho các trường hợp đầy đủ với các biến đã chọn.
ITT keeps available repeated records through a mixed-model path.|ITT는 혼합모형을 통해 이용 가능한 반복측정 기록을 유지합니다.|ITTは混合モデルを用いて、利用可能な反復測定記録を保持します。|ITT通过混合模型保留可用的重复测量记录。|ITT conserva los registros disponibles de medidas repetidas mediante un modelo mixto.|L’ITT conserve les observations répétées disponibles au moyen d’un modèle mixte.|ITT berücksichtigt die verfügbaren Messwiederholungsdaten mithilfe eines gemischten Modells.|ITT giữ lại các bản ghi đo lặp sẵn có thông qua mô hình hỗn hợp.
A long-format subject-random-intercept model was fitted from the RM selections.|선택한 반복측정 변수로 장형 대상자 임의절편 모형을 적합했습니다.|選択した反復測定変数から、対象者ごとのランダム切片を持つ縦長形式のモデルを適合しました。|使用所选重复测量变量拟合了长格式的受试者随机截距模型。|Se ajustó un modelo en formato largo con intercepto aleatorio por sujeto a partir de las variables seleccionadas de medidas repetidas.|Un modèle au format long avec intercept aléatoire par sujet a été ajusté à partir des variables de mesures répétées sélectionnées.|Aus den ausgewählten Messwiederholungsvariablen wurde ein Modell im Langformat mit zufälligem Interzept je Person angepasst.|Đã ước lượng mô hình ở định dạng dài với hệ số chặn ngẫu nhiên theo đối tượng từ các biến đo lặp đã chọn.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
