import json
from pathlib import Path
keys = ['meta.input_review.' + k for k in ['categorical_prefix','continuous_prefix','missing_id','title']] + ['meta.input_error.' + k for k in ['pairs','unique','numeric','conflict']]
rows = {
'en':['Categorical: ','Continuous: ','Missing study ID','Input review','Enter moderators as name=value pairs separated by semicolons.','Moderator names must be unique within each type.','Every continuous moderator value must be numeric and finite.','A moderator name cannot be used as both categorical and continuous in the same row.'],
'ko':['범주형: ','연속형: ','연구 ID 없음','입력 확인 사항','조절변수를 이름=값 형식으로 입력하고 세미콜론으로 구분하세요.','각 유형 내에서 조절변수 이름은 중복될 수 없습니다.','모든 연속형 조절변수 값은 유한한 숫자여야 합니다.','같은 행에서 동일한 조절변수 이름을 범주형과 연속형에 함께 사용할 수 없습니다.'],
'ja':['カテゴリ型：','連続型：','研究IDなし','入力内容の確認','調整変数は名前=値の形式で入力し、セミコロンで区切ってください。','各タイプ内で調整変数名は一意である必要があります。','連続型調整変数の値はすべて有限の数値である必要があります。','同じ行で同じ調整変数名をカテゴリ型と連続型の両方に使用することはできません。'],
'zh':['分类：','连续：','缺少研究 ID','输入检查','请以名称=值的形式输入调节变量，并用分号分隔。','同一类型内的调节变量名称不能重复。','所有连续调节变量的值必须是有限数值。','同一行中，同一调节变量名称不能同时用于分类和连续类型。'],
'es':['Categórico: ','Continuo: ','Falta el ID del estudio','Revisión de entradas','Introduzca los moderadores como pares nombre=valor separados por punto y coma.','Los nombres de los moderadores deben ser únicos dentro de cada tipo.','Todos los valores de los moderadores continuos deben ser numéricos y finitos.','Un nombre de moderador no puede usarse como categórico y continuo en la misma fila.'],
'fr':['Catégoriel : ','Continu : ','ID de l’étude manquant','Vérification des saisies','Saisissez les modérateurs sous forme de paires nom=valeur séparées par des points-virgules.','Les noms des modérateurs doivent être uniques au sein de chaque type.','Toutes les valeurs des modérateurs continus doivent être numériques et finies.','Un nom de modérateur ne peut pas être utilisé à la fois comme catégoriel et continu dans une même ligne.'],
'de':['Kategorial: ','Kontinuierlich: ','Studien-ID fehlt','Eingabeprüfung','Geben Sie Moderatoren als durch Semikolons getrennte Name=Wert-Paare ein.','Moderatornamen müssen innerhalb jedes Typs eindeutig sein.','Alle Werte kontinuierlicher Moderatoren müssen numerisch und endlich sein.','Ein Moderatorname kann in derselben Zeile nicht sowohl kategorial als auch kontinuierlich verwendet werden.'],
'vi':['Phân loại: ','Liên tục: ','Thiếu ID nghiên cứu','Kiểm tra dữ liệu nhập','Nhập biến điều tiết theo dạng tên=giá trị, phân cách bằng dấu chấm phẩy.','Tên biến điều tiết phải duy nhất trong mỗi loại.','Mọi giá trị biến điều tiết liên tục phải là số hữu hạn.','Không thể dùng cùng một tên biến điều tiết cho cả loại phân loại và liên tục trong cùng một dòng.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update(dict(zip(keys, values)))
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
