import json
from pathlib import Path
keys=['prefix','empty','filename','snapshot','image','folder','no_figures','format','named','decode','length']
rows={
'en':['Invalid figure folder prefix.','No figure snapshots are available.','Invalid figure filename.','Invalid PNG snapshot.','Invalid PNG image.','Could not create the figure folder.','No figures to save.','Unsupported format','Invalid PNG snapshot: %s','Could not decode PNG snapshot: %s','Invalid PNG snapshot: %s (encoded length %s)'],
'ko':['그림 폴더 접두사가 올바르지 않습니다.','저장할 그림 스냅샷이 없습니다.','그림 파일명이 올바르지 않습니다.','PNG 스냅샷이 올바르지 않습니다.','PNG 이미지가 올바르지 않습니다.','그림 폴더를 만들지 못했습니다.','저장할 그림이 없습니다.','지원하지 않는 형식입니다.','PNG 스냅샷이 올바르지 않습니다: %s','PNG 스냅샷을 디코딩하지 못했습니다: %s','PNG 스냅샷이 올바르지 않습니다: %s (인코딩 길이 %s)'],
'ja':['図のフォルダー接頭辞が無効です。','保存できる図のスナップショットがありません。','図のファイル名が無効です。','PNGスナップショットが無効です。','PNG画像が無効です。','図のフォルダーを作成できませんでした。','保存する図がありません。','サポートされていない形式です。','PNGスナップショットが無効です: %s','PNGスナップショットをデコードできませんでした: %s','PNGスナップショットが無効です: %s（エンコード長 %s）'],
'zh':['图形文件夹前缀无效。','没有可保存的图形快照。','图形文件名无效。','PNG 快照无效。','PNG 图像无效。','无法创建图形文件夹。','没有可保存的图形。','不支持的格式。','PNG 快照无效：%s','无法解码 PNG 快照：%s','PNG 快照无效：%s（编码长度 %s）'],
'es':['El prefijo de la carpeta de figuras no es válido.','No hay capturas de figuras disponibles.','El nombre del archivo de figura no es válido.','La captura PNG no es válida.','La imagen PNG no es válida.','No se pudo crear la carpeta de figuras.','No hay figuras para guardar.','Formato no compatible','Captura PNG no válida: %s','No se pudo decodificar la captura PNG: %s','Captura PNG no válida: %s (longitud codificada %s)'],
'fr':['Le préfixe du dossier des figures est invalide.','Aucune capture de figure n’est disponible.','Le nom du fichier de figure est invalide.','La capture PNG est invalide.','L’image PNG est invalide.','Impossible de créer le dossier des figures.','Aucune figure à enregistrer.','Format non pris en charge','Capture PNG invalide : %s','Impossible de décoder la capture PNG : %s','Capture PNG invalide : %s (longueur encodée %s)'],
'de':['Das Präfix des Abbildungsordners ist ungültig.','Es sind keine Abbildungsschnappschüsse verfügbar.','Der Dateiname der Abbildung ist ungültig.','Der PNG-Schnappschuss ist ungültig.','Das PNG-Bild ist ungültig.','Der Abbildungsordner konnte nicht erstellt werden.','Keine Abbildungen zum Speichern vorhanden.','Nicht unterstütztes Format','Ungültiger PNG-Schnappschuss: %s','PNG-Schnappschuss konnte nicht dekodiert werden: %s','Ungültiger PNG-Schnappschuss: %s (kodierte Länge %s)'],
'vi':['Tiền tố thư mục hình không hợp lệ.','Không có ảnh chụp hình để lưu.','Tên tệp hình không hợp lệ.','Ảnh chụp PNG không hợp lệ.','Hình ảnh PNG không hợp lệ.','Không thể tạo thư mục hình.','Không có hình để lưu.','Định dạng không được hỗ trợ','Ảnh chụp PNG không hợp lệ: %s','Không thể giải mã ảnh chụp PNG: %s','Ảnh chụp PNG không hợp lệ: %s (độ dài mã hóa %s)'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'result.figure_error.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
