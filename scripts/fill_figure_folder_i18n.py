import json
from pathlib import Path
rows={
 'en':'Select a folder to save StatEdu Studio figures',
 'ko':'그림 저장 위치 선택',
 'ja':'StatEdu Studioの図を保存するフォルダーを選択',
 'zh':'选择保存 StatEdu Studio 图形的文件夹',
 'es':'Seleccione una carpeta para guardar las figuras de StatEdu Studio',
 'fr':'Sélectionnez un dossier pour enregistrer les figures de StatEdu Studio',
 'de':'Ordner zum Speichern der StatEdu Studio-Abbildungen auswählen',
 'vi':'Chọn thư mục để lưu hình StatEdu Studio',
}
for lang,value in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations']['file_dialog.figure_folder']=value
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
