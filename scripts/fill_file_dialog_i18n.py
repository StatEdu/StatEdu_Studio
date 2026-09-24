import json
from pathlib import Path

keys = ['open_settings','save_settings','settings_files','open_data','data_files','all_files']
rows = {
 'en':['Open StatEdu Studio Settings','Save StatEdu Studio Settings','StatEdu Studio Settings','Open StatEdu Studio Data','Data files','All files'],
 'ko':['StatEdu Studio 설정 열기','StatEdu Studio 설정 저장','StatEdu Studio 설정 파일','StatEdu Studio 데이터 열기','데이터 파일','모든 파일'],
 'ja':['StatEdu Studioの設定を開く','StatEdu Studioの設定を保存','StatEdu Studio設定ファイル','StatEdu Studioのデータを開く','データファイル','すべてのファイル'],
 'zh':['打开 StatEdu Studio 设置','保存 StatEdu Studio 设置','StatEdu Studio 设置文件','打开 StatEdu Studio 数据','数据文件','所有文件'],
 'es':['Abrir configuración de StatEdu Studio','Guardar configuración de StatEdu Studio','Archivos de configuración de StatEdu Studio','Abrir datos de StatEdu Studio','Archivos de datos','Todos los archivos'],
 'fr':['Ouvrir les paramètres de StatEdu Studio','Enregistrer les paramètres de StatEdu Studio','Fichiers de paramètres de StatEdu Studio','Ouvrir les données de StatEdu Studio','Fichiers de données','Tous les fichiers'],
 'de':['StatEdu Studio-Einstellungen öffnen','StatEdu Studio-Einstellungen speichern','StatEdu Studio-Einstellungsdateien','StatEdu Studio-Daten öffnen','Datendateien','Alle Dateien'],
 'vi':['Mở cài đặt StatEdu Studio','Lưu cài đặt StatEdu Studio','Tệp cài đặt StatEdu Studio','Mở dữ liệu StatEdu Studio','Tệp dữ liệu','Tất cả tệp'],
}
extra_keys=['open_design','save_design','design_files','choose_default_folder']
extra_rows={
 'en':['Open StatEdu Complex Sample Design','Save StatEdu Complex Sample Design','StatEdu Complex Sample Design','Choose default file save location'],
 'ko':['StatEdu 복합표본 설계 열기','StatEdu 복합표본 설계 저장','StatEdu 복합표본 설계 파일','기본 파일 저장 위치 선택'],
 'ja':['StatEduの複雑標本設計を開く','StatEduの複雑標本設計を保存','StatEdu複雑標本設計ファイル','既定のファイル保存先を選択'],
 'zh':['打开 StatEdu 复杂抽样设计','保存 StatEdu 复杂抽样设计','StatEdu 复杂抽样设计文件','选择默认文件保存位置'],
 'es':['Abrir diseño de muestreo complejo de StatEdu','Guardar diseño de muestreo complejo de StatEdu','Diseño de muestreo complejo de StatEdu','Elegir la ubicación predeterminada para guardar archivos'],
 'fr':['Ouvrir le plan de sondage complexe de StatEdu','Enregistrer le plan de sondage complexe de StatEdu','Plan de sondage complexe de StatEdu',"Choisir l’emplacement par défaut pour enregistrer les fichiers"],
 'de':['StatEdu-Design für komplexe Stichproben öffnen','StatEdu-Design für komplexe Stichproben speichern','StatEdu-Design für komplexe Stichproben','Standardspeicherort für Dateien auswählen'],
 'vi':['Mở thiết kế mẫu phức tạp StatEdu','Lưu thiết kế mẫu phức tạp StatEdu','Thiết kế mẫu phức tạp StatEdu','Chọn vị trí lưu tệp mặc định'],
}
for lang, values in rows.items():
 path=Path('i18n')/(lang+'.json')
 obj=json.loads(path.read_text(encoding='utf-8'))
 obj['translations'].update({'file_dialog.'+key:value for key,value in zip(keys,values)})
 obj['translations'].update({'file_dialog.'+key:value for key,value in zip(extra_keys,extra_rows[lang])})
 path.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
