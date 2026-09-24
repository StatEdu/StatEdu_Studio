import json
from pathlib import Path
keys=['no_content','no_tables','excel_package','browser','pdf_path','office_browser','pdf_failed']
rows={
'en':['No displayed result tables or figures are available to export.','No displayed result tables are available to export.','Could not finalize Excel package.','Chrome or Edge was not found. Install Chrome/Edge or set STATEDU_CHROME.','The generated PDF could not be saved to the selected path.','Chrome or Edge is required to export model diagrams to Office.','PDF export failed.'],
'ko':['내보낼 결과 표나 그림이 없습니다.','내보낼 결과 표가 없습니다.','Excel 파일 구성을 완료하지 못했습니다.','Chrome 또는 Edge를 찾지 못했습니다. Chrome/Edge를 설치하거나 STATEDU_CHROME을 설정하세요.','생성된 PDF를 선택한 경로에 저장하지 못했습니다.','모형 도표를 Office로 내보내려면 Chrome 또는 Edge가 필요합니다.','PDF 내보내기에 실패했습니다.'],
'ja':['エクスポートできる結果の表や図がありません。','エクスポートできる結果の表がありません。','Excelファイルの作成を完了できませんでした。','ChromeまたはEdgeが見つかりません。Chrome/Edgeをインストールするか、STATEDU_CHROMEを設定してください。','生成されたPDFを選択したパスに保存できませんでした。','モデル図をOfficeにエクスポートするにはChromeまたはEdgeが必要です。','PDFのエクスポートに失敗しました。'],
'zh':['没有可导出的结果表格或图形。','没有可导出的结果表格。','无法完成 Excel 文件打包。','未找到 Chrome 或 Edge。请安装 Chrome/Edge 或设置 STATEDU_CHROME。','无法将生成的 PDF 保存到所选路径。','将模型图导出到 Office 需要 Chrome 或 Edge。','PDF 导出失败。'],
'es':['No hay tablas ni figuras de resultados disponibles para exportar.','No hay tablas de resultados disponibles para exportar.','No se pudo finalizar el paquete de Excel.','No se encontró Chrome ni Edge. Instale Chrome/Edge o configure STATEDU_CHROME.','No se pudo guardar el PDF generado en la ruta seleccionada.','Se requiere Chrome o Edge para exportar diagramas de modelos a Office.','Error al exportar el PDF.'],
'fr':['Aucun tableau ni graphique de résultats n’est disponible pour l’exportation.','Aucun tableau de résultats n’est disponible pour l’exportation.','Impossible de finaliser le package Excel.','Chrome ou Edge est introuvable. Installez Chrome/Edge ou définissez STATEDU_CHROME.','Impossible d’enregistrer le PDF généré à l’emplacement sélectionné.','Chrome ou Edge est nécessaire pour exporter les diagrammes de modèles vers Office.','L’exportation PDF a échoué.'],
'de':['Es sind keine Ergebnistabellen oder Abbildungen zum Exportieren verfügbar.','Es sind keine Ergebnistabellen zum Exportieren verfügbar.','Das Excel-Paket konnte nicht fertiggestellt werden.','Chrome oder Edge wurde nicht gefunden. Installieren Sie Chrome/Edge oder setzen Sie STATEDU_CHROME.','Die erzeugte PDF-Datei konnte nicht am ausgewählten Pfad gespeichert werden.','Zum Exportieren von Modelldiagrammen nach Office ist Chrome oder Edge erforderlich.','Der PDF-Export ist fehlgeschlagen.'],
'vi':['Không có bảng hoặc hình kết quả để xuất.','Không có bảng kết quả để xuất.','Không thể hoàn tất gói tệp Excel.','Không tìm thấy Chrome hoặc Edge. Hãy cài đặt Chrome/Edge hoặc đặt STATEDU_CHROME.','Không thể lưu PDF đã tạo vào đường dẫn đã chọn.','Cần Chrome hoặc Edge để xuất sơ đồ mô hình sang Office.','Xuất PDF thất bại.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'result.export_error.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
