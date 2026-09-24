"""Exact log-link rate/mean effect descriptions; shared dictionary owner applies."""
import json
from pathlib import Path

rows = {
 'en': ['Incidence rate ratio = exp(beta); log incidence rate ratio = beta from a log-link regression model.', 'Mean ratio = exp(beta); log mean ratio = beta from a log-link regression model.'],
 'ko': ['발생률비 = exp(beta); 로그 연결함수 회귀모형에서 log(발생률비) = beta입니다.', '평균비 = exp(beta); 로그 연결함수 회귀모형에서 log(평균비) = beta입니다.'],
 'ja': ['発生率比 = exp(beta); 対数リンク回帰モデルではlog(発生率比) = betaです。', '平均比 = exp(beta); 対数リンク回帰モデルではlog(平均比) = betaです。'],
 'zh': ['发生率比 = exp(beta); 在对数链接回归模型中，log(发生率比) = beta。', '均值比 = exp(beta); 在对数链接回归模型中，log(均值比) = beta。'],
 'es': ['Razón de tasas de incidencia = exp(beta); en un modelo de regresión con enlace logarítmico, log(razón de tasas de incidencia) = beta.', 'Razón de medias = exp(beta); en un modelo de regresión con enlace logarítmico, log(razón de medias) = beta.'],
 'fr': ['Rapport des taux d’incidence = exp(beta); dans un modèle de régression à lien logarithmique, log(rapport des taux d’incidence) = beta.', 'Rapport des moyennes = exp(beta); dans un modèle de régression à lien logarithmique, log(rapport des moyennes) = beta.'],
 'de': ['Inzidenzratenverhältnis = exp(beta); in einem Regressionsmodell mit Log-Link gilt log(Inzidenzratenverhältnis) = beta.', 'Mittelwertverhältnis = exp(beta); in einem Regressionsmodell mit Log-Link gilt log(Mittelwertverhältnis) = beta.'],
 'vi': ['Tỷ số tỷ suất mới mắc = exp(beta); trong mô hình hồi quy với hàm liên kết log, log(tỷ số tỷ suất mới mắc) = beta.', 'Tỷ số trung bình = exp(beta); trong mô hình hồi quy với hàm liên kết log, log(tỷ số trung bình) = beta.'],
}
for lang, values in rows.items():
 path = Path('i18n') / (lang + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update(dict(zip(['sample_size.result.note_loglink_irr', 'sample_size.result.note_loglink_mean_ratio'], values)))
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
