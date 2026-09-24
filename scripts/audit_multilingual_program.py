"""Read-only product audit: preserve three-day evidence and rerun representative contracts."""
import concurrent.futures
import csv
import json
import os
from pathlib import Path
import subprocess
import time

root = Path(__file__).resolve().parent.parent
os.chdir(root)
out = root / 'tmp/multilingual-program-audit'
out.mkdir(parents=True, exist_ok=True)
reports = []
for date in ('20260916','20260917','20260918'):
    for path in sorted((root/'docs').glob('*MULTILINGUAL*'+date+'*.md')):
        text = path.read_text(encoding='utf-8-sig')
        reports.append({'date':date,'file':path.relative_to(root).as_posix(),'title':next((s.lstrip('# ') for s in text.splitlines() if s.startswith('# ')),path.stem),'text':text})
(out/'three-day-evidence.json').write_text(json.dumps(reports,ensure_ascii=False,indent=2),encoding='utf-8')
tests = '''validate_i18n_contract.R
validate_multilingual_coverage.R
validate_multilingual_rendering.R
validate_multilingual_table_roles.R
validate_file_dialog_i18n.R
validate_data_csv_dialog_i18n.R
validate_preferences_browse_i18n.R
validate_history_dialog_i18n.R
validate_history_errors_i18n.R
validate_merge_ui_i18n.R
validate_merge_errors_i18n.R
validate_id_aggregate_ui_i18n.R
validate_id_aggregate_errors_i18n.R
validate_wide_long_status_i18n.R
validate_wide_long_errors_i18n.R
validate_current_export_errors_i18n.R
validate_collection_export_errors_i18n.R
validate_figure_errors_i18n.R
validate_structural_options_languages.R
validate_structural_reporting_settings_i18n.R
validate_structural_toolbar_i18n.R
validate_structural_multigroup_ui_i18n.R
validate_glm_options_multilingual.R
validate_correlation_i18n.R
validate_factor_i18n.R
validate_complex_design_i18n.R
validate_complex_regression_i18n.R
validate_survival_cox_overview_i18n.R
validate_survival_adjusted_overview_i18n.R
validate_longitudinal_model_names_i18n.R'''.splitlines()
env = os.environ.copy()
env.update(LANG='Korean_Korea.utf8',LC_ALL='Korean_Korea.utf8',STATEDU_MODULE_CACHE='false',STATEDU_NO_PACKAGE_INSTALL='true')
rscript = root/'packaging/electron/runtime/R-4.5.3/bin/Rscript.exe'
def run(name):
    started = time.monotonic()
    try:
        p = subprocess.run([str(rscript),str(root/'scripts'/name)],cwd=root,env=env,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=240)
        code, output = p.returncode,p.stdout
    except subprocess.TimeoutExpired as e:
        code, output = 124,e.stdout or b''
    (out/(name+'.log')).write_bytes(output)
    result = {'test':name,'exit_code':code,'seconds':round(time.monotonic()-started,1),'log':name+'.log'}
    print(json.dumps(result),flush=True)
    return result
results=[]
with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
    for result in pool.map(run,tests):
        results.append(result)
        (out/'results.json').write_text(json.dumps(results,indent=2),encoding='utf-8')
print(json.dumps({'reports':len(reports),'tests':len(results),'passed':sum(r['exit_code']==0 for r in results)}),flush=True)
