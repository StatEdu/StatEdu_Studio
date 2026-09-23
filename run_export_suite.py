from pathlib import Path
import json,subprocess,concurrent.futures,time,hashlib
root=Path(__file__).resolve().parent;p=root/'outputs/spss_phase41_20260907'
def snapshot():
 return {str(f.relative_to(root)):hashlib.sha256(f.read_bytes()).hexdigest() for f in list((root/'R').glob('*.R'))+[root/'www/style.css',root/'VERSION']}
(p/'source_hashes.json').write_text(json.dumps(snapshot(),indent=2))
items=json.loads((p/'suite.json').read_text());status=[]
def run(item):
 start=time.time();folder=root/item['output']
 with (folder/'generation.log').open('w',encoding='utf8') as log:
  gen=subprocess.run([str(root/'packaging/electron/runtime/R-4.5.3/bin/Rscript.exe'),item['script']],cwd=root,stdout=log,stderr=subprocess.STDOUT)
 check=None
 if gen.returncode==0:
  with (folder/'file_checks.log').open('w',encoding='utf8') as log:check=subprocess.run(['python',str(p/'check_files.py'),item['output']],cwd=root,stdout=log,stderr=subprocess.STDOUT).returncode
 return dict(**item,generation_exit=gen.returncode,check_exit=check,seconds=round(time.time()-start,1))
with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
 for done in concurrent.futures.as_completed([pool.submit(run,i) for i in items]):
  s=done.result();status.append(s);(p/'suite_status.json').write_text(json.dumps(sorted(status,key=lambda x:x['phase']),indent=2));print(s['family'],s['generation_exit'],s['check_exit'],s['seconds'],flush=True)
(p/'source_unchanged.json').write_text(json.dumps({'unchanged':snapshot()==json.loads((p/'source_hashes.json').read_text())}))
