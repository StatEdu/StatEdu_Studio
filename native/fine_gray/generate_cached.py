from pathlib import Path
import re
root=Path(__file__).resolve().parent / 'src'
source=(root/'crr.f').read_text(encoding='utf-8')

start=source.index('      subroutine crrvv(')
end=source.index('      subroutine crrsr(',start)
block=source[start:end].replace('subroutine crrvv(', 'subroutine crrvvc(',1)
marker='      integer lc,icrsk(ncg),j1,j2,ldf2,iflg'
assert block.count(marker)==1
addition='''
      integer icache
      double precision expcache(n)
      if (ncov2.ne.0) then
         call crrvv(t2,ici,n,x,ncov,np,x2,ncov2,tf,ndf,wt,ncg,
     $        icg,b,v,v2,vt,xb,xbt,qu,st,ss2,icrsk,ss3,ss4)
         return
      endif
      do 900 icache=1,n
         call covt(icache,1,ncov,x,n,ncov2,x2,tf,ndf,b,wk,xbt)
         expcache(icache)=exp(wk)
 900  continue'''
# Change original inner calls before inserting the one-time cache population.
pattern=r'call covt\((\w+),[^\n]+\)'
calls=list(re.finditer(pattern,block));assert len(calls)==8,len(calls)
lines=block.splitlines();row=None;replaced_exp=0
for i,line in enumerate(lines):
    m=re.search(pattern,line)
    if m:
        row=m.group(1)
        lines[i]=line[:m.start()]+f'call covt_static({row},ncov,x,n,xbt)'
    elif 'exp(wk)' in line:
        assert row is not None
        lines[i]=line.replace('exp(wk)',f'expcache({row})');replaced_exp+=1
assert replaced_exp==7,replaced_exp
block='\n'.join(lines)+'\n'
block=block.replace(marker,marker+addition)
helper='''
      subroutine covt_static(j,ncov,x,n,xbt)
      integer j,ncov,n,k
      double precision x(n,ncov),xbt(ncov)
      do 901 k=1,ncov
         xbt(k)=x(j,k)
 901  continue
      return
      end
'''
candidate=source+'\n'+block+helper
for line in candidate.splitlines():
    if line and line[0] not in 'cC*' and len(line.rstrip())>72:
        raise AssertionError(line)
(root/'crr_cached.f').write_text(candidate, encoding='utf-8', newline='\n')
print('Created original and isolated cached source; 8 inner covt calls, 7 exp sites replaced; time-varying path falls back.')
