# 평가자 간 일치도 대표 결과 다국어 보완

보조 일치도 지수 제목, 평가자 수, 일치율, 명목형 지수 설명과 정규성 기준 안내 등 10개 문구를 8개 언어 사전에 등록했다. 계산 엔진은 변경하지 않았다.

## 검증

- `scripts/validate_interrater_i18n.R`: 실제 100행 자료에서 ICC와 2명/3명 평가자의 명목형 일치도를 실행했다. 8개 언어의 본표 셀·제목·주석 영어 동일성, 보조표 언어와 표시된 번역 문구, 평가자 라벨 `Normality`, `Yes`, `사용자 평가자` 보존 검사 통과.
- `Complete paired ratings.`, `Requires the same number of ratings per subject.`는 이 자료에서 권장 본표에 속하므로 영어 유지가 맞다. 번역 사전에는 등록했지만 이 문구들이 보조표로 이동하는 실제 분기는 이번에 재현하지 않았다.
- 기존 `validate_interrater.R` 및 `validate_i18n_contract.R` 통과.
- 일본어 실제 세 결과 저장 fixture: `tmp/interrater-i18n/entries.rds`. 내보내기 로그: `tmp/interrater-i18n-exports.log`.
- 현재·누적 HTML/Word/HWPX/Excel 내용 비교 및 PDF 생성 통과. 실제 PDF 텍스트 검사도 두 경로 모두 통과.

## 남은 범위

순서형 가중치, ICC 부트스트랩, 결측·범주 불균형에 따른 추천 변경, 옵션·입력 오류·언어 왕복을 추가 확인해야 한다. 대표 결과 근거는 16개 화면, 전용 결과 검증 공백은 14개 화면으로 갱신했다. 전체 완료 판정은 하지 않았으며 설치본은 만들지 않았다.
