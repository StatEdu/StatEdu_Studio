import json
from pathlib import Path

# The seven other catalogs already contain these shared GLM/IPW translations.
path = Path('i18n/ko.json')
obj = json.loads(path.read_text(encoding='utf-8'))
obj['translations'].update({
    'analysis.ui.observation_status_had_no_variation_unit_weights_were_used': '관측 상태에 변이가 없어 단위 가중치를 사용했습니다.',
    'analysis.ui.no_fully_observed_predictors_were_available_for_the_observation_model_intercept_only_ipw_was_used_treat_this_as_a_weak_ipw_sensitivity_analysis': '관측모형에 사용할 완전히 관측된 예측변수가 없어 절편만 포함한 IPW를 사용했습니다. 제한적인 IPW 민감도 분석으로 해석하십시오.',
})
path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
