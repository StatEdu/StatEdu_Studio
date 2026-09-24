const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
const context={window:{}};vm.createContext(context);
vm.runInContext(fs.readFileSync('www/model-canvas/state.js','utf8'),context);
// Expose the existing private handler only inside this test VM.
vm.runInContext(fs.readFileSync('www/model-canvas/toolbar.js','utf8').replace('bind: bind,','bind: bind, testHandleAction: handleAction,'),context);
const toolbar=context.window.StatEduModelCanvas.toolbar;
for(const lang of ['en','ko','ja','zh','es','fr','de','vi']){
 const i18n=JSON.parse(fs.readFileSync(`tmp/structural-canvas-shell-i18n/${lang}.json`,'utf8'));
 const mode={textContent:''},validation={textContent:'',classList:{toggle(name,value){this[name]=value;}}};
 const paper={textContent:'',setAttribute(key,value){this[key]=value;}};
 const instance={language:lang,analysisType:'cfa',i18n,state:{mode:'select',selectedNodeIds:[],canvas:{paper:'A4',orientation:'landscape'}},validation:{errors:[1,2],warnings:[1,2,3]},root:{querySelector(s){return s==='.custom-model-mode-status'?mode:s==='.structural-validation-status'?validation:s==='.custom-model-paper-status'?paper:null;}}};
 for(const [value,key] of [['addObserved','mode_add_observed'],['addLatent','mode_add_latent'],['addHigherOrderLatent','mode_add_higher_order'],['covariance','mode_covariance']]){
  instance.state.mode=value;toolbar.updateStatus(instance);assert.equal(mode.textContent,i18n[key]);
 }
 instance.state.selectedNodeIds=['user1','user2'];toolbar.updateStatus(instance);
 assert.equal(mode.textContent,i18n.mode_covariance+' · '+i18n.selected_count.replace('{count}',2));
 assert.equal(paper.title,i18n.paper_change_title);assert.equal(paper['aria-label'],i18n.paper_change_aria);
 assert.equal(paper.textContent,'A4 landscape');
 toolbar.testHandleAction(instance,'validationOnly',null);
 assert.equal(validation.textContent,i18n.validation_counts.replace('{errors}',2).replace('{warnings}',3));
 assert.equal(validation.classList['has-errors'],true);
 instance.validation={errors:[],warnings:[]};toolbar.testHandleAction(instance,'validationOnly',null);
 assert.equal(validation.textContent,i18n.validation_counts.replace('{errors}',0).replace('{warnings}',0));
 assert.equal(validation.classList['has-errors'],false);
 console.log('PASS:',lang,'actual JS mode/status handlers, counts and error styling');
}
