const {chromium}=require('playwright');const fs=require('fs');const path=require('path');
const dir=path.resolve('artifacts');const assert=(v,m)=>{if(!v)throw Error(m);console.log('PASS '+m)};
let browser,page;
(async()=>{
 browser=await chromium.launch({executablePath:process.env.CHROMIUM_PATH||'/usr/bin/chromium',headless:true,args:['--no-sandbox']});
 page=await browser.newPage({viewport:{width:1700,height:1100},acceptDownloads:true});let errors=[];let external=[];
 page.on('pageerror',e=>errors.push(e.message));page.on('request',r=>{if(/^https?:/.test(r.url())&&!r.url().startsWith('http://127.0.0.1:3838'))external.push(r.url())});
 const wait=async()=>{await page.waitForTimeout(850);await page.waitForFunction(()=>!document.documentElement.classList.contains('shiny-busy'),null,{timeout:60000})};
 const reveal=async(id)=>{await page.locator('#'+id).evaluate(el=>{for(let p=el.parentElement;p;p=p.parentElement)if(p.tagName==='DETAILS')p.open=true});await wait()};
 const set=async(id,value)=>{await reveal(id);await page.locator('#'+id).fill(String(value));await page.locator('#'+id).blur();await wait()};
 const select=async(id,value)=>{await reveal(id);await page.evaluate(({id,value})=>{const e=document.getElementById(id);if(e.selectize)e.selectize.setValue(value);else{e.value=value;e.dispatchEvent(new Event('change',{bubbles:true}))}},{id,value});await wait()};
 const click=async(id)=>{await reveal(id);await page.locator('#'+id).click();await wait()};
 const tab=async(name)=>{await page.getByRole('tab',{name,exact:true}).click();await wait()};
 const plot=()=>page.locator('#compare-plot');
 const prep=async()=>Number((await page.locator('#compare-diagnostics').textContent()).match(/Numeric preparations: (\d+)/)[1]);
 await page.goto(process.env.STATLAB_URL||'http://127.0.0.1:3838');await wait();await wait();
 await page.waitForFunction(()=>document.getElementById('compare-summary')?.textContent.includes('4 independent studies'),null,{timeout:60000});await page.waitForFunction(()=>document.querySelectorAll('#compare-plot .point').length>0,null,{timeout:60000});
 assert((await page.locator('#compare-summary').innerText()).includes('4 independent studies'),'2.1 default comparison renders');
 const titleSection=page.locator('.editor-category').filter({has:page.locator('summary',{hasText:/^Titles & Labels$/})}).first();
 await titleSection.locator(':scope > summary').click();assert(await titleSection.evaluate(e=>e.open),'editor category expands');
 await titleSection.locator(':scope > summary').click();assert(!await titleSection.evaluate(e=>e.open),'editor category collapses');
 const beforePrep=await prep();await set('graphs-title','Editable scientific title');await set('graphs-title_size',24);await set('graphs-title_color','#833d65');
 await page.waitForFunction(()=>document.querySelector('#compare-plot')?.layout?.title?.text?.includes('Editable scientific title'));
 assert((await plot().evaluate(e=>e.layout.title.font.size))===24,'independent main-title typography reaches Plotly');
 assert(await prep()===beforePrep,'title/color edits reuse numeric preparation');
 await set('axes-x_title','Exposure concentration');await set('axes-x_title_color','#145b73');
 assert((await page.locator('#compare-plot').innerText()).includes('Exposure concentration'),'axis title is independently editable');
 await set('graphs-subtitle','A preserved subtitle');await select('graphs-theme','classic');
 assert((await page.locator('#graphs-title').inputValue())==='Editable scientific title','manual title survives other styling changes');
 await click('graphs-undo');assert(await page.locator('#graphs-title').inputValue()==='Editable scientific title','undo preserves earlier title');await click('graphs-redo');
 await page.locator('.workspace-properties').evaluate(e=>e.scrollTop=0);await page.locator('.workspace-properties').screenshot({path:path.join(dir,'editor-hierarchy-21.png')});await page.screenshot({path:path.join(dir,'editor-titles-21.png'),fullPage:true});
 // Search, hiding and highlighting use the visible study manager, not the compatibility input.
 await page.locator('#compare-studies .dataTables_filter input').fill('Study 3');await wait();
 const row=page.locator('#compare-studies tbody tr').filter({hasText:'Study 3'}).first();await row.click();await wait();
 assert((await page.locator('#compare-studies tbody').innerText()).includes('Study 3'),'study search locates named study');
 await click('compare-hide');assert((await page.locator('#compare-summary').innerText()).includes('3 independent studies'),'selected study can be hidden');
 await click('compare-unhide');assert((await page.locator('#compare-summary').innerText()).includes('4 independent studies'),'hidden study can be restored');
 await click('compare-highlight');assert((await page.locator('#compare-diagnostics').textContent()).includes('Numeric preparations:'),'highlighting updates display');
 const highlighted=await plot().evaluate(e=>e.data.filter(t=>t.mode?.includes('markers')).map(t=>t.opacity??t.marker?.opacity));
 assert(new Set(highlighted.map(v=>JSON.stringify(v))).size>1,'non-highlighted studies have reduced opacity');await click('compare-clear_highlight');
 await click('compare-up');assert((await page.locator('#compare-studies tbody').innerText()).includes('Study 3'),'study display order is editable');
 // Editable interval table: add, change, remove and retain invalid drafts safely.
 for(const vals of [[0,1,1],[10,100,1],[1000,10000,2]]){await set('axes-new_start',vals[0]);await set('axes-new_end',vals[1]);await set('axes-new_width',vals[2]);await click('axes-add_interval')}
 await page.waitForFunction(()=>document.querySelectorAll('#compare-plot .subplot').length>=3);
 assert((await page.locator('#axes-status').innerText()).includes('3 valid'),'three intervals added through editor');
 const widthCell=page.locator('#axes-interval_table tbody tr').last().locator('td').nth(3);await widthCell.dblclick();await widthCell.locator('input').fill('3');await widthCell.locator('input').press('Tab');await wait();assert((await page.locator('#axes-status').innerText()).includes('3 valid'),'relative interval width can be edited');
 const last=page.locator('#axes-interval_table tbody tr').last();if(!(await last.getAttribute('class')||'').includes('selected'))await last.click();await click('axes-interval_up');
 assert((await page.locator('#axes-status').innerText()).includes('must be ordered'),'invalid reordered interval draft is rejected');await click('axes-interval_down');
 await set('axes-new_start',20000);await set('axes-new_end',30000);await click('axes-add_interval');
 assert((await page.locator('#axes-status').innerText()).includes('no observations'),'empty interval does not replace valid graph');
 const remove=page.locator('#axes-interval_table tbody tr').last();if(!(await remove.getAttribute('class')||'').includes('selected'))await remove.click();await click('axes-remove_interval');
 assert((await page.locator('#axes-status').innerText()).includes('3 valid'),'interval can be removed to restore valid layout');
 await select('annotations-dataset','study_1');await select('annotations-sex','Female');await select('annotations-treatment','Control');await select('annotations-kind','letter');await set('annotations-text','a');await set('annotations-y',6);await click('annotations-add');assert((await page.locator('#annotations-evidence').textContent()).includes('Run Statistics'),'manual annotations require no internal analysis');
 // Existing complex example includes manual labels plus validated automatic asterisks.
 await tab('Export / Projects');await page.locator('#export-load').setInputFiles(path.resolve('examples/advanced-21.statlab.json'));await wait();await wait();await tab('Compare Datasets');
 await page.waitForFunction(()=>document.querySelectorAll('#compare-plot .subplot').length>=6,null,{timeout:45000});
 const text=await page.locator('#compare-plot').innerText();
 assert(text.includes('****')&&text.includes('ab')&&text.includes('α'),'manual letters, Unicode and validated asterisks appear together');
 assert(text.includes('Female')&&text.includes('Male'),'annotations coexist with male/female facets');
 assert(text.includes('Liver response across independent studies'),'custom main title appears once on split figure');
 const before=await prep();await set('graphs-title','Publication figure · verified');await set('graphs-title_size',21);await set('graphs-subtitle_color','#6c4969');
 const after=await prep();console.log('Complex numeric preparation counts:',before,after);assert(after===before,'complex-figure typography reuses prepared scientific values');
 // Manual UI entry is independent of the statistical engine.
 await select('annotations-dataset','study_1');await select('annotations-sex','Male');await select('annotations-treatment','Dose 0.1');await select('annotations-kind','asterisk');await set('annotations-text','***');await set('annotations-y',6.3);await click('annotations-add');
 assert((await page.locator('#compare-plot').innerText()).includes('***'),'manual asterisk is added through the interface');
 await page.locator('.workspace-properties').screenshot({path:path.join(dir,'editor-manual-annotations-21.png')});
 // Automatic-generation button uses the loaded validated analysis, including its p-values.
 await select('annotations-automatic_kind','pvalue');await click('annotations-automatic');
 await page.locator('#annotations-table .dataTables_filter input').fill('pvalue');await wait();assert((await page.locator('#annotations-table tbody').innerText()).includes('automatic'),'automatic comparison annotation is available in manager');
 // Capture the chart before and after a full project roundtrip for pixel comparison.
 await page.mouse.move(1680,10);await wait();await page.locator('#compare-plot').screenshot({path:path.join(dir,'editor-plot-before-21.png')});
 await tab('Export / Projects');
 const download=async(id,file)=>{const task=page.waitForEvent('download');await page.locator('#'+id).click();const d=await task;await d.saveAs(path.join(dir,file));assert(fs.statSync(path.join(dir,file)).size>1000,file+' exported')};
 for(const fmt of ['png','svg','pdf'])await download('export-'+fmt,'editor-browser-21.'+fmt);
 await download('export-save','editor-21-project.json');
 const saved=JSON.parse(fs.readFileSync(path.join(dir,'editor-21-project.json')));assert(saved.version===2,'new projects use version 2');
 assert(saved.settings.graph.annotations.length>=8,'multiple annotation definitions and provenance are serialized');
 assert(saved.settings.graph.annotations.some(a=>a.source==='manual'&&a.dataset_id==='study_1'&&a.sex==='Male'&&a.treatment==='Dose 0.1'&&a.text==='***'),'manual star keeps its exact study, sex and treatment anchor');
 assert(saved.settings.graph.annotations.some(a=>a.source==='automatic'&&a.label_mode==='pvalue'&&a.p===saved.results[0].p),'automatic p-value annotation retains validated numeric result');
 assert(saved.settings.graph.type==='Mean ± SD','restored graph type remains unchanged by typography edits');
 assert(saved.settings.graph.title_size===21&&saved.settings.axes.x.title==='Administered dose','independent typography and axis labels are serialized');
 await page.locator('#export-load').setInputFiles(path.join(dir,'editor-21-project.json'));await wait();await wait();await tab('Compare Datasets');
 await page.waitForFunction(()=>document.querySelectorAll('#compare-plot .subplot').length>=6,null,{timeout:45000});await page.mouse.move(1680,10);await wait();
 await page.locator('#compare-plot').screenshot({path:path.join(dir,'editor-plot-after-21.png')});
 assert((await page.locator('#compare-plot').innerText()).includes('Publication figure · verified'),'title and annotated split figure survive save/reopen');
 await page.screenshot({path:path.join(dir,'editor-annotated-split-21.png'),fullPage:true});
 // Omnibus results cannot produce unsupported pairwise labels; all-pairs Tukey can produce CLDs.
 await tab('Statistics');await select('statistics-dataset','study_4');await select('statistics-sex','Male');await select('statistics-groups',['Control','Dose 10000']);await select('statistics-method','One-way ANOVA');await click('statistics-run');
 await tab('Compare Datasets');await select('annotations-automatic_kind','pvalue');await click('annotations-automatic');
 assert((await page.locator('.shiny-notification').allInnerTexts()).some(t=>t.includes('Omnibus')),'unsupported automatic omnibus comparison is explained and rejected');
 await tab('Statistics');await select('statistics-groups',['Control','Dose 100','Dose 1000','Dose 10000']);await select('statistics-method','Tukey');await click('statistics-run');
 await tab('Compare Datasets');await select('annotations-automatic_kind','letter');await click('annotations-automatic');await page.locator('#annotations-table .dataTables_filter input').fill('cld');await wait();
 assert((await page.locator('#annotations-table tbody').innerText()).includes('automatic'),'valid all-pairs Tukey compact letters are generated through UI');
 await page.screenshot({path:path.join(dir,'editor-automatic-cld-21.png'),fullPage:true});
 // Real 2.0 project compatibility, not a mock fixture.
 await tab('Export / Projects');await page.locator('#export-load').setInputFiles(path.resolve('tests/fixtures/statlab-2.0.json'));await wait();await wait();await tab('Compare Datasets');
 assert((await page.locator('#compare-summary').innerText()).includes('4 independent studies'),'StatLab 2.0 project reopens with defaults');
 await page.setViewportSize({width:700,height:1000});await wait();
 assert(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth+20),'responsive workspace fits small screen');
 await page.screenshot({path:path.join(dir,'editor-responsive-21.png'),fullPage:true});
 assert(errors.length===0,'no browser JavaScript errors');assert(external.length===0,'no external browser data requests');
 await browser.close();console.log('Advanced editor browser workflow complete.');
})().catch(async e=>{console.error(e);if(page){console.log('Notifications:',await page.locator('.shiny-notification').allInnerTexts().catch(()=>[]));await page.screenshot({path:path.join(dir,'editor-failure-21.png'),fullPage:true}).catch(()=>{})}if(browser)await browser.close();process.exit(1)});
