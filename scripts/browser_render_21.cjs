// Focused final split/facet title regression, downloads and pixel-roundtrip captures.
const {chromium}=require('playwright');const fs=require('fs');const path=require('path');
(async()=>{const b=await chromium.launch({executablePath:process.env.CHROMIUM_PATH||'/usr/bin/chromium',headless:true,args:['--no-sandbox']});try{
 const p=await b.newPage({viewport:{width:1700,height:1100},acceptDownloads:true});const errors=[];p.on('pageerror',e=>errors.push(e.message));
 const check=(ok,name)=>{if(!ok)throw Error(name);console.log('PASS '+name)};
 const ready=async()=>{await p.waitForFunction(()=>document.getElementById('compare-plot')?.layout?.title?.text?.includes('Publication figure'),null,{timeout:60000});await p.waitForFunction(()=>!document.documentElement.classList.contains('shiny-busy'));await p.mouse.move(1680,10);await p.waitForTimeout(2500);await p.waitForFunction(()=>{const e=document.getElementById('compare-plot');return e&&!e.classList.contains('recalculating')&&Number(getComputedStyle(e).opacity)>.99&&!document.documentElement.classList.contains('shiny-busy')},null,{timeout:60000});await p.waitForTimeout(600)};
 const tab=async(name)=>{await p.getByRole('tab',{name,exact:true}).click()};
 await p.goto(process.env.STATLAB_URL||'http://127.0.0.1:3838');await p.waitForFunction(()=>document.querySelectorAll('#compare-plot .point').length>0,null,{timeout:60000});
 await tab('Export / Projects');await p.locator('#export-load').setInputFiles(path.resolve('artifacts/editor-21-project.json'));await p.waitForTimeout(1200);await tab('Compare Datasets');await ready();
 const labels=await p.locator('#compare-plot').evaluate(e=>e.layout.annotations);
 check(labels.filter(x=>x.text?.includes('Administered dose')).length===1,'split/facet preview has one shared customized X title');
 check(labels.filter(x=>x.text?.includes('Relative liver weight')).length===1,'split/facet preview has one shared customized Y title');
 check(labels.find(x=>x.text?.includes('Administered dose')).font.color==='#145b73','split/facet axis-title color preserved');
 check((await p.locator('#compare-plot').innerText()).includes('****'),'split/facet significance labels remain present');
 await p.locator('#compare-plot').screenshot({path:path.resolve('artifacts/editor-plot-before-21.png')});await p.screenshot({path:path.resolve('artifacts/editor-annotated-split-21.png'),fullPage:true});
 await tab('Export / Projects');for(const ext of ['png','svg','pdf']){const task=p.waitForEvent('download');await p.locator('#export-'+ext).click();await(await task).saveAs(path.resolve('artifacts/editor-browser-21.'+ext));check(fs.statSync('artifacts/editor-browser-21.'+ext).size>1000,ext.toUpperCase()+' final annotated export')}
 const task=p.waitForEvent('download');await p.locator('#export-save').click();await(await task).saveAs(path.resolve('artifacts/editor-render-roundtrip-21.json'));
 await p.locator('#export-load').setInputFiles(path.resolve('artifacts/editor-render-roundtrip-21.json'));await p.waitForTimeout(1200);await tab('Compare Datasets');await ready();
 await p.locator('#compare-plot').screenshot({path:path.resolve('artifacts/editor-plot-after-21.png')});check((await p.locator('#compare-plot').innerText()).includes('Administered dose'),'shared axis titles survive reopening');
 check(errors.length===0,'final split/facet render has no JavaScript errors');
 }finally{await b.close()}})().catch(e=>{console.error(e);process.exit(1)});
