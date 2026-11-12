// Documentation screenshots against the verified local app; no scientific data changes.
const {chromium}=require('playwright');const path=require('path');
(async()=>{const b=await chromium.launch({executablePath:process.env.CHROMIUM_PATH||'/usr/bin/chromium',headless:true,args:['--no-sandbox']});try{
 const p=await b.newPage({viewport:{width:1700,height:1100}});await p.goto(process.env.STATLAB_URL||'http://127.0.0.1:3838');
 await p.waitForFunction(()=>document.querySelectorAll('#compare-plot .point').length>0,null,{timeout:60000});
 await p.getByRole('tab',{name:'Export / Projects',exact:true}).click();await p.locator('#export-load').setInputFiles(path.resolve('examples/advanced-21.statlab.json'));
 await p.waitForTimeout(1500);await p.getByRole('tab',{name:'Compare Datasets',exact:true}).click();
 await p.waitForFunction(()=>document.getElementById('compare-plot')?.layout?.title?.text?.includes('Liver response'),null,{timeout:60000});
 await p.evaluate(()=>{document.querySelector('.study-manager').open=false;document.querySelectorAll('.editor-category').forEach(e=>e.open=false);document.querySelector('.workspace-properties').scrollTop=0});
 await p.screenshot({path:path.resolve('artifacts/editor-hierarchy-21.png'),fullPage:true});
 await p.locator('.editor-category').filter({has:p.locator(':scope > summary',{hasText:'Titles & Labels'})}).first().evaluate(e=>e.open=true);
 await p.evaluate(()=>{const e=document.getElementById('graphs-title');for(let n=e.parentElement;n;n=n.parentElement)if(n.tagName==='DETAILS')n.open=true;e.scrollIntoView({block:'center'})});
 await p.screenshot({path:path.resolve('artifacts/editor-title-controls-21.png'),fullPage:true});
 console.log('PASS hierarchy and title-property screenshots captured');
 }finally{await b.close()}})().catch(e=>{console.error(e);process.exit(1)});
