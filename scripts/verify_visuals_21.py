"""Compare project-roundtrip images and validate complex vector exports (Pillow required)."""
from pathlib import Path
from PIL import Image, ImageChops, ImageStat
import json
import subprocess
import xml.etree.ElementTree as ET

root = Path(__file__).resolve().parents[1]
a = root / 'artifacts'
checks = []
def check(condition, description, **details):
    checks.append(dict(check=description, passed=bool(condition), **details))
    print(('PASS ' if condition else 'FAIL ') + description)

def pixels(before, after, exact):
    one, two = Image.open(a / before).convert('RGB'), Image.open(a / after).convert('RGB')
    if one.size != two.size:
        check(False, before + ' roundtrip dimensions', before=one.size, after=two.size)
        return
    diff = ImageChops.difference(one, two)
    rms = sum(x*x for x in ImageStat.Stat(diff).rms) ** .5 / (3 ** .5)
    mask = ImageChops.lighter(ImageChops.lighter(diff.getchannel('R'), diff.getchannel('G')), diff.getchannel('B'))
    changed = 1 - mask.histogram()[0] / (one.width * one.height)
    check(diff.getbbox() is None if exact else rms < 2 and changed < .02,
          before + ' project roundtrip pixels', exact=exact, rms=rms, changed_fraction=changed,
          dimensions=one.size)

pixels('advanced-21.png', 'advanced-21-restored.png', True)
pixels('editor-plot-before-21.png', 'editor-plot-after-21.png', False)
for stem, title in [('advanced-21', 'Liver response across independent studies'),
                    ('editor-browser-21', 'Publication figure · verified')]:
    tree = ET.parse(a / (stem + '.svg'))
    text = '\n'.join(''.join(e.itertext()) for e in tree.iter() if e.tag.endswith('}text'))
    pdf = subprocess.check_output(['pdftotext', str(a / (stem + '.pdf')), '-'], text=True)
    for fmt, content in [('SVG', text), ('PDF', pdf)]:
        check(content.count(title) == 1, stem + ' ' + fmt + ' single customized title')
        check(all(t in content for t in ['Female', 'Male', 'ab', 'α', '****']),
              stem + ' ' + fmt + ' facets, letters, Unicode and asterisks')
        check('Administered dose' in content and 'Relative liver weight' in content,
              stem + ' ' + fmt + ' customized axis titles')
        check('1000' in content and '10000' in content and '10' in content,
              stem + ' ' + fmt + ' split-panel range labels')
    check((a / (stem + '.png')).stat().st_size > 10000, stem + ' PNG rendered')
(a / 'visual-regression-21.json').write_text(json.dumps(checks, indent=2) + '\n')
if not all(c['passed'] for c in checks):
    raise SystemExit(1)
