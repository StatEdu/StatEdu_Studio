"""Expand generated style references and compare effective formatting to baseline."""
from copy import deepcopy
from pathlib import Path
from zipfile import ZipFile
import sys
import xml.etree.ElementTree as E

W = '{http://schemas.openxmlformats.org/wordprocessingml/2006/main}'

def norm(node):
    children = [norm(c) for c in node]
    # Independent property order differs when inherited values are expanded.
    if node.tag in (W+'pPr', W+'rPr'):
        children.sort(key=lambda c: c[0])
    return (node.tag, sorted(node.attrib.items()), node.text if node.text and node.text.strip() else '', children)

def verify(before, after):
    with ZipFile(before) as a, ZipFile(after) as b:
        old = E.fromstring(a.read('word/document.xml'))
        new = E.fromstring(b.read('word/document.xml'))
        old_styles = E.fromstring(a.read('word/styles.xml'))
        styles = E.fromstring(b.read('word/styles.xml'))
        shared = {n.attrib[W+'styleId']: n for n in styles if n.attrib.get(W+'styleId','').startswith('StatEduTable')}
        assert shared, 'Expected shared paragraph styles'
        references = 0
        for ppr in new.iter(W+'pPr'):
            reference = ppr.find(W+'pStyle')
            if reference is None or reference.attrib.get(W+'val') not in shared:
                continue
            definition = shared[reference.attrib[W+'val']]
            assert definition.attrib[W+'type'] == 'paragraph'
            reference.attrib[W+'val'] = definition.find(W+'basedOn').attrib[W+'val']
            inherited = definition.find(W+'pPr')
            assert inherited is not None
            direct = {n.tag for n in ppr}
            for child in inherited:
                if child.tag not in direct:
                    ppr.append(deepcopy(child))
            references += 1
        assert references > 0
        for rpr in new.iter(W+'rPr'):
            reference = rpr.find(W+'rStyle')
            if reference is None or reference.attrib.get(W+'val') not in shared:
                continue
            definition = shared[reference.attrib[W+'val']]
            assert definition.attrib[W+'type'] == 'character'
            rpr.remove(reference)
            direct = {n.tag for n in rpr}
            for child in definition.find(W+'rPr'):
                if child.tag not in direct:
                    rpr.append(deepcopy(child))
        assert norm(old) == norm(new), 'Document content or effective formatting changed'
        for definition in shared.values():
            styles.remove(definition)
        assert norm(old_styles) == norm(styles), 'Original styles changed'
        media = lambda z: sorted(z.read(n) for n in z.namelist() if n.startswith('word/media/'))
        assert media(a) == media(b)
        print('PASS',Path(after).name,len(shared),'shared styles;',references,'references; identical effective document and images')

if __name__ == '__main__':
    verify(*sys.argv[1:3])
