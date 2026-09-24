"""Inspect the actual PDF and HWPX outputs of the publication-style fixtures."""
from pathlib import Path
from zipfile import ZipFile
import json
import logging
import re
from lxml import etree
import pdfplumber
logging.getLogger("pdfminer.pdffont").setLevel(logging.ERROR)
root = Path("tmp/regression-publication-style")
def normalize(text):
    return re.sub(r"\s+", "", text)
for mode in ("current", "accumulated"):
    expected = json.loads((root / f"{mode}-expected.json").read_text(encoding="utf-8"))
    with pdfplumber.open(root / f"{mode}.pdf") as pdf:
        assert "StatEdu" in (pdf.pages[0].extract_text() or ""), "PDF cover is missing"
        assert "Variable" not in (pdf.pages[0].extract_text() or ""), "Results overlap the cover"
        # PDF content order preserves wrapped cells; geometric line order interleaves adjacent columns.
        text = normalize("".join(page.extract_text(use_text_flow=True) or "" for page in pdf.pages))
        for value in expected:
            assert normalize(value) in text, (mode, value)
        assert any(page.width > page.height for page in pdf.pages)
    with ZipFile(root / f"{mode}.hwpx") as archive:
        tables = []
        for name in archive.namelist():
            if re.fullmatch(r"Contents/section\d+\.xml", name):
                tables += etree.fromstring(archive.read(name)).xpath("//*[local-name()='tbl']")
        widths = [float(n.get("width")) for n in tables[2].xpath("./*[local-name()='tr'][1]/*[local-name()='tc']/*[local-name()='cellSz']")]
        assert abs(widths[0]/sum(widths)-.28) < .005
        assert abs(widths[1]/sum(widths)-.16) < .005
    print(f"PASS: {mode} PDF headings/cells/notes, landscape page and HWPX conditional widths")

def word_signature(path):
    with ZipFile(path) as archive:
        doc = etree.fromstring(archive.read("word/document.xml"))
        ns = {"w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main"}
        return (normalize("".join(doc.xpath("//w:t/text()",namespaces=ns))),
                [normalize("".join(t.xpath(".//w:t/text()",namespaces=ns))) for t in doc.xpath("//w:tbl",namespaces=ns)],
                [(n.get("{"+ns["w"]+"}w"),n.get("{"+ns["w"]+"}h")) for n in doc.xpath("//w:pgSz",namespaces=ns)])
assert word_signature(root / "accumulated.docx") == word_signature(root / "roundtrip.docx")
for name in ("current.docx", "accumulated.docx", "roundtrip.docx"):
    with ZipFile(root / name) as archive:
        doc = etree.fromstring(archive.read("word/document.xml"))
        body = list(doc.find("{*}body"))
        tables = [i for i,n in enumerate(body) if etree.QName(n).localname == "tbl"]
        between = body[tables[0]+1:tables[1]]
        paragraphs = [n for n in between if etree.QName(n).localname == "p"]
        text = lambda n: "".join(n.xpath(".//*[local-name()='t']/text()"))
        assert len(paragraphs) == 2 and "HC3 SE" in text(paragraphs[0]) and text(paragraphs[1]) == "", name
print("PASS: PDF cover and exactly one blank paragraph after table notes in Word and reopened HWPX")
print("PASS: Hancom reopen preserves complete text, table order and section dimensions")
