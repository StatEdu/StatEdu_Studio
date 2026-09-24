from pathlib import Path
import logging
from zipfile import ZipFile
from lxml import etree
import pdfplumber
logging.getLogger("pdfminer.pdffont").setLevel(logging.ERROR)

root = Path("tmp/regression-variable-width")
for mode, count in [("current", 1), ("accumulated", 3)]:
    with ZipFile(root / f"{mode}.xlsx") as archive:
        for i in range(1, count + 1):
            sheet = etree.fromstring(archive.read(f"xl/worksheets/sheet{i}.xml"))
            widths = [float(n.get("width")) for n in sheet.xpath("//*[local-name()='cols']/*")]
            assert widths[0] > max(widths[1:]), widths
    with ZipFile(root / f"{mode}.hwpx") as archive:
        tables = []
        for name in archive.namelist():
            if name.startswith("Contents/section") and name.endswith(".xml"):
                section = etree.fromstring(archive.read(name))
                tables.extend(section.xpath("//*[local-name()='tbl']"))
        assert len(tables) == count, len(tables)
        for table in tables:
            widths = [float(n.get("width")) for n in table.xpath("./*[local-name()='tr'][1]/*[local-name()='tc']/*[local-name()='cellSz']")]
            assert widths[0] > max(widths[1:]), widths
            assert widths[0] / sum(widths) >= .279, widths
    with pdfplumber.open(root / f"{mode}.pdf") as pdf:
        text = "".join((p.extract_text() or "") for p in pdf.pages)
        compact = "".join(text.split())
        assert "GCM_엄마나이(35세기준):35세이상" in compact
        assert "LLCI=lowerconfidencelimit" in compact
    print(f"PASS: {mode}, Excel/HWPX column widths and PDF label/note content")
