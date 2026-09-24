"""Compare the captured DOCX with Hancom's HWPX -> DOCX round trip."""
import hashlib
from pathlib import Path
import sys
import xml.etree.ElementTree as ET
import zipfile

folder = Path(sys.argv[1] if len(sys.argv) > 1 else "tmp/result-fidelity")
W = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"


def signature(path):
    with zipfile.ZipFile(path) as package:
        document = ET.fromstring(package.read("word/document.xml"))
        text = lambda node: "".join("".join(t.itertext()) for t in node.iter(W + "t"))
        normalize = lambda value: "".join(value.split())
        return {
            "text": normalize(text(document)),
            "tables": [normalize(text(table)) for table in document.iter(W + "tbl")],
            "sections": [(int(page.get(W + "w")), int(page.get(W + "h"))) for page in document.iter(W + "pgSz")],
            "images": sorted(hashlib.sha256(package.read(name)).hexdigest() for name in package.namelist() if name.startswith("word/media/")),
        }


before = signature(folder / "accumulated.docx")
after = signature(folder / "roundtrip.docx")
for field in before:
    assert before[field] == after[field], f"Round-trip mismatch: {field}"
assert len(before["tables"]) == 3
assert before["images"]
print("PASS: complete text, table values/order, exact section dimensions, and unchanged embedded image bytes")
