"""Verify PDF text and render a contact sheet for the higher-order HTMT fixture."""
import json
import re
from pathlib import Path
import pypdfium2 as pdfium
from pypdf import PdfReader
from PIL import Image, ImageDraw

root = Path("tmp/cfa-higher-htmt")
expected = json.loads((root / "expected.json").read_text(encoding="utf-8"))
normalize = lambda text: re.sub(r"\s+", "", text)
for mode in ("current", "accumulated"):
    path = root / f"{mode}.pdf"
    text = normalize("".join(page.extract_text() or "" for page in PdfReader(path).pages))
    for value in expected:
        assert normalize(value) in text, (mode, value)
    document = pdfium.PdfDocument(str(path))
    thumbs = []
    for index in range(len(document)):
        page = document[index].render(scale=1).to_pil().convert("RGB")
        page.thumbnail((600, 850))
        cell = Image.new("RGB", (620, 880), "#cccccc")
        cell.paste(page, (10, 25))
        ImageDraw.Draw(cell).text((10, 5), f"{mode} page {index+1}", fill="black")
        thumbs.append(cell)
    sheet = Image.new("RGB", (620 * min(3, len(thumbs)), 880 * ((len(thumbs)+2)//3)), "white")
    for index, thumb in enumerate(thumbs):
        sheet.paste(thumb, ((index % 3)*620, (index//3)*880))
    sheet.save(root / f"{mode}-pages.png")
    print(f"PASS: {mode} PDF text; {len(document)} pages rendered.")
