"""Verify publication output text and render IPA export pages for inspection."""
import json
import re
import unicodedata
from pathlib import Path
from pypdf import PdfReader
import pypdfium2 as pdfium
from PIL import Image, ImageDraw

root = Path("tmp/ipa-validation")
normalize = lambda s: re.sub(r"\s+", "", unicodedata.normalize("NFKC", s))
for mode in ("current", "accumulated"):
    path = root / f"{mode}.pdf"
    reader = PdfReader(path)
    for page in reader.pages:
        assert abs(float(page.mediabox.width) - 176 / 25.4 * 72) < 2
        assert abs(float(page.mediabox.height) - 250 / 25.4 * 72) < 2
    text = normalize("".join(page.extract_text() or "" for page in reader.pages))
    for value in json.loads((root / f"{mode}-expected.json").read_text(encoding="utf-8")):
        assert normalize(value) in text, (mode, value)
    pdf = pdfium.PdfDocument(str(path))
    thumbs = []
    for index in range(len(pdf)):
        page = pdf[index].render(scale=1).to_pil().convert("RGB")
        page.thumbnail((420, 600))
        cell = Image.new("RGB", (440, 625), "#dddddd")
        cell.paste(page, (10, 20))
        ImageDraw.Draw(cell).text((10, 4), f"{mode} {index+1}", fill="black")
        thumbs.append(cell)
    sheet = Image.new("RGB", (1320, 625 * ((len(thumbs)+2)//3)), "white")
    for index, thumb in enumerate(thumbs):
        sheet.paste(thumb, ((index % 3)*440, (index//3)*625))
    sheet.save(root / f"{mode}-pages.png")
    print(f"PASS: {mode} PDF text; {len(pdf)} pages rendered.")
