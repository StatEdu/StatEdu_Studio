"""Read-only text extraction for result-note export acceptance tests."""
import sys
import logging
from pathlib import Path
import pdfplumber
logging.getLogger("pdfminer.pdffont").setLevel(logging.ERROR)

with pdfplumber.open(sys.argv[1]) as document:
    Path(sys.argv[2]).write_text("\n".join(page.extract_text() or "" for page in document.pages) + "\n", encoding="utf-8")
