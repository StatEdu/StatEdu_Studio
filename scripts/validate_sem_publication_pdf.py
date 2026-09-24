import logging
import pdfplumber

logging.getLogger("pdfminer").setLevel(logging.ERROR)
for mode in ("current", "accumulated"):
    with pdfplumber.open(f"tmp/sem-publication/{mode}.pdf") as pdf:
        texts = [page.extract_text() or "" for page in pdf.pages]
        assert "통계 분석 보고서" in texts[0]
        for number in range(7, 11):
            hits = [i for i, text in enumerate(texts) if f"Table {number}." in text]
            assert len(hits) == 1
            assert "Direct effect" in texts[hits[0]], "Title separated from its table"
            if number >= 9:
                page = pdf.pages[hits[0]]
                assert page.width < page.height
        mi_pages = [i for i, text in enumerate(texts) if "34.15" in text]
        assert len(mi_pages) == 1
        page = pdf.pages[mi_pages[0]]
        assert page.width > page.height
        print(f"PASS: {mode}: cover, grouped headers, titles kept with tables, portrait CIs and landscape MI")
