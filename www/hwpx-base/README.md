# Native HWPX defaults

Structural XML defaults from an empty Hancom HWPX document generated during development. `picture.xml` contains only a sanitized shape skeleton (no image bytes or user text). No captured report, author metadata, or user data is bundled here.

The application builds styles, sections, tables, paragraphs, image manifests, and ZIP packages itself in `R/result_hwpx_native.R`. Hancom and DOCX are not part of the save path. The development-only Hancom round-trip validator is optional and is not invoked by the writer.

OWPML reference: https://github.com/hancom-io/hwpx-owpml-model and https://tech.hancom.com/python-hwpx-parsing-1/
