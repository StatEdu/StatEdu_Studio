"""Generate aligned public documentation. Edit docs/i18n, then run with --check in CI."""
import argparse
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LANGUAGES = ('ko', 'en', 'ja', 'zh', 'es', 'fr', 'de', 'vi')


def release_archive(language):
    path = ROOT / f'docs/reference/CHANGELOG_ARCHIVE_{language.upper()}.md'
    archive = path.read_text(encoding='utf-8-sig')
    archive = archive[archive.index('## v1.2.0'):].strip()
    reference = (ROOT / 'docs/reference/CHANGELOG_ARCHIVE_EN.md').read_text(encoding='utf-8-sig')
    reference = reference[reference.index('## v1.2.0'):].strip()
    headings = lambda text: re.findall(r'^## v[^\r\n]+', text, re.M)
    assert headings(archive) == headings(reference), f'Incomplete release archive: {language}'
    if language not in ('ko', 'en'):
        sections = lambda text: re.split(r'(?m)^## ', text)[1:]
        for source, translated in zip(sections(reference), sections(archive)):
            assert len(re.findall(r'^- ', source, re.M)) == len(re.findall(r'^- ', translated, re.M)), (
                f'Missing release entries: {language} {source.splitlines()[0]}')
    return archive


def outputs():
    releases = json.loads((ROOT / 'docs/i18n/releases.json').read_text(encoding='utf-8'))
    manifest = {}
    expected_ids = None
    for language in LANGUAGES:
        lines = (ROOT / f'docs/i18n/{language}.txt').read_text(encoding='utf-8').splitlines()
        titles, subtitles = lines[0].split('|'), lines[1].split('|')
        rows = [line.split('|') for line in lines[2:] if line]
        assert len(titles) == 4 and len(subtitles) == 3
        assert all(len(row) == 4 and all(row) for row in rows), language
        ids = [row[0] for row in rows]
        assert len(ids) == len(set(ids)) == 19, language
        expected_ids = expected_ids or ids
        assert ids == expected_ids, language
        manifest[language] = {}
        for index, (key, stem) in enumerate((('analysis_methods', 'ANALYSIS_METHODS'), ('method_notes', 'METHOD_NOTES'))):
            path = f'docs/{stem}_{language.upper()}.md'
            detailed = ROOT / f'docs/i18n/method_notes/{language}.md'
            if index == 1 and detailed.exists():
                body = detailed.read_text(encoding='utf-8')
                headings = re.findall(r'^## (.+)$', body, re.M)
                assert len(headings) >= 25, language
                toc = []
                for number, heading in enumerate(headings, 1):
                    anchor = f'method-{number}'
                    toc.append(f'- [{heading}](#{anchor})')
                    body = body.replace(f'## {heading}\n', f'<a id="{anchor}"></a>\n\n## {heading}\n', 1)
                yield path, f'# {titles[index]} — StatEdu Studio 1.3.0\n\n{subtitles[index]}\n\n## {titles[3]}\n\n' + '\n'.join(toc) + '\n\n' + body
                manifest[language][key] = dict(title=titles[index], subtitle=subtitles[index], path=path)
                continue
            parts = [f'# {titles[index]} — StatEdu Studio 1.3.0', subtitles[index], f'## {titles[3]}']
            parts.append('\n'.join(f'{i}. [{row[1]}](#{row[0]})' for i, row in enumerate(rows, 1)))
            for i, row in enumerate(rows, 1):
                parts.extend([f'<a id="{row[0]}"></a>', f'## {i}. {row[1]}', row[index + 2]])
            yield path, '\n\n'.join(parts) + '\n'
            manifest[language][key] = dict(title=titles[index], subtitle=subtitles[index], path=path)
        path = 'CHANGELOG.md' if language == 'en' else f'CHANGELOG_{language.upper()}.md'
        parts = [f'# {titles[2]}', '## v1.3.0 - 2026-09-20']
        parts.extend('- ' + entry for entry in releases[language][:5])
        additions = json.loads((ROOT / 'docs/i18n/release_130_additions.json').read_text(encoding='utf-8'))[language]
        parts.extend('- ' + entry for entry in additions['history'])
        parts.append(release_archive(language))
        yield path, '\n\n'.join(parts) + '\n'
        manifest[language]['version_history'] = dict(title=titles[2], subtitle=subtitles[2], path=path)
        for key, stem in (('overview', 'OVERVIEW'), ('user_guide', 'USER_GUIDE'), ('validation', 'VALIDATION')):
            path = f'docs/{stem}_{language.upper()}.md'
            body = (ROOT / f'docs/i18n/release_130/{key}_{language}.md').read_text(encoding='utf-8')
            yield path, body
            subtitle = f'StatEdu Studio 1.3.0 — {additions[key]}' if key == 'overview' else additions['subtitle']
            manifest[language][key] = dict(title=additions[key], subtitle=subtitle, path=path)
    yield 'docs/i18n/document_specs.json', json.dumps(manifest, ensure_ascii=False, indent=2) + '\n'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    count = 0
    for name, content in outputs():
        path = ROOT / name
        if args.check:
            assert path.exists() and path.read_text(encoding='utf-8') == content, f'Stale generated document: {name}'
        else:
            path.write_text(content, encoding='utf-8', newline='\n')
        count += 1
    print(f'{"Checked" if args.check else "Generated"} {count} files across {len(LANGUAGES)} languages.')


if __name__ == '__main__':
    main()
