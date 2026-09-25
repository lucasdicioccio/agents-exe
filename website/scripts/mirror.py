#!/usr/bin/env python3
"""Turns one repository markdown file into a Kitchen-Sink article on stdout.

    mirror.py --kind docs|specs|readme-section --source PATH --title T \
              --topic TOPIC --summary TEXT --github URL --date D [--section-from H2 --section-to H2]

Two things Kitchen-Sink's cmark renderer needs done for it: pipe tables are
not CommonMark, so they become raw <table> blocks (md_tables_to_html.py), and
the repository's relative links are rewritten to where the mirrored pages
live: `documentation/x.md` becomes `/docs-x.html`, `todos/x.md` becomes
`/specs-x.html`, an image becomes its raw.githubusercontent.com URL, and any
other repository-relative link becomes a GitHub blob URL, so nothing on the
site points at a path that only exists in a checkout.
"""
import argparse
import os
import re
import sys

sys.path.insert(0, os.path.dirname(__file__))
from md_tables_to_html import convert as tables  # noqa: E402

REPO = "https://github.com/lucasdicioccio/agents-exe/blob/main"
RAW = "https://raw.githubusercontent.com/lucasdicioccio/agents-exe/main"


def rewrite_links(text, base_dir):
    def repl(m):
        bang, label, target = m.group(1), m.group(2), m.group(3)
        if re.match(r"^[a-z]+:", target) or target.startswith("#") or target.startswith("/"):
            return m.group(0)
        path, _, anchor = target.partition("#")
        norm = os.path.normpath(os.path.join(base_dir, path)) if base_dir else os.path.normpath(path)
        anchor = ("#" + anchor) if anchor else ""
        if bang:
            return f"![{label}]({RAW}/{norm})"
        dm = re.fullmatch(r"documentation/([^/]+)\.md", norm)
        sm = re.fullmatch(r"todos/([^/]+)\.md", norm)
        if dm:
            return f"[{label}](/docs-{dm.group(1)}.html{anchor})"
        if sm:
            return f"[{label}](/specs-{sm.group(1)}.html{anchor})"
        return f"[{label}]({REPO}/{norm}{anchor})"

    return re.sub(r"(!?)\[([^\]]*)\]\(([^)\s]+)\)", repl, text)


def neutralise_numeric_hashtags(text):
    """`PR #8` would become a #8 hashtag page; an entity renders the same
    and is not a hashtag. Code spans and fences are left alone."""
    out, in_fence = [], False
    for line in text.split("\n"):
        if line.startswith("```"):
            in_fence = not in_fence
        if not in_fence:
            line = re.sub(r"(?<![\w`&])#(\d+)\b", r"&#35;\1", line)
        out.append(line)
    return "\n".join(out)


def readme_section(text, heading_from, heading_to):
    lines = text.split("\n")
    start = next(i for i, l in enumerate(lines) if l.strip() == heading_from)
    end = next(i for i, l in enumerate(lines) if i > start and l.strip() == heading_to)
    return "\n".join(lines[start:end])


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--kind", required=True, choices=["docs", "specs", "readme-section"])
    p.add_argument("--source", required=True)
    p.add_argument("--title", required=True)
    p.add_argument("--topic", required=True)
    p.add_argument("--keywords", default="")
    p.add_argument("--summary", required=True)
    p.add_argument("--github", required=True)
    p.add_argument("--date", required=True)
    p.add_argument("--section-from", default="")
    p.add_argument("--section-to", default="")
    p.add_argument("--intro", default="")
    p.add_argument("--figure", action="append", default=[],
                   help="NAME.dot|caption: a diagram from website/src, shown under the page's title")
    a = p.parse_args()

    text = open(a.source).read()
    if a.kind == "readme-section":
        text = readme_section(text, a.section_from, a.section_to)
        base_dir = ""
    else:
        base_dir = os.path.dirname(a.source)
    text = tables(neutralise_numeric_hashtags(rewrite_links(text, base_dir)))
    summary_text = neutralise_numeric_hashtags(rewrite_links(a.summary, base_dir))
    if a.figure:
        figures = []
        for spec in a.figure:
            name, _, caption = spec.partition("|")
            figures.append(f"![{caption}](/gen/images/{name}.png)")
        lines = text.split("\n")
        at = next((i for i, l in enumerate(lines) if l.startswith("# ")), -1) + 1
        text = "\n".join(lines[:at] + [""] + figures + [""] + lines[at:])
    image_link = ""
    if a.figure:
        image_link = ',"imageLink": "/gen/images/%s.png"' % a.figure[0].partition("|")[0]
    keywords = [k.strip() for k in a.keywords.split(",") if k.strip()]
    kw = ", ".join('"%s"' % k.replace('"', '\\"') for k in keywords)
    title = a.title.replace('"', '\\"')
    summary = summary_text.replace('"', '\\"')
    out = f'''=base:build-info.json
{{"layout":"article"
,"publicationStatus":"Public"
}}

=base:preamble.json
{{"author": "Lucas DiCioccio"
,"date": "{a.date}"
,"title": "{title}"{image_link}
}}

=base:topic.json
{{"topics":["{a.topic}"]
,"keywords":[{kw}]
}}

=base:social.json
{{"twitter": "lucasdicioccio"
,"linkedin": "lucasdicioccio"
,"github": "lucasdicioccio"
,"mastodon": "https://fosstodon.org/@lucasdicioccio"
}}

=base:summary.cmark
{summary}

=base:main-content.cmark

*Generated from [`{a.source}`]({a.github}), the repository is the canonical source and may be ahead of this page.*

{a.intro}
{text}

=base:main-css.tramaj-json
{{ "format": "css"
, "contents":
  [ ""
  , "@import \\"`$ctx.pathPrefix`/css/dev.css\\";"
  , "@import \\"`$ctx.pathPrefix`/css/colors.css\\";"
  , "@import \\"`$ctx.pathPrefix`/css/article.css\\";"
  , "@import \\"`$ctx.pathPrefix`/css/navigation.css\\";"
  ]
}}
'''
    sys.stdout.write(out)


if __name__ == "__main__":
    main()
