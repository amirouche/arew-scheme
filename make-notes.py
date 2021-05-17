#!/usr/bin/env python3
"""make-notes.py.

Usage:
  make-notes.py init
  make-notes.py make <base-url>
  make-notes.py render <template>
"""
import logging
import os
import sys
from html import escape as htmlescape
from pathlib import Path

import daiquiri
from docopt import docopt
from datetime import datetime
from datetime import timezone
from lxml.html import fromstring as string2html
from feedgen.feed import FeedGenerator

from jinja2 import Environment
from jinja2 import FileSystemLoader

import mistune
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import html


daiquiri.setup(logging.DEBUG, outputs=('stderr',))
log = daiquiri.getLogger(__name__)


class HighlightRenderer(mistune.Renderer):
    def block_code(self, code, lang):
        if not lang:
            out = '\n<div><pre>{}</pre></div>\n'
            return out.format(mistune.escape(code.strip()))
        lexer = get_lexer_by_name(lang, stripall=True)
        formatter = html.HtmlFormatter()
        return highlight(code, lexer, formatter)


renderer = HighlightRenderer()
markdown = mistune.Markdown(renderer=renderer)


def jinja(template, templates, **context):
    templates = os.path.abspath(templates)
    env = Environment(loader=FileSystemLoader((templates,)))
    template = env.get_template(template)
    out = template.render(**context)
    return out


# make

def make_post_from_markdown(path, base):
    log.info('markdown: %r', path)
    with path.open('r') as f:
        body = f.read()
    body = markdown(body)
    html = '<div>{}</div>'.format(body)
    div = string2html(html)
    try:
        title = div.xpath('h1/text()')[0]
    except indexerror:
        msg = "seems like there is not title in: %s"
        log.critical(msg, path)
        sys.exit(1)
    log.debug('title is: %s', title)
    date = title[:len('2017/03/01')]
    date = datetime.strptime(date, '%Y/%m/%d')
    date = date.replace(tzinfo=timezone.utc)
    title = title[len('2017/03/01') + 3 :].strip()
    log.debug('publication date is: %s', date)
    post = {
        'title': title,
        'date': date,
        'html': html,
        'path': path,
    }
    log.debug('rendering blog post')
    page = jinja('post.jinja2', os.getcwd(), base=base, **post)
    filename = path.name.split('.')
    filename[-1] = 'html'
    filename = '.'.join(filename)
    output = path.parent / filename
    post['filename'] = str(output.relative_to(os.getcwd()))
    with output.open('w') as f:
        f.write(page)
    log.debug('wrote: %s', output)
    return post


def make_post_from_directory(path, base):
    log.info('directory %r', path)
    parts = []
    for subpath in path.glob('*.md'):
        out = dict(name=subpath.name)
        parts.append(out)
        with subpath.open('r') as f:
            source = f.read()
        html = markdown(source)
        html = "<div>{}</div>".format(html)
        out["html"] = html
        html = string2html(html)
        for level in [1,2]:
            headers = html.xpath('h{}/text()'.format(level))
            if not headers:
                continue
            header = headers[0]
            out["header"] = header
            break
    parts = sorted(parts, key=lambda x: x["name"])
    first = parts.pop()
    # compute title and date
    title = first["header"]
    date = title[:len('2017/03/01')]
    date = datetime.strptime(date, '%Y/%m/%d')
    date = date.replace(tzinfo=timezone.utc)
    title = title[len('2017/03/01') + 3 :].strip()
    # sort other files according to the header
    parts = sorted(parts, key=lambda x: x["header"])
    html = first["html"] + ''.join(part["html"] for part in parts)
    post = dict(
        title=title,
        date=date,
        html=html,
        path=path,
        filename=path.name,
    )
    log.debug('rendering blog post')
    page = jinja('post.jinja2', os.getcwd(), base=base, **post)
    output = path / "index.html"
    with output.open('w') as f:
        f.write(page)
    log.debug('wrote: %s', output)
    return post


def make(root, base):
    root = Path(root)
    log.info('getting started at: %s', root)
    blog = root / 'notes'
    paths = blog.glob('*/*')
    posts = []
    for path in paths:
        if str(path).endswith(".html"):
            continue
        if str(path).endswith(".md"):
            post = make_post_from_markdown(path, base)
        if path.is_dir():
            post = make_post_from_directory(path, base)
        posts.append(post)
    posts.sort(key=lambda x: x['date'], reverse=True)
    # populate feed
    output = root / 'feed.xml'
    log.info('generating feed at: %s', output)
    feed = FeedGenerator()
    feed.id(base)
    feed.title('Ruse Scheme')
    feed.subtitle('Perfection is the limit.')
    feed.link(href=base + '/feed.xml', rel='self')
    for post in posts:
        entry = feed.add_entry()
        url = base + '/blog/' + post['filename']
        entry.id(url)
        entry.title(post['title'])
        entry.link(href=url)
        entry.content(htmlescape(post["html"]))
        entry.published(post['date'].isoformat())
    feed.rss_file(str(output))
    log.info('rendering index')
    page = jinja('index.jinja2', os.getcwd(), base=base, posts=posts)
    output = Path(os.getcwd()) / 'index.html'
    with output.open('w') as f:
        f.write(page)


def main():
    args = docopt(__doc__, version='make-notes.py 0.2')
    if args.get('init'):
        raise NotImplementedError()
    elif args.get('make'):
        base = args['<base-url>']
        make(os.getcwd(), base)
    elif args.get('render'):
        # render markdown to html
        content = markdown(sys.stdin.read())
        # render template with `content`
        template = args['<template>']
        templates = os.path.abspath(template)
        templates = os.path.dirname(templates)
        out = jinja(template, templates, content=content)
        print(out)


if __name__ == '__main__':
    main()
