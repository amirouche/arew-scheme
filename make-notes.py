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
from pathlib import Path

import daiquiri
from docopt import docopt
from datetime import datetime
from datetime import timezone
from lxml.html import fromstring as string2html
from lxml.html import tostring as html2string
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
        title = div.xpath('h1')[0]
    except IndexError:
        msg = "seems like there is not title in: %s"
        log.critical(msg, path)
        sys.exit(1)
    title.getparent().remove(title)
    title = title.text
    log.debug('title is: %s', title)
    date = title[:len('2017-03-01')]
    date = datetime.strptime(date, '%Y-%m-%d')
    date = date.replace(tzinfo=timezone.utc)
    title = title[len('2017-03-01') + 3 :].strip()
    log.debug('publication date is: %s', date)
    html = html2string(div, encoding='unicode')
    post = {
        'title': title,
        'date': date,
        'html': html,
        'path': path,
    }
    log.debug('rendering blog note')
    page = jinja('note.jinja2', os.getcwd(), base=base, **post)
    filename = path.name.split('.')
    filename[-1] = 'html'
    filename = '.'.join(filename)
    output = path.parent / filename
    post['filename'] = str(output.relative_to(os.getcwd()))
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
            raise NotImplementedError()
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
    feed.updated(posts[0]["date"])
    for post in posts:
        entry = feed.add_entry()
        url = base + '/' + post['filename']
        entry.id(url)
        entry.title(post['title'])
        entry.link(href=url)
        entry.content(post["html"], type="html")
        entry.published(post['date'].isoformat())
        entry.updated(post["date"])
    feed.atom_file(str(output))
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
