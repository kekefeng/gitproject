# crawl my douban blog
import sqlite3
from lxml import etree
from ..article.models import Article

import requests
import pandas as pd

def save_in_sqlite3():
    conn = sqlite3.connect('blog.sqlite3')
    df = pd.DataFrame(bloglist)
    df.to_sql('blog', conn)

def download_blog(page):
    title = page.xpath('//h1/text()')[0]
    time = page.xpath('//span[@class="pub-date"]/text()')[0]
    author = page.xpath('//a[@class="note-author"]/text()')[0]
    content = page.xpath('//div[@class="note"]/text()')[0]
    bloglist.append([title, time, author, content])

def find_blog_and_download_in(page):
    blogurllist = page.xpath('//h3/a/@href')
    for blogurl in blogurllist:
        blogpage = get_selector(blogurl)
        download_blog(blogpage)

def get_selector(url):
    print(f'downloading {url} ....')
    selector = etree.HTML(requests.get(url).text)
    return selector

def get_nextpage(page):
    next_url = page.xpath('//span[@class="next"]/a/@href')
    if next_url:
        return get_selector(next_url[0])
    else:
        return None

def scan_page(page):
    find_blog_and_download_in(page)
    nextpage = get_nextpage(page)
    if nextpage is not None:
        scan_page(nextpage)
    save_in_sqlite3()

bloglist = []
if __name__ == '__main__':
    index_url = 'https://www.douban.com/people/81529665/notes?start=0&type=note'
    page = get_selector(index_url)
    scan_page(page)