from config import *
import item_parser
import os
import requests
import lxml.html

def clone(element):
    return lxml.html.etree.fromstring(lxml.html.etree.tostring(element))

def toHTMLFilename(url):
    return MSVETA_DIR_HTML + os.path.sep + url.replace("/", "-").replace(".", "-").replace("?", "").replace("=", "-").removeprefix("https:--").replace("--", "") + ".html"

def extractItems(url):
    items = []
    filename = toHTMLFilename(url)
    src = open(filename).read()
    root = lxml.html.fromstring(src)

    divs = root.xpath(MSVETA_STAT_PARSE_ITEMS)

    for div in divs:
        itemUrl = clone(div).xpath('//div/a')[1].xpath("@href")[0]
        items.append(MSVETA_BASE + itemUrl[1:len(itemUrl)])

    return items

def getItems(url, writer):
    filename = toHTMLFilename(url)

    if not download(filename):
        return

    stats = item_parser.parse(filename)
    sortedStats = []

    if stats != None:
        for k in MSVETA_STATS_SORTED:
            if len(stats[k]) != len(MSVETA_STAT_DEFAULT_REQUIRED):
                sortedStats.append(stats[k])
        writer.write(sortedStats)

def download(url):
    filename = toHTMLFilename(url)

    if not os.path.exists(filename):
        try:
            web = requests.get(url, proxies = MSVETA_PROXIES)
            with open(filename, "w") as html:
                html.write(web.text)
        except Exception:
            print("Can't download", url)
            return False

    return True
