from config import *
import writer
import crawler
import item_parser
import os
import multiprocessing

if not os.path.isdir(MSVETA_DIR_HTML):
    os.mkdir(MSVETA_DIR_HTML)

urls = []

for i in range(MSVETA_START, MSVETA_END + 1):
    urls.append(MSVETA_WEB_PAGE_BASE + str(i))

writer = writer.csv_writer(MSVETA_OUTPUT, MSVETA_STATS_SORTED)

def generateDataset(url):
    global writer

    filename = crawler.toHTMLFilename(url)

    if not crawler.download(url):
        return

    stats = item_parser.parse(filename)
    sortedStats = []

    if stats != None:
        for k in MSVETA_STATS_SORTED:
            if len(stats[k]) != len(MSVETA_STAT_DEFAULT_REQUIRED):
                sortedStats.append(stats[k])
            else:
                return
        writer.write(sortedStats)

pool = multiprocessing.Pool()

for url in urls:
    if crawler.download(url):
        items = crawler.extractItems(url)
        pool.map(generateDataset, items)
