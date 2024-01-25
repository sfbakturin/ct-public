from config import *
import lxml.html
import bs4

def parseStats(filename, stats):
    src = open(filename).read()
    soup = bs4.BeautifulSoup(src, features = "lxml")

    table = soup.find("table", attrs = {"class" : MSVETA_STAT_TABLE_CLASS})
    table_body = table.find("tbody")

    rows = table_body.find_all("tr")

    for row in rows:
        cols = row.find_all("td")

        if len(cols) == 3:
            stat = cols[0].find(text = True).extract()
            if stat in stats:
                stats[stat] = cols[1].find(text = True).extract().strip()


def parseName(filename):
    src = open(filename).read()
    root = lxml.html.fromstring(src)
    divs = root.xpath(MSVETA_STAT_PARSE_NAME)

    if len(divs) == 0:
        return None
    else:
        return divs[0].strip()

def parsePrice(filename):
    src = open(filename).read()
    root = lxml.html.fromstring(src)
    divs = root.xpath(MSVETA_STAT_PARSE_PRICE)

    if len(divs) == 0:
        return None
    else:
        return divs[0].replace("руб.", "").replace(" ", "").strip()

def parse(filename):
    stats = MSVETA_STATS

    name = parseName(filename)

    if (name == None) or (not name.lower().startswith("люстра")):
        return None

    price = parsePrice(filename)

    if price == None:
        return None

    stats[MSVETA_STAT_NAME] = name
    stats[MSVETA_STAT_PRICE] = price

    parseStats(filename, stats)

    return stats
