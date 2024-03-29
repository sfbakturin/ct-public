import random

MSVETA_BASE = "https://www.msveta.ru/"
MSVETA_DIR_HTML = "html"
MSVETA_DIR_PICS = "pics"
MSVETA_OUTPUT = "msveta.csv"
MSVETA_START = 1
MSVETA_END = 15

MSVETA_WEB_PAGE_BASE = "https://www.msveta.ru/catalog/lyustry/?PAGEN_1="
MSVETA_WEB_PROXY_HTTP = ["68.183.135.221", "35.236.207.242", "138.199.48.1"]

MSVETA_PROXIES = {
    'http' : random.choice(MSVETA_WEB_PROXY_HTTP)
}

MSVETA_STAT_NAME = "Наименование"
MSVETA_STAT_PRICE = "Цена"
MSVETA_STAT_VOLTAGE = "Напряжение, В"
MSVETA_STAT_BRAND = "Бренд"
MSVETA_STAT_DIAMETER = "Диаметр"
MSVETA_STAT_HEIGHT = "Высота"
MSVETA_STAT_REINFORCEMENT_COLORS = "Цвет арматуры"
MSVETA_STAT_SHADE_COLORS = "Цвет плафонов"
MSVETA_STAT_NUMBER_LAMPS = "Количество ламп"
MSVETA_STAT_POWER = "Мощность, Вт"
MSVETA_STAT_GENERAL_POWER = "Общая мощность, Вт"
MSVETA_STAT_STYLE = "Стиль"
MSVETA_STAT_BASE = "Цоколь"

MSVETA_STAT_DEFAULT_REQUIRED = ""
MSVETA_STAT_DEFAULT_OPTIONAL = "отсутствует"

MSVETA_STAT_SEPARATOR = ";"
MSVETA_STAT_TABLE_CLASS = "stats"

MSVETA_STATS = {
    MSVETA_STAT_NAME : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_PRICE : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_VOLTAGE : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_BRAND : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_DIAMETER : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_HEIGHT : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_REINFORCEMENT_COLORS : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_SHADE_COLORS : MSVETA_STAT_DEFAULT_OPTIONAL,
    MSVETA_STAT_NUMBER_LAMPS : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_POWER : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_GENERAL_POWER : MSVETA_STAT_DEFAULT_REQUIRED,
    MSVETA_STAT_STYLE : MSVETA_STAT_DEFAULT_OPTIONAL,
    MSVETA_STAT_BASE : MSVETA_STAT_DEFAULT_REQUIRED
}
MSVETA_STATS_SORTED = [
    MSVETA_STAT_NAME, MSVETA_STAT_PRICE, MSVETA_STAT_VOLTAGE,
    MSVETA_STAT_BRAND, MSVETA_STAT_DIAMETER, MSVETA_STAT_HEIGHT,
    MSVETA_STAT_REINFORCEMENT_COLORS, MSVETA_STAT_SHADE_COLORS,
    MSVETA_STAT_NUMBER_LAMPS, MSVETA_STAT_POWER, MSVETA_STAT_GENERAL_POWER,
    MSVETA_STAT_STYLE, MSVETA_STAT_BASE
]

MSVETA_STAT_PARSE_NAME = "//h1[starts-with(@class, 'changeName')]/text()"
MSVETA_STAT_PARSE_PRICE = "//div[@id = 'elementTools']/div[@class = 'fixContainer']/div[@class = 'mainTool']/div[@class = 'price']/span[starts-with(@class, 'carrentPrice')]/text()"
MSVETA_STAT_PARSE_ITEMS = "//div[@class = 'item product sku']" 
