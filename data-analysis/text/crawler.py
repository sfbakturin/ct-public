import os
import wikipediaapi
import tqdm

from config import *

wiki = wikipediaapi.Wikipedia("Data Analysis Lab (sfbakturn@mail.ru)", "en")

def mkpath_model(topic, name = None, ext = True):
	base_path = os.path.join(DIR_DATASET, topic)
	if name:
		path = os.path.join(base_path, name.replace("/", "-"))
	else:
		return base_path
	if ext:
		return path + ".txt"
	else:
		return path

def get_names_by_cats(category, level = 0, max_level = 1):
	names = []
	for c in category.values():
		if not "Template:" in c.title and not "Category:" in c.title and not "Talk:" in c.title and not c.title.startswith("Lists of") and not c.title.startswith("Order of") and not c.title.startswith("ISO") and not c.title.startswith("List of") and not c.title.startswith("Index of") and not c.title.startswith("Portal:") and not "Wikipedia" in c.title:
			names.append(c.title)
		if c.ns == wikipediaapi.Namespace.CATEGORY and level < max_level:
			names.extend(get_names_by_cats(c.categorymembers, level = level + 1, max_level = max_level))
	return names

def get_summary_by_title(title):
	page = wiki.page(title)
	return page.summary.partition("\n")[0]

for topic in tqdm.tqdm(DAT_TOPICS):
	cat = wiki.page("Category:" + topic).categorymembers
	names = get_names_by_cats(cat)
	for name in tqdm.tqdm(names):
		if not name: continue
		path = mkpath_model(topic, name)
		if os.path.exists(path): continue
		summary = get_summary_by_title(name)
		with open(path, "w") as f:
			f.write(summary)
