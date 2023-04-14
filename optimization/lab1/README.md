# Методы оптимизации. Лабораторная работа №1

## Официальная часть

**Создатели**:

* [*@sfbakturin*](https://github.com/sfbakturin)
* [*@Nomad192*](https://github.com/Nomad192)
* [*@SotnikovMaksim*](https://github.com/SotnikovMaksim)

## Структура проекта

Для облегчения понимания структуры проекта, следует прочесть эту часть. Структура по каталогам проекта выглядит так:

* **`Documentation/`** - директория с официальной информацией о лабораторной работой, вспомогательных файлов от преподавателей.
* **`Old/`** - директория с временными рабочими сырыми Jupyter Notebook выполняемых основных заданий.
* **`Image/`** - директория с сгенерированными картинками для полного отчета. Все названия соответствуют следующему шаблону: *Ta_Fc_\*.png* или же *Ta_Pb_Fc_\*.png*, где *a* - номер задания, *b* - номер пункта, *\** - дополнительная короткая информация.
* **`HQ/`** - директория с высококачественными сгенерированными картинками для просмотра отдельно от отчета. Все названия соответствуют следующему шаблону: *Ta_Pb_Fc_\*_HQ.png*, где *a* - номер задания, *b* - номер пункта, *\** - дополнительная короткая информация.
* **`Data/`** - директория с `.csv` данными от полученных графиков. Все названия соответствуют следующему шаблону: *Ta_Fc_\*.csv* или же *Ta_Pb_Fc_\*.csv*, где *a* - номер задания, *b* - номер пункта, *\** - дополнительная краткая информация.

Главные файлы в корневом директории:

* **`T1.ipynb`**, **`T2.ipynb`**, **`T3.ipynb`**, **`T4.ipynb`**, **`T5.ipynb`**, **`T6-7.ipynb`** - сырые Jupyter Notebook выполняемых основных заданий.
* **`AT.ipynb`** -  сырой Jupyter Notebook выполняемого дополнительного задания.
* **`report.tex`** - исходный файл отчета в формате $\TeX$.
* **`.gitignore`** - все игнорируемые `git` файлы.
* **`README.md`** - этот файл.

## Самостоятельная сборка

Перед запуском Jupyter Notebook файлов следует убедиться, что все необходимые директории - `Image/`, `HQ/`, `Data/` - были созданы в корневом каталоге проекта. Для непосредственного запуска могут понадобятся соответствующие пакеты, устанавливаемые через [`pip`](https://jupyter.org/install).

Для сборки $\TeX$ документа Вам понадобится установить [TeX Live](https://www.tug.org/texlive/) или любую другую замену с полным набором пакетов. Также, для корректного отображения кода в системе вёрстки $\TeX$ Вам может понадобится установленный пакет [pygments](https://pygments.org/). Для установки введите в терминале:

```bash
pip install pygments=2.11.2
```