# Домашнее задание 2

## Условие

Целью этого домашнего задания является реализация простого однотактного процессора, который реализует
маленькое модмножество архитектуры MIPS (команды `lw`, `sw`, `add`, `sub`, `and`, `or`, `slt`, `beq`),
с помощью Verilog.

## Часть 1

### Реализация составных частей микроархитектуры.

В этой части вам будет необходимо реализовать некоторые составные части вашей микроархитектуры, а именно:

* АЛУ.
* Управляющее устройство.
* Регистровый файл.
* Устройства памяти команд и данных.

Чтение из памяти и регистрового файла асинхронное, запись происходит по фронту сигнала синхронизации.

Обратите внимание, что память команд изначально заполняется двоичными данными из файла и является read-only.

Команды кодируются следующим образом:

| Команда | opcode | rs    | rt    | imm              |
|---------|--------|-------|-------|------------------|
| lw      | 100011 | xxxxx | xxxxx | xxxxxxxxxxxxxxxx |
| sw      | 101011 | xxxxx | xxxxx | xxxxxxxxxxxxxxxx |
| beq     | 000100 | xxxxx | xxxxx | xxxxxxxxxxxxxxxx |
| addi*   | 001000 | xxxxx | xxxxx | xxxxxxxxxxxxxxxx |

`imm` для команды `beq` задает количество инструкций, на которое нужно сдвинуться относительно _следующей_ за `beq`.

`imm` -- знаковая 16-битная константа.

`*` Реализация `addi` является опциональной и может принести еще 2 дополнительных балла за реализацию
управляющего устройства.

| Команда | opcode | rs    | rt    | rd    | shamt | funct  |
|---------|--------|-------|-------|-------|-------|--------|
| add     | 000000 | xxxxx | xxxxx | xxxxx | 00000 | 100000 |
| sub     | 000000 | xxxxx | xxxxx | xxxxx | 00000 | 100010 |
| and     | 000000 | xxxxx | xxxxx | xxxxx | 00000 | 100100 |
| or      | 000000 | xxxxx | xxxxx | xxxxx | 00000 | 100101 |
| slt     | 000000 | xxxxx | xxxxx | xxxxx | 00000 | 101010 |

Напонимание: биты нумеруются справа налево, т. е. самый правый бит является самым младшим битом команды.

## Часть 2

### Реализация процессора

В этой части вам будет необходимо склеить модули из предыдущей части воедино, чтобы получить рабочий процессор.

Данный модуль имеет порт тактового генератора и порты для взаимодействия с памятью команд, данных и регистровым файлом.

При реализации процессора вам могут понадобиться вспомогательные модули для мультиплексоров, расширения знаковой константы, D-триггер для `PC` и т. д.