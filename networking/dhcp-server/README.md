# DHCP-сервер

**Среда выполнения**: виртуальная машина (VirtualBox), выбранная операционная система - Ubuntu Desktop.

## Часть 1. Сервер

Установим приложение `isc-dhcp-server` на систему для дальнейшего запуска DHCP-сервера.

Настроим его на выданный адрес для сети, отредактировав конфигурационный файл:

* Наш `subnet` - это выданный адрес (`10.179.165.0`), `netmask` - соответствующая по длине префикс битов (в данном случае, `24` - `255.255.255.0`).
* Наш `range` - это выдаваемые адреса для клиентов. Возьмем, например, весь возможный *range* `10.179.165.0` и `10.179.165.254`.
* Укажем `subnet-mask` - ту же маску.
* Укажем `routers` - какой-нибудь адрес для нашего DHCP-сервера из выбранного `range`.

Также дадим адрес из `range` поданному интерфейсу `tap0` (через команду `ip addr add dev tap0`) для будущего клиента и перезапустим сервис `isc-dhcp-server`:

```bash
sudo service isc-dhcp-server stop
sudo service isc-dhcp-server start
```

Для IPv6 адресов воспользуемся `radvd`. В конфигурационном файле зададим интерфейс `tap0` с следующими параметрами:

* Добавим флаг `AdvSendAdvert on;` - по документации это то, что он будет периодически отправлять предложения от сервера и отвечать на на запросы *router solicitation*.
* Добавим префикс нашей подсети `prefix ...` без каких-либо дополнительных настроек.

Включим проброс IPv6 пакетов. Например, можно воспользоваться командой:

```bash
sudo sysctl -w net.ipv6.conf.all.forwarding=1
```

И, наконец, перезапустим сервис `radvd`.

```bash
sudo service radvd stop
sudo service radvd start
```

## Часть 2. Данные клиента

Вновь воспользуемся кривой тропой как в прошлый раз: посмотрим на системные логи. Для этого, проверим наше решение и сохраним логи в первый раз, а затем ещё проверим и ещё раз сохраним логи, так, чтобы можно было легко найти новые данные по запросам бота.

```bash
# проверим решение
cat /var/log/syslog | grep -i "DHCP" > log.out
# проверим решение ещё раз
cat /var/log/syslog | grep -i "DHCP" > log2.out
```

Найдем, откуда начинаются новые логи и увидим следующее:

```log
Apr 19 11:43:40 net-vm dhcpd[3977]: DHCPDISCOVER from 02:c6:d4:20:0b:f9 (57b2f415e90f) via tap0
Apr 19 11:43:44 net-vm dhcpd[3977]: ns1.example.org: host unknown.
Apr 19 11:43:44 net-vm dhcpd[3977]: ns2.example.org: host unknown.
Apr 19 11:43:44 net-vm dhcpd[3977]: DHCPOFFER on 10.179.165.0 to 02:c6:d4:20:0b:f9 (a2dc72d7bd59) via tap0
Apr 19 11:43:44 net-vm dhcpd[3977]: DHCPREQUEST for 10.179.165.0 (10.179.165.27) from 02:c6:d4:20:0b:f9 (a2dc72d7bd59) via tap0
Apr 19 11:43:44 net-vm dhcpd[3977]: DHCPACK on 10.179.165.0 to 02:c6:d4:20:0b:f9 (a2dc72d7bd59) via tap0
Apr 19 11:43:51 net-vm dhcpd[3977]: reuse_lease: lease age 7 (secs) under 25% threshold, reply with unaltered, existing lease for 10.179.165.0
Apr 19 11:43:51 net-vm dhcpd[3977]: DHCPREQUEST for 10.179.165.0 from 02:c6:d4:20:0b:f9 (a2dc72d7bd59) via tap0
Apr 19 11:43:51 net-vm dhcpd[3977]: DHCPACK on 10.179.165.0 to 02:c6:d4:20:0b:f9 (a2dc72d7bd59) via tap0
```

Ответ: `02:c6:d4:20:0b:f9`.
