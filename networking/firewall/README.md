# Файрвол

**Среда выполнения**: виртуальная машина (VirtualBox), выбранная операционная система - Ubuntu Desktop.

## Часть 1. Две сети

Для начала сделаем то, что написано в условии задания, а именно создадим два VPN-подключения, эмулирующих подключение к двум независимым свитчам.

```bash
sudo openvpn --config config-linux.ovpn --setenv UV_NETWORK internalA # первый терминал
sudo openvpn --config config-linux.ovpn --setenv UV_NETWORK internalB # второй терминал
```

Теперь установим себе адреса под *Сеть A* и *Сеть B*. Будем считать, что `tap0` - это *A*, `tap1` - *B*.

```bash
sudo ip addr change 10.54.161.1/24 dev tap0
sudo ip addr change 10.226.229.1/24 dev tap1
```

Проверим наше решение. Теперь мы хотим форвардинг из *A* в *B*, и наоборот. Сделаем это самым простым способом, установив глобальный флаг на форвардинг вообще для всех.

```bash
sudo sysctl -w net.ipv4.ip_forward=1
```

## Часть 2. Ограничиваем подключения

Начнем ограничивать *сеть B* в возможности TCP-соединений в *сеть A*. Для этого воспользуемся утилитой `iptables`, который нужен для настройки таблиц с правилами фильтраций IP-пакетов. Итак, мы хотим ограничить из *сети B* (`tap1`) TCP-соединения (`tcp`) в *сеть A* (`tap0`).

```bash
sudo iptables -A FORWARD -i tap1 -o tap0 --syn -p tcp -j DROP
# --syn - синхронизация
```

Далее, защитим наше устройство. Так как в нашей сети есть только *A* и *B*, ограничим поступления из *A* в *B* по всем портам, кроме определенных. Для этого мы *явно* разрешим UDP-соединения к нашему *destination* по двум портам, а затем скажем, что внезависимости от порта мы запрещаем всех остальных.

```bash
sudo iptables -A FORWARD -i tap0 -d 10.226.229.107 -p udp --dport 3001 -j ACCEPT
sudo iptables -A FORWARD -i tap0 -d 10.226.229.107 -p udp --dport 2308 -j ACCEPT
sudo iptables -A FORWARD -i tap0 -d 10.226.229.107 -p udp  -j DROP
```

Наконец, отфильтруем содержимое из *A* в *B*. Для этого мы сделаем практически тоже самое, но в качестве *match*'инга мы установим тип *string*, введем нашу вредную строку и выберем алгоритм поиска подстроки.

```bash
sudo iptables -A FORWARD -i tap0 -o tap1 -p tcp -m string --string "solutions-22" --algo bm -j DROP
```

## Часть 3. NAT

Самый простой метод настройки NAT - это дать такое правило, которое бы применялось ко всему трафику. Здесь мы в `iptables` теперь указываем таблицу `nat`, в качесте правила, а именно, когда мы хотим применять, будет выходом из устройства и, наконец, то, что будет маскировать адрес.

```bash
sudo iptables -t nat -A POSTROUTING -j MASQUERADE
```

И теперь настроем ICMP. Теперь мы будем изменять таблицу с пакетами `mangle` для пакетов `icmp` и установим ограничение по времени на 25.

```bash
sudo iptables -t mangle -A FORWARD -i tap0 -o tap1 -p icmp -j TTL --ttl-set 25
```

И теперь принудительно будем терять 25% от всех.

```bash
sudo iptables -t mangle -A FORWARD -i tap0 -o tap1 -p icmp -m statistic --mode random --probability 0.25 -j DROP
```
