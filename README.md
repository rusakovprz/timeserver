Сервер времени на Erlang.
=========================

Сервер отдающий подключенным клиентам «точное» время с заданным тайм-аутом. 

Задача имеет учебный характер (изучение Etlang/OTP), и имеет следующие цели:

- практическое изучение языка Erlang и его библиотек;

- практическое изучение фреймворка OTP;

Задача будет решаться в несколько этапов. На каждом этапе задача будет усложняться. 

Этап 1. Разработка Gen_server реализующего ТCP сервер и отправляющий клиенту текущее время с заданным таймаутом.

Изучение:

- поведение ОТР gen_server;

- библиотека timer;

- библиотека gen_tcp;

- библиотека string.

Этап 2.

- Реализовать Gen_server ожидающий ТCP подключение клиента.

- Реализовать Gen_server работающий с клиентом по ТCP.

- Реализовать Gen_server возвращающий текущее системное время

- Реализовать Supervisor обеспечивающий запуск и контроль над серверами

Изучение поведения Supervisor.

Этап 3.

- добавить шифрование трафика
- обеспечить одновременную работу с несколькими клиентами
- оптимизировать протокол взаимодействия и архитектуру сервера в части доставки до клиента «актуального» времени.

Изучение:

- библиотека ssl;
- библиотека ssh.
