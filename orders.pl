:- module(orders, [log_order/8]).
:- use_module(library(csv)).
:- dynamic order_counter/1.

% =========================
% Generar código incremental
% =========================
next_order_code(Code) :-
    (   order_counter(N)
    ->  N1 is N + 1,
        retract(order_counter(N))
    ;   N1 = 1
    ),
    asserta(order_counter(N1)),
    format(atom(Code), 'ORD~|~`0t~d~4+', [N1]).

% =========================
% Registrar una orden (sin imprimir nada en HTTP)
% =========================
log_order(Date, ProductCode, ProductName, Quantity, Place, ReceiverName, Status, OrderCode) :-
    next_order_code(OrderCode),

    % Crear encabezado si el archivo no existe
    (   \+ exists_file('orders.csv')
    ->  open('orders.csv', write, Stream, [encoding(utf8)]),
        csv_write_stream(Stream, [row('Date','Code','Product','Quantity','DeliveryPlace','ReceiverName','Status','OrderCode')], []),
        close(Stream)
    ;   true
    ),

    % Escribir la nueva orden
    open('orders.csv', append, Stream, [encoding(utf8)]),
    csv_write_stream(Stream,
        [row(Date, ProductCode, ProductName, Quantity, Place, ReceiverName, Status, OrderCode)], []),
    close(Stream).

