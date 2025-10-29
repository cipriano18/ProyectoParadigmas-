:- module(orders, [log_order/8]).
:- use_module(library(csv)).

% =========================================
% log_order(Date, Code, ProductName, Quantity, Place, ReceiverName, Status, OrderCode)
% =========================================
log_order(Date, Code, ProductName, Quantity, Place, ReceiverName, StatusIn, _OrderCode) :-
    % Si no se pasa estado, usar 'Pendiente'
    (   var(StatusIn)
    ->  Status = 'Pendiente'
    ;   Status = StatusIn
    ),

    % Crear archivo si no existe
    (   exists_file('orders.csv')
    ->  true
    ;   csv_write_file('orders.csv',
            [row('Date','Code','Product','Quantity','DeliveryPlace','ReceiverName','Status','OrderCode')], [])
    ),

    % Leer pedidos existentes
    csv_read_file('orders.csv', Rows, [functor(row), arity(8)]),
    length(Rows, TotalRows),
    (   TotalRows > 1
    ->  nth1(TotalRows, Rows, LastRow),
        arg(8, LastRow, LastOrderCode),
        atom_concat('ORD', NumAtom, LastOrderCode),
        atom_number(NumAtom, Num),
        NextNum is Num + 1
    ;   NextNum = 1
    ),
    format(atom(NewOrderCode), 'ORD~|~`0t~d~4+', [NextNum]),  % genera ORD0001, ORD0002, ...

    % Escribir al CSV
    open('orders.csv', append, Stream),
    csv_write_stream(Stream,
        [row(Date, Code, ProductName, Quantity, Place, ReceiverName, Status, NewOrderCode)], []),
    close(Stream),

    format(' Pedido registrado: ~w (~w unidades) - Código ~w (~w)~n',
           [ProductName, Quantity, NewOrderCode, Status]).
