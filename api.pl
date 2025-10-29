:- module(api, [start_server/1]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(products).
:- use_module(orders).

% =========================
% Handlers principales
% =========================
:- http_handler(root(products), products_handler, []).
:- http_handler(root(category), category_handler, []).
:- http_handler(root(update), update_stock, []).
:- http_handler(root(orders), orders_post_handler, [method(post)]).
:- http_handler(root(orders/list), orders_get_handler, [method(get)]).
:- http_handler(root(orders/update), update_order_status_handler, [method(post)]).

% =========================
% Inicio del servidor
% =========================
start_server(Port) :-
    catch(
        http_server(http_dispatch, [port(Port), bind_address('0.0.0.0')]),
        error(socket_error(eaddrinuse, _), _),
        format('  Puerto ~w ya está en uso, el servidor sigue activo.~n', [Port])
    ),
    format(' Servidor iniciado en el puerto ~w~n', [Port]).

% =========================
% GET /products?name=Banano
% =========================
products_handler(Request) :-
    http_parameters(Request, [name(QueryAtom, [atom])]),
    downcase_atom(QueryAtom, QueryLower),
    findall(
        json{code:Code, name:Name, category:Category, price:Price, stock:Stock, unit:Unit, description:Description},
        (
            product(Code, Name, Category, Price, Stock, Unit, Description),
            atom_string(Name, NameStr),
            string_lower(NameStr, NameLower),
            sub_string(NameLower, _, _, _, QueryLower)
        ),
        Results
    ),
    reply_json(Results).

% =========================
% GET /update?code=P0001&quantity=50
% =========================
update_stock(Request) :-
    http_parameters(Request, [
        code(Code, [atom]),
        quantity(Quantity, [integer])
    ]),
    (   product(Code, Name, Category, Price, Stock, Unit, Description)
    ->  NewStock is Stock - Quantity,
        retract(product(Code, Name, Category, Price, Stock, Unit, Description)),
        assertz(product(Code, Name, Category, Price, NewStock, Unit, Description)),
        update_csv(Code, NewStock),
        reply_json(json{status: "Stock actualizado", code: Code, new_stock: NewStock})
    ;   reply_json(json{error: "Producto no encontrado"}, [status(404)])
    ).

% =========================
% GET /category?type=frutas
% =========================
category_handler(Request) :-
    http_parameters(Request, [type(TypeStr, [atom])]),
    atom_string(Type, TypeStr),
    findall(
        json{name:Name, stock:Stock},
        product(_, Name, Type, _, Stock, _, _),
        Results
    ),
    reply_json(Results).

% =========================
% POST /orders → Crear una orden 
% =========================
orders_post_handler(Request) :-
    http_read_json_dict(Request, Data),

    % Extraer campos del JSON recibido
    Code = Data.code,
    Name = Data.name,
    Quantity = Data.quantity,
    Place = Data.place,
    Receiver = Data.receiver,
    Date = Data.date,
    ( _{status:S} :< Data -> Status = S ; Status = 'Pendiente' ),

    log_order(Date, Code, Name, Quantity, Place, Receiver, Status, OrderCode),

    % Devolver respuesta JSON al frontend
    reply_json(json{
        status: "Pedido registrado correctamente",
        code: Code,
        product: Name,
        quantity: Quantity,
        delivery_place: Place,
        receiver: Receiver,
        order_code: OrderCode,
        date: Date,
        order_status: Status
    }).
% =========================
% GET /orders/list → Listar todas las órdenes
% =========================
orders_get_handler(_Request) :-
    (   exists_file('orders.csv')
    ->  csv_read_file('orders.csv', Rows, [functor(row), arity(8)]),
        Rows = [_Header | Data],
        findall(
            json{
                date: Date,
                code: Code,
                product: Product,
                quantity: Quantity,
                delivery_place: Place,
                receiver: Receiver,
                status: Status,
                order_code: OrderCode
            },
            (
                member(Row, Data),
                Row =.. [_|[Date, Code, Product, Quantity, Place, Receiver, Status, OrderCode]]
            ),
            JsonList
        ),
        reply_json(JsonList)
    ;   reply_json([], [status(200)])
    ).
% =========================
% POST /orders/update
% =========================

update_order_status_handler(Request) :-
    http_read_json_dict(Request, Data),
    atom_string(OrderCode, Data.order_code),   
    NewStatus = Data.status,

    (   exists_file('orders.csv')
    ->  csv_read_file('orders.csv', Rows, [functor(row), arity(8)]),
        Rows = [Header | DataRows],
        (   select(row(Date, Code, Product, Quantity, Place, Receiver, _OldStatus, OrderCode),
                   DataRows,
                   row(Date, Code, Product, Quantity, Place, Receiver, NewStatus, OrderCode),
                   UpdatedRows)
        ->  open('orders.csv', write, Stream, [encoding(utf8)]),
            csv_write_stream(Stream, [Header | UpdatedRows], []),
            close(Stream),
            reply_json(json{
                status: "Estado de orden actualizado correctamente",
                order_code: OrderCode,
                new_status: NewStatus
            })
        ;   reply_json(json{error: "Orden no encontrada"}, [status(404)])
        )
    ;   reply_json(json{error: "Archivo de órdenes no existe"}, [status(404)])
    ).
