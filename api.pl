:- module(api, [start_server/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(products).  % Importa los hechos
:- use_module(orders).     % Importa los hechos

:- http_handler(root(products), products_handler, []).
:- http_handler(root(category), category_handler, []).
:- http_handler(root(update), update_stock, []).
:- http_handler(root(orders), orders_handler, [method(post)]).
:- http_handler(root(update), update_stock, []).

start_server :-
    (   getenv('PORT', PortAtom)
    ->  atom_number(PortAtom, Port)
    ;   Port = 8080
    ),
    http_server(http_dispatch, [port(Port), bind_address('0.0.0.0')]),
    format('Servidor iniciado en el puerto ~w~n', [Port]).



% ========================
% Endpoint de productos
% ========================

% Búsqueda insensible, encuentra BANANO o Banano o banano
% Endpoint "GET": /products?name=Banano
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

% Endpoint "UPDATE" para stock:
%http://localhost:8080/update?code=P0001&quantity=50

update_stock(Request) :-
    http_parameters(Request, [
        code(Code, [atom]),
        quantity(Quantity, [integer])
    ]),
    (   product(Code, Name, Category, Price, Stock, Unit, Description)
    ->  (   Stock >= Quantity
        ->  NewStock is Stock - Quantity,
            retract(product(Code, Name, Category, Price, Stock, Unit, Description)),
            assertz(product(Code, Name, Category, Price, NewStock, Unit, Description)),
            update_csv(Code, NewStock),
            reply_json(json{status: "Stock actualizado", code: Code, new_stock: NewStock})
        ;   reply_json(json{error: "Stock insuficiente"}, [status(400)])
        )
    ;   reply_json(json{error: "Producto no encontrado"}, [status(404)])
    ).

% Endpoint "GET" para categoría:
%% Endpoint: /category?type=frutas
category_handler(Request) :-
    http_parameters(Request, [type(TypeStr, [atom])]),
    atom_string(Type, TypeStr),
    findall(
        json{name:Name, stock:Stock},
        product(_, Name, Type, _, Stock, _, _),
        Results
    ),
    reply_json(Results).



% ========================
% Endpoint de ORDENES
% ========================

update_stock(Request) :-
    http_read_json_dict(Request, Data),
    Code = Data.code,
    Quantity = Data.quantity,
    Place = Data.place,
    Receiver = Data.receiver,
    Date = Data.date,
    ( _{status: S} :< Data -> Status = S ; Status = 'Pendiente' ),
    (   product(Code, Name, Category, Price, Stock, Unit, Description)
    ->  NewStock is Stock - Quantity,
        retract(product(Code, Name, Category, Price, Stock, Unit, Description)),
        assertz(product(Code, Name, Category, Price, NewStock, Unit, Description)),
        update_csv(Code, NewStock),
        log_order(Date, Code, Name, Quantity, Place, Receiver, Status, _),
        reply_json(json{
            status: "Pedido registrado correctamente",
            code: Code,
            product: Name,
            place: Place,
            receiver: Receiver,
            order_status: Status,
            date: Date,
            new_stock: NewStock
        })
    ;   reply_json(json{error: "Producto no encontrado"}, [status(404)])
    ).


% ========================
% Endpoint GET /orders
% ========================
orders_handler(_Request) :-
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