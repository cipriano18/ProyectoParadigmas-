:- use_module(products).
:- use_module(api).

:- initialization(start).

start :-
    load_products,
    api:start_server,  % 👈 nota el "api:" — llama al predicado del módulo api
    writeln('Productos cargados correctamente.').



% consult('C:/Users/Reyner/Documents/UNA_2025/II_SEMESTRE/PARADIGMAS/Proyecto_prolog/supermarket.pl').
% consult('supermarket.pl').

% ========================
% COMO USAR EL PROYECTO
% 1. Desde swi-prolog ejecutar
% consult('C:/Users/Reyner/Documents/UNA_2025/II_SEMESTRE/PARADIGMAS/Proyecto_prolog/supermarket.pl').


% 3. O con extensión ejecutar: "swipl"
% consult('supermarket.pl').

% ========================
