-module(xds_external).

-include_lib("xqerl/include/xqerl.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         rotate/2, 
         rotate_fun/0, 
         sort_swap/2, 
         sort_swap_fun/0
        ]).

rotate(Ctx, [Bin]) -> rotate(Ctx, Bin);
rotate(_, Bin) -> rotate_1(Bin).

sort_swap(Ctx, [Bin]) -> sort_swap(Ctx, Bin);
sort_swap(_, Bin) -> sort_swap_1(Bin).

%% xqerl function to rotate the table once to the right
rotate_fun() ->
    #xqFunction{arity = 1,
                params = [#seqType{type = 'xs:string', occur = one}],
                type = #seqType{type = 'xs:string', occur = one},
                body = fun rotate/2, 
                external = true}.

%% xqerl function to swap cols and rows
sort_swap_fun() ->
    #xqFunction{arity = 1,
                params = [#seqType{type = 'xs:string', occur = one}],
                type = #seqType{type = 'xs:integer', occur = one},
                body = fun sort_swap/2, 
                external = true}.

%% ====================================================================
%% Internal functions
%% ====================================================================

rotate_1(<<A1, A2, A3, A4, A5, A6, A7, A8, A9, 
           B1, B2, B3, B4, B5, B6, B7, B8, B9, 
           C1, C2, C3, C4, C5, C6, C7, C8, C9, 
           D1, D2, D3, D4, D5, D6, D7, D8, D9, 
           E1, E2, E3, E4, E5, E6, E7, E8, E9, 
           F1, F2, F3, F4, F5, F6, F7, F8, F9, 
           G1, G2, G3, G4, G5, G6, G7, G8, G9, 
           H1, H2, H3, H4, H5, H6, H7, H8, H9, 
           I1, I2, I3, I4, I5, I6, I7, I8, I9>>) ->
    <<I1, H1, G1, F1, E1, D1, C1, B1, A1, 
      I2, H2, G2, F2, E2, D2, C2, B2, A2, 
      I3, H3, G3, F3, E3, D3, C3, B3, A3, 
      I4, H4, G4, F4, E4, D4, C4, B4, A4, 
      I5, H5, G5, F5, E5, D5, C5, B5, A5, 
      I6, H6, G6, F6, E6, D6, C6, B6, A6, 
      I7, H7, G7, F7, E7, D7, C7, B7, A7, 
      I8, H8, G8, F8, E8, D8, C8, B8, A8, 
      I9, H9, G9, F9, E9, D9, C9, B9, A9>>.

sort_swap_1(<<A1, AS:8/binary, BS:9/binary, CS:9/binary,
              DS:9/binary, ES:9/binary, FS:9/binary, GS:9/binary,
              HS:9/binary, IS:9/binary>>) ->
    Bk = sort_key(BS, A1),
    Ck = sort_key(CS, A1),
    Dk = sort_key(DS, A1),
    Ek = sort_key(ES, A1),
    Fk = sort_key(FS, A1),
    Gk = sort_key(GS, A1),
    Hk = sort_key(HS, A1),
    Ik = sort_key(IS, A1),
    A = if 
            Bk < Ck ->
                <<A1, AS:8/binary, BS:9/binary, CS:9/binary>>;
            true -> 
                <<A1, AS:8/binary, CS:9/binary, BS:9/binary>>
        end,
    {Dk2, D} = 
        if 
            Dk < Ek, Ek < Fk ->
                {Dk, <<DS:9/binary, ES:9/binary, FS:9/binary>>};
            Ek < Fk, Fk < Dk ->
                {Ek, <<ES:9/binary, FS:9/binary, DS:9/binary>>};
            Fk < Dk, Dk < Ek ->
                {Fk, <<FS:9/binary, DS:9/binary, ES:9/binary>>};
            Dk < Fk, Fk < Ek ->
                {Dk, <<DS:9/binary, FS:9/binary, ES:9/binary>>};
            Ek < Dk, Dk < Fk ->
                {Ek, <<ES:9/binary, DS:9/binary, FS:9/binary>>};
            true -> %Fk < Ek, Ek < Dk ->
                {Fk, <<FS:9/binary, ES:9/binary, DS:9/binary>>}
        end,
    {Gk2, G} = 
        if 
            Gk < Hk, Hk < Ik ->
                {Gk, <<GS:9/binary, HS:9/binary, IS:9/binary>>};
            Hk < Ik, Ik < Gk ->
                {Hk, <<HS:9/binary, IS:9/binary, GS:9/binary>>};
            Ik < Gk, Gk < Hk ->
                {Ik, <<IS:9/binary, GS:9/binary, HS:9/binary>>};
            Gk < Ik, Ik < Hk ->
                {Gk, <<GS:9/binary, IS:9/binary, HS:9/binary>>};
            Hk < Gk, Gk < Ik ->
                {Hk, <<HS:9/binary, GS:9/binary, IS:9/binary>>};
            true -> %Ik < Hk, Hk < Gk ->
                {Ik, <<IS:9/binary, HS:9/binary, GS:9/binary>>}
        end,
    Ret = 
        case Dk2 < Gk2 of
            true -> <<A/binary, D/binary, G/binary>>;
            false -> <<A/binary, G/binary, D/binary>>
        end,
    sort_swap_2(Ret).

sort_swap_2(<<A1, A2, A3, A4, A5, A6, A7, A8, A9, 
              B1, B2, B3, B4, B5, B6, B7, B8, B9, 
              C1, C2, C3, C4, C5, C6, C7, C8, C9, 
              D1, D2, D3, D4, D5, D6, D7, D8, D9, 
              E1, E2, E3, E4, E5, E6, E7, E8, E9, 
              F1, F2, F3, F4, F5, F6, F7, F8, F9, 
              G1, G2, G3, G4, G5, G6, G7, G8, G9, 
              H1, H2, H3, H4, H5, H6, H7, H8, H9, 
              I1, I2, I3, I4, I5, I6, I7, I8, I9>>) ->
    S1 = [A1,B1,C1,D1,E1,F1,G1,H1,I1],
    S2 = [A2,B2,C2,D2,E2,F2,G2,H2,I2],
    S3 = [A3,B3,C3,D3,E3,F3,G3,H3,I3],
    S4 = [A4,B4,C4,D4,E4,F4,G4,H4,I4],
    S5 = [A5,B5,C5,D5,E5,F5,G5,H5,I5],
    S6 = [A6,B6,C6,D6,E6,F6,G6,H6,I6],
    S7 = [A7,B7,C7,D7,E7,F7,G7,H7,I7],
    S8 = [A8,B8,C8,D8,E8,F8,G8,H8,I8],
    S9 = [A9,B9,C9,D9,E9,F9,G9,H9,I9],
    K2 = sort_key(S2, A1),
    K3 = sort_key(S3, A1),
    K4 = sort_key(S4, A1),
    K5 = sort_key(S5, A1),
    K6 = sort_key(S6, A1),
    K7 = sort_key(S7, A1),
    K8 = sort_key(S8, A1),
    K9 = sort_key(S9, A1),
    A = if 
            K2 < K3 -> [S1,S2,S3];
            true -> [S1,S3,S2]
        end,
    {Dk2, D} = 
        if 
            K4 < K5, K5 < K6 -> {K4, [S4,S5,S6]};
            K5 < K6, K6 < K4 -> {K5, [S5,S6,S4]};
            K6 < K4, K4 < K5 -> {K6, [S6,S4,S5]};
            K4 < K6, K6 < K5 -> {K4, [S4,S6,S5]};
            K5 < K4, K4 < K6 -> {K5, [S5,S4,S6]};
            true -> {K6, [S6,S5,S4]}
        end,
    {Gk2, G} = 
        if 
            K7 < K8, K8 < K9 -> {K7, [S7,S8,S9]};
            K8 < K9, K9 < K7 -> {K8, [S8,S9,S7]};
            K9 < K7, K7 < K8 -> {K9, [S9,S7,S8]};
            K7 < K9, K9 < K8 -> {K7, [S7,S9,S8]};
            K8 < K7, K7 < K9 -> {K8, [S8,S7,S9]};
            true -> {K9, [S9,S8,S7]}
        end,
    Ret = 
        case Dk2 < Gk2 of
            true -> A ++ D ++ G;
            false -> A ++ G ++ D
        end,
    iolist_to_binary(revert(Ret, [])).

revert([[]|_], []) -> [];
revert([], L) -> 
    revert(lists:reverse(L), []);
revert([[H|T]|R], L) -> 
    [H|revert(R, [T|L])].


sort_key(Bin, K) when is_binary(Bin) ->
    case Bin of
        <<_:1/binary, K, _/binary>> -> 2;
        <<_:2/binary, K, _/binary>> -> 3;
        <<_:3/binary, K, _/binary>> -> 4;
        <<_:4/binary, K, _/binary>> -> 5;
        <<_:5/binary, K, _/binary>> -> 6;
        <<_:6/binary, K, _/binary>> -> 7;
        <<_:7/binary, K, _/binary>> -> 8;
        _ -> 9
    end;
sort_key(List, K) ->
    case List of
        [_,K|_] -> 2;
        [_,_,K|_] -> 3;
        [_,_,_,K|_] -> 4;
        [_,_,_,_,K|_] -> 5;
        [_,_,_,_,_,K|_] -> 6;
        [_,_,_,_,_,_,K|_] -> 7;
        [_,_,_,_,_,_,_,K|_] -> 8;
        _ -> 9
    end.
