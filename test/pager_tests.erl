-module(pager_tests).

-include_lib("eunit/include/eunit.hrl").

mock_buffer(V) ->
    <<V:1024/unit:8>>.

gen_pages(N, State) ->
    g_pages(N, State, 0).
g_pages(N, State, N) ->
    State;
g_pages(N, State, Counter) ->
    S = memory_pager:set(Counter, mock_buffer(1), State),
    g_pages(N, S, Counter + 1).

api_test() ->
    Mp = memory_pager:new(),

    %% Create 4 pages
    Mp1 = gen_pages(4, Mp),

    [
        {0, {Off0, B0}},
        {1, {Off1, B1}},
        {2, {Off2, B2}},
        {3, {Off3, B3}}
    ] = memory_pager:collect(
        Mp1
    ),

    %% Check starting offsets
    0 = Off0,
    1 * 1024 = Off1,
    2 * 1024 = Off2,
    3 * 1024 = Off3,

    %% Check buffer sizes
    1024 = byte_size(B0),
    1024 = byte_size(B1),
    1024 = byte_size(B2),
    1024 = byte_size(B3),

    %% Check buffer contents
    X = mock_buffer(1),
    ?assertEqual(X, B0),
    ?assertEqual(X, B1),
    ?assertEqual(X, B2),
    ?assertEqual(X, B3),

    %% Check page doesn't exist
    {none, _} = memory_pager:get(5, Mp1),

    %% Check a page does exist
    {ok, {_, Buf}, _} = memory_pager:get(2, Mp1),
    ?assertEqual(X, Buf),

    %% Change a bunch of bytes to 0
    ChangeBuf = <<1:2/unit:8, 0:1022/unit:8>>,
    Mp2 = memory_pager:set(2, ChangeBuf, Mp1),
    {ok, {Off2, Buf1}, _} = memory_pager:get(2, Mp2),
    ?assertEqual(ChangeBuf, Buf1),

    4 = memory_pager:num_of_pages(Mp2),
    1024 = memory_pager:pagesize_in_bytes(Mp2),
    ok.

truncate_test() ->
    %% Buffer is correct, do nothing
    <<1, 1, 1, 1>> = memory_pager:truncate_buffer(<<1, 1, 1, 1>>, 4),

    %% To big
    <<1, 1, 1, 1>> = memory_pager:truncate_buffer(<<1, 1, 1, 1, 2, 2, 2, 2>>, 4),

    %% To small
    <<0, 0, 0, 0>> = memory_pager:truncate_buffer(<<>>, 4),
    <<1, 0, 0, 0>> = memory_pager:truncate_buffer(<<1>>, 4),
    <<1, 1, 0, 0>> = memory_pager:truncate_buffer(<<1, 1>>, 4),
    <<1, 1, 1, 0>> = memory_pager:truncate_buffer(<<1, 1, 1>>, 4),

    ok.

misc_test() ->
    ?assertError({badarg, not_power_of_two}, memory_pager:new(1000)),
    Mp = memory_pager:new(),
    0 = memory_pager:pagenum_for_byte_index(0, Mp),
    0 = memory_pager:pagenum_for_byte_index(1000, Mp),
    1 = memory_pager:pagenum_for_byte_index(2000, Mp),
    2 = memory_pager:pagenum_for_byte_index(3000, Mp),
    97 = memory_pager:pagenum_for_byte_index(100000, Mp),
    ok.
