%%% @doc
%%%  A utility to create small memory buffers for an application without needing to allocate
%%%  one big memory buffer. For example, maybe you need to reference data by bytes at random
%%%  locations but you don't know which locations use. Or, you're processing
%%%  chunks that may live at different byte positions. Rather than trying to create 1 large
%%%  buffer to handle this, you can use memory_page to allocate only the buffers needed.
%%%
%%%  Say you have data at bit positions: 10, 1048, and will have others at higher ranges. You
%%%  could use this:
%%%  1 big buffer
%%%  [ | | | | | | ... ]
%%%  0                 4k
%%%
%%%  Even though you may not need it all. Or you could do this: Use Smaller buffers at
%%%  differnt offsets on demand:
%%%  [0] -> [ | | ...]
%%%         0       1023
%%%  [1] -> [ | | ...]
%%%        1024     2048
%%%
%%%  Memory Pager does the latter.
%%% @end
-module(memory_pager).

-export([
    new/0,
    new/1,
    get/2,
    set/3,
    num_of_pages/1,
    pagesize_in_bytes/1,
    collect/1,
    truncate_buffer/2
]).

-define(PAGE_SIZE, 1024).
-define(NUM_PAGES, 32768).

%% @doc Create a new memory pager with a default page size of 1024
new() ->
    new(?PAGE_SIZE).

%% @doc Create a new memory pager with the given page size.  Note:
%% 'PageSize' must be a power of 2 in 'bytes'.
new(PageSize) when PageSize rem 2 =:= 0 ->
    {
        array:new([{size, ?NUM_PAGES}, {fixed, false}, {default, nil}]),
        PageSize
    }.

%% @doc Get a page by page num.
%% Returns {ok, Page, State} | {none, State} if the page doesn't exist
get(PageNum, {Pages, _} = State) ->
    case array:get(PageNum, Pages) of
        nil -> {none, State};
        Page -> {ok, Page, State}
    end.

%% @doc Insert or update a buffer at the given page number. If the buffer
%% is not the same as the Page size, it will be truncated to fit the Page size.
%% Returns State
set(PageNum, Buffer, {Pages, Size}) ->
    Buf = truncate_buffer(Buffer, Size),
    %% demand paging.  Via the array, a new page will be created if it doesn't exist
    NewPages = array:set(PageNum, create_page(PageNum, Size, Buf), Pages),
    {NewPages, Size}.

%% @docs Return the number of pages
num_of_pages({Pages, _}) ->
    array:sparse_size(Pages).

%% @doc Return the page size. This is set on creation
pagesize_in_bytes({_, Size}) ->
    Size.

%% @doc Return a list of all valid pages. Each entry in the list
%% contains {PageNum, Page}.
collect({Pages, _}) ->
    array:sparse_to_orddict(Pages).

%% @private Create a page for the given buffer
create_page(PageNum, Size, Buffer) ->
    Offset = PageNum * Size,
    {Offset, Buffer}.

%% Deal with incoming buffers that may not have the comfigured page size.
%% - If the incoming buffer is the page size, do nothing and return the buffer
%% - If the incoming buffer is larger than the page size, Copy page size bytes
%% to a new buffer, return it, and disgard the rest.
%% - If the incoming buffer is smaller than the page size, append '0s' to the
%% buffer to make it the correct page size.
truncate_buffer(Buffer, Size) when byte_size(Buffer) =:= Size ->
    %% Same size, ok.
    Buffer;
truncate_buffer(Buffer, Size) when byte_size(Buffer) > Size ->
    %% Buffer is bigger
    <<A:Size/binary, _/binary>> = Buffer,
    A;
truncate_buffer(Buffer, Size) ->
    %% Buffer is smaller append '0s'.
    Diff = Size - byte_size(Buffer),
    Zeros = <<0:Diff/unit:8>>,
    <<Buffer/binary, Zeros/binary>>.