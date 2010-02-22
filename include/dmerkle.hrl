-define(DMERKLE_VERSION, 2).
-define(STATIC_HEADER, 93).

-define(d_from_blocksize(BlockSize), trunc((BlockSize - 17)/16)).
-define(pointers_from_blocksize(BlockSize), (lib_misc:ceiling(math:log(BlockSize)/math:log(2)) - 3)).
-define(pointer_for_size(Size, BlockSize), (if Size =< 16 -> 1; Size =< BlockSize -> ?pointers_from_blocksize(Size); true -> last end)).
-define(size_for_pointer(N), (2 bsl (N+2))).
-define(headersize_from_blocksize(BlockSize), (?STATIC_HEADER + ?pointers_from_blocksize(BlockSize) * 8)).
-define(aligned(Ptr, HeaderSize, BlockSize), (((Ptr - (HeaderSize)) rem BlockSize) == 0)).
-define(block(Ptr, HeaderSize, BlockSize), ((Ptr - (HeaderSize)) div BlockSize)).

-record(node, {m=0, keys=[], children=[], offset=eof}).
-record(leaf, {m=0, values=[], offset=eof}).
-record(free, {offset,size=0,pointer=0}).
