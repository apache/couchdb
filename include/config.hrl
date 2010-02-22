
-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, true).
%we don't want to turn protocol buffers on by default, since the library is not included
%it should be very easy for new users to start up an instance
-record(config, {n=3,
                 r=1,
                 w=1,
                 q=6,
                 directory,
                 web_port,
                 text_port=11222,
                 storage_mod=dets_storage,
                 blocksize=4096,
                 thrift_port=9200,
                 pb_port=undefined,
                 buffered_writes=undefined,
                 cache=undefined,
                 cache_size=1048576,
                 hash_module=partitions,
                 meta=[]
                }).

-endif.
