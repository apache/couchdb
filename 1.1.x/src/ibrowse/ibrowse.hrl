-ifndef(IBROWSE_HRL).
-define(IBROWSE_HRL, "ibrowse.hrl").

-record(url, {
          abspath,
          host,
          port,
          username,
          password,
          path,
          protocol,
          host_type  % 'hostname', 'ipv4_address' or 'ipv6_address'
}).

-record(lb_pid, {host_port, pid}).

-record(client_conn, {key, cur_pipeline_size = 0, reqs_served = 0}).

-record(ibrowse_conf, {key, value}).

-endif.
