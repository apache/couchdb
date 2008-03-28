%%%-------------------------------------------------------------------
%%% Defines
%%%-------------------------------------------------------------------

-define(TFTP_DEFAULT_PORT,  69).% Default server port

-define(TFTP_OPCODE_RRQ,    1). % Read request
-define(TFTP_OPCODE_WRQ,    2). % Write request
-define(TFTP_OPCODE_DATA,   3). % Data
-define(TFTP_OPCODE_ACK,    4). % Acknowledgement
-define(TFTP_OPCODE_ERROR,  5). % Error
-define(TFTP_OPCODE_OACK,   6). % Option acknowledgment

-define(TFTP_ERROR_UNDEF,   0). % Not defined, see error message (if any)
-define(TFTP_ERROR_ENOENT,  1). % File not found.
-define(TFTP_ERROR_EACCES,  2). % Access violation.
-define(TFTP_ERROR_ENOSPC,  3). % Disk full or allocation exceeded.
-define(TFTP_ERROR_BADOP,   4). % Illegal TFTP operation.
-define(TFTP_ERROR_BADBLK,  5). % Unknown transfer ID.
-define(TFTP_ERROR_EEXIST,  6). % File already exists.
-define(TFTP_ERROR_BADUSER, 7). % No such user.
-define(TFTP_ERROR_BADOPT,  8). % Unrequested or illegal option.

-record(tftp_msg_req,     {access, filename, mode, options, local_filename}).
-record(tftp_msg_data,    {block_no, data}).
-record(tftp_msg_ack,     {block_no}).
-record(tftp_msg_error,   {code, text, details}).
-record(tftp_msg_oack,    {options}).

-record(config, {parent_pid   = self(),
		 udp_socket,
		 udp_options  = [binary, {reuseaddr, true}, {active, once}],
		 udp_host     = "localhost",
		 udp_port     = ?TFTP_DEFAULT_PORT,
		 port_policy  = random,
		 use_tsize    = false,
		 max_tsize    = infinity, % Filesize
		 max_conn     = infinity,
		 rejected     = [],
		 polite_ack   = false,
		 debug_level  = none,
		 timeout,
		 user_options = [],
		 callbacks    = []}).

-record(callback, {regexp, internal, module, state, block_no, count}).

