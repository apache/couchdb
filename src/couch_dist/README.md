# couch_dist

Erlang communicates with its own protocol over TCP (Transmission Control Protocol).
It can also be configured to run its protocol over a TLS (Transport Layer Security)
connection which itself is over TCP.

`couch_dist` implements a custom distribution protocol -- `couch`, which allows
using TLS for Erlang distribution between nodes, with the ability to connect to
some nodes using TCP as well.

`TLS` can provide extra verification and security, but requires proper
certificates and configuration to set up the environment.

## Set up a custom Erlang distribution

1. Specify the distribution protocol in `vm.args`
2. Specify some nodes to use TCP only in `vm.args` (optional)
3. Generate certificates using `certs`
4. Specify security and other SSL options in `couch_dist.conf`

Examples:

1. `vm.args`:

      ```vm.args
      -proto_dist couch
      -couch_dist no_tls '"clouseau@127.0.0.1"'
      -ssl_dist_optfile </absolute_path/to/couch_dist.conf>
      ```

2. `couch_dist.conf`:

    - `erlserver.pem`: contains the certificate and its private key.
    - `{fail_if_no_peer_cert, true}`: In previous OTP versions it could be specified on both server side and client
      side, but in OTP 26 it can only be used on server side,
      see [OTP 26 Highlights](https://www.erlang.org/blog/otp-26-highlights/#ssl-improved-checking-of-options).

       ```couch_dist.conf
       [
         {server, [
           {cacertfile, "</absolute_path/to/ca-cert.pem>"},
           {certfile,   "</absolute_path/to/erlserver.pem>"},
           {secure_renegotiate, true},
           {verify, verify_peer},
           {fail_if_no_peer_cert, true}
         ]},
         {client, [
           {cacertfile, "</absolute_path/to/ca-cert.pem>"},
           {certfile,   "</absolute_path/to/cert.pem>"},
           {keyfile,    "</absolute_path/to/key.pem>"},
           {secure_renegotiate, true},
           {verify, verify_peer}
         ]}
       ].
       ```

## Generate Certificate

This is an example of using `elixir-certs` to generate certificates, but it is
not an endorsement of a specific expiration limit, key size or algorithm.

```bash
cd src/couch_dist/certs

# Generate CA certificate and key
./certs self-signed \
  --out-cert ca-cert.pem --out-key ca-key.pem \
  --template root-ca \
  --subject "/CN=CouchDB Root CA"

# Generate node certificate and key
./certs create-cert \
  --issuer-cert ca-cert.pem --issuer-key ca-key.pem \
  --out-cert cert.pem --out-key key.pem \
  --template server \
  --subject "/CN=127.0.0.1"

# Generate `erlserver.pem`
cat key.pem cert.pem >erlserver.pem

# Parse certificate to verify:
# Certificate needs to match the node's hostname
./parse_cert.escript cert.pem
["127.0.0.1"]
```

Thanks to Roger Lipscombe for creating [`elixir-certs`](https://github.com/rlipscombe/elixir-certs)
which simplifies the process of generating `X.509` certificates.

Also, thanks to Robert Newson for finding `elixir-certs` and adding the feature
to [easily pass `host` and `node` parameters to certificates](https://github.com/rnewson/elixir-certs/).

## Development

You can run CouchDB with `--enable-tls` mode, which will automatically generate
vm.args, certificates, and configuration files.

```bash
# Enable custom TLS distribution when starting CouchDB
./configure --dev && make && ./dev/run --enable-tls
# Or using the short flag `-t`
./configure --dev && make && ./dev/run -t

# Verify in another terminal
./dev/remsh-tls
(node1@127.0.0.1)1> net_kernel:nodes_info().
{ok,[{'node2@127.0.0.1',
         [{owner,<0.420.0>},
          {state,up},
          {address,
              {net_address,{{127,0,0,1},51814},"127.0.0.1",tls,inet}},
          {type,normal},
          {in,39},
          {out,38}]},
     {'node3@127.0.0.1',
         [{owner,<0.422.0>},
          {state,up},
          {address,
              {net_address,{{127,0,0,1},51813},"127.0.0.1",tls,inet}},
          {type,normal},
          {in,40},
          {out,39}]},
     {'remsh90016@127.0.0.1',
         [{owner,<0.778.0>},
          {state,up},
          {address,
              {net_address,{{127,0,0,1},51837},"127.0.0.1",tls,inet}},
          {type,hidden},
          {in,13},
          {out,18}]}]}
```

You can also set specific nodes to use TCP only:

```bash
# Specify node2@127.0.0.1 to use TCP only
./configure --dev && make && ./dev/run -t --no-tls node2@127.0.0.1
# Or use short node names
./configure --dev && make && ./dev/run -t --no-tls node2,node3

# Verify in another terminal
./dev/remsh-tls
(node1@127.0.0.1)1> net_kernel:nodes_info().
{ok,[{'node2@127.0.0.1',
         [{owner,<0.402.0>},
          {state,up},
          {address,
              {net_address,{{127,0,0,1},51750},"127.0.0.1",tcp,inet}},
          {type,normal},
          {in,55},
          {out,50}]},
     {'node3@127.0.0.1',
         [{owner,<0.408.0>},
          {state,up},
          {address,
              {net_address,{{127,0,0,1},51751},"127.0.0.1",tcp,inet}},
          {type,normal},
          {in,67},
          {out,49}]},
     {'remsh88631@127.0.0.1',
         [{owner,<0.794.0>},
          {state,up},
          {address,
              {net_address,{{127,0,0,1},51779},"127.0.0.1",tls,inet}},
          {type,hidden},
          {in,29},
          {out,50}]}]}
```
