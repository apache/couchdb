# Source code: https://github.com/rnewson/elixir-certs/
# credo:disable-for-this-file
# Important: run this with the wrapper script, so that umask is set correctly.

Mix.install([{:x509, "~> 0.8.3"}, {:optimus, "~> 0.2"}])

defmodule Certs do
  def main(argv) do
    Certs.Options.new!() |> Optimus.parse!(argv) |> run()
  end

  defp run(
         {[:self_signed],
          %Optimus.ParseResult{
            options: %{subject: subject, out_cert: out_cert, out_key: out_key, template: template}
          }}
       ) do
    ca_key = X509.PrivateKey.new_ec(:secp256r1)

    ca_crt = X509.Certificate.self_signed(ca_key, subject, template: template(template, subject, "", ""))

    File.write!(out_key, X509.PrivateKey.to_pem(ca_key), [:exclusive])
    File.chmod!(out_key, 0o400)

    File.write!(out_cert, X509.Certificate.to_pem(ca_crt), [:exclusive])
    File.chmod!(out_cert, 0o444)
  end

  defp run(
         {[:create_cert],
          %Optimus.ParseResult{
            options: %{
              subject: subject,
              host: host,
              node: node,
              issuer_cert: issuer_cert,
              issuer_key: issuer_key,
              out_cert: out_cert,
              out_key: out_key,
              template: template
            }
          }}
       ) do
    issuer_cert = File.read!(issuer_cert) |> X509.Certificate.from_pem!()
    issuer_key = File.read!(issuer_key) |> X509.PrivateKey.from_pem!()

    key = X509.PrivateKey.new_ec(:secp256r1)
    pub = X509.PublicKey.derive(key)

    crt =
      X509.Certificate.new(pub, subject, issuer_cert, issuer_key,
        template: template(template, subject, host, node)
      )

    File.write!(out_key, X509.PrivateKey.to_pem(key), [:exclusive])
    File.chmod!(out_key, 0o400)

    File.write!(out_cert, X509.Certificate.to_pem(crt), [:exclusive])
    File.chmod!(out_cert, 0o444)
  end

  defp run(_) do
    Certs.Options.new!() |> Optimus.help() |> IO.puts()
  end

  defp template("root-ca", _subject, _host, _node), do: :root_ca

  defp template("server", subject, _host, _node) do
    [commonName] =
      X509.RDNSequence.new(subject)
      |> X509.RDNSequence.get_attr(:commonName)

    import X509.Certificate.Extension

    %X509.Certificate.Template{
      # 1 year, plus a 30 days grace period
      validity: 365 + 30,
      hash: :sha256,
      extensions: [
        basic_constraints: basic_constraints(false),
        key_usage: key_usage([:digitalSignature, :keyEncipherment]),
        ext_key_usage: ext_key_usage([:serverAuth, :clientAuth]),
        subject_key_identifier: true,
        authority_key_identifier: true,
        subject_alt_name: subject_alt_name([commonName])
      ]
    }
  end

  defp template("node", _subject, host, node) do
    import X509.Certificate.Extension

    %X509.Certificate.Template{
      # 1 year, plus a 30 days grace period
      validity: 365 + 30,
      hash: :sha256,
      extensions: [
        basic_constraints: basic_constraints(false),
        key_usage: key_usage([:digitalSignature, :keyEncipherment]),
        ext_key_usage: ext_key_usage([:serverAuth, :clientAuth]),
        subject_key_identifier: true,
        authority_key_identifier: true,
        subject_alt_name: subject_alt_name([host]),
        subject_alt_name: subject_alt_name([{:directoryName, X509.RDNSequence.new("CN=" <> node, :otp)}])
      ]
    }
  end
end

defmodule Certs.Options do
  def new!() do
    Optimus.new!(
      name: "certs",
      description: "certs",
      version: "0.2",
      allow_unknown_args: false,
      parse_double_dash: true,
      subcommands: [
        self_signed: [
          name: "self-signed",
          about: "Create a self-signed certificate",
          options: [
            subject: [
              long: "--subject",
              value_name: "SUBJECT",
              required: true,
              parser: :string
            ],
            out_cert: [
              long: "--out-cert",
              value_name: "OUT_CRT",
              required: true,
              parser: :string
            ],
            out_key: [
              long: "--out-key",
              value_name: "OUT_KEY",
              required: true,
              parser: :string
            ],
            template: [
              long: "--template",
              value_name: "TEMPLATE",
              required: true,
              parser: :string
            ]
          ]
        ],
        create_cert: [
          name: "create-cert",
          options: [
            subject: [
              long: "--subject",
              value_name: "SUBJECT",
              required: true,
              parser: :string
            ],
            host: [
              long: "--host",
              value_name: "HOST",
              required: false,
              parser: :string
            ],
            node: [
              long: "--node",
              value_name: "NODE",
              required: false,
              parser: :string
            ],
            issuer_cert: [
              long: "--issuer-cert",
              value_name: "ISSUER_CRT",
              required: true,
              parser: :string
            ],
            issuer_key: [
              long: "--issuer-key",
              value_name: "ISSUER_KEY",
              required: true,
              parser: :string
            ],
            out_cert: [
              long: "--out-cert",
              value_name: "OUT_CRT",
              required: true,
              parser: :string
            ],
            out_key: [
              long: "--out-key",
              value_name: "OUT_KEY",
              required: true,
              parser: :string
            ],
            template: [
              long: "--template",
              value_name: "TEMPLATE",
              required: true,
              parser: :string
            ]
          ]
        ]
      ]
    )
  end
end

Certs.main(System.argv())

# vim: set ft=elixir
