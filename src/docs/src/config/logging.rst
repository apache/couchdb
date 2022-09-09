.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

.. default-domain:: config
.. highlight:: ini

=======
Logging
=======

.. _config/log:

Logging options
================

.. config:section:: log :: Logging Options

    CouchDB logging configuration.

    .. config:option:: writer :: Set the log writer to use.

        Current writers include:

        - ``stderr``: Logs are sent to stderr.
        - ``file``: Logs are sent to the file set in
          :option:`log file <log/file>`.
        - ``syslog``: Logs are sent to the syslog daemon.
        - ``journald``: Logs are sent to stderr without timestamp and log
          levels compatible with sd-daemon.

        You can also specify a full module name here if implement your own
        writer::

            [log]
            writer = stderr

    .. config:option:: file :: Logging file path

        Specifies the location of file for logging output. Only used by the
        ``file`` :option:`writer <log/writer>`::

            [log]
            file = /var/log/couchdb/couch.log

        This path should be readable and writable for user that runs CouchDB
        service (`couchdb` by default).

    .. config:option:: write_buffer :: File log write buffer size

       Specifies the size of the file log write buffer in bytes, to enable
       delayed log writes. Only used by the ``file``
       :option:`writer <log/writer>`::

            [log]
            write_buffer = 0

    .. config:option:: write_delay :: Wait delay before commiting logs to disk

        Specifies the wait in milliseconds before committing logs to disk, to
        enable delayed log writes. Only used by the ``file``
        :option:`writer <log/writer>`::

            [log]
            write_delay = 0

    .. config:option:: level :: Logging verbose level

        .. versionchanged:: 1.3 Added ``warning`` level.

        Logging level defines how verbose and detailed logging will be::

            [log]
            level = info

        Available levels:

        - ``debug``: Detailed debug logging.
        - ``info``: Informative logging. Includes HTTP requests headlines,
          startup of an external processes etc.
        - ``notice``
        - ``warning`` or ``warn``: Warning messages are alerts about edge situations that
          may lead to errors. For instance, compaction daemon alerts about low
          or insufficient disk space at this level.
        - ``error`` or ``err``: Error level includes only things that go wrong, like crash
          reports and HTTP error responses (5xx codes).
        - ``critical`` or ``crit``
        - ``alert``
        - ``emergency`` or ``emerg``
        - ``none``: Disables logging any messages.

    .. config:option:: include_sasl :: Include SASL information in logs

        Includes `SASL`_ information in logs::

            [log]
            include_sasl = true

        .. _SASL: http://www.erlang.org/doc/apps/sasl/

    .. config:option:: syslog_host :: Syslog host

        .. note::

            Setting `syslog_host` is mandatory for ``syslog`` to work!

        Specifies the syslog host to send logs to. Only used by the
        ``syslog`` :option:`writer <log/writer>`::

            [log]
            syslog_host = localhost

    .. config:option:: syslog_port :: Syslog port

        Specifies the syslog port to connect to when sending logs. Only used by
        the ``syslog`` :option:`writer <log/writer>`::

            [log]
            syslog_port = 514

    .. config:option:: syslog_appid :: Application name for syslog

        Specifies application name to the ``syslog``
        :option:`writer <log/writer>`::

            [log]
            syslog_appid = couchdb

    .. config:option:: syslog_facility :: Syslog designations for message sources

        Specifies the syslog facility to use with the ``syslog``
        :option:`writer <log/writer>`::

            [log]
            syslog_facility = local2

    .. note::
        CouchDB's ``syslog`` only knows how to use UDP logging. Please ensure that your
        ``syslog`` server has UDP logging enabled.

        For ``rsyslog`` you can enable the UDP module `imudp` in ``/etc/rsyslog.conf``::

            # provides UDP syslog reception
            module(load="imudp")
            input(type="imudp" port="514")
