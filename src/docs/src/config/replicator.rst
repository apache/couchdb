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

.. highlight:: ini

==========
Replicator
==========

.. _config/replicator:

Replicator Database Configuration
=================================

.. config:section:: replicator :: Replicator Database Configuration

    .. config:option:: max_jobs :: Maximum replications jobs

        .. versionadded:: 2.1

        Number of actively running replications.
        This value represents the threshold to trigger the automatic replication
        scheduler.
        The system will check every ``interval`` milliseconds how many replication
        jobs are running, and if there are more than ``max_jobs`` active jobs,
        the scheduler will pause-and-restart up to ``max_churn`` jobs in the
        scheduler queue.
        Making this value too high could cause performance issues, while making
        it too low could mean replications jobs might not have enough time to make
        progress before getting unscheduled again.
        This parameter can be adjusted at runtime and will take effect during next
        rescheduling cycle::

             [replicator]
             max_jobs = 500

    .. config:option:: interval :: Checking interval of replication jobs

        .. versionadded:: 2.1

        Scheduling interval in milliseconds.
        During each reschedule cycle the scheduler might start or stop up to ``max_churn``
        number of jobs::

             [replicator]
             interval = 60000

    .. config:option:: max_churn :: Maximum number of jobs to start and stop

        .. versionadded:: 2.1

        Maximum number of replication jobs to start and stop during rescheduling.
        This parameter, along with ``interval``, defines the rate of job replacement.
        During startup, however, a much larger number of jobs could be started
        (up to ``max_jobs``) in a short period of time::

             [replicator]
             max_churn = 20

    .. config:option:: max_history :: Maximum number of events recorded for each job

        Maximum number of events recorded for each job. This parameter defines
        an upper bound on the consecutive failure count for a job, and in turn
        the maximum backoff factor used when determining the delay before the job
        is restarted. The longer the length of the crash count, the longer the
        possible length of the delay::

             [replicator]
             max_history = 20

    .. config:option:: update_docs :: Update replication document with error and \
        triggered states

        .. versionadded:: 2.1

        When set to ``true`` replicator will update replication document with
        error and triggered states. This approximates pre-2.1 replicator
        behavior::

             [replicator]
             update_docs = false

    .. config:option:: worker_batch_size :: Batch size of workers

        With lower batch sizes checkpoints are done more frequently. Lower
        batch sizes also reduce the total amount of used RAM memory::

            [replicator]
            worker_batch_size = 500

    .. config:option:: worker_processes :: Number of worker processes

        More worker processes can give higher network throughput but can also
        imply more disk and network IO::

            [replicator]
            worker_processes = 4

    .. config:option:: http_connections :: Maximum number of HTTP connections

        Maximum number of HTTP connections per replication::

            [replicator]
            http_connections = 20

    .. config:option:: connection_timeout :: Per replication connection timeout

        HTTP connection timeout per replication.
        This is divided by three (3) when the replicator makes changes feed requests.
        Even for very fast/reliable networks it might need to be increased if
        a remote database is too busy::

            [replicator]
            connection_timeout = 30000

    .. config:option:: retries_per_request :: Number of retries per request

        .. versionchanged:: 2.1.1

        If a request fails, the replicator will retry it up to N times. The
        default value for N is 5 (before version 2.1.1 it was 10). The requests
        are retried with a doubling exponential backoff starting at 0.25
        seconds. So by default requests would be retried in 0.25, 0.5, 1, 2, 4
        second intervals. When number of retires is exhausted, the whole
        replication job is stopped and will retry again later::

            [replicator]
            retries_per_request = 5

    .. config:option:: socket_options :: Erlang socket options

        Some socket options that might boost performance in some scenarios:

        - ``{nodelay, boolean()}``
        - ``{sndbuf, integer()}``
        - ``{recbuf, integer()}``
        - ``{priority, integer()}``

        See the `inet`_ Erlang module's man page for the full list of options::

            [replicator]
            socket_options = [{keepalive, true}, {nodelay, false}]

        .. _inet: http://www.erlang.org/doc/man/inet.html#setopts-2

    .. config:option:: checkpoint_interval :: Replication checkpoint interval

        .. versionadded:: 1.6

        Defines replication checkpoint interval in milliseconds.
        :ref:`Replicator <replicator>` will :get:`requests </{db}>` from the
        Source database at the specified interval::

            [replicator]
            checkpoint_interval = 5000

        Lower intervals may be useful for frequently changing data, while
        higher values will lower bandwidth and make fewer requests for
        infrequently updated databases.

    .. config:option:: use_checkpoints :: Use checkpoints during replication

        .. versionadded:: 1.6

        If ``use_checkpoints`` is set to ``true``, CouchDB will make
        checkpoints during replication and at the completion of replication.
        CouchDB can efficiently resume replication from any of these
        checkpoints::

            [replicator]
            use_checkpoints = true

        .. note::
            Checkpoints are stored in :ref:`local documents <api/local>`
            on both the source and target databases (which requires write
            access).

        .. warning::
            Disabling checkpoints is **not recommended** as CouchDB will scan
            the Source database's changes feed from the beginning.

    .. config:option:: cert_file :: Path to user PEM certificate file

        Path to a file containing the user's certificate::

            [replicator]
            cert_file = /full/path/to/server_cert.pem

    .. config:option:: key_file :: Path to private user PEM file

        Path to file containing user's private PEM encoded key::

            [replicator]
            key_file = /full/path/to/server_key.pem

    .. config:option:: password :: Optional password for protected key file

        String containing the user's password. Only used if the private key file
        is password protected::

            [replicator]
            password = somepassword

    .. config:option:: verify_ssl_certificates :: Check peer certificates

        Set to true to validate peer certificates::

            [replicator]
            verify_ssl_certificates = false

    .. config:option:: ssl_trusted_certificates_file :: Trusted peer certificates

        File containing a list of peer trusted certificates (in the PEM
        format)::

            [replicator]
            ssl_trusted_certificates_file = /etc/ssl/certs/ca-certificates.crt

    .. config:option:: ssl_certificate_max_depth :: Maximum peer certificate \
        depth checking size

        Maximum peer certificate depth (must be set even if certificate
        validation is off)::

            [replicator]
            ssl_certificate_max_depth = 3

    .. config:option:: auth_plugins :: List of replicator client authentication plugins

        .. versionadded:: 2.2

        List of replicator client authentication plugins. Plugins will
        be tried in order and the first to initialize successfully will
        be used. By default there are two plugins available:
        `couch_replicator_auth_session` implementing session (cookie)
        authentication, and `couch_replicator_auth_noop` implementing basic
        authentication. For backwards compatibility, the no-op plugin should be used at
        the end of the plugin list::

          [replicator]
          auth_plugins = couch_replicator_auth_session,couch_replicator_auth_noop

    .. config:option:: usage_coeff :: Usage coefficient decay

        .. versionadded:: 3.2.0

        Usage coefficient decays historic fair share usage every
        scheduling cycle. The value must be between 0.0 and 1.0. Lower
        values will ensure historic usage decays quicker and higher
        values means it will be remembered longer::

            [replicator]
            usage_coeff = 0.5

    .. config:option:: priority_coeff :: Priority coefficient decays

        .. versionadded:: 3.2.0

       Priority coefficient decays all the job priorities such that they slowly
       drift towards the front of the run queue. This coefficient defines a maximum
       time window over which this algorithm would operate. For example, if this
       value is too small (0.1), after a few cycles quite a few jobs would end up at
       priority 0, and would render this algorithm useless. The default value of
       0.98 is picked such that if a job ran for one scheduler cycle, then didn't
       get to run for 7 hours, it would still have priority > 0. 7 hours was picked
       as it was close enough to 8 hours which is the default maximum error backoff
       interval::

            [replicator]
            priority_coeff = 0.98

.. _config/replicator.shares:

Fair Share Replicator Share Allocation
======================================

.. config:section:: replicator.shares :: Per-Database Fair Share Allocation

    .. config:option:: $replicator_db :: Value for a replicator database

        .. versionadded:: 3.2.0

        Fair share configuration section. Higher share values results in a
        higher chance that jobs from that db get to run. The default
        value is 100, minimum is 1 and maximum is 1000. The
        configuration may be set even if the database does not exist.

        In this context the option ``$replicator_db`` acts as a placeholder
        for your replicator database name. The default replicator database is
        ``_replicator``. Additional replicator databases can be created. To be
        recognized as such by the system, their database names should end with
        ``/_replicator``. See the :ref:`Replicator Database <replicator>`
        section for more info.

        ::

            [replicator.shares]
            _replicator = 50
            foo/_replicator = 25
            bar/_replicator = 25
