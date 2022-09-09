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
Resharding
==========

.. _config/reshard:

Resharding Configuration
========================

.. config:section:: resharding :: Resharding Configuration

    .. config:option:: max_jobs

        Maximum number of resharding jobs per cluster node. This includes
        completed, failed, and running jobs. If the job appears in the
        _reshard/jobs HTTP API results it will be counted towards the limit.
        When more than ``max_jobs`` jobs have been created, subsequent requests
        will start to fail with the ``max_jobs_exceeded`` error::

             [reshard]
             max_jobs = 48

    .. config:option:: max_history

        Each resharding job maintains a timestamped event log. This setting
        limits the maximum size of that log::

             [reshard]
             max_history = 20

    .. config:option:: max_retries

        How many times to retry shard splitting steps if they fail. For
        example, if indexing or topping off fails, it will be retried up to
        this many times before the whole resharding job fails::

             [reshard]
             max_retries = 1

    .. config:option:: retry_interval_sec

        How long to wait between subsequent retries::

             [reshard]
             retry_interval_sec = 10

    .. config:option:: delete_source

        Indicates if the source shard should be deleted after resharding has
        finished. By default, it is ``true`` as that would recover the space
        utilized by the shard. When debugging or when extra safety is required,
        this can be switched to ``false``::

             [reshard]
             delete_source = true

    .. config:option:: update_shard_map_timeout_sec

        How many seconds to wait for the shard map update operation to
        complete. If there is a large number of shard db changes waiting to
        finish replicating, it might be beneficial to increase this timeout::

            [reshard]
            update_shard_map_timeout_sec = 60

    .. config:option:: source_close_timeout_sec

        How many seconds to wait for the source shard to close. "Close" in this
        context means that client requests which keep the database open have
        all finished::

            [reshard]
            source_close_timeout_sec = 600

    .. config:option:: require_node_param

        Require users to specify a ``node`` parameter when creating resharding
        jobs. This can be used as a safety check to avoid inadvertently
        starting too many resharding jobs by accident::

            [reshard]
            require_node_param = false

    .. config:option:: require_range_param

        Require users to specify a ``range`` parameter when creating resharding
        jobs. This can be used as a safety check to avoid inadvertently
        starting too many resharding jobs by accident::

            [reshard]
            require_range_param = false
