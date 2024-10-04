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
Scanner
=======

Configure background scanning plugins.

.. _config/scanner:

.. versionadded:: 3.4

Scanner Options
===============

.. config:section:: couch_scanner :: Scanner Options

    .. config:option:: interval_sec

        How often to check for configuration changes and start/stop plugins.
        The default is 5 seconds. ::

            [couch_scanner]
            interval_sec = 5

    .. config:option:: min_penalty_sec

        Minimum time to force a plugin to wait before running again after a
        crash. Defaults to 30 seconds. ::

            [couch_scanner]
            min_penalty_sec = 30

    .. config:option:: max_penalty_sec

        Maximum time to force a plugin to wait after repeated crashes. The
        default is 8 hours (in seconds). ::

            [couch_scanner]
            min_penalty_sec = 28800

    .. config:option:: heal_threshold_sec

        If plugin runs successfully without crashing for this long, reset its
        repeated error count. Defaults to 5 minutes (in seconds). ::

            [couch_scanner]
            heal_threshold_sec = 300

    .. config:option:: db_rate_limit

        Database processing rate limit. This will also be the rate at which
        design documents are fetched. The rate is shared across all running
        plugins. ::

            [couch_scanner]
            db_rate_limit = 25

    .. config:option:: shard_rate_limit

       Limits the rate at which plugins may open db shard files on a node. The
       rate is shared across all running plugins. ::

            [couch_scanner]
            db_shard_limit = 50

    .. config:option:: doc_rate_limit

        Limit the rate at which plugins open documents. The rate is shared
        across all running plugins. ::

            [couch_scanner]
            doc_rate_limit = 1000

.. config:section:: couch_scanner_plugins :: Enable Scanner Plugins

    .. config:option:: {plugin}

        Which plugins are enabled. By default plugins are disabled. ::

            [couch_scanner_plugins]
            couch_scanner_plugin_ddoc_features = false
            couch_scanner_plugin_find = false
            couch_quickjs_scanner_plugin = false

.. config:section:: {plugin} :: General Plugin Settings

These settings apply to all the plugins. Some plugins may also have individual
settings in their ``[{plugin}]`` section.

    .. config:option:: after

        Run plugin on or after this time. The default is to run once after the
        node starts. Possible time formats are: unix seconds
        (ex. ``1712338014``) or date/time: ``YYYY-MM-DD``, ``YYYY-MM-DDTHH``,
        ``YYYY-MM-DDTHH:MM``. Times are in UTC. ::

         [{plugin}]
         after = restart

    .. config:option:: repeat

        Run the plugin periodically. By default it will run once after node the
        node starts. Possible period formats are: ``{num}_{timeunit}`` (ex.:
        ``1000_sec``, ``30_min``, ``8_hours``, ``24_hour``, ``2_days``,
        ``3_weeks``, ``1_month``) or ``{weekday}`` (ex.: ``mon``, ``monday``,
        ``Thu``, etc.) ::

          [{plugin}]
          repeat = restart

.. config:section:: {plugin}.skip_dbs :: Skip databases

    .. config:option:: {tag}

        Skip over databases if their names match any of these regexes. ::

            [{plugin}.skip_dbs]
            regex1 = a|b
            regex2 = bar(.*)baz

.. config:section:: {plugin}.skip_ddocs :: Skip design documents

    .. config:option:: {tag}

        Skip over design documents if their DocIDs match any of these regexes. ::

            [{plugin}.skip_ddocs]
            regex1 = x|y|z
            regex2 = c(d|e)f

.. config:section:: {plugin}.skip_docs :: Skip documents

    .. config:option:: {tag}

        Skip over documents if their DocIds match any of the regexes. ::

            [{plugin}.skip_docs]
            regex1 = k|l
            regex2 = mno$

.. config:section:: couch_scanner_plugin_find.regexes :: Configure the "Find" plugin

    .. config:option:: {tag}

        Configure regular expressions to find. The format is {tag} = {regex}
        Reports will be emitted to the log as warnings mentioning only their
        tag. By default, no regular expressions are defined and the plugin will
        run but won't report anything. ::

            [couch_scanner_plugin_find.regexes]
            regex1 = s3cret(1|2|3)
            regex2 = n33dl3

.. config:section:: couch_scanner_plugin_ddoc_features :: Configure the "Design doc features" plugin

    .. config:option:: updates

        Report if design documents have update handlers. Enabled by default. ::

            [couch_scanner_plugin_ddoc_features]
            updates = true

    .. config:option:: shows

        Report if design documents have shows. Enabled by default. ::

            [couch_scanner_plugin_ddoc_features]
            shows = true

    .. config:option:: rewrites

        Report if design documents have rewrites. Enabled by default. ::

            [couch_scanner_plugin_ddoc_features]
            rewrites = true

    .. config:option:: filters

        Report if design documents have Javascript filters. Disabled by default. ::

            [couch_scanner_plugin_ddoc_features]
            filters = false

    .. config:option:: reduce

        Report if design documents have Javascript reduce functions. Disabled by default. ::

            [couch_scanner_plugin_ddoc_features]
            reduce = false

    .. config:option:: validate_doc_update

        Report if design documents have validate document update functions.
        Disabled by default. ::

            [couch_scanner_plugin_ddoc_features]
            validate_doc_update = false

    .. config:option:: run_on_first_node

        Run plugin on the first node or all the nodes. The default is to run
        only on the first node of the cluster. If the value is "false" each
        node of the cluster will process a consistent subset of the databases
        so scanning will go faster but might consume more resources. Report if
        design documents have validate document update functions. ::

            [couch_scanner_plugin_ddoc_features]
            run_on_first_node = true

    .. config:option:: ddoc_report

        Emit reports for each design doc or aggregate them per database.
        Emitting them per design doc will indicate the design document name,
        however if there are too many design documents, that may generate a lot
        of logs. The default is to aggregate reports per database. ::

            [couch_scanner_plugin_ddoc_features]
            ddoc_report = false
