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

========================
Miscellaneous Parameters
========================

.. _config/attachments:

Configuration of Attachment Storage
===================================

.. config:section:: attachments :: Configuration of Attachment Storage

    .. config:option:: compression_level :: Set zlib compression level

        Defines zlib compression level for the attachments from ``1`` (lowest,
        fastest) to ``9`` (highest, slowest). A value of ``0`` disables
        compression::

            [attachments]
            compression_level = 8

    .. config:option:: compressible_types :: List of attachment types for compression

        Since compression is ineffective for some types of files, it is
        possible to let CouchDB compress only some types of attachments,
        specified by their MIME type::

            [attachments]
            compressible_types = text/*, application/javascript, application/json, application/xml

.. _config/stats:

Statistic Calculation
=====================

.. config:section:: stats :: Statistic Calculation

    .. config:option:: interval :: Statistics gathering interval

        Interval between gathering statistics in seconds::

            [stats]
            interval = 10

.. _config/uuids:

UUIDs Configuration
===================

.. config:section:: uuids :: UUIDs Configuration

    .. config:option:: algorithm :: Generation Algorithm

        .. versionchanged:: 1.3 Added ``utc_id`` algorithm.

        CouchDB provides various algorithms to generate the UUID values that
        are  used for document `_id`'s by default::

            [uuids]
            algorithm = sequential

        Available algorithms:

        - ``random``: 128 bits of random awesome. All awesome, all the time:

          .. code-block:: javascript

              {
                  "uuids": [
                      "5fcbbf2cb171b1d5c3bc6df3d4affb32",
                      "9115e0942372a87a977f1caf30b2ac29",
                      "3840b51b0b81b46cab99384d5cd106e3",
                      "b848dbdeb422164babf2705ac18173e1",
                      "b7a8566af7e0fc02404bb676b47c3bf7",
                      "a006879afdcae324d70e925c420c860d",
                      "5f7716ee487cc4083545d4ca02cd45d4",
                      "35fdd1c8346c22ccc43cc45cd632e6d6",
                      "97bbdb4a1c7166682dc026e1ac97a64c",
                      "eb242b506a6ae330bda6969bb2677079"
                  ]
              }

        - ``sequential``: Monotonically increasing ids with random increments.
          The first 26 hex characters are random, the last 6 increment in
          random amounts until an overflow occurs. On overflow, the random
          prefix is regenerated and the process starts over.

          .. code-block:: javascript

              {
                  "uuids": [
                      "4e17c12963f4bee0e6ec90da54804894",
                      "4e17c12963f4bee0e6ec90da5480512f",
                      "4e17c12963f4bee0e6ec90da54805c25",
                      "4e17c12963f4bee0e6ec90da54806ba1",
                      "4e17c12963f4bee0e6ec90da548072b3",
                      "4e17c12963f4bee0e6ec90da54807609",
                      "4e17c12963f4bee0e6ec90da54807718",
                      "4e17c12963f4bee0e6ec90da54807754",
                      "4e17c12963f4bee0e6ec90da54807e5d",
                      "4e17c12963f4bee0e6ec90da54808d28"
                  ]
              }

        - ``utc_random``: The time since Jan 1, 1970 UTC, in microseconds. The
          first 14 characters are the time in hex. The last 18 are random.

          .. code-block:: javascript

              {
                  "uuids": [
                      "04dd32b3af699659b6db9486a9c58c62",
                      "04dd32b3af69bb1c2ac7ebfee0a50d88",
                      "04dd32b3af69d8591b99a8e86a76e0fb",
                      "04dd32b3af69f4a18a76efd89867f4f4",
                      "04dd32b3af6a1f7925001274bbfde952",
                      "04dd32b3af6a3fe8ea9b120ed906a57f",
                      "04dd32b3af6a5b5c518809d3d4b76654",
                      "04dd32b3af6a78f6ab32f1e928593c73",
                      "04dd32b3af6a99916c665d6bbf857475",
                      "04dd32b3af6ab558dd3f2c0afacb7d66"
                  ]
              }

        - ``utc_id``: The time since Jan 1, 1970 UTC, in microseconds, plus the
          ``utc_id_suffix`` string. The first 14 characters are the time in
          hex. The :option:`uuids/utc_id_suffix` string value is appended to
          these.

          .. code-block:: javascript

              {
                  "uuids": [
                      "04dd32bd5eabcc@mycouch",
                      "04dd32bd5eabee@mycouch",
                      "04dd32bd5eac05@mycouch",
                      "04dd32bd5eac28@mycouch",
                      "04dd32bd5eac43@mycouch",
                      "04dd32bd5eac58@mycouch",
                      "04dd32bd5eac6e@mycouch",
                      "04dd32bd5eac84@mycouch",
                      "04dd32bd5eac98@mycouch",
                      "04dd32bd5eacad@mycouch"
                  ]
              }

        .. note::
            **Impact of UUID choices:** the choice of UUID has a significant
            impact on the layout of the B-tree, prior to compaction.

            For example, using a sequential UUID algorithm while uploading a
            large batch of documents will avoid the need to rewrite many
            intermediate B-tree nodes. A random UUID algorithm may require
            rewriting intermediate nodes on a regular basis, resulting in
            significantly decreased throughput and wasted disk space space due to
            the append-only B-tree design.

            It is generally recommended to set your own UUIDs, or use the
            sequential algorithm unless you have a specific need and take into
            account the likely need for compaction to re-balance the B-tree and
            reclaim wasted space.

    .. config:option:: utc_id_suffix :: UTC ID Suffix

        .. versionadded:: 1.3

        The ``utc_id_suffix`` value will be appended to UUIDs generated by the
        ``utc_id`` algorithm. Replicating instances should have unique
        ``utc_id_suffix`` values to ensure uniqueness of ``utc_id`` ids. ::

            [uuid]
            utc_id_suffix = my-awesome-suffix

    .. config:option:: max_count :: Per-Request UUID Limit

        .. versionadded:: 1.5.1

        No more than this number of UUIDs will be sent in a single request. If
        more UUIDs are requested, an HTTP error response will be thrown. ::

            [uuid]
            max_count = 1000

.. _config/vendor:

Vendor information
==================

.. config:section:: vendor :: Vendor information

    .. versionadded:: 1.3

    CouchDB distributors have the option of customizing CouchDB's welcome
    message. This is returned when requesting ``GET /``. ::

        [vendor]
        name = The Apache Software Foundation
        version = 1.5.0

.. _config/csp:

Content-Security-Policy
=======================

.. config:section:: csp :: Content-Security-Policy

    You can configure ``Content-Security-Policy`` header for Fauxton, attachments and
    show/list functions separately. See `MDN Content-Security-Policy <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy>`_
    for more details on CSP.

    .. config:option:: utils_enable :: Enable Content-Security-Policy header (Fauxton)

        Enable the sending of the header ``Content-Security-Policy`` for ``/_utils``.
        Defaults to ``true``::

            [csp]
            utils_enable = true

    .. config:option:: utils_header_value :: Set CSP-Header value (Fauxton)

        Specifies the exact header value to send. Defaults to::

            [csp]
            utils_header_value = default-src 'self'; img-src 'self'; font-src *; script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';

    .. config:option:: attachments_enable :: Enable CSP-Header (attachments)

        Enable sending the ``Content-Security-Policy`` header for attachments::

            [csp]
            attachments_enable = true

    .. config:option:: attachments_header_value :: Set CSP-Header value (attachments)

        Specifies the exact header value to send. Defaults to::

            [csp]
            attachments_header_value = sandbox

    .. config:option:: showlist_enable :: Enable CSP-Header (Show/List-Functions)

        Enable sending the ``Content-Security-Policy`` header for show and list functions::

            [csp]
            showlist_enable = true

    .. config:option:: showlist_header_value :: Set CSP-Header value (Show/List-Functions)

        Specifies the exact header value to send. Defaults to::

            [csp]
            showlist_header_value = sandbox

    The pre 3.2.0 behaviour is still honoured, but we recommend updating
    to the new format.

    Experimental support of CSP headers for ``/_utils`` (Fauxton).

    .. config:option:: enable :: Enable CSP-Header (deprecated)

        Enable the sending of the Header ``Content-Security-Policy``::

            [csp]
            enable = true

    .. config:option:: header_value :: Set CSP-Header value (deprecated)

        You can change the default value for the Header which is sent::

            [csp]
            header_value = default-src 'self'; img-src *; font-src *;

.. _config/purge:

Configuration of Database Purge
===============================

.. config:section:: purge :: Configuration of Database Purge

    .. config:option:: max_document_id_number :: Allowed number of documents \
        per Delete-Request

        .. versionadded:: 3.0

        Sets the maximum number of documents allowed in a single purge request::

            [purge]
            max_document_id_number = 100

    .. config:option:: max_revisions_number :: Allowed number of accumulated \
        revisions per Purge-Request

        .. versionadded:: 3.0

        Sets the maximum number of accumulated revisions allowed in a single purge
        request::

            [purge]
            max_revisions_number = 1000

    .. config:option:: index_lag_warn_seconds :: Allowed duration for purge \
        checkpoint document

        .. versionadded:: 3.0

        Sets the allowed duration when index is not updated for local purge checkpoint
        document. Default is 24 hours::

            [purge]
            index_lag_warn_seconds = 86400

.. _config/prometheus:

Configuration of Prometheus Endpoint
====================================

.. config:section:: prometheus :: Configuration of Prometheus Options

    .. config:option:: additional_port :: Enable a separate, non-authenticated port \
        for prometheus data

        .. versionadded:: 3.2

        Sets whether or not to create a separate, non-authenticated port (default is false)::

            [prometheus]
            additional_port = true

    .. config:option:: bind_address :: IP address binding

        .. versionadded:: 3.2

        The IP address to bind::

            [prometheus]
            bind_address = 127.0.0.1

    .. config:option:: port :: Port for querying Prometheus data without authentication

        .. versionadded:: 3.2

        The port on which clients can query prometheus endpoint data without authentication::

            [prometheus]
            port = 17986
