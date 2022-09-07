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

.. _cve:

======================
Security Issues / CVEs
======================

.. toctree::
    :maxdepth: 1
    :glob:

    *

.. _cve/report:

===================================================
Reporting New Security Problems with Apache CouchDB
===================================================

The Apache Software Foundation takes a very active stance in eliminating
security problems and denial of service attacks against Apache CouchDB.

We strongly encourage folks to report such problems to our private security
mailing list first, before disclosing them in a public forum.

Please note that the security mailing list should only be used for reporting
undisclosed security vulnerabilities in Apache CouchDB and managing the
process of fixing such vulnerabilities. We cannot accept regular bug reports
or other queries at this address. All mail sent to this address that does not
relate to an undisclosed security problem in the Apache CouchDB source code
will be ignored.

If you need to report a bug that isn't an undisclosed security vulnerability,
please use the `bug reporting page`_.

Questions about:

- How to configure CouchDB securely
- If a vulnerability applies to your particular application
- Obtaining further information on a published vulnerability
- Availability of patches and/or new releases

should be address to the `users mailing list`_. Please see the `mailing
lists page`_ for details of how to subscribe.

The private security mailing address is: `security@couchdb.apache.org`_

Please read `how the Apache Software Foundation handles security`_ reports to
know what to expect.

Note that all networked servers are subject to denial of service attacks,
and we cannot promise magic workarounds to generic problems (such as a client
streaming lots of data to your server, or re-requesting the same URL
repeatedly). In general our philosophy is to avoid any attacks which can
cause the server to consume resources in a non-linear relationship to the
size of inputs.

.. _bug reporting page: https://github.com/apache/couchdb/issues
.. _mailing lists page: http://couchdb.apache.org/#mailing-list
.. _how the Apache Software Foundation handles security: http://apache.org/security/committers.html
.. _security@couchdb.apache.org: mailto:security@couchdb.apache.org
.. _users mailing list: mailto:user@couchdb.apache.org
