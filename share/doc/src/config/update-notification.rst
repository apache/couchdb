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

.. TODO: move `update-notification` reference to special article about this
   feature

.. _update-notifications:
.. _config/update-notification:

``[update_notification]`` :: Update notifications
=================================================

CouchDB is able to spawn OS processes to notify them about recent databases
updates. The notifications are in form of JSON messages sent as a line of text,
terminated by ``CR`` (``\n``) character, to the OS processes through `stdout`::

  [update_notification]
  ;unique notifier name=/full/path/to/exe -with "cmd line arg"
  index_updater = ruby /usr/local/bin/index_updater.rb


The update notification messages are depend upon of event type:

- **Database created**:

  .. code-block:: javascript

    {"type":"created","db":"dbname"}


- **Database updated**:  this event raises when any document gets updated for
  specified database:

  .. code-block:: javascript

    {"type":"updated","db":"dbname"}


- **Design document updated**: for design document updates there is special
  event raised in additional to regular db update one:

  .. code-block:: javascript

    {"type":"ddoc_updated","db":"dbname","id":"_design/ddoc_name"}


- **Database deleted**:

  .. code-block:: javascript

    {"type":"deleted","db":"dbname"}

.. note:: New line (``\n``) trailing character was removed from examples.
