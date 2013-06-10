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

.. _config/attachments:

``[attachments]`` :: Configuration of Attachment Storage
========================================================

These options are under ``[attachments]`` section.


.. _config/attachments/compression_level:

``compression_level``
---------------------

Defines zlib compression level for the attachments from ``1`` (lowest, fastest)
to ``9`` (highest, slowest). ``0`` value disables compression::

  [attachments]
  compression_level = 8


.. _config/attachments/compressible_types:

``compressible_types``
----------------------

While not all attached files could be compressed well, it's possible to let
CouchDB compress only specific attachments according by their MIME type::

  [attachments]
  compressible_types = text/*, application/javascript, application/json, application/xml

