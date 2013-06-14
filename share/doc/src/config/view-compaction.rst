.. Licensed under the Apache License, Version 2.0 (the "License")you may not
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

.. _config/view_compaction:

``[view_compaction]`` :: Views Compaction Options
====================================================

These options are under ``[view_compaction]`` section.

.. _config/view_compaction/keyvalue_buffer_size:

``keyvalue_buffer_size`` :: Key-Values buffer size
--------------------------------------------------

Specifies maximum copy buffer size in bytes used during compaction::

  [view_compaction]
  keyvalue_buffer_size = 2097152

