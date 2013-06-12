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

.. _config/database_compaction:

``[database_compaction]`` :: Database Compaction Options
========================================================

These options are under ``[database_compaction]`` section. 

.. _config/database_compaction/doc_buffer_size:

``doc_buffer_size`` :: Documents buffer size
--------------------------------------------

Specifies maximum copy buffer size in bytes::

  [database_compaction]
  doc_buffer_size = 524288


.. _config/database_compaction/checkpoint_after:

``checkpoint_after`` :: Checkpoint trigger
------------------------------------------

Triggers checkpoint after special amount of bytes was successfully copied to
the compacted database::

  [database_compaction]
  checkpoint_after = 5242880

