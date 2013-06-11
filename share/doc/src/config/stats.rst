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

.. _config/stats:

``[stats]`` :: Statistic Calculation
====================================

Controls rate and intervals of statistic gathering. These options are under
``[stats]`` section.

.. _config/stats/rate:

``rate``
--------

Rate of statistics gathering in milliseconds::

  [stats]
  rate = 1000


.. _config/stats/samples:

``samples``
-----------

Samples are used tracks the mean and standard value deviation within specified
intervals (in seconds)::

  [stats]
  samples = [0, 60, 300, 900]

