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


.. _install/gentoo:

Installation on Gentoo
======================

The simplest way is to use an the `ebuild`_ and install via `portage`_
(``emerge``). This takes care of dependencies, creating the `couchdb` user,
basically everything you need to get up and running.

.. code-block:: text

    emerge --ask --verbose couchdb


.. _ebuild: http://devmanual.gentoo.org/quickstart/index.html
.. _portage: http://www.gentoo.org/doc/en/handbook/handbook-x86.xml?part=2&chap=1
