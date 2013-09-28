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


.. _api/ddoc/rewrite:

``/db/_design/design-doc/_rewrite/path``
========================================

.. http:any:: /{db}/_design/{ddoc}/_rewrite/{path}

  Rewrites the specified path by rules defined in the specified design document.

  The rewrite rules are defined in *array* field of the design document called
  ``rewrites``. Each rule is an *object* with next structure:

  - **from** (*string*): The path rule used to bind current uri to the rule.
    It use pattern matching for that
  - **to** (*string*): Rule to rewrite an url. It can contain variables
    depending on  binding variables discovered during pattern matching and
    query args (url args and from the query member)
  - **method** (*string*): Method to bind the request method to the rule.
    Default is ``"*"``
  - **query** (*object*): Query args you want to define they can contain
    dynamic variable by binding the key

  The ``to``and ``from`` paths may contains string patterns with leading ``:``
  or ``*`` characters.

  For example: ``/somepath/:var/*``

  - This path is converted in Erlang list by splitting ``/``
  - Each ``var`` are converted in atom
  - ``""`` are converted to ``''`` atom
  - The pattern matching is done by splitting ``/`` in request url in a list of
    token
  - A string pattern will match equal token
  - The star atom (``'*'`` in single quotes) will match any number of tokens,
    but may only be present as the last `pathterm` in a `pathspec`
  - If all tokens are matched and all `pathterms` are used, then the `pathspec`
    matches

  The pattern matching is done by first matching the request method to a rule.
  By default all methods match a rule. (method is equal to ``"*"`` by default).
  Then It will try to match the path to one rule. If no rule match, then a
  :http:statuscode:`404` response returned.

  Once a rule is found we rewrite the request url using the ``to`` and ``query``
  fields. The identified token are matched to the rule and will replace var.
  If ``'*'`` is found in the rule it will contain the remaining part if it
  exists.
  
  Examples:
  
  +--------------------------------------+----------+------------------+-------+
  |               Rule                   |    Url   |  Rewrite to      | Tokens|
  +======================================+==========+==================+=======+
  | {"from": "/a", "to": "/some"}        | /a       | /some            |       |
  +--------------------------------------+----------+------------------+-------+
  | {"from": "/a/\*", "to": "/some/\*}   | /a/b/c   | /some/b/c        |       |
  +--------------------------------------+----------+------------------+-------+
  | {"from": "/a/b", "to": "/some"}      | /a/b?k=v | /some?k=v        | k=v   |
  +--------------------------------------+----------+------------------+-------+
  | {"from": "/a/b", "to": "/some/:var"} | /a/b     | /some/b?var=b    | var=b |
  +--------------------------------------+----------+------------------+-------+
  | {"from": "/a/:foo/",                 | /a/b/c   | /some/b/c?foo=b  | foo=b |
  | "to": "/some/:foo/"}                 |          |                  |       |
  +--------------------------------------+----------+------------------+-------+
  | {"from": "/a/:foo", "to": "/some",   | /a/b     | /some/?k=b&foo=b | foo=b |
  | "query": { "k": ":foo" }}            |          |                  |       |
  +--------------------------------------+----------+------------------+-------+
  | {"from": "/a", "to": "/some/:foo"}   | /a?foo=b | /some/?b&foo=b   | foo=b |
  +--------------------------------------+----------+------------------+-------+

  Request method, header, query parameters, request payload and response body
  are depended on endpoint to which url will be rewrited.

  :param db: Database name
  :param ddoc: Design document name
  :param path: URL path to rewrite
