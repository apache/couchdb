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

========================================
``/db/_design/design-doc/_rewrite/path``
========================================

.. warning::

    Rewrites are deprecated in CouchDB 3.0, and will be removed in CouchDB 4.0.

.. http:any:: /{db}/_design/{ddoc}/_rewrite/{path}
    :synopsis: Rewrites HTTP request for the specified path by using stored
               array of routing rules or JavaScript function

    Rewrites the specified path by rules defined in the specified design
    document. The rewrite rules are defined by the ``rewrites`` field of the
    design document. The ``rewrites`` field can either be a *string* containing
    the a rewrite function or an *array* of rule definitions.

Using a stringified function for ``rewrites``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. versionadded:: 2.0

    When the ``rewrites`` field is a stringified function, the query server is used
    to pre-process and route requests.

    The function takes a :ref:`request2_object`.

    The return value of the function will cause the server to rewrite the
    request to a new location or immediately return a response.

    To rewrite the request, return an object containing the following
    properties:

    - **path** (*string*): Rewritten path.
    - **query** (*array*): Rewritten query. If omitted, the original
      query keys are used.
    - **headers** (*object*): Rewritten headers. If omitted, the original
      request headers are used.
    - **method** (*string*): HTTP method of rewritten request (``"GET"``,
      ``"POST"``, etc). If omitted, the original request method is used.
    - **body** (*string*): Body for ``"POST"``/``"PUT"`` requests. If omitted,
      the original request body is used.

    To immediately respond to the request, return an object containing the
    following properties:

    - **code** (*number*): Returned HTTP status code (``200``, ``404``, etc).
    - **body** (*string*): Body of the response to user.

    **Example A**. Restricting access.

    .. code-block:: javascript

        function(req2) {
          var path = req2.path.slice(4),
            isWrite = /^(put|post|delete)$/i.test(req2.method),
            isFinance = req2.userCtx.roles.indexOf("finance") > -1;
          if (path[0] == "finance" && isWrite && !isFinance) {
            // Deny writes to  DB "finance" for users
            // having no "finance" role
            return {
              code: 403,
              body: JSON.stringify({
                error: "forbidden".
                reason: "You are not allowed to modify docs in this DB"
              })
            };
          }
          // Pass through all other requests
          return { path: "../../../" + path.join("/") };
        }

    **Example B**. Different replies for JSON and HTML requests.

    .. code-block:: javascript

        function(req2) {
          var path = req2.path.slice(4),
            h = headers,
            wantsJson = (h.Accept || "").indexOf("application/json") > -1,
            reply = {};
          if (!wantsJson) {
            // Here we should prepare reply object
            // for plain HTML pages
          } else {
            // Pass through JSON requests
            reply.path = "../../../"+path.join("/");
          }
          return reply;
        }

Using an array of rules for ``rewrites``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    When the ``rewrites`` field is an array of rule objects, the server will
    rewrite the request based on the first matching rule in the array.

    Each rule in the array is an *object* with the following fields:

    - **method** (*string*): HTTP request method to bind the request method to
      the rule. If omitted, uses ``"*"``, which matches all methods.
    - **from** (*string*): The pattern used to compare against the URL and
      define dynamic variables.
    - **to** (*string*): The path to rewrite the URL to. It can contain
      variables depending on binding variables discovered during pattern
      matching and query args (URL args and from the query member).
    - **query** (*object*): Query args passed to the rewritten URL. They may
      contain dynamic variables.

    The ``to`` and ``from`` paths may contains string patterns with leading
    ``:`` or ``*`` characters to define dynamic variables in the match.

    The first rule in the ``rewrites`` array that matches the incoming request
    is used to define the rewrite. To match the incoming request, the
    rule's ``method`` must match the request's HTTP method and the rule's
    ``from`` must match the request's path using the following pattern matching
    logic.

    - The *from* pattern and URL are first split on ``/`` to get a list of
      tokens. For example, if *from* field is ``/somepath/:var/*`` and the URL
      is ``/somepath/a/b/c``, the tokens are ``somepath``, ``:var``, and
      ``*`` for the *from* pattern and ``somepath``, ``a``, ``b``, and
      ``c`` for the URL.
    - Each token starting with ``:`` in the pattern will match the
      corresponding token in the URL and define a new dynamic variable whose
      name is the remaining string after the ``:`` and value is the token from
      the URL. In this example, the ``:var`` token will match ``b``
      and set ``var`` = ``a``.
    - The star token ``*`` in the pattern will match any number of tokens in
      the URL and must be the last token in the pattern. It will define a
      dynamic variable with the remaining tokens. In this example, the ``*``
      token will match the ``b`` and ``c`` tokens and set ``*`` =
      ``b/c``.
    - The remaining tokens must match exactly for the pattern to be considered
      a match. In this example, ``somepath`` in the pattern matches
      ``somepath`` in the URL and all tokens in the URL have matched, causing
      this rule to be a match.

    Once a rule is found, the request URL is rewritten using the ``to`` and
    ``query`` fields. Dynamic variables are substituted into the ``:`` and
    ``*`` variables in these fields to produce the final URL.

    If no rule matches, a :statuscode:`404` response is returned.

    Examples:

    +-----------------------------------+----------+------------------+-------+
    |               Rule                |    URL   |  Rewrite to      | Tokens|
    +===================================+==========+==================+=======+
    | {"from": "/a",                    | /a       | /some            |       |
    |  "to": "/some"}                   |          |                  |       |
    +-----------------------------------+----------+------------------+-------+
    | {"from": "/a/\*",                 | /a/b/c   | /some/b/c        |       |
    |  "to": "/some/\*}                 |          |                  |       |
    +-----------------------------------+----------+------------------+-------+
    | {"from": "/a/b",                  | /a/b?k=v | /some?k=v        | k=v   |
    |  "to": "/some"}                   |          |                  |       |
    +-----------------------------------+----------+------------------+-------+
    | {"from": "/a/b",                  | /a/b     | /some/b?var=b    | var=b |
    |  "to": "/some/:var"}              |          |                  |       |
    +-----------------------------------+----------+------------------+-------+
    | {"from": "/a/:foo/",              | /a/b/c   | /some/b/c?foo=b  | foo=b |
    |  "to": "/some/:foo/"}             |          |                  |       |
    +-----------------------------------+----------+------------------+-------+
    | {"from": "/a/:foo",               | /a/b     | /some/?k=b&foo=b | foo=b |
    |  "to": "/some",                   |          |                  |       |
    |  "query": { "k": ":foo" }}        |          |                  |       |
    +-----------------------------------+----------+------------------+-------+
    | {"from": "/a",                    | /a?foo=b | /some/?b&foo=b   | foo=b |
    |  "to": "/some/:foo"}              |          |                  |       |
    +-----------------------------------+----------+------------------+-------+

    Request method, header, query parameters, request payload and response body
    are dependent on the endpoint to which the URL will be rewritten.

    :param db: Database name
    :param ddoc: Design document name
    :param path: URL path to rewrite
