## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

import os
import sys

sys.path.insert(0, os.path.abspath('../ext'))

extensions = ["sphinx.ext.todo", "sphinx.ext.extlinks", 'github', 'httpdomain']

source_suffix = ".rst"

master_doc = "index"

nitpicky = True

version = "1.4"

release = "1.4.0"

project = u"Apache CouchDB"

copyright = u"2013, The Apache Software Foundation"

highlight_language = "json"

pygments_style = "sphinx"

html_theme = "default"

templates_path = ["../templates"]

html_static_path = ["../static"]

html_title = "Apache CouchDB " + version + " Manual"

html_style = "rtd.css"

html_logo = "../images/logo.png"

html_favicon = "../images/favicon.ico"

html_sidebars = {
    "**": [
        "searchbox.html",
        "localtoc.html",
        "relations.html",
        "utilities.html",
        "help.html",
    ]
}

text_newlines = "native"

latex_documents = [(
    "index",
    "CouchDB.tex",
    project,
    "",
    "manual",
    True
)]

latex_elements = {
    "papersize": "a4paper"
}

texinfo_documents = [(
    "index",
    "CouchDB",
    project,
    "",
    "CouchDB",
    "The Apache CouchDB database",
    "Databases",
    True
)]

extlinks = {
    'issue': ('https://issues.apache.org/jira/browse/COUCHDB-%s', 'COUCHDB-'),
    'commit': ('https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=commit;h=%s', '#')
}

github_project = 'apache/couchdb'

github_branch = 'master'

github_docs_path = 'share/doc/src'
