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

import datetime
import os
import sys

import sphinx_rtd_theme

sys.path.insert(0, os.path.abspath("../ext"))

needs_sphinx = "1.5"

extensions = [
    "sphinx.ext.todo",
    "sphinx.ext.extlinks",
    "github",
    "httpdomain",
    "configdomain",
]

source_suffix = ".rst"

nitpicky = True

# should be over-written using rebar-inherited settings
version = "3.2"
release = "3.2.0"

project = "Apache CouchDB\u00ae"

copyright = "%d, %s" % (
    datetime.datetime.now().year,
    "Apache Software Foundation. CouchDB\u00ae is a registered trademark of the "
    + "Apache Software Foundation",
)

primary_domain = "http"

pygments_style = "sphinx"

html_theme = "sphinx_rtd_theme"
html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]

html_theme_options = {"canonical_url": "http://docs.couchdb.org/en/stable/"}

templates_path = ["../templates"]

html_static_path = ["../static"]

html_title = " ".join([project, version, "Documentation"])

# html_style = "css/rtd_theme.css"

html_logo = "../images/logo.png"

html_favicon = "../images/favicon.ico"

html_use_index = False

html_additional_pages = {"download": "pages/download.html", "index": "pages/index.html"}

html_context = {
    "ga_code": "UA-658988-6",
    # Enable the "Edit in GitHub link within the header of each page.
    "display_github": False,
    # Set the following variables to generate the resulting github URL for each page.
    # Format Template: https://{{ github_host|default("github.com") }}/{{ github_user }}/{{ github_repo }}/blob/{{ github_version }}{{ conf_py_path }}{{ pagename }}{{ suffix }}
    "github_user": "apache",
    "github_repo": "couchdb-documentation",
    "github_version": "master/src/",
}

master_doc = "index"

text_newlines = "native"

latex_documents = [("index", "CouchDB.tex", project, "", "manual", True)]

latex_elements = {"papersize": "a4paper"}

texinfo_documents = [
    (
        "contents",
        "CouchDB",
        project,
        "",
        "CouchDB",
        "The Apache CouchDB database",
        "Databases",
        True,
    )
]

extlinks = {
    "issue": ("%s-%%s" % "https://issues.apache.org/jira/browse/COUCHDB", "COUCHDB-"),
    "ghissue": ("https://github.com/apache/couchdb/issues/%s", "#"),
    "commit": (
        "https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=commit;h=%s",
        "#",
    ),
}

github_project = "apache/couchdb-documentation"

html_context["git_branch"] = github_branch = "master"

github_docs_path = "src"


def setup(app):
    app.add_css_file("css/rtd_theme.css")
