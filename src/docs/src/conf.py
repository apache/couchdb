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
from pathlib import Path

import sphinx_rtd_theme

sys.path.insert(0, os.path.abspath("../ext"))

extensions = [
    "sphinx.ext.todo",
    "sphinx.ext.extlinks",
    "sphinxcontrib.httpdomain",
    "sphinxcontrib.jquery",
    "sphinx_copybutton",
    "configdomain",
]

nitpicky = True

# load version numbers from version.mk
version_file = Path().absolute().joinpath("../../../version.mk").resolve()
with open(version_file) as file:
    props = dict(line.strip().split("=", 1) for line in file)

version = f"{props['vsn_major']}.{props['vsn_minor']}"
release = f"{props['vsn_major']}.{props['vsn_minor']}.{props['vsn_patch']}"

project = "Apache CouchDB\u00ae"

copyright = "%d, %s" % (
    datetime.datetime.now().year,
    "Apache Software Foundation. CouchDB\u00ae is a registered trademark of the "
    + "Apache Software Foundation",
)

primary_domain = "http"

pygments_style = "sphinx"

html_theme = "sphinx_rtd_theme"

html_theme_options = {"canonical_url": "https://docs.couchdb.org/en/stable/"}

templates_path = ["../templates"]

html_static_path = ["../static"]

html_title = " ".join([project, version, "Documentation"])

html_logo = "../images/logo.png"

html_favicon = "../images/favicon.ico"

html_use_index = False

html_additional_pages = {"download": "pages/download.html", "index": "pages/index.html"}

html_context = {
    # Enable the "Edit in GitHub link within the header of each page.
    "display_github": False,
    # Set the following variables to generate the resulting github URL for each page.
    # Format Template: https://{{ github_host|default("github.com") }}/{{ github_user }}/{{ github_repo }}/blob/{{ github_version }}{{ conf_py_path }}{{ pagename }}{{ suffix }}
    "github_version": "main",
    "conf_py_path": "/src/docs/src/",
    "github_user": "apache",
    "github_repo": "couchdb",
    "rtd_ver": os.environ.get("READTHEDOCS_VERSION_NAME") or "latest",
}

http_index_shortname = "API Reference"

http_index_localname = "API Quick Reference"

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
    "issue": ("https://issues.apache.org/jira/browse/COUCHDB-%s", "COUCHDB-%s"),
    "ghissue": ("https://github.com/apache/couchdb/issues/%s", "#%s"),
    "commit": (
        "https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=commit;h=%s",
        "#%s",
    ),
}


def setup(app):
    app.add_css_file("css/rtd_theme.css")
    app.add_css_file("css/tablefix.css")
