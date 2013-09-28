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
import re
import sys

sys.path.insert(0, os.path.abspath('../ext'))

extensions = ["sphinx.ext.todo", "sphinx.ext.extlinks", 'github', 'httpdomain']

_info = {}
_regex = re.compile('m4_define\(\[(.+)\],\s+\[(.+)\]\)')
_acinclude_m4 = '../../../acinclude.m4'
_acinclude_m4_in = '../../../acinclude.m4.in'
if os.path.exists(_acinclude_m4):
    _source = _acinclude_m4
elif os.path.exists(_acinclude_m4_in):
    _source = _acinclude_m4_in
else:
    _source = None
if _source is not None:
    _info = dict(_regex.findall(open(_source).read()))
else:
    raise ValueError('''Project information source wasn't found. We're assume
that it's located within "acinclude.m4" file at the root of the project, but
looks like there is no such file there.''')

source_suffix = ".rst"

nitpicky = True

version = '.'.join([
    _info['LOCAL_VERSION_MAJOR'],
    _info['LOCAL_VERSION_MINOR']
])

release = '.'.join([
    _info['LOCAL_VERSION_MAJOR'],
    _info['LOCAL_VERSION_MINOR'],
    _info['LOCAL_VERSION_REVISION']
]) + (
    _info['LOCAL_VERSION_STAGE'] + '' + _info['LOCAL_VERSION_RELEASE']
    if _info.get('LOCAL_VERSION_RELEASE') == '%revision%'
    else '-dev'
)

project = _info['LOCAL_PACKAGE_NAME']

copyright = '%d, %s' % (
    datetime.datetime.now().year,
    _info['LOCAL_PACKAGE_AUTHOR_NAME']
)

highlight_language = "json"

primary_domain = "http"

pygments_style = "sphinx"

html_theme = "couchdb"

html_theme_path = ['../templates']

templates_path = ["../templates"]

html_static_path = ["../static"]

html_title = ' '.join([
    project,
    version,
    'Documentation'
])

html_style = "rtd.css"

html_logo = "../images/logo.png"

html_favicon = "../images/favicon.ico"

html_use_index = False

html_additional_pages = {
    'download': 'pages/download.html',
    'index': 'pages/index.html'
}

html_context = {}

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
    "contents",
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
    "contents",
    "CouchDB",
    project,
    "",
    "CouchDB",
    "The Apache CouchDB database",
    "Databases",
    True
)]

extlinks = {
    'issue': ('%s-%%s' % _info['LOCAL_BUG_URI'], 'COUCHDB-'),
    'commit': ('https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=commit;h=%s', '#')
}

github_project = 'apache/couchdb'

html_context['git_branch'] = github_branch = 'master'

github_docs_path = 'share/doc/src'

del _info, _regex, _acinclude_m4, _acinclude_m4_in, _source
