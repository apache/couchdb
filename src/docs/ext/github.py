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


def get_github_url(app, view, path):
    return "https://github.com/{project}/{view}/{branch}/{path}".format(
        project=app.config.github_project,
        view=view,
        branch=app.config.github_branch,
        path=path,
    )


def html_page_context(app, pagename, templatename, context, doctree):
    # base template for common sphinx pages like search or genindex
    # there is no need to provide github show/edit links for them
    if templatename != "page.html":
        return

    # ok, I'm aware about that this is wrong way to concat url segments
    # but this is one is most portable between 2.x and 3.x versions
    # plus it fits our current requirements. But still, patches are welcome (:
    path = os.path.join(
        app.config.github_docs_path,
        os.path.relpath(doctree.get("source"), app.builder.srcdir),
    )
    context["github_show_url"] = get_github_url(app, "blob", path)
    context["github_edit_url"] = get_github_url(app, "edit", path)


def setup(app):
    app.add_config_value("github_project", "", True)
    app.add_config_value("github_branch", "master", True)
    app.add_config_value("github_docs_path", "", True)
    app.connect("html-page-context", html_page_context)
