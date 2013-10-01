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

from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.domains import Domain, ObjType, Index
from sphinx.directives import ObjectDescription
from sphinx.util.nodes import make_refnode


class ConfigObject(ObjectDescription):

    def handle_signature(self, sig, signode):
        if '::' in sig:
            name, descr = map(lambda i: i.strip(), sig.split('::'))
        else:
            name, descr = sig.strip(), ''

        signode['name'] = name
        signode['descr'] = descr

        domain, objtype = self.name.split(':')
        if objtype == 'section':
            self.env.temp_data['section'] = signode['name']
            name = '[%s]' % signode['name']

        signode += addnodes.desc_name(name, name)

        return signode['name']

    def needs_arglist(self):
        return False

    def add_target_and_index(self, name, sig, signode):
        section = self.env.temp_data['section']
        domain, objtype = self.name.split(':')
        data = self.env.domaindata[domain][objtype]
        if objtype == 'section':
            data[name] = (self.env.docname, signode['descr'])
            signode['ids'].append(signode['name'])
        elif objtype == 'option':
            idx = '%s/%s' % (section, signode['name'])
            data[idx] = (self.env.docname, signode['descr'])
            signode['ids'].append(idx)
        else:
            assert 'unknown object type %r' % objtype


class ConfigIndex(Index):

    name = 'ref'
    localname = 'Configuration Reference'
    shortname = 'Config Reference'

    def generate(self, docnames=None):
        content = dict(
            (name, [(name, 1, info[0], name, '', '', info[1])])
            for name, info in self.domain.data['section'].items()
        )

        options = self.domain.data['option']
        for idx, info in sorted(options.items()):
            path, descr = info
            section, name = idx.split('/', 1)
            content[section].append((
                name, 2, path,
                '%s/%s' % (section, name),
                '', '', descr
            ))

        return (sorted(content.items()), False)


class ConfigDomain(Domain):

    name = 'config'
    label = 'CONFIG'

    object_types = {
        'section': ObjType('section', 'section', 'obj'),
        'option': ObjType('option', 'option', 'obj'),
    }

    directives = {
        'section': ConfigObject,
        'option': ConfigObject,
    }

    roles = {
        'section': XRefRole(),
        'option': XRefRole(),
    }

    initial_data = {
        'section': {},
        'option': {}
    }

    indices = [ConfigIndex]

    def resolve_xref(self, env, fromdocname, builder, typ, target,
                     node, contnode):
        if typ == 'section':
            info = self.data[typ][target]
            title = '[%s]' % target
        elif typ == 'option':
            assert '/' in target, 'option without section: %r' % target
            section, option = target.split('/', 1)
            info = self.data[typ][target]
            title = option
        else:
            assert 'unknown role %r for target %r' % (typ, target)
        return make_refnode(builder, fromdocname, info[0], target, contnode,
                            title)


def setup(app):
    app.add_domain(ConfigDomain)
