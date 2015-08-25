# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

import random
from string import digits, hexdigits

def joiner(*items):
    return ''.join(item() for item in items)  

def roll(item, n1, n2=None):
    n2 = n2 or n1
    return lambda: ''.join(item() for _ in xrange(random.randint(n1, n2)))

def rand(collection):
    return lambda: random.choice(collection)

def floating_point_literal():
    return joiner(roll(rand(' '), 0, 2),
            roll(rand('+-'), 0, 1),
            roll(rand(digits), 2, 20),
            rand('.'),
            roll(rand(digits), 2, 20),
            roll(rand('eE'), 1, 1),
            roll(rand('+-'), 0, 1),
            roll(rand(digits), 2, 20),
            roll(rand('fF'), 0, 1),
            roll(rand(' '), 0, 2))

def hex_floating_point_literal():
    return joiner(roll(rand(' '), 0, 2),
            rand('0'),
            roll(rand('xX'), 1, 1),
            roll(rand(hexdigits), 1, 6),
            rand('.'),
            roll(rand(hexdigits), 1, 7),
            roll(rand('pP'), 1, 1),
            roll(rand('+-'), 0, 1),
            roll(rand(digits), 1, 4),
            roll(rand('fFdD'), 0, 1),
            roll(rand(' '), 0, 2))