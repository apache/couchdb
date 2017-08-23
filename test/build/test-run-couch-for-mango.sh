#!/bin/sh
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

./dev/run -n 1 --admin=testuser:testpass &
export SERVER_PID=$!
sleep 10
curl http://dev:15984
cd src/mango/
nosetests

EXIT_STATUS=$?
if [ ! -z "$SERVER_PID" ]; then
  kill $SERVER_PID
fi
exit $EXIT_STATUS
