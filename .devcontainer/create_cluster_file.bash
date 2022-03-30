#! /bin/bash

#
# create_cluster_file.bash
#
# This source file is part of the FoundationDB open source project
#
# Copyright 2013-2018 Apple Inc. and the FoundationDB project authors
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This script creates a cluster file for a server or client.
# This takes the cluster file path from the FDB_CLUSTER_FILE
# environment variable, with a default of /etc/foundationdb/fdb.cluster
#
# The name of the coordinator must be defined in the FDB_COORDINATOR environment
# variable, and it must be a name that can be resolved through DNS.

function create_cluster_file() {
	FDB_CLUSTER_FILE=${FDB_CLUSTER_FILE:-/etc/foundationdb/fdb.cluster}
	mkdir -p $(dirname $FDB_CLUSTER_FILE)

	if [[ -n "$FDB_CLUSTER_FILE_CONTENTS" ]]; then
		echo "$FDB_CLUSTER_FILE_CONTENTS" > $FDB_CLUSTER_FILE
	elif [[ -n $FDB_COORDINATOR ]]; then
		coordinator_ip=$(getent hosts $FDB_COORDINATOR | awk {'print $1'})
		x=$(getent hosts $FDB_COORDINATOR)
		y=$(getent hosts fdb)
		echo "Output of getent: ${x}"
		echo "Output of getent raw: ${y}"
		echo "coordinator_ip: ${coordinator_ip}"
		if [[ -z "$coordinator_ip" ]]; then
			echo "Failed to look up coordinator address for $FDB_COORDINATOR" 1>&2
			exit 1
		fi
		coordinator_port=${FDB_COORDINATOR_PORT:-4500}
		echo "docker:docker@$coordinator_ip:$coordinator_port" > $FDB_CLUSTER_FILE
	else
		echo "FDB_COORDINATOR environment variable not defined" 1>&2
		exit 1
	fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
	create_cluster_file "$@"
fi
