//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.apache.couchdb.nouveau.core;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

final class StripedLock<K> {

    private final ReadWriteLock[] locks;

    public StripedLock(final int lockCount) {
        this.locks = new ReadWriteLock[lockCount];
        for (int i = 0; i < locks.length; i++) {
            this.locks[i] = new ReentrantReadWriteLock();
        }
    }

    public Lock readLock(final K key) {
        return readWriteLock(key).readLock();
    }

    public Lock writeLock(final K key) {
        return readWriteLock(key).writeLock();
    }

    private ReadWriteLock readWriteLock(final K key) {
        return locks[Math.floorMod(key.hashCode(), locks.length)];
    }
}
